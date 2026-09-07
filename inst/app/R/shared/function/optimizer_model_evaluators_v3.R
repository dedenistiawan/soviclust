# =============================================================================
# soviclust — Model-Specific Optimizer Evaluators v3
# Patch v3.2b: LFGWC evaluator
# =============================================================================
#
# Design
# ------
# The canonical metaheuristic engine keeps RAW centroid positions as search
# state. A model-specific evaluator projects each raw candidate exactly once.
#
# LFGWC candidate evaluation:
#
#   raw centroid
#       -> base FCM membership
#       -> one local geographic projection using row-standardized W_std
#       -> projected centroid
#       -> feasible spatial Xie-Beni fitness
#
# One call through `.soviclust_v3_eval()` remains exactly one NFE.
#
# This file intentionally does NOT change:
# - the nine canonical optimizer movement operators;
# - FGWC evaluator behavior;
# - max_nfe / NFE accounting;
# - LFGWC wrapper / Shiny dispatching.
#
# Wrapper integration is deferred to Patch v3.2d.
# =============================================================================


.soviclust_v3_validate_lfgwc_weights <- function(
    W_std,
    n = NULL,
    tol = 1e-8) {

  W_std <- as.matrix(W_std)

  if (
    !is.numeric(W_std) ||
    length(dim(W_std)) != 2L ||
    nrow(W_std) < 2L ||
    nrow(W_std) != ncol(W_std)
  ) {
    stop(
      "`W_std` must be a square numeric matrix.",
      call. = FALSE
    )
  }

  if (any(!is.finite(W_std))) {
    stop(
      "`W_std` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(W_std < 0)) {
    stop(
      "`W_std` must contain non-negative spatial weights.",
      call. = FALSE
    )
  }

  if (
    !is.null(n) &&
    (
      length(n) != 1L ||
      !is.numeric(n) ||
      !is.finite(n) ||
      as.integer(n) != nrow(W_std)
    )
  ) {
    stop(
      "`W_std` dimensions must match the number of observations.",
      call. = FALSE
    )
  }

  if (any(abs(diag(W_std)) > tol)) {
    stop(
      "`W_std` diagonal must be zero for LFGWC neighborhood weights.",
      call. = FALSE
    )
  }

  rs <- rowSums(W_std)

  if (any(!is.finite(rs)) || any(rs <= 0)) {
    stop(
      "Every row of `W_std` must contain positive total neighbor weight.",
      call. = FALSE
    )
  }

  if (any(abs(rs - 1) > tol)) {
    stop(
      "`W_std` must be row-standardized so every row sums to 1.",
      call. = FALSE
    )
  }

  W_std
}


.soviclust_v3_lfgwc_modify <- function(
    membership,
    W_std,
    alpha) {

  membership <- normalize_membership(
    as.matrix(membership)
  )

  if (
    !is.numeric(alpha) ||
    length(alpha) != 1L ||
    !is.finite(alpha) ||
    alpha < 0 ||
    alpha > 1
  ) {
    stop(
      "`alpha` must be one finite value between 0 and 1.",
      call. = FALSE
    )
  }

  W_std <- .soviclust_v3_validate_lfgwc_weights(
    W_std,
    n = nrow(membership)
  )

  neighbor_membership <- W_std %*% membership

  local_membership <- (
    alpha * membership +
      (1 - alpha) * neighbor_membership
  )

  normalize_membership(
    local_membership
  )
}


.soviclust_v3_lfgwc_evaluator <- function(W_std) {

  # Validate structural properties at factory creation time. Observation-count
  # compatibility is validated again when a candidate is evaluated.
  W_std <- .soviclust_v3_validate_lfgwc_weights(
    W_std
  )

  evaluator <- list(
    model_type = "LFGWC",
    fitness_type = "lfgwc_spatial_XB_feasible",
    evaluate = function(
        ctx,
        search_centers,
        m,
        distance,
        order,
        alpha,
        a,
        b,
        require_all_clusters = TRUE) {

      # Current soviclust LFGWC implementation follows Euclidean FCM
      # membership (Grekousis-style LFGWC). Do not silently turn this into a
      # different Minkowski model.
      if (
        !is.character(distance) ||
        length(distance) != 1L ||
        tolower(distance) != "euclidean"
      ) {
        stop(
          "LFGWC model-specific evaluator currently requires `distance = \"euclidean\"`.",
          call. = FALSE
        )
      }

      # `a` and `b` belong to construction of W_std (e.g. SIM-PF). Once W_std
      # is supplied to the evaluator, the local projection itself uses the
      # already row-standardized weights.
      invisible(a)
      invisible(b)

      data <- as.matrix(
        ctx$data
      )

      local_W <- .soviclust_v3_validate_lfgwc_weights(
        W_std,
        n = nrow(data)
      )

      search_centers <- clamp_centroids(
        as.matrix(search_centers),
        data
      )

      base_u <- membership_from_centroids(
        data = data,
        centers = search_centers,
        m = m,
        distance = distance,
        order = order
      )$u

      local_u <- .soviclust_v3_lfgwc_modify(
        membership = base_u,
        W_std = local_W,
        alpha = alpha
      )

      local_centers <- centroid_from_membership(
        data = data,
        uij = local_u,
        m = m
      )

      hard_cluster <- apply(
        local_u,
        1,
        which.max
      )

      occupied <- length(
        unique(hard_cluster)
      )

      requested <- nrow(
        search_centers
      )

      xb <- XB1(
        data = data,
        uij = local_u,
        vi = local_centers,
        m = m
      )

      local_j <- fgwc_objective(
        data = data,
        uij = local_u,
        centers = local_centers,
        m = m,
        distance = distance,
        order = order
      )

      feasible <- (
        is.finite(xb) &&
          (
            !require_all_clusters ||
              occupied == requested
          )
      )

      list(
        search_centers = search_centers,
        membership = local_u,
        centroid = local_centers,
        cluster = hard_cluster,
        occupied_clusters = occupied,
        feasible = feasible,
        fitness = if (feasible) {
          as.numeric(xb)
        } else {
          Inf
        },
        xb = as.numeric(xb),
        spatial_obj = as.numeric(local_j),
        model_type = "LFGWC"
      )
    }
  )

  .soviclust_v3_validate_evaluator(
    evaluator
  )
}
