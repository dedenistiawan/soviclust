# =============================================================================
# R/sovi_computation/sovi_computation_server.R
# Server Logic Tab SoVI Computation
#
# DIPANGGIL DARI : server.R via sovi_computation_server(input, output, session, rv)
# DEPENDENSI     :
#   - R/core/sovi_core.R  (run_sovi_core)
#   - R/core/map_core.R   (build_leaflet_sovi)
#   - R/core/helpers.R    (VULN_CLASSES, VULN_PAL)
# =============================================================================

sovi_computation_server <- function(input, output, session, rv, unlock_tab) {

  # ==========================================================================
  # CACHE — Simpan hasil SoVI agar tidak dihitung ulang jika data sama
  # Key: digest::digest() dari seluruh input yang mempengaruhi hasil
  # ==========================================================================
  sovi_cache <- reactiveVal(list(hash = NULL, result = NULL))

  # ==========================================================================
  # OUTPUT: Performance Advisor — estimasi waktu berdasarkan ukuran data
  # ==========================================================================
  output$perf_advisor <- renderUI({
    req(rv$data, rv$sovi_vars)
    n   <- nrow(rv$data)
    p   <- length(rv$sovi_vars)
    est <- dplyr::case_when(
      n > 2000 ~ paste0("\u26a0\ufe0f Dataset besar (", n, " baris \u00d7 ", p, " var). ",
                        "Estimasi PCA: ~10-30 detik."),
      n > 500  ~ paste0("\u23f1 Dataset sedang (", n, " baris \u00d7 ", p, " var). ",
                        "Estimasi PCA: ~3-8 detik."),
      TRUE     ~ paste0("\u26a1 Dataset kecil (", n, " baris \u00d7 ", p, " var). ",
                        "Estimasi PCA: < 2 detik.")
    )
    tags$div(
      class = "info-card",
      style = "padding:8px 14px; margin-bottom:8px; font-size:13px;",
      icon("tachometer-alt"), " ", est,
      if (n > 1000)
        tags$span(style = "color:#e67e22;",
                  tags$br(),
                  icon("lightbulb"),
                  " Tips: Gunakan subset data dulu untuk eksplorasi, lalu run pada dataset lengkap.")
    )
  })

  # ==========================================================================
  # OUTPUT: Progress box (kosong saat awal)
  # ==========================================================================
  output$sovi_progress <- renderUI({ NULL })

  # ==========================================================================
  # OBSERVER: Tombol Run SoVI
  # ==========================================================================
  observeEvent(input$run_sovi, {
    req(rv$data, rv$vars_ok, rv$sovi_vars)

    # ── Buat cache key dari semua input yang mempengaruhi hasil ───────────────
    cache_key <- tryCatch(
      digest::digest(list(
        data_sub  = rv$data[, rv$sovi_vars, drop = FALSE],
        neg_vars  = rv$neg_vars,
        method    = input$direction_method,
        threshold = input$sovi_threshold,
        rotation  = input$pca_rotation
      )),
      error = function(e) NULL
    )

    # ── Cache hit: data & params tidak berubah ─────────────────────────────────
    cached <- sovi_cache()
    if (!is.null(cache_key) &&
        !is.null(cached$hash) &&
        identical(cached$hash, cache_key)) {
      rv$sovi_result <- cached$result
      rv$sovi_ok     <- TRUE
      output$sovi_progress <- renderUI({
        div(class = "progress-box status-ok",
            icon("bolt"), " SoVI diambil dari cache \u2014 data tidak berubah!")
      })
      showNotification(
        "\u26a1 Cache hit! Hasil SoVI sebelumnya digunakan (data & parameter sama).",
        type = "message", duration = 4
      )
      unlock_tab("tab_analysis"); unlock_tab("tab_clustgeo_adv")
      unlock_tab("tab_fgwc");    unlock_tab("tab_lfgwc")
      unlock_tab("tab_alfgwc");  unlock_tab("tab_sovi_analysis")
      unlock_tab("tab_download")
      return()
    }

    # ── Cache miss: hitung ulang ──────────────────────────────────────────────
    output$sovi_progress <- renderUI({
      div(class = "progress-box", style = "background:#cce5ff;",
          icon("spinner", class = "fa-spin"),
          " Menghitung SoVI \u2014 6 fase pipeline...")
    })

    # Progress tracker: run_sovi_core melaporkan posisi absolut (0..1)
    # kita konversi ke incProgress (delta)
    last_pct <- 0
    prog_fn  <- function(pct, msg = "") {
      delta    <- max(0, pct - last_pct)
      last_pct <<- pct
      incProgress(delta, detail = msg)
    }

    withProgress(message = "Running SoVI Pipeline...", value = 0, {

      result <- tryCatch({
        run_sovi_core(
          data              = rv$data,
          sovi_vars         = rv$sovi_vars,
          neg_vars          = rv$neg_vars,
          direction_method  = input$direction_method,
          loading_threshold = input$sovi_threshold,
          pca_rotation      = input$pca_rotation,
          id_col            = input$id_col,
          name_col          = input$name_col,
          progress_fn       = prog_fn
        )
      }, error = function(e) {
        showNotification(paste("Error SoVI:", e$message),
                         type = "error", duration = 10)
        NULL
      })

      rv$sovi_result <- result
      rv$sovi_ok     <- !is.null(result)

      # Simpan ke cache
      if (!is.null(result) && !is.null(cache_key)) {
        sovi_cache(list(hash = cache_key, result = result))
      }
    })

    # Unlock semua tab downstream jika berhasil
    if (isTRUE(rv$sovi_ok)) {
      unlock_tab("tab_analysis");     unlock_tab("tab_clustgeo_adv")
      unlock_tab("tab_fgwc");         unlock_tab("tab_lfgwc")
      unlock_tab("tab_alfgwc");       unlock_tab("tab_sovi_analysis")
      unlock_tab("tab_download")
      showNotification(
        "\u2713 SoVI berhasil dihitung. Tab Extended Analysis, Cluster Analysis & Downloads sudah terbuka.",
        type = "message", duration = 5
      )
    }

    output$sovi_progress <- renderUI({
      if (isTRUE(rv$sovi_ok))
        div(class = "progress-box status-ok",
            icon("check"), " SoVI berhasil dihitung!")
      else
        div(class = "progress-box status-err",
            icon("times"), " Gagal. Periksa data & konfigurasi.")
    })
  })

  # ==========================================================================
  # TAB 1: DIAGNOSTIK PCA
  # ==========================================================================
  
  # Tabel KMO per variabel
  output$pca_kmo_df <- DT::renderDT({
    req(rv$sovi_result)
    pca <- rv$sovi_result$pca_out
    DT::datatable(pca$kmo_df,
                  options  = list(dom = "t", pageLength = 20),
                  rownames = FALSE,
                  caption  = paste0("KMO Overall = ",
                                    round(pca$kmo_overall, 3),
                                    " (", pca$kmo_label, ")"))
  })
  
  # Bartlett's Test
  output$pca_bartlett <- renderPrint({
    req(rv$sovi_result)
    pca <- rv$sovi_result$pca_out
    cat("=== Bartlett's Test of Sphericity ===\n")
    cat("Chi-square :", pca$bartlett_chi, "\n")
    cat("df         :", pca$bartlett_df,  "\n")
    cat("p-value    :",
        format(pca$bartlett_p, scientific = TRUE, digits = 4), "\n")
    if (pca$bartlett_p < 0.05)
      cat("\u2192 SIGNIFIKAN (p < 0.05) \u2014 data layak untuk PCA \u2713")
    else
      cat("\u2192 TIDAK SIGNIFIKAN \u2014 data mungkin tidak layak untuk PCA")
  })
  
  # Communality per variabel
  output$pca_communality <- DT::renderDT({
    req(rv$sovi_result)
    df <- rv$sovi_result$pca_out$comm_df
    DT::datatable(df,
                  options  = list(dom = "t", pageLength = 20),
                  rownames = FALSE,
                  caption  = "Communality per Variabel") |>
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          c("Sangat Baik", "Cukup Baik", "Batas Minimum", "Rendah"),
          c("#d4edda",     "#fff3cd",    "#ffeeba",        "#f8d7da")
        )
      )
  })
  
  # ==========================================================================
  # TAB 2: VARIANSI & LOADING
  # ==========================================================================
  
  # Tabel variansi yang dijelaskan per komponen
  # CATATAN: unclass() diperlukan karena Vaccounted dari psych::principal
  # adalah matrix dengan class "psych". unclass() melepas atribut tersebut
  # agar t() dan as.data.frame() bekerja dengan benar.
  output$pca_variance <- DT::renderDT({
    req(rv$sovi_result)
    vexp <- rv$sovi_result$pca_out$var_expl   # matrix metrik × RC
    mat  <- unclass(vexp)                      # lepas class psych
    df   <- as.data.frame(round(t(mat), 4))   # transpose → RC × metrik
    DT::datatable(df,
                  options  = list(dom = "t", scrollX = TRUE),
                  rownames = TRUE,
                  caption  = paste0("Total Variansi Dijelaskan: ",
                                    rv$sovi_result$pca_out$total_var, "%"))
  })
  
  # Loading matrix
  # CATATAN: unclass() diperlukan karena loadings dari psych::principal
  # adalah objek kelas "loadings" bukan matrix biasa, sehingga as.matrix()
  # langsung akan menghasilkan dimensi yang salah.
  output$pca_loadings <- DT::renderDT({
    req(rv$sovi_result)
    raw_load <- rv$sovi_result$pca_out$loadings
    load_mat <- round(as.matrix(unclass(raw_load)), 4)
    df       <- as.data.frame(load_mat)
    DT::datatable(df,
                  options  = list(dom = "t", scrollX = TRUE),
                  rownames = TRUE,
                  caption  = "Loading Matrix") |>
      DT::formatStyle(
        colnames(df),
        backgroundColor = DT::styleInterval(
          c(-0.5, 0.5),
          c("#FADBD8", "white", "#D5F5E3")
        )
      )
  })
  
  # ==========================================================================
  # TAB 3: ASSIGNMENT VARIABEL
  # ==========================================================================
  
  output$sovi_assignment <- DT::renderDT({
    req(rv$sovi_result)
    df <- rv$sovi_result$selection_out$assignment
    # Filter hanya variabel yang terassign (component tidak NA)
    df <- df[!is.na(df$component), ]
    df <- df[order(df$component), ]
    DT::datatable(df,
                  options  = list(pageLength = 20, dom = "t"),
                  rownames = FALSE,
                  caption  = "Assignment Variabel ke Komponen")
  })
  
  # ==========================================================================
  # TAB 4: SKOR SoVI
  # ==========================================================================
  
  output$sovi_table <- DT::renderDT({
    req(rv$sovi_result)
    sovi_df   <- rv$sovi_result$sovi_df
    rc_cols   <- grep("^RC", names(sovi_df), value = TRUE)
    show_cols <- c(input$id_col, input$name_col,
                   "sovi_raw", "sovi_score", "vuln_class", rc_cols)
    show_cols <- show_cols[show_cols %in% names(sovi_df)]
    show_df   <- sovi_df[, show_cols, drop = FALSE]
    
    # Bulatkan kolom numerik
    num_cols <- intersect(c("sovi_raw", "sovi_score", rc_cols), names(show_df))
    show_df[, num_cols] <- round(show_df[, num_cols, drop = FALSE], 4)
    
    DT::datatable(show_df,
                  filter   = "top",
                  options  = list(pageLength = 15, scrollX = TRUE),
                  rownames = FALSE)
  })
  
  # ==========================================================================
  # TAB 5: DISTRIBUSI KELAS
  # ==========================================================================
  
  output$sovi_class_plot <- renderPlot({
    req(rv$sovi_result)
    sovi_df   <- rv$sovi_result$sovi_df
    tbl       <- as.data.frame(table(Class = sovi_df$vuln_class))
    tbl$Pct   <- round(tbl$Freq / nrow(sovi_df) * 100, 1)
    tbl$Class <- factor(tbl$Class, levels = VULN_CLASSES)
    
    ggplot2::ggplot(tbl, ggplot2::aes(x = Class, y = Freq, fill = Class)) +
      ggplot2::geom_bar(stat = "identity", width = 0.65) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(Freq, "\n(", Pct, "%)")),
        vjust = -0.3, size = 3.5
      ) +
      ggplot2::scale_fill_manual(values = unname(VULN_PAL)) +
      ggplot2::labs(
        title = "Distribusi Kelas Kerentanan",
        x     = NULL,
        y     = "Jumlah Distrik"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x     = ggplot2::element_text(angle = 20, hjust = 1)
      )
  })
  
  output$sovi_class_table <- DT::renderDT({
    req(rv$sovi_result)
    tbl        <- as.data.frame(
      table(Kelas = rv$sovi_result$sovi_df$vuln_class)
    )
    tbl$Persen <- round(tbl$Freq / sum(tbl$Freq) * 100, 1)
    DT::datatable(tbl, options = list(dom = "t"), rownames = FALSE)
  })
  
  # ==========================================================================
  # TAB 6: PETA SoVI (Leaflet)
  # ==========================================================================
  
  output$sovi_map <- leaflet::renderLeaflet({
    req(rv$sovi_result, rv$shp, input$join_shp)
    build_leaflet_sovi(
      sovi_df  = rv$sovi_result$sovi_df,
      shp      = rv$shp,
      join_shp = input$join_shp,
      join_df  = input$id_col,
      name_col = input$name_col
    )
  })
  
  # ==========================================================================
  # TAB 7: TOP 10 DISTRIK PALING RENTAN
  # ==========================================================================
  
  output$sovi_top10 <- DT::renderDT({
    req(rv$sovi_result)
    top10 <- rv$sovi_result$sovi_df |>
      dplyr::arrange(dplyr::desc(sovi_score)) |>
      dplyr::select(dplyr::any_of(c(input$id_col, input$name_col,
                                    "sovi_raw", "sovi_score", "vuln_class"))) |>
      head(10)
    top10$sovi_raw   <- round(top10$sovi_raw,   4)
    top10$sovi_score <- round(top10$sovi_score, 4)
    DT::datatable(top10,
                  options = list(dom = "t"),
                  rownames = FALSE,
                  caption  = "Top 10 Distrik Paling Rentan")
  })
  
} # end sovi_computation_server()