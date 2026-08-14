# =============================================================================
# R/downloads/downloads_server.R
# Server Logic Tab Downloads
#
# DIPANGGIL DARI : server.R via downloads_server(input, output, session, rv)
# BERISI         : Semua downloadHandler untuk CSV dan PNG
# DEPENDENSI     :
#   - R/core/helpers.R  (normalize_id, VULN_CLASSES, VULN_PAL)
#   - Package: dplyr, ggplot2, tmap, RColorBrewer
# =============================================================================

downloads_server <- function(input, output, session, rv) {
  
  # ==========================================================================
  # CSV DOWNLOADS — SoVI Core
  # ==========================================================================
  
  # SoVI Result lengkap
  output$dl_sovi_csv <- downloadHandler(
    filename = function() paste0("sovi_result_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$sovi_result)
      write.csv(rv$sovi_result$sovi_df, file, row.names = FALSE)
    }
  )
  
  # Assignment variabel ke komponen
  output$dl_assignment <- downloadHandler(
    filename = function() paste0("variable_assignment_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$sovi_result)
      df <- rv$sovi_result$selection_out$assignment
      # Hanya simpan variabel yang terassign (component tidak NA)
      write.csv(df[!is.na(df$component), ], file, row.names = FALSE)
    }
  )
  
  # ==========================================================================
  # CSV DOWNLOADS — Extended Analysis
  # ==========================================================================
  
  # Dominant Component
  output$dl_dominant <- downloadHandler(
    filename = function() paste0("dominant_component_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$dominant_df)
      write.csv(rv$dominant_df, file, row.names = FALSE)
    }
  )
  
  # Component Profile
  output$dl_profile <- downloadHandler(
    filename = function() paste0("component_profile_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$profile_df)
      write.csv(rv$profile_df, file, row.names = FALSE)
    }
  )
  
  # Cluster Result (Extended Analysis ClustGeo sederhana)
  output$dl_cluster <- downloadHandler(
    filename = function() paste0("cluster_result_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$cluster_res)
      sovi_df <- rv$cluster_res$sovi_df
      rc_cols <- grep("^RC", names(sovi_df), value = TRUE)
      write.csv(
        sovi_df[, c(input$id_col, input$name_col,
                    "sovi_score", "vuln_class", "cluster", rc_cols)],
        file, row.names = FALSE
      )
    }
  )
  
  # LISA Result
  output$dl_lisa <- downloadHandler(
    filename = function() paste0("lisa_result_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$moran_res)
      out          <- as.data.frame(rv$moran_res$peta)
      out$geometry <- NULL
      keep_cols    <- c(input$join_shp, input$name_col,
                        "sovi_score", "lisa_I", "lisa_p", "lisa_quad")
      keep_cols    <- keep_cols[keep_cols %in% names(out)]
      write.csv(out[, keep_cols], file, row.names = FALSE)
    }
  )
  
  # Sensitivity Analysis
  output$dl_sensitivity <- downloadHandler(
    filename = function() paste0("sensitivity_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$sensitivity, rv$sovi_result)
      s  <- rv$sensitivity$sens_scores
      df <- data.frame(
        ID      = rv$sovi_result$sovi_df[[input$id_col]],
        Nama    = rv$sovi_result$sovi_df[[input$name_col]],
        thr_0.5 = s$thr_0.5,
        thr_0.6 = s$thr_0.6,
        thr_0.7 = s$thr_0.7
      )
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Cutter Comparison
  output$dl_cutter <- downloadHandler(
    filename = function() paste0("cutter_comparison_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(rv$cutter_df)
      write.csv(rv$cutter_df$comparison_df, file, row.names = FALSE)
    }
  )
  
  # ==========================================================================
  # PNG DOWNLOADS — Maps
  # ==========================================================================
  
  # Peta SoVI (PNG)
  output$dl_map_sovi <- downloadHandler(
    filename = function() paste0("map_sovi_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$sovi_result, rv$shp)
      sovi_df <- rv$sovi_result$sovi_df
      sovi_df[[input$id_col]] <- normalize_id(sovi_df[[input$id_col]])
      shp     <- rv$shp
      shp[[input$join_shp]]   <- normalize_id(shp[[input$join_shp]])
      
      peta <- dplyr::left_join(shp, sovi_df,
                               by = setNames(input$id_col, input$join_shp))
      peta$vuln_class <- factor(peta$vuln_class,
                                levels = VULN_CLASSES, ordered = TRUE)
      
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(
          fill        = "vuln_class",
          fill.scale  = tmap::tm_scale_categorical(values = unname(VULN_PAL)),
          fill.legend = tmap::tm_legend(title = "Kelas SoVI"),
          col = "grey40", lwd = 0.4
        ) +
        tmap::tm_title("Social Vulnerability Index (SoVI)") +
        tmap::tm_compass(type = "arrow",
                         position = c("left","bottom"), size = 1.5) +
        tmap::tm_scalebar(position = c("left","bottom"), text.size = 0.6) +
        tmap::tm_layout(legend.outside          = TRUE,
                        legend.outside.position = "right")
      
      tmap::tmap_save(m, filename = file,
                      width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )
  
  # Peta Cluster — Extended Analysis (PNG)
  output$dl_map_cluster <- downloadHandler(
    filename = function() paste0("map_cluster_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$cluster_res, rv$shp)
      df    <- rv$cluster_res$sovi_df
      k     <- rv$cluster_res$k
      df[[input$id_col]]    <- normalize_id(df[[input$id_col]])
      shp   <- rv$shp
      shp[[input$join_shp]] <- normalize_id(shp[[input$join_shp]])
      
      peta         <- dplyr::left_join(shp, df,
                                       by = setNames(input$id_col, input$join_shp))
      peta$cluster <- as.factor(peta$cluster)
      pal_c        <- RColorBrewer::brewer.pal(max(k, 3), "Set2")[1:k]
      
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(
          fill        = "cluster",
          fill.scale  = tmap::tm_scale_categorical(values = pal_c),
          fill.legend = tmap::tm_legend(title = "Cluster"),
          col = "grey40", lwd = 0.3
        ) +
        tmap::tm_title("Vulnerability Typology \u2014 ClustGeo") +
        tmap::tm_layout(legend.outside          = TRUE,
                        legend.outside.position = "right")
      
      tmap::tmap_save(m, filename = file,
                      width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )
  
  # Peta LISA (PNG)
  output$dl_map_lisa <- downloadHandler(
    filename = function() paste0("map_lisa_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$moran_res)
      peta        <- rv$moran_res$peta
      lisa_colors <- rv$moran_res$lisa_colors
      peta$lisa_quad <- factor(peta$lisa_quad, levels = names(lisa_colors))
      
      tmap::tmap_mode("plot")
      m <- tmap::tm_shape(peta) +
        tmap::tm_polygons(
          fill        = "lisa_quad",
          fill.scale  = tmap::tm_scale_categorical(
            values = unname(lisa_colors)
          ),
          fill.legend = tmap::tm_legend(title = "LISA Cluster"),
          col = "grey40", lwd = 0.3
        ) +
        tmap::tm_title("Local Spatial Autocorrelation (LISA)") +
        tmap::tm_layout(legend.outside          = TRUE,
                        legend.outside.position = "right")
      
      tmap::tmap_save(m, filename = file,
                      width = 3000, height = 2400, dpi = 300)
      tmap::tmap_mode("view")
    }
  )
  
  # ==========================================================================
  # PNG DOWNLOADS — Figures
  # ==========================================================================
  
  # Figure Dominant Component (PNG)
  output$dl_fig_dominant <- downloadHandler(
    filename = function() paste0("fig_dominant_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$dominant_df)
      df  <- rv$dominant_df
      tbl <- as.data.frame(table(Component = df$dom_comp))
      tbl$Pct <- round(tbl$Freq / nrow(df) * 100, 1)
      
      p <- ggplot2::ggplot(
        tbl, ggplot2::aes(x = Component, y = Freq, fill = Component)
      ) +
        ggplot2::geom_bar(stat = "identity", width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(Freq, "\n(", Pct, "%)")),
          vjust = -0.3, size = 3.5
        ) +
        ggplot2::scale_fill_brewer(palette = "Set2") +
        ggplot2::labs(
          title = "Distribusi Komponen Dominan",
          x     = "Komponen",
          y     = "Jumlah Distrik"
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "none")
      
      ggplot2::ggsave(file, p, width = 8, height = 5, dpi = 300)
    }
  )
  
  # Figure Sensitivity Scatter (PNG)
  output$dl_fig_scatter <- downloadHandler(
    filename = function() paste0("fig_sensitivity_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$sensitivity)
      df   <- rv$sensitivity$scores_df
      corr <- rv$sensitivity$corr_mat
      
      p <- ggplot2::ggplot(df) +
        ggplot2::geom_point(
          ggplot2::aes(x = thr_0.5, y = thr_0.6,
                       color = "\u03c4=0.5 vs 0.6"),
          alpha = 0.4, size = 0.9
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = thr_0.5, y = thr_0.7,
                       color = "\u03c4=0.5 vs 0.7"),
          alpha = 0.4, size = 0.9
        ) +
        ggplot2::geom_abline(linetype = "dashed", color = "grey60") +
        ggplot2::annotate("text", x = 0.05, y = 0.93,
                          label = paste0(
                            "\u03c1(0.5,0.6) = ", round(corr["thr_0.5","thr_0.6"], 3),
                            "\n\u03c1(0.5,0.7) = ", round(corr["thr_0.5","thr_0.7"], 3)
                          ),
                          size = 3.5, hjust = 0
        ) +
        ggplot2::scale_color_manual(
          values = c("\u03c4=0.5 vs 0.6" = "#1f78b4",
                     "\u03c4=0.5 vs 0.7" = "#e31a1c")
        ) +
        ggplot2::labs(
          title = "Sensitivity Analysis",
          x     = "SoVI Score (\u03c4=0.5)",
          y     = "SoVI Score (\u03c4=0.6 / 0.7)",
          color = ""
        ) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")
      
      ggplot2::ggsave(file, p, width = 7, height = 6, dpi = 300)
    }
  )
  
} # end downloads_server()