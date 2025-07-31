#' Interactive single-plate widget
#'
#' Shows **one** 96-well plate at a time, coloured by `fill_var`, with
#' client-side drop-downs for *Plate ID*, *Media* and *Time-point*.
#'
#' **New:** the Plotly tooltip now displays
#' ```
#' <colony.value>
#' constructID: <constructID>
#' experiment : <experiment>
#' Condition  : <media> | <timepoint>
#' ```
#'
#' @inheritParams plotPlates
#' @param preserve_aspect Keep the plate’s 85 × 127 mm proportions (`TRUE`).
#' @param width,height Optional fixed widget size in pixels (set to `NA`
#'   to allow the browser to choose).
#' @return An \link[htmltools]{html} widget ready for R Markdown, Quarto,
#'   or any modern browser.
#' @export
interactive_plotPlates <- function(plate_tbl,
                                   fill_var,
                                   circle_size     = 5,
                                   log_transform   = FALSE,
                                   viridis_palette = "viridis",
                                   shared_scale    = FALSE,
                                   preserve_aspect = TRUE,
                                   width           = NA,
                                   height          = NA) {

  ## ---- checks --------------------------------------------------------- ##
  req <- c("plateID", "media", "timepoint",
           "rowID", "colID", "constructID", "experiment",
           fill_var)
  miss <- setdiff(req, names(plate_tbl))
  if (length(miss))
    usethis::ui_stop("`plate_tbl` is missing column(s): ",
                     paste(miss, collapse = ", "))

  ok_pal <- c("viridis", "magma", "plasma", "inferno", "cividis")
  if (!viridis_palette %in% ok_pal)
    usethis::ui_stop("`viridis_palette` must be one of: {paste(ok_pal, collapse=', ')}")

  ## ---- global colour limits ------------------------------------------ ##
  limits <- NULL
  if (shared_scale) {
    rng <- plate_tbl |>
      dplyr::mutate(
        v = .data[[fill_var]],
        v = if (log_transform) ifelse(v > 0, log10(v), NA_real_) else v
      ) |>
      dplyr::summarise(lo = min(v, na.rm = TRUE),
                       hi = max(v, na.rm = TRUE))
    if (is.finite(rng$lo) && is.finite(rng$hi) && rng$lo < rng$hi)
      limits <- c(rng$lo, rng$hi)
    else
      usethis::ui_warn("Shared scale requested, but no finite values after log-transform.")
  }
  breaks_val <- if (!is.null(limits)) pretty(limits, 5) else ggplot2::waiver()

  ## ---- transform & share --------------------------------------------- ##
  plate_df <- plate_tbl |>
    dplyr::mutate(
      .fill_raw = .data[[fill_var]],
      .fill     = if (log_transform)
        ifelse(.fill_raw > 0, log10(.fill_raw), NA_real_)
      else .fill_raw,
      ## tooltip text ---------------------------------------------------- ##
      .tooltip  = sprintf(
        "%s\nconstructID: %s\nexperiment: %s\nCondition: %s | %s",
        .fill_raw, constructID, experiment, media, timepoint
      )
    )

  ## use highlight_key() so the object is data-frame-like (ggplot2 ≥ 3.5 friendly)
  sd <- plotly::highlight_key(plate_df)   # carries its own SharedData internally

  ## ---- build ggplot --------------------------------------------------- ##
  p <- ggplot2::ggplot(
    sd,
    ggplot2::aes(x = .data$colID,
                 y = .data$rowID,
                 fill = .data$.fill,
                 text = .data$.tooltip)
  ) +
    ggplot2::geom_point(shape = 21, size = circle_size, colour = "white") +
    ggplot2::scale_x_continuous(breaks = sort(unique(plate_df$colID))) +
    ggplot2::scale_y_reverse(breaks = sort(unique(plate_df$rowID))) +
    viridis::scale_fill_viridis(
      option = viridis_palette,
      limits = limits,
      breaks = breaks_val,
      oob    = scales::squish
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title      = ggplot2::element_blank(),
      axis.text       = ggplot2::element_text(colour = "gray50"),
      legend.title    = ggplot2::element_blank(),
      legend.position = "left"
    )

  ## ---- convert to plotly --------------------------------------------- ##
  plt <- plotly::ggplotly(
    p,
    width  = if (!is.na(width))  width  else NULL,
    height = if (!is.na(height)) height else NULL,
    tooltip = "text"   # <-- use our custom tooltip
  )

  if (preserve_aspect) {
    ratio <- 85 / 127   # rows / cols  (≈ 0.669)
    plt <- plotly::layout(
      plt,
      yaxis = list(scaleanchor = "x", scaleratio = ratio)
    )
  }

  ## ---- assemble widget ------------------------------------------------ ##
  htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$div(
        style = "display:flex;gap:1rem;margin-bottom:0.5em",
        crosstalk::filter_select("plateID_sel", "Plate ID",
                                 sd, ~plateID, multiple = FALSE),
        crosstalk::filter_select("media_sel", "Media",
                                 sd, ~media, multiple = FALSE),
        crosstalk::filter_select("tp_sel", "Time-point",
                                 sd, ~timepoint, multiple = FALSE)
      ),
      plt
    )
  )
}
