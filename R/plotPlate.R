#' Plot a Single Plate
#'
#' Generates a plate map for one `plateID`, colouring wells by a chosen
#' variable.  When a caller supplies `fill_limits`, those limits (and matching
#' breaks) are used so that several plates can share a common colour scale.
#'
#' @inheritParams plotPlates
#' @param fill_limits `numeric(2)` or `NULL`.  If not‐NULL, fixes the lower /
#'   upper limits of the colour palette and uses *identical legend breaks*.
#' @param show_legend `logical(1)`.  If `FALSE` the legend is suppressed.  This
#'   lets a wrapper display a single shared legend.
#'
#' @return A `ggplot2` object.
#' @export
plotPlate <- function(plate_tbl,
                      fill_var,
                      circle_size     = 5,
                      log_transform   = FALSE,
                      viridis_palette = "viridis",
                      fill_limits     = NULL,
                      show_legend     = TRUE) {

  ## ---- checks ---------------------------------------------------------- ##
  if (!"plateID"  %in% names(plate_tbl))
    usethis::ui_stop("`plate_tbl` must contain a `plateID` column.")
  if (!fill_var   %in% names(plate_tbl))
    usethis::ui_stop("Variable '{fill_var}' not found in `plate_tbl`.")

  valid_pal <- c("viridis", "magma", "plasma", "inferno", "cividis")
  if (!viridis_palette %in% valid_pal)
    usethis::ui_stop("`viridis_palette` must be one of: {paste(valid_pal, collapse=', ')}")

  ## ---- pick one plate -------------------------------------------------- ##
  ids <- sort(unique(plate_tbl$plateID))
  if (!length(ids))
    usethis::ui_stop("`plate_tbl` contains no `plateID` values.")
  if (length(ids) > 1) {
    warning("Multiple plateID values supplied (", paste(ids, collapse = ", "),
            "); plotting only plateID = ", ids[1], call. = FALSE)
  }

  ## ---- prepare data ---------------------------------------------------- ##
  plate_df <- plate_tbl |>
    dplyr::mutate(
      .fill_raw = .data[[fill_var]],
      .fill     = if (log_transform) {
        # convert ≤0 to NA, otherwise log10
        ifelse(.fill_raw > 0, log10(.fill_raw), NA_real_)
      } else {
        .fill_raw
      }
    )

  ## ---- title ----------------------------------------------------------- ##
  title_str <- sprintf(
    "Plate: %s | %s | %s -- %s",
    ids[1],
    paste(unique(plate_df$media),     collapse = "/"),
    paste(unique(plate_df$timepoint), collapse = "/"),
    fill_var
  )

  ## ---- shared breaks (for global scale) ------------------------------- ##
  breaks_val <- if (!is.null(fill_limits)) {
    pretty(fill_limits, n = 5)
  } else ggplot2::waiver()

  ## ---- draw ------------------------------------------------------------ ##
  ggplot2::ggplot(
    plate_df,
    ggplot2::aes(x = .data$colID, y = .data$rowID, fill = .data$.fill)
  ) +
    ggplot2::geom_point(shape = 21, size = circle_size, colour = "white") +
    ggplot2::scale_x_continuous(breaks = unique(plate_df$colID)) +
    ggplot2::scale_y_reverse(breaks = unique(plate_df$rowID)) +
    viridis::scale_fill_viridis(
      option = viridis_palette,
      limits = fill_limits,
      breaks = breaks_val,
      oob    = scales::squish
    ) +
    ggplot2::labs(
      title = title_str,
      fill  = if (log_transform) paste0("log10(", fill_var, ")") else fill_var
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title        = ggplot2::element_blank(),
      axis.text         = ggplot2::element_text(colour = "gray50"),
      legend.title      = ggplot2::element_blank(),
      legend.position   = if (show_legend) "left" else "none",
      legend.justification = "center",
      aspect.ratio      = 85/127
    )
}
