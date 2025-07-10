#' Plot a Single Plate
#' @inheritParams plotPlates
#' @param fill_limits Numeric length-2 or NULL.  When non-NULL the colour scale
#'   is fixed to these limits.
#' @param show_legend Logical; hide legend if FALSE.  Useful when combining many
#'   plates.
#'
#' @return A `ggplot` object.
#'
#' @export
plotPlate <- function(plate_tbl,
                      fill_var,
                      circle_size     = 5,
                      log_transform   = FALSE,
                      viridis_palette = "viridis",
                      fill_limits     = NULL,
                      show_legend     = TRUE) {

  # --- checks ------------------------------------------------------------- #
  if (!"plateID" %in% names(plate_tbl))
    usethis::ui_stop("`plate_tbl` must contain a `plateID` column.")
  if (!fill_var %in% names(plate_tbl))
    usethis::ui_stop("Variable '{fill_var}' not found in `plate_tbl`.")

  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")
  if (!viridis_palette %in% valid_palettes)
    usethis::ui_stop(
      "Argument `viridis_palette` must be one of: {paste(valid_palettes, collapse = ', ')}"
    )

  # --- choose a single plate --------------------------------------------- #
  ids <- unique(plate_tbl$plateID)
  if (length(ids) == 0)
    usethis::ui_stop("`plate_tbl` contains no `plateID` values.")
  if (length(ids) > 1) {
    ids <- sort(ids)
    warning("Multiple plateID values supplied (", paste(ids, collapse = ", "),
            "); plotting only plateID = ", ids[1], call. = FALSE)
  }
  plate_df <- dplyr::filter(plate_tbl, .data$plateID == ids[1])

  # --- prepare data ------------------------------------------------------- #
  plate_df <- plate_df |>
    dplyr::mutate(
      .fill_raw = .data[[fill_var]],
      .fill     = if (log_transform) log10(.data$.fill_raw) else .data$.fill_raw
    )

  title_str <- sprintf(
    "Plate: %s | %s | %s -- %s",
    ids[1],
    paste(unique(plate_df$media),     collapse = "/"),
    paste(unique(plate_df$timepoint), collapse = "/"),
    fill_var
  )

  # --- plotting ----------------------------------------------------------- #
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
