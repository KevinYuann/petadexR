#' Plot a Plate Layout
#'
#' Given a plate table in Logan format (as returned by [readPlate()]),
#' draws one circle per well arranged by column (`colID`) and row (`rowID`),
#' and colours each well by a chosen variable, optionally log10-transformed,
#' using a viridis-derived palette. The plot panel maintains a fixed aspect ratio,
#' the legend is placed on the left (15% width), and axis titles are removed
#' while tick labels are shown in gray50 at each row/column position.
#'
#' @param plate_tbl A **tibble** as returned by [readPlate()].
#' @param fill_var `character(1)`. Name of a column in `plate_tbl`
#'   (e.g. `"colony.size"` or `"colony.value"`) to map to fill.
#' @param circle_size `numeric(1)`. Size of each well circle. Default `5`.
#' @param log_transform `logical(1)`. If `TRUE`, apply `log10()` to the fill variable. Default `FALSE`.
#' @param viridis_palette `character(1)`. Name of a viridis palette (one of
#'   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`). Default `"viridis"`.
#'
#' @return A `ggplot` object with fixed 127:85 aspect ratio and left-side legend.
#'
#' @examples
#' \dontrun{
#' tbl <- readPlate(system.file("extdata/tmp.csv", package = "petadex"))
#' plotPlate(tbl, fill_var = "colony.value", circle_size = 4,
#'           log_transform = TRUE, viridis_palette = "magma")
#' }
#'
#' @importFrom dplyr arrange mutate
#' @importFrom rlang .data
#' @import ggplot2
#' @import viridis
#' @importFrom grid unit
#' @export
plotPlate <- function(plate_tbl,
                      fill_var,
                      circle_size     = 5,
                      log_transform   = FALSE,
                      viridis_palette = "viridis") {

  # fail early if missing column
  if (!fill_var %in% names(plate_tbl)) {
    usethis::ui_stop("Variable '{fill_var}' not found in plate_tbl.")
  }

  # validate palette
  valid_palettes <- c("viridis", "magma", "plasma", "inferno", "cividis")
  if (!viridis_palette %in% valid_palettes) {
    usethis::ui_stop(
      paste0("`viridis_palette` must be one of: ",
             paste(valid_palettes, collapse = ", "))
    )
  }

  # prepare the fill column and consistent draw order
  plate_df <- plate_tbl %>%
    dplyr::mutate(.fill_raw = .data[[fill_var]]) %>%
    dplyr::mutate(.fill = if (log_transform) log10(.data$.fill_raw) else .data$.fill_raw) %>%
    dplyr::arrange(.data$rowID, .data$colID)

  # build the plot
  ggplot2::ggplot(plate_df, ggplot2::aes(
    x    = colID,
    y    = rowID,
    fill = .fill
  )) +
    ggplot2::geom_point(
      shape  = 21,
      size   = circle_size,
      colour = "black"
    ) +
    # fixed axis ticks at well positions
    ggplot2::scale_x_continuous(breaks = unique(plate_df$colID)) +
    ggplot2::scale_y_reverse(breaks = unique(plate_df$rowID)) +
    viridis::scale_fill_viridis(option = viridis_palette) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth  = grid::unit(0.05, "npc"),
        barheight = grid::unit(0.75,   "npc")
      )
    ) +
    ggplot2::labs(
      fill  = if (log_transform) paste0("log10(", fill_var, ")") else fill_var,
      title = paste(
        "Plate layout coloured by", if (log_transform) paste0("log10(", fill_var, ")") else fill_var
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # no axis titles
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      # tick labels in gray50
      axis.text.x        = element_text(color = "gray50"),
      axis.text.y        = element_text(color = "gray50"),
      # legend layout and fixed aspect
      legend.title         = element_blank(),
      legend.position      = "left",
      legend.justification = "center",
      aspect.ratio         = 85/127,
      legend.margin        = ggplot2::margin(0, 0, 0, 0)
    )
}
