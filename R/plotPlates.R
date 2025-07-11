#' Plot Multiple Plates – ordered by timepoint
#'
#' Generates one panel per unique `(plateID, media, timepoint)` and combines
#' **all** panels into a single near-square grid.
#' Panels are ordered by **increasing `timepoint`**, then `plateID`, then
#' `media`, so earlier timepoints always appear first.
#'
#' @inheritParams plotPlate
#' @param shared_scale Logical; if `TRUE` apply one min/max colour range across
#'   all panels.
#' @param plate_ids Optional vector restricting which `plateID`s to plot.
#'
#' @return Invisibly returns the `gtable` created by
#'   `gridExtra::grid.arrange()`.  The grid is printed as a side-effect.
#' @export
plotPlates <- function(plate_tbl,
                       fill_var,
                       circle_size     = 5,
                       log_transform   = FALSE,
                       viridis_palette = "viridis",
                       shared_scale    = FALSE,
                       plate_ids       = NULL) {

  ## --- basic checks ---------------------------------------------------- ##
  req <- c("plateID", "media", "timepoint", "rowID", "colID", fill_var)
  miss <- setdiff(req, names(plate_tbl))
  if (length(miss))
    usethis::ui_stop("`plate_tbl` is missing column(s): ",
                     paste(miss, collapse = ", "))

  ## --- optional plate filter ------------------------------------------ ##
  if (!is.null(plate_ids)) {
    absent <- setdiff(plate_ids, plate_tbl$plateID)
    if (length(absent))
      warning("Requested plateID(s) not found: ",
              paste(absent, collapse = ", "), call. = FALSE)
    plate_tbl <- dplyr::filter(plate_tbl, .data$plateID %in% plate_ids)
  }
  if (!nrow(plate_tbl))
    usethis::ui_stop("No data remain after filtering by `plate_ids`.")

  ## --- global colour limits (optional) -------------------------------- ##
  limits <- NULL
  if (shared_scale) {
    rng <- plate_tbl |>
      dplyr::mutate(
        v = .data[[fill_var]],
        v = if (log_transform) ifelse(v > 0, log10(v), NA_real_) else v
      ) |>
      dplyr::summarise(lo = min(v, na.rm = TRUE),
                       hi = max(v, na.rm = TRUE))

    if (is.finite(rng$lo) && is.finite(rng$hi) && rng$lo < rng$hi) {
      limits <- c(rng$lo, rng$hi)
    } else {
      usethis::ui_warn("Shared scale requested, but no finite values after log-transform.")
      limits <- NULL
    }
  }

  ## --- unique combos, sorted by timepoint ----------------------------- ##
  combos <- plate_tbl |>
    dplyr::distinct(plateID, media, timepoint) |>
    dplyr::arrange(timepoint, plateID, media)

  n_plot <- nrow(combos)
  ncol   <- ceiling(sqrt(n_plot))
  nrow   <- ceiling(n_plot / ncol)

  ## --- build each panel ----------------------------------------------- ##
  plots <- purrr::map(seq_len(n_plot), function(i) {
    combo <- combos[i, ]

    sub_tbl <- dplyr::filter(
      plate_tbl,
      .data$plateID   == combo$plateID   &
        .data$media     == combo$media     &
        .data$timepoint == combo$timepoint
    )

    plotPlate(
      plate_tbl       = sub_tbl,
      fill_var        = fill_var,
      circle_size     = circle_size,
      log_transform   = log_transform,
      viridis_palette = viridis_palette,
      fill_limits     = limits,
      show_legend     = TRUE
    )
  })

  ## --- assemble single grid ------------------------------------------- ##
  grid_obj <- gridExtra::grid.arrange(
    grobs = plots,
    ncol  = ncol,
    nrow  = nrow
  )

  invisible(grid_obj)
}
