#' Export a grid of `plotPlates()` panels to PDF
#'
#' `plotPlateArray()` is a convenience wrapper that loops over every unique
#' `(plateID, media)` pair in `plate_tbl`, generates a single-page
#' [plotPlates()] panel for that subset, and writes:
#'
#' * **One multi-page PDF** (`filename`, default `"all_plates.pdf"`) that
#'   contains *every* panel, each on its own page
#' * **One stand-alone PDF per panel** (named
#'   `"plate_<ID>_<media>.pdf"` inside `output_dir`)
#'
#' The function **inherits all plotting arguments from
#' [plotPlates()]**, passing them straight through, so the appearance of each
#' panel is identical to calling `plotPlates()` directly.
#'
#' @inheritParams plotPlates
#' @param filename Name of the multi-page PDF written to `output_dir`.
#'   Default `"all_plates.pdf"`.
#' @param output_dir Directory into which the PDFs are written.
#'   Created if it does not exist.  Default `"."`.
#' @param width,height Page dimensions in **inches** (landscape).  Defaults
#'   `16 × 8`.
#'
#' @return (Invisibly) `NULL`; called for its side-effects.
#' @export
plotPlateArray <- function(plate_tbl,
                           fill_var        = "colony.size",
                           circle_size     = 5,
                           log_transform   = FALSE,
                           viridis_palette = "viridis",
                           shared_scale    = FALSE,
                           plate_ids       = NULL,
                           filename        = "plateArray.pdf",
                           output_dir      = ".",
                           width           = 16,
                           height          = 8) {

  ## ---- column checks & optional filter ------------------------------- ##
  req_cols <- c("plateID", "media")
  missing  <- setdiff(req_cols, names(plate_tbl))
  if (length(missing))
    usethis::ui_stop("`plate_tbl` is missing column(s): ",
                     paste(missing, collapse = ", "))

  if (!is.null(plate_ids)) {
    absent <- setdiff(plate_ids, plate_tbl$plateID)
    if (length(absent))
      warning("Requested plateID(s) not in data: ",
              paste(absent, collapse = ", "), call. = FALSE)
    plate_tbl <- dplyr::filter(plate_tbl, .data$plateID %in% plate_ids)
  }
  if (!nrow(plate_tbl))
    usethis::ui_stop("No rows remain after filtering by `plate_ids`.")

  ## ---- directory & filenames ----------------------------------------- ##
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  safe_med <- function(x) stringr::str_replace_all(x, "[^[:alnum:]_]+", "_")
  panel_fname <- function(pid, med)
    sprintf("plate_%02d_%s.pdf", pid, safe_med(med))

  master_path <- file.path(output_dir, filename)

  ## ---- open multi-page PDF ------------------------------------------- ##
  grDevices::pdf(master_path,
                 width   = width,
                 height  = height,
                 paper   = "special",
                 onefile = TRUE)
  master_dev <- grDevices::dev.cur()

  ## ---- iterate over plate/media pairs -------------------------------- ##
  combos <- plate_tbl |>
    dplyr::distinct(plateID, media) |>
    dplyr::arrange(plateID, media)

  for (i in seq_len(nrow(combos))) {
    pid <- combos$plateID[i]
    med <- combos$media[i]

    sub_tbl <- dplyr::filter(
      plate_tbl,
      .data$plateID == pid,
      .data$media   == med
    )
    if (nrow(sub_tbl) == 0L) next

    ## stand-alone PDF -------------------------------------------------- ##
    #ind_path <- file.path(output_dir, panel_fname(pid, med))
    # grDevices::pdf(ind_path,
    #                width   = width,
    #                height  = height,
    #                paper   = "special",
    #                onefile = FALSE)
    # ind_dev <- grDevices::dev.cur()

    grob <- plotPlates(
      plate_tbl       = sub_tbl,
      fill_var        = fill_var,
      circle_size     = circle_size,
      log_transform   = log_transform,
      viridis_palette = viridis_palette,
      shared_scale    = shared_scale
    )

    #grDevices::dev.off(ind_dev)              # close stand-alone PDF

    ## add a new page to master PDF ------------------------------------- ##
    grDevices::dev.set(master_dev)
    grid::grid.newpage()
    grid::grid.draw(grob)
  }

  grDevices::dev.off(master_dev)             # close master PDF
  invisible(NULL)
}
