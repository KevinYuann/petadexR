#' Aggregate biological replicates in plate data
#'
#' Collapses replicate wells produced by [`readPlate()`] into a single
#' summary row per experimental condition, defined by the combination of
#' **plateID, media, timepoint, and constructID**.  The user supplies the
#' measurement columns to aggregate (defaults are **colony.size** and
#' **colony.value**); for each of those columns the function appends three
#' new variables: `<name>_mean`, `<name>_sd`, and `<name>_n`
#' (number of non-missing replicates contributing to the summary).
#'
#' @param plate.tbl A tibble returned by [`readPlate()`] – must contain
#'   `plateID`, `media`, `timepoint`, and `constructID`.
#' @param value.cols Character vector of column names to summarise.
#'   **Default:** `c("colony.size", "colony.value")`.
#'
#' @return A tibble with one row per unique
#'   \{plateID, media, timepoint, constructID\} combination, plus mean,
#'   SD, and N columns for every variable listed in `value.cols`.
#'
#' @details
#' * Non-numeric `value.cols` are ignored with a warning.
#' * Any `value.cols` missing from the input trigger an error.
#'
#' @examples
#' \dontrun{
#' tbl <- readPlate("experiment.csv")
#' agg <- aggregatePlate(tbl, value.cols = c("fluorescence", "OD600"))
#' }
#'
#' @export
aggregatePlate <- function(plate.tbl,
                           value.cols = c("colony.size", "colony.value")) {

  ## ── sanity checks ──────────────────────────────────────────────────────
  stopifnot(is.data.frame(plate.tbl))

  key_cols      <- c("plateID", "media", "timepoint", "constructID")
  missing_key   <- setdiff(key_cols, names(plate.tbl))
  if (length(missing_key) > 0) {
    usethis::ui_stop(
      "Input is missing required column(s): {paste(missing_key, collapse = ', ')}"
    )
  }

  missing_vals <- setdiff(value.cols, names(plate.tbl))
  if (length(missing_vals) > 0) {
    usethis::ui_stop(
      "Requested value column(s) not found in data: {paste(missing_vals, collapse = ', ')}"
    )
  }

  ## keep only numeric value columns
  numeric_vals <- value.cols[vapply(plate.tbl[value.cols], is.numeric, logical(1))]
  non_numeric  <- setdiff(value.cols, numeric_vals)
  if (length(non_numeric) > 0) {
    usethis::ui_warn(
      "Skipping non-numeric column(s): {paste(non_numeric, collapse = ', ')}"
    )
  }
  if (length(numeric_vals) == 0) {
    usethis::ui_stop("No numeric columns supplied to summarise.")
  }

  ## ── aggregation ────────────────────────────────────────────────────────
  plate.tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(numeric_vals),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ stats::sd(.x, na.rm = TRUE),
          n    = ~ sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
}
