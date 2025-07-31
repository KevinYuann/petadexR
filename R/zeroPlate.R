#' Zero (subtract) the background based on plate negative-control
#'
#' `zeroPlate()` rescales a numeric measurement column (default
#' **`colony.value`**) by subtract every value in a plate table by the
#' **mean value observed in control wells**.
#'
#' Control wells are defined by a match between **`normalize.to`** and
#' the contents of the **`gene`** column.  Normalisation is performed
#' *within* each combination of `plateID`, `media`, and `timepoint`
#' so that different plates, media, or sampling times are treated
#' independently.
#'
#' @param plate.tbl A tibble returned by [`readPlate()`]; must contain
#'   `plateID`, `media`, `timepoint`, `gene`, and the column named in
#'   `value.col`.
#' @param value.col  `character(1)` – name of the **numeric** column to
#'   normalise.  **Default:** `"colony.value"`.
#' @param normalize.to `character(1)` – value in `gene` that identifies
#'   control wells (e.g. `"WT"` or `"Empty"`). **Required.**
#' @param suffix `character(1)` – suffix appended to create the new
#'   column (`"<value.col>_<suffix>"`).  **Default:** `"norm"`.
#' @param nfunc `character(1)` – normalization function to be applied
#'   **Default:** `"subtract"` or `"divide"`
#'
#' @return *plate.tbl* plus one extra numeric column
#'   `<value.col>_<suffix>` holding the normalised values
#'   (`NA` where the control mean is unavailable or zero).
#'
#' @section Warnings:
#' * If **no control wells** are found for a `{plateID, media, timepoint}`
#'   group, that group’s normalised values are set to `NA` and a warning
#'   is emitted.
#' * A **zero control mean** also yields `NA` for the affected rows with
#'   a warning, preventing division-by-zero artefacts.
#'
#' @examples
#' \dontrun{
#' raw_tbl <- readPlate("my_plate.csv")
#'
#' # Normalise colony.value to wells whose gene is "WT"
#' norm_tbl <- zeroPlate(raw_tbl, normalize.to = "WT")
#' }
#'
#' @export
zeroPlate      <- function(plate.tbl,
                           value.col    = "colony.value",
                           normalize.to,
                           suffix       = "norm",
                           nfunc        = "subtract") {

  ## ── sanity checks ────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(plate.tbl),
    length(value.col)    == 1, is.character(value.col),
    length(normalize.to) == 1, is.character(normalize.to)
  )

  key_cols <- c("plateID", "media", "timepoint", "gene")
  missing  <- setdiff(key_cols, names(plate.tbl))
  if (length(missing) > 0) {
    usethis::ui_stop("Input is missing column(s): {paste(missing, collapse = ', ')}")
  }

  if (!value.col %in% names(plate.tbl))
    usethis::ui_stop("Column '{value.col}' not found in input.")

  if (!is.numeric(plate.tbl[[value.col]]))
    usethis::ui_stop("Column '{value.col}' must be numeric.")/

  ## ── compute control means per plate/media/timepoint ──────────────────
  ctrl_means <- plate.tbl |>
    dplyr::filter(.data$gene == normalize.to) |>
    dplyr::group_by(plateID, media, timepoint) |>
    dplyr::summarise(ctrl_mean = mean(.data[[value.col]], na.rm = TRUE),
                     .groups   = "drop")

  ## warn about groups with no controls
  lost <- dplyr::anti_join(
    plate.tbl |> dplyr::distinct(plateID, media, timepoint),
    ctrl_means,
    by = c("plateID", "media", "timepoint")
  )
  if (nrow(lost) > 0)
    usethis::ui_warn(
      "No control wells (gene == '{normalize.to}') for {nrow(lost)} ",
      "plate/media/timepoint group(s); normalised values set to NA."
    )

  ## ── join & normalise ─────────────────────────────────────────────────
  if (nfunc == 'division'){
    out <- plate.tbl |>
      dplyr::left_join(ctrl_means,
                       by = c("plateID", "media", "timepoint")) |>
      dplyr::mutate(
        "{value.col}_{suffix}" := dplyr::if_else(
          is.na(ctrl_mean) | ctrl_mean == 0,
          NA_real_,
          .data[[value.col]] / ctrl_mean
        )
      ) |>
      dplyr::select(-ctrl_mean)
  } else if (nfunc == 'subtract'){
    out <- plate.tbl |>
      dplyr::left_join(ctrl_means,
                       by = c("plateID", "media", "timepoint")) |>
      dplyr::mutate(
        "{value.col}_{suffix}" := .data[[value.col]] - ctrl_mean ) |>
      dplyr::select(-ctrl_mean)
  } #else { usethis::ui_stop("nfunc must be 'subtract' or 'division'") }

  ## division-by-zero check
  if (any(ctrl_means$ctrl_mean == 0, na.rm = TRUE))
    usethis::ui_warn(
      "One or more control means were zero; corresponding normalised values set to NA."
    )

  out
}
