#' Scale a measurement column between plate-level controls
#'
#' `scalePlate()` rescales a numeric measurement column (default
#' **`colony.value`**) so that the **mean** of *negative-control* wells
#' maps to **0** and the **mean** of *positive-control* wells maps to
#' **1**.  All other wells are placed on the same linear scale.
#'
#' Controls are identified via string equality in the **`gene`** column.
#' Scaling is performed **within each** `{plateID, media, timepoint}`
#' triple so that different plates, media, and sampling times remain
#' independent.
#'
#' @inheritParams normalizePlate
#' @param negative.control `character(1)` – value in `gene` that labels
#'   the negative-control wells (baseline **0**).
#' @param positive.control `character(1)` – value in `gene` that labels
#'   the positive-control wells (baseline **1**).
#' @param suffix `character(1)` – suffix for the new column name.
#'
#' @return The input tibble plus one numeric column
#'   `<value.col>_<suffix>` containing the scaled values.
#'
#' @export
scalePlate <- function(plate.tbl,
                       value.col         = "colony.value",
                       negative.control,
                       positive.control,
                       suffix            = "scaled") {

  ## ── sanity checks ────────────────────────────────────────────────────
  stopifnot(
    is.data.frame(plate.tbl),
    length(value.col)        == 1, is.character(value.col),
    length(negative.control) == 1, is.character(negative.control),
    length(positive.control) == 1, is.character(positive.control)
  )

  key_cols <- c("plateID", "media", "timepoint", "gene")
  missing  <- setdiff(key_cols, names(plate.tbl))
  if (length(missing) > 0)
    usethis::ui_stop("Input is missing column(s): {paste(missing, collapse = ', ')}")

  if (!value.col %in% names(plate.tbl))
    usethis::ui_stop("Column '{value.col}' not found in input.")
  if (!is.numeric(plate.tbl[[value.col]]))
    usethis::ui_stop("Column '{value.col}' must be numeric.")

  ## ── compute control means per plate/media/timepoint ────────────────
  ctrl_means <- plate.tbl |>
    dplyr::filter(.data$gene %in% c(negative.control, positive.control)) |>
    dplyr::mutate(ctrl_type = dplyr::case_when(
      .data$gene == negative.control ~ "neg",
      .data$gene == positive.control ~ "pos"
    )) |>
    dplyr::group_by(plateID, media, timepoint, ctrl_type) |>
    dplyr::summarise(ctrl_mean = mean(.data[[value.col]], na.rm = TRUE),
                     .groups   = "drop") |>
    tidyr::pivot_wider(names_from = ctrl_type,
                       values_from = ctrl_mean)

  ## ── warnings for missing controls / zero span ───────────────────────
  if (anyNA(ctrl_means$neg))
    usethis::ui_warn(
      "Negative control ('{negative.control}') missing in {sum(is.na(ctrl_means$neg))} group(s); scaled values set to NA."
    )
  if (anyNA(ctrl_means$pos))
    usethis::ui_warn(
      "Positive control ('{positive.control}') missing in {sum(is.na(ctrl_means$pos))} group(s); scaled values set to NA."
    )
  if (any(!is.na(ctrl_means$neg) & ctrl_means$neg == ctrl_means$pos))
    usethis::ui_warn(
      "Control means equal in {sum(!is.na(ctrl_means$neg) & ctrl_means$neg == ctrl_means$pos)} group(s); scaled values set to NA."
    )

  ## ── merge & scale ──────────────────────────────────────────────────
  plate.tbl |>
    dplyr::left_join(ctrl_means,
                     by = c("plateID", "media", "timepoint")) |>
    dplyr::mutate(
      "{value.col}_{suffix}" := dplyr::case_when(
        is.na(neg) | is.na(pos)   ~ NA_real_,       # missing controls
        neg == pos               ~ NA_real_,       # zero span
        TRUE ~ (.data[[value.col]] - neg) / (pos - neg)
      )
    ) |>
    dplyr::select(-neg, -pos)
}
