#' Read the PETadex phenotyping results from Plate CSV
#'
#' A convenience wrapper around **readr** that ensures the correct column
#' types and provides a friendly error message if the file is missing.
#'
#' @param file `character(1)`. Path to the CSV file.
#'   Defaults to the bundled example file shipped with **petadex**:
#'   `system.file("extdata", "tmp.csv", package = "petadex")`.
#'
#' @return A [`tibble`][tibble::tibble] with 16 columns:
#' \describe{
#'   \item{p, c, r}{integer. Plate, Column and Row indices.}
#'   \item{gene, plasmid, constructID, twist_order}{character. Meta‑data describing the expressed construct.}
#'   \item{experiment, BHET, timepoint, construct}{character. Assay identifiers.}
#'   \item{colony.size}{integer. Raw colony size in pixels.}
#'   \item{colony.value, intensity.norm.neg.ctrl}{double. Normalised phenotypes.}
#'   \item{DNAseq, ORFseq}{factor. Full nucleotide sequences.}
#' }
#'
#' After loading the file, columns are renamed to the canonical **petadexR**
#' schema.  The `timepoint` column can contain
#'
#' * `<number>[D|d]`   – days
#' * `<number>[H|h]`   – hours
#' * `<number>[M|m]`   – minutes
#' * bare `<number>`   – hours
#'
#' Values are converted to **hours** (numeric, fractional for minutes) and a
#' companion column `time_unit` is inserted _immediately after_ `timepoint`,
#' recording the original unit (`"day"`, `"hour"`, or `"minute"`).
#'
#' @export
#'
#' @examples
#' tbl <- readPlate()
#' dplyr::glimpse(tbl)
readPlate <- function(file) {

  if (!file.exists(file)) {
    usethis::ui_stop("File '{file}' does not exist.")
  }

  plate.tbl <-
    readr::read_csv(
      file,
      progress  = FALSE,
      col_types = readr::cols(
        p                         = readr::col_integer(),
        c                         = readr::col_integer(),
        r                         = readr::col_integer(),
        gene                      = readr::col_character(),
        plasmid                   = readr::col_integer(),
        constructID               = readr::col_character(),
        twist_order               = readr::col_factor(),
        experiment                = readr::col_character(),
        BHET                      = readr::col_factor(),
        timepoint                 = readr::col_character(),   # <- read as char
        construct                 = readr::col_character(),
        colony.size               = readr::col_integer(),
        colony.value              = readr::col_double(),
        intensity.norm.neg.ctrl   = readr::col_double(),
        DNAseq                    = readr::col_factor(),
        ORFseq                    = readr::col_factor()
      )
    )

  colnames(plate.tbl) <- c("plateID", "colID", "rowID",
                           "gene", "plasmid", "constructID", "twist_order",
                           "experiment", "media",
                           "timepoint", "construct",
                           "colony.size", "colony.value", "intensity.norm.neg.ctrl",
                           "DNAseq", "ORFseq")

  ## --------------------------------------------------------------------
  ##  Parse `timepoint`  (new block)
  ## --------------------------------------------------------------------
  tp_raw   <- stringr::str_trim(plate.tbl$timepoint)
  is_blank <- is.na(tp_raw) | tp_raw == ""

  # match number (int/float) + optional unit char
  mat <- stringr::str_match(tp_raw,
                            "^([0-9]+(?:\\.[0-9]+)?)\\s*([DdHhMm]?)$")

  bad <- !is_blank & is.na(mat[, 1])
  if (any(bad)) {
    usethis::ui_stop(
      "Unrecognised `timepoint` value(s): ",
      paste(unique(tp_raw[bad]), collapse = ", ")
    )
  }

  val  <- as.numeric(mat[, 2])
  unit <- tolower(mat[, 3])
  unit[unit == ""] <- "h"                 # default → hours

  tp_hours <- dplyr::case_when(
    unit == "d" ~ val * 24,
    unit == "h" ~ val,
    unit == "m" ~ val / 60,
    TRUE        ~ NA_real_
  )

  time_unit <- dplyr::case_when(
    unit == "d" ~ "day",
    unit == "h" ~ "hour",
    unit == "m" ~ "minute",
    TRUE        ~ NA_character_
  )

  plate.tbl <- plate.tbl |>
    dplyr::mutate(
      timepoint = tp_hours,
      time_unit = time_unit
    ) |>
    dplyr::relocate(time_unit, .after = timepoint)

  plate.tbl <- plate.tbl[ !duplicated(plate.tbl), ]

  return(plate.tbl)
}
