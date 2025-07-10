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
#' @export
#'
#' @examples
#' tbl <- readPlate()
#' dplyr::glimpse(tbl)
readPlate <- function(file){

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
        timepoint                 = readr::col_factor(),
        construct                 = readr::col_character(),
        colony.size               = readr::col_integer(),
        colony.value              = readr::col_double(),
        intensity.norm.neg.ctrl   = readr::col_double(),
        DNAseq                    = readr::col_factor(),
        ORFseq                    = readr::col_factor() )
    )

  colnames(plate.tbl) <- c("plateID", "colID", "rowID",
                           "gene", "plasmid", "constructID", "twist_order",
                           "experiment", "media",
                           "timepoint", "construct",
                           "colony.size", "colony.value", "intensity.norm.neg.ctrl",
                           "DNAseq", "ORFseq")

  return(plate.tbl)
}
