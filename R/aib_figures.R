#' Copy and run the paper figures script
#'
#' @description
#' Copies the paper figures script to your working directory and optionally
#' runs it. The script produces four publication-ready figures illustrating
#' bias trajectory, transition asymmetry, amplification index, and cumulative
#' burden from a toy simulation (N=20, T=3).
#'
#' @param run Logical. If TRUE (default), runs the script immediately.
#'   If FALSE, just copies the file for you to inspect and edit first.
#' @param dest Character. Destination filename. Default \code{file.path(tempdir(), "paper_figures.R")}.
#'
#' @return The path to the copied script, invisibly.
#'
#' @examples
#' \donttest{
#' # Copy and run immediately
#' aib_figures()
#'
#' # Just copy to inspect first
#' aib_figures(run = FALSE, dest = file.path(tempdir(), "paper_figures.R"))
#' }
#'
#' @export
aib_figures <- function(run = TRUE, dest = file.path(tempdir(), "paper_figures.R")) {
  src <- system.file("scripts", "paper_figures.R", package = "AIBias")
  if (!nzchar(src)) {
    abort("Could not find paper_figures.R inside the AIBias package.")
  }
  file.copy(src, dest, overwrite = TRUE)
  cli_alert_success("Script copied to: {normalizePath(dest)}")
  if (run) {
    cli_alert_info("Running paper_figures.R ...")
    source(dest, local = FALSE)
  } else {
    cli_alert_info("Open '{dest}' to inspect or edit, then run source('{dest}')")
  }
  invisible(normalizePath(dest))
}
