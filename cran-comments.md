## Resubmission
This is a resubmission. The following issues raised by the CRAN reviewer
have been addressed:

* Added `\value` tags to `print.aibias.Rd` and `summary.aibias.Rd`
  documenting return values and side effects.
* Replaced `\dontrun{}` with `\donttest{}` in `aib_bootstrap.Rd`
  and `aib_figures.Rd`.
* Changed the default `dest` argument in `aib_figures()` from the
  working directory to `file.path(tempdir(), "paper_figures.R")` and
  updated the example accordingly.
* References in DESCRIPTION already follow CRAN format.

## R CMD check results
0 errors | 0 warnings | 0 notes

## Test environments
* Windows 11, R 4.5.1

