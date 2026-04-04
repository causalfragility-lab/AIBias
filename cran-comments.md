## Resubmission (v0.1.1)
This is a resubmission addressing all comments from the CRAN reviewer
(Benjamin Altmann) plus an internal consistency fix:

* Added `\value` tags to `print.aibias.Rd` and `summary.aibias.Rd`.
* Replaced `\dontrun{}` with `\donttest{}` in `aib_bootstrap.Rd`
  and `aib_figures.Rd`.
* Changed default `dest` in `aib_figures()` to `file.path(tempdir(),
  "paper_figures.R")` to comply with CRAN file-writing policy.
* Revised amplification index sign convention to mechanism-oriented
  formulation; added `sign` argument for backward compatibility.

## R CMD check results
0 errors | 0 warnings | 0 notes

