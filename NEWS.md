# AIBias 0.1.1

* Updated amplification index to a mechanism-oriented sign convention:
  `A_t = (p11_ref - p11_group) - (p01_ref - p01_group)`.
  Negative values indicate recovery-driven amplification against the focal
  group; positive values indicate retention-driven amplification.
* Added `sign` argument to `aib_amplify()` with options `"mechanism"`
  (default) and `"legacy"` for backward compatibility.
* Improved documentation to clarify sign interpretation.
* Fixed: `\dontrun{}` replaced with `\donttest{}` in examples.
* Fixed: added `\value` tags to `print.aibias` and `summary.aibias`.
* Fixed: `aib_figures()` default path now uses `tempdir()`.

 # AIBias 0.1.0

* Initial CRAN submission.

* Core functions: `aib_build()`, `aib_describe()`, `aib_transition()`,
  `aib_amplify()`, `aib_adjust()`, `aib_bootstrap()`, `aib_audit()`.

* Four plot types via `plot.aibias()`: trajectory, heatmap, transition,
  amplification.

* Includes `lending_panel` synthetic dataset (600 applicants x 6 years,
  3 racial groups).

* Demo script reproducing all four paper figures:
  `demo("paper_figures", package = "AIBias")`.

## Resubmission notes

* Removed author surnames from DESCRIPTION to avoid spell check false
  positives (Barocas, Hardt, Narayanan, Gelbach replaced with neutral
  citation wording).

* Added `.gitignore` to exclude `cran-comments.md` from the repository.
