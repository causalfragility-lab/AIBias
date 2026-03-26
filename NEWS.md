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
