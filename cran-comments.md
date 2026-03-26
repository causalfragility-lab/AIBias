## R CMD check results

Tested on: Windows 11 x64, R 4.5.1 (2025-06-13 ucrt)

0 errors | 0 warnings | 1 note

### Note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note is caused by a network/firewall restriction on the test
  machine that prevents R from verifying the current time against an
  external server. It is not related to the package itself and does
  not appear on machines with unrestricted internet access.

---

## Test environments

* Windows 11 x64 (build 26200), R 4.5.1

---

## devtools::test() results

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 41 ]

All 41 tests pass with zero warnings.

---

## Package summary

AIBias provides a framework for longitudinal bias auditing in sequential
decision systems. It treats algorithmic bias as a dynamic process,
estimating bias trajectories, transition asymmetries (recovery and
retention gaps), amplification indices, and cumulative burden measures
from panel data. Core functions: aib_build(), aib_describe(),
aib_transition(), aib_amplify(), aib_adjust(), aib_bootstrap(),
aib_audit(). Includes a synthetic lending panel dataset (lending_panel)
and a demo script reproducing all four paper figures.

---

## Dependencies

All dependencies are on CRAN:
  dplyr, tidyr, ggplot2, rlang, cli, purrr, tibble, mgcv, lme4, Matrix

---

## Downstream dependencies

This is a new submission. There are no existing downstream dependencies.
```

---

Then add it to `.Rbuildignore` so it is not included in the built package. Open `.Rbuildignore` and add this line at the bottom:
```
^cran-comments\.md$
