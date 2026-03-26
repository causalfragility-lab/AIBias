## Test environments

* Windows 11 x64 (build 26200), R 4.5.1 (local)
* Windows Server 2022 x64, R-devel (2026-03-25 r89703 ucrt), via devtools::check_win_devel()

---

## R CMD check results

### Local (Windows 11, R 4.5.1)

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  Caused by a network/firewall restriction on the local machine that
  prevents R from verifying the current time against an external server.
  Not related to the package itself.

### Win-devel (CRAN server)

0 errors | 0 warnings | 2 notes

* NOTE: Possibly misspelled words in DESCRIPTION:
  Barocas, Gelbach, Hardt, Narayanan, al, et, operationalizes
  These are all author surnames or standard English terms. Not misspelled.

* NOTE: Non-standard file found at top level: cran-comments.md
  This is the standard CRAN submission comments file.

---

## devtools::test() results

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 41 ]

All 41 tests pass with zero failures and zero warnings.

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

All Imports are on CRAN:
  dplyr, tidyr, ggplot2, rlang, cli, purrr, tibble

All Suggests are on CRAN:
  mgcv, lme4, boot, knitr, rmarkdown, testthat

---

## Downstream dependencies

This is a new submission. There are no existing downstream dependencies.

