# AIBias <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **Longitudinal Bias Auditing for Sequential Decision Systems**

[![R-CMD-check](https://github.com/yourusername/AIBias/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/AIBias/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Why AIBias?

Standard fairness tools ask: *"Is there a disparity at this moment?"*

AIBias asks: *"How does disparity evolve over repeated decisions, and are earlier decisions making it worse?"*

In sequential systems — loan approvals, parole reviews, hiring pipelines, content moderation — a denial at time $t$ changes the features available at $t+1$. Static fairness metrics miss this compounding entirely.

AIBias implements three pillars of longitudinal bias analysis:

| Pillar | Question | Key Estimand |
|---|---|---|
| **Trajectory** | How does disparity evolve? | $B_{g,r}(t)$, $CB_{g,r}(T)$ |
| **Transition** | Where does compounding occur? | $\Delta^{01}_{g,r}$, $\Delta^{11}_{g,r}$ |
| **Amplification** | Do prior decisions worsen later inequality? | $A_{g,r}(t)$ |

## Installation

```r
# Install from GitHub
remotes::install_github("yourusername/AIBias")

# Or install locally from source
devtools::install("path/to/AIBias")
```

### First-time setup

After installing, generate the example dataset:

```r
# From the package source directory:
source("data-raw/generate_data.R")
```

## Quick Start

```r
library(AIBias)
data(lending_panel)

# One-shot full audit
result <- aib_audit(
  lending_panel,
  id        = "applicant_id",
  time      = "year",
  group     = "race",
  decision  = "approved",
  ref_group = "White"
)

summary(result)
plot(result, type = "trajectory")
plot(result, type = "heatmap")
plot(result, type = "transition")
plot(result, type = "amplification")
```

## Step-by-Step Workflow

```r
# 1. Build the audit object
obj <- aib_build(lending_panel,
                 id       = "applicant_id",
                 time     = "year",
                 group    = "race",
                 decision = "approved")

# 2. Bias trajectories + cumulative burden
obj <- aib_describe(obj, ref_group = "White")
obj$bias$cumulative

# 3. Transition matrices + recovery/retention gaps
obj <- aib_transition(obj, ref_group = "White")
obj$transitions$recovery_gap
obj$transitions$retention_gap

# 4. Amplification index
obj <- aib_amplify(obj, ref_group = "White")
obj$amplification$cumulative

# 5. Covariate-adjusted trajectories
obj <- aib_adjust(obj,
                  formula   = ~ income + credit_score,
                  method    = "glm",
                  ref_group = "White")

# 6. Bootstrap confidence intervals
obj <- aib_bootstrap(obj, B = 500, seed = 42)

# 7. Full summary report
summary(obj)
```

## Core Estimands

### Bias Trajectory
$$B_{g,r}(t) = \pi_g(t) - \pi_r(t)$$

The raw group disparity at each time point.

### Cumulative Bias Burden
$$CB_{g,r}(T) = \frac{1}{\sum_t w_t} \sum_{t=1}^{T} w_t B_{g,r}(t)$$

Average disparity experienced across the audit horizon.

### Recovery and Retention Gaps
$$\Delta^{01}_{g,r} = Pr(D_t=1 \mid D_{t-1}=0, A=g) - Pr(D_t=1 \mid D_{t-1}=0, A=r)$$
$$\Delta^{11}_{g,r} = Pr(D_t=1 \mid D_{t-1}=1, A=g) - Pr(D_t=1 \mid D_{t-1}=1, A=r)$$

Measure whether disadvantaged groups recover from denials / retain approvals at lower rates.

### Amplification Index
$$A_{g,r}(t) = B_{g,r}(t \mid 1) - B_{g,r}(t \mid 0)$$

Non-zero amplification means prior decisions are modifying current group disparities.

## Formal Definition: Bias Amplification

A decision system exhibits **bias amplification** for group $g$ relative to reference $r$ if:

1. $|B_{g,r}(t)| > |B_{g,r}(s)|$ for some $t > s$, **and**
2. $A_{g,r}(t) \neq 0$, or $P_g(t) \neq P_r(t)$

**Proposition:** If $p_g^{11}(t) < p_r^{11}(t)$ and $p_g^{01}(t) < p_r^{01}(t)$ for all $t$, then disparity is non-decreasing over time, producing nonnegative cumulative burden against group $g$.

## Package Structure

```
R/
  aib_build.R        # Constructor + validator
  aib_describe.R     # Bias trajectories + cumulative burden
  aib_transition.R   # Markov transition matrices + gaps
  aib_amplify.R      # Amplification index
  aib_adjust.R       # Covariate adjustment (GLM/GAM/GLMM)
  aib_bootstrap.R    # Cluster bootstrap CIs
  aib_audit.R        # One-shot pipeline wrapper
  summary.aibias.R   # print/summary S3 methods
  plot.aibias.R      # ggplot2 visualizations
  utils.R            # Internal helpers
  data.R             # Dataset documentation
```

## Running Tests

```r
devtools::test()
```

## Citation

```
@software{AIBias2025,
  title  = {AIBias: Longitudinal Bias Auditing for Sequential Decision Systems},
  year   = {2025},
  url    = {https://github.com/yourusername/AIBias}
}
```

## Contributing

Issues and pull requests welcome. See `CONTRIBUTING.md` for guidelines.
