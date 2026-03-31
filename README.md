# AIBias

> **Longitudinal Bias Auditing for Sequential Decision Systems**

[![R-CMD-check](https://github.com/causalfragility-lab/AIBias/workflows/R-CMD-check/badge.svg)](https://github.com/causalfragility-lab/AIBias/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Why AIBias?

Standard fairness tools ask: *"Is there a disparity at this moment?"*

AIBias asks: *"How does disparity evolve over repeated decisions, and are earlier decisions making it worse?"*

In sequential systems — loan approvals, parole reviews, hiring pipelines, content moderation — a denial at time $t$ changes the features available at $t+1$. Static fairness metrics miss this compounding entirely.

AIBias treats algorithmic bias as a **longitudinal process**. It implements three pillars of dynamic bias analysis:

| Pillar | Question | Key Estimand |
|---|---|---|
| **Trajectory** | How does disparity evolve over time? | $B_{g,r}(t)$, $CB_{g,r}(T)$ |
| **Transition** | Where does compounding occur? | $\Delta^{01}_{g,r}$, $\Delta^{11}_{g,r}$ |
| **Amplification** | Do prior decisions worsen later inequality? | $A_{g,r}(t)$ |

---

## Installation

```r
# Install from GitHub
remotes::install_github("causalfragility-lab/AIBias")
```

Once accepted on CRAN:

```r
install.packages("AIBias")
```

---

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

---

## Step-by-Step Workflow

```r
library(AIBias)
data(lending_panel)

# 1. Build the audit object
obj <- aib_build(lending_panel,
                 id       = "applicant_id",
                 time     = "year",
                 group    = "race",
                 decision = "approved")

# 2. Bias trajectories and cumulative burden
obj <- aib_describe(obj, ref_group = "White")
obj$bias$cumulative

# 3. Transition matrices, recovery and retention gaps
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

# 7. Full summary
summary(obj)
```

---

## Paper Figures (Toy Simulation)

Reproduce the four methods paper figures using the built-in demo:

```r
library(AIBias)
demo("paper_figures", package = "AIBias")

# or equivalently:
aib_figures()
```

This runs a toy simulation (N = 20, T = 3, 2 groups) and displays:

- **Figure 1** -- Bias trajectory with 95% bootstrap CI
- **Figure 2** -- Group-time disparity heatmap
- **Figure 3** -- Transition probabilities (recovery vs retention)
- **Figure 4** -- Amplification index over time

---

## Core Estimands

### Bias Trajectory
$$B_{g,r}(t) = \pi_g(t) - \pi_r(t)$$

The raw group disparity at each time point.

### Cumulative Bias Burden
$$CB_{g,r}(T) = \frac{1}{\sum_t w_t} \sum_{t=1}^{T} w_t B_{g,r}(t)$$

Average disparity experienced across the full audit horizon.

### Recovery and Retention Gaps
$$\Delta^{01}_{g,r} = Pr(D_t=1 \mid D_{t-1}=0, A=g) - Pr(D_t=1 \mid D_{t-1}=0, A=r)$$
$$\Delta^{11}_{g,r} = Pr(D_t=1 \mid D_{t-1}=1, A=g) - Pr(D_t=1 \mid D_{t-1}=1, A=r)$$

Measure whether disadvantaged groups recover from denials and retain approvals at lower rates than the reference group.

### Amplification Index
$$A_{g,r}(t) = B_{g,r}(t \mid 1) - B_{g,r}(t \mid 0)$$

Non-zero amplification means prior decision state is actively modifying current group disparities.

---

## Formal Definition: Bias Amplification

A decision system exhibits **bias amplification** for group $g$ relative to reference $r$ if:

1. $|B_{g,r}(t)| > |B_{g,r}(s)|$ for some $t > s$ (disparity grows over time), **and**
2. $A_{g,r}(t) \neq 0$ (prior decisions modulate current disparity), **or** $P_g(t) \neq P_r(t)$ (group transition matrices differ)

**Proposition:** If $p_g^{11}(t) < p_r^{11}(t)$ and $p_g^{01}(t) < p_r^{01}(t)$ for all $t$, then under common initial conditions the approval probability for group $g$ weakly decreases relative to group $r$ over time, producing nonnegative cumulative burden against group $g$.

This distinguishes *static persistent bias* (constant gap) from *dynamic compounding bias* (self-reinforcing gap driven by the decision process itself).

---

## Package Structure

```
R/
  aib_build.R        # Constructor and validator
  aib_describe.R     # Bias trajectories and cumulative burden
  aib_transition.R   # Markov transition matrices and gaps
  aib_amplify.R      # Amplification index
  aib_adjust.R       # Covariate adjustment (GLM / GAM / GLMM)
  aib_bootstrap.R    # Cluster bootstrap confidence intervals
  aib_audit.R        # One-shot pipeline wrapper
  aib_figures.R      # Helper to access paper figure script
  plot.aibias.R      # ggplot2 visualizations
  summary.aibias.R   # print / summary S3 methods
  utils.R            # Internal helpers
  data.R             # Dataset documentation

demo/
  paper_figures.R    # Reproduces all 4 paper figures

inst/scripts/
  paper_figures.R    # Accessible via system.file() after install
```

---

## Running Tests

```r
devtools::test()
# Expected: FAIL 0 | WARN 0 | SKIP 0 | PASS 41
```

---

## Citation

If you use AIBias in your research, please cite:

```bibtex
@software{Hait2026AIBias,
  title  = {AIBias: Longitudinal Bias Auditing for Sequential Decision Systems},
  author = {Hait, Subir},
  year   = {2026},
  url    = {https://github.com/causalfragility-lab/AIBias}
}
```

---

## Author

**Subir Hait**  
Michigan State University  
haitsubi@msu.edu

---

## License

MIT License. See [LICENSE](LICENSE) for details.
