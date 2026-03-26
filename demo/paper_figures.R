# AIBias: Paper Figures (N=20, T=3 toy simulation)
# Uses the package's built-in plot() method to produce all 4 figures.
# Run with: demo("paper_figures", package = "AIBias")
#       or: aib_figures()

library(AIBias)
library(ggplot2)

# =============================================================
# 1. SIMULATE  (N=20 applicants, T=3 annual waves, 2 groups)
# =============================================================
# Known transition parameters:
#
#   Group    | P(D=1 | prev denied) | P(D=1 | prev approved)
#   ---------|--------------------- |------------------------
#   Majority |        0.60          |          0.80
#   Minority |        0.30          |          0.55
#
# Both groups start at the same 50% initial approval rate.
# The minority group has lower recovery AND lower retention,
# generating dynamic amplification of disparity over time.

set.seed(2024)
N  <- 20
TT <- 3

ids    <- paste0("ID", sprintf("%02d", 1:N))
groups <- rep(c("Majority", "Minority"), each = N / 2)

p_recover <- c(Majority = 0.60, Minority = 0.30)
p_retain  <- c(Majority = 0.80, Minority = 0.55)

rows <- vector("list", N * TT)
k    <- 1L
for (i in seq_len(N)) {
  g    <- groups[i]
  D    <- integer(TT)
  D[1] <- rbinom(1, 1, 0.50)
  for (t in 2:TT)
    D[t] <- rbinom(1, 1, ifelse(D[t - 1] == 1, p_retain[g], p_recover[g]))
  for (t in seq_len(TT)) {
    rows[[k]] <- data.frame(
      id       = ids[i],
      time     = t,
      group    = g,
      decision = D[t],
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }
}
panel <- do.call(rbind, rows)

cat("Simulated panel: N=20, T=3, 2 groups\n")
cat("Approval rates by group and time:\n")
print(round(tapply(panel$decision, list(panel$group, panel$time), mean), 2))
cat("\n")

# =============================================================
# 2. RUN FULL AUDIT
# =============================================================
obj <- aib_build(panel, "id", "time", "group", "decision")
obj <- aib_describe(obj,   ref_group = "Majority")
obj <- aib_transition(obj, ref_group = "Majority")
obj <- aib_amplify(obj,    ref_group = "Majority")
obj <- aib_bootstrap(obj,  B = 500, seed = 42)

summary(obj)

# =============================================================
# 3. FIGURE 1 - Bias Trajectory B_g,r(t)
#    Line plot: Minority bias trajectory over time
#    with 95% bootstrap CI shaded band
# =============================================================
fig1 <- plot(obj, type = "trajectory")
print(fig1)

# =============================================================
# 4. FIGURE 2 - Group-Time Disparity Surface (heatmap)
#    Tile plot: red = disadvantaged, green = advantaged
# =============================================================
fig2 <- plot(obj, type = "heatmap")
print(fig2)

# =============================================================
# 5. FIGURE 3 - Transition Probabilities by Group
#    Bar plot: Recovery (0->1) vs Retention (1->1)
# =============================================================
fig3 <- plot(obj, type = "transition")
print(fig3)

# =============================================================
# 6. FIGURE 4 - Amplification Index A(t)
#    Line plot: A_g,r(t) over time
#    Non-zero => prior decisions compound current disparity
# =============================================================
fig4 <- plot(obj, type = "amplification")
print(fig4)

cat("\n=== All 4 figures displayed ===\n")

# =============================================================
# NOTE: These figures are produced by the package's built-in
# plot.aibias() method and work on any real dataset too:
#
#   data(lending_panel)
#   result <- aib_audit(lending_panel,
#               id = "applicant_id", time = "year",
#               group = "race",      decision = "approved",
#               ref_group = "White")
#   plot(result, type = "trajectory")
#   plot(result, type = "heatmap")
#   plot(result, type = "transition")
#   plot(result, type = "amplification")
# =============================================================
