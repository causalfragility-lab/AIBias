#!/usr/bin/env Rscript
# Run this script once to regenerate data/lending_panel.rda
# Rscript data-raw/lending_panel.R

set.seed(42)

N      <- 600L
T_     <- 6L
groups <- c("White", "Black", "Hispanic")
group_probs <- c(0.5, 0.3, 0.2)

applicant_id <- paste0("APP_", sprintf("%04d", seq_len(N)))
race_vec     <- sample(groups, N, replace = TRUE, prob = group_probs)

base_prob <- c(White = 0.72, Black = 0.48, Hispanic = 0.54)
p11       <- c(White = 0.82, Black = 0.62, Hispanic = 0.68)
p01       <- c(White = 0.65, Black = 0.38, Hispanic = 0.44)

rows <- vector("list", N * T_)
k    <- 1L

for (i in seq_len(N)) {
  g           <- race_vec[i]
  income_mean <- c(White = 75, Black = 52, Hispanic = 58)[g]
  incomes     <- round(pmax(20, rnorm(T_, income_mean, 15)))
  credit      <- round(pmax(300, pmin(850, rnorm(T_, 680, 80))))

  D    <- integer(T_)
  D[1] <- rbinom(1, 1, base_prob[g])

  for (t in 2:T_) {
    prob_t <- if (D[t-1] == 1) p11[g] else p01[g]
    adj    <- 0.003 * (incomes[t] - income_mean) +
              0.0005 * (credit[t] - 680)
    prob_t <- pmax(0.01, pmin(0.99, prob_t + adj))
    D[t]   <- rbinom(1, 1, prob_t)
  }

  for (t in seq_len(T_)) {
    rows[[k]] <- data.frame(
      applicant_id = applicant_id[i],
      year         = 2015L + t - 1L,
      race         = g,
      income       = incomes[t],
      credit_score = credit[t],
      approved     = D[t],
      stringsAsFactors = FALSE
    )
    k <- k + 1L
  }
}

lending_panel      <- do.call(rbind, rows)
lending_panel$race <- factor(lending_panel$race,
                              levels = c("White", "Black", "Hispanic"))

dir.create("data", showWarnings = FALSE)
save(lending_panel, file = "data/lending_panel.rda", compress = "bzip2")
cat("lending_panel saved to data/lending_panel.rda\n")
cat("Rows:", nrow(lending_panel), "\n")
cat("Approval rates by group:\n")
print(tapply(lending_panel$approved, lending_panel$race, mean))
