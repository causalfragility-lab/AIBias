# data-raw/lending_panel.R
# Generates the synthetic lending_panel example dataset

set.seed(42)

N      <- 600    # applicants
T      <- 6      # years
groups <- c("White", "Black", "Hispanic")
group_probs <- c(0.5, 0.3, 0.2)

# Assign applicants to groups
applicant_id <- paste0("APP_", sprintf("%04d", seq_len(N)))
race <- sample(groups, N, replace = TRUE, prob = group_probs)

# Group-specific base approval probabilities
base_prob <- c(White = 0.72, Black = 0.48, Hispanic = 0.54)

# Transition probabilities: P(approved_t | approved_{t-1}, group)
p11 <- c(White = 0.82, Black = 0.62, Hispanic = 0.68)  # retain approval
p01 <- c(White = 0.65, Black = 0.38, Hispanic = 0.44)  # recover after denial

# Build panel
rows <- vector("list", N * T)
k    <- 1L

for (i in seq_len(N)) {
  g   <- race[i]
  # Covariates: income (mean shifts by group to reflect real-world gaps)
  income_mean <- c(White = 75, Black = 52, Hispanic = 58)[g]
  incomes     <- round(pmax(20, rnorm(T, income_mean, 15)))
  credit      <- round(pmax(300, pmin(850, rnorm(T, 680, 80))))

  D <- integer(T)
  # Initial decision
  D[1] <- rbinom(1, 1, base_prob[g])

  for (t in 2:T) {
    prob_t <- if (D[t-1] == 1) p11[g] else p01[g]
    # Small covariate adjustment
    adj    <- 0.003 * (incomes[t] - income_mean) +
              0.0005 * (credit[t] - 680)
    prob_t <- pmax(0.01, pmin(0.99, prob_t + adj))
    D[t]   <- rbinom(1, 1, prob_t)
  }

  for (t in seq_len(T)) {
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

lending_panel <- do.call(rbind, rows)
lending_panel$race <- factor(lending_panel$race,
                              levels = c("White", "Black", "Hispanic"))

usethis::use_data(lending_panel, overwrite = TRUE)
