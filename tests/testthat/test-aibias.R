library(testthat)
library(AIBias)

# ---- Helper: minimal synthetic panel ------------------------------------

make_panel <- function(seed = 1, N = 120, T_ = 5) {
  set.seed(seed)
  groups <- c("A", "B")
  ids    <- paste0("u", seq_len(N))
  race   <- rep(groups, each = N / 2)
  p11    <- c(A = 0.80, B = 0.55)
  p01    <- c(A = 0.60, B = 0.35)
  base   <- c(A = 0.70, B = 0.45)

  rows <- vector("list", N * T_)
  k    <- 1L
  for (i in seq_len(N)) {
    g    <- race[i]
    D    <- integer(T_)
    D[1] <- rbinom(1, 1, base[g])
    for (t in 2:T_) {
      pr  <- if (D[t-1] == 1) p11[g] else p01[g]
      D[t] <- rbinom(1, 1, pr)
    }
    for (t in seq_len(T_)) {
      rows[[k]] <- data.frame(id = ids[i], time = t, group = g,
                               decision = D[t], stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  do.call(rbind, rows)
}


# =========================================================================
# aib_build
# =========================================================================

test_that("aib_build returns aibias object", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_s3_class(obj, "aibias")
})

test_that("aib_build stores correct metadata", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_equal(obj$meta$n_units,  120)
  expect_equal(obj$meta$n_times,  5)
  expect_equal(length(obj$meta$groups), 2)
})

test_that("aib_build coerces decision to 0/1 integer", {
  d           <- make_panel()
  d$decision  <- as.logical(d$decision)
  obj         <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_true(all(d$decision %in% c(TRUE, FALSE)))
  expect_true(is.integer(obj$data$decision))
})

test_that("aib_build errors on missing columns", {
  d <- make_panel()
  expect_error(
    aib_build(d, "id", "time", "group", "NOTEXIST", verbose = FALSE),
    regexp = "Missing columns"
  )
})


# =========================================================================
# aib_describe
# =========================================================================

test_that("aib_describe populates bias slot", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  expect_false(is.null(obj$bias))
  expect_true("trajectory" %in% names(obj$bias))
  expect_true("cumulative" %in% names(obj$bias))
})

test_that("bias trajectory has correct dimensions", {
  d     <- make_panel()
  obj   <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj   <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  traj  <- obj$bias$trajectory
  # one non-reference group (B) x 5 time points
  expect_equal(nrow(traj), 5)
  expect_equal(unique(traj$group), "B")
})

test_that("B_raw is pi_g - pi_r", {
  d   <- make_panel(seed = 7)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  traj <- obj$bias$trajectory

  # Verify by manual calculation for t=1
  t1_A <- mean(d$decision[d$group == "A" & d$time == 1])
  t1_B <- mean(d$decision[d$group == "B" & d$time == 1])
  expect_equal(traj$B_raw[traj$time == 1], t1_B - t1_A, tolerance = 1e-10)
})

test_that("aib_persistence returns correct columns", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  p   <- aib_persistence(obj, threshold = 0.02)
  expect_true(all(c("group", "T_persistent", "frac_persist") %in% names(p)))
})


# =========================================================================
# aib_transition
# =========================================================================

test_that("aib_transition populates transitions slot", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  expect_false(is.null(obj$transitions))
  expect_true("matrices" %in% names(obj$transitions))
})

test_that("transition matrices are 2x2", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  for (m in obj$transitions$matrices) {
    expect_equal(dim(m), c(2, 2))
  }
})

test_that("transition rows sum to 1", {
  d   <- make_panel(seed = 3)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  for (m in obj$transitions$matrices) {
    rs <- rowSums(m)
    expect_equal(rs, c(from_0 = 1, from_1 = 1), tolerance = 1e-10)
  }
})

test_that("recovery gap is negative for disadvantaged group", {
  # By construction group B has lower p01 than A (p01_B=0.35 vs p01_A=0.60)
  # Use large N and fixed seed to ensure reliable detection
  d   <- make_panel(seed = 123, N = 600)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  rg  <- obj$transitions$recovery_gap
  expect_true(rg$recovery_gap[rg$group == "B"] < 0)
})


# =========================================================================
# aib_amplify
# =========================================================================

test_that("aib_amplify populates amplification slot", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  obj <- aib_amplify(obj,    ref_group = "A", verbose = FALSE)
  expect_false(is.null(obj$amplification))
  expect_true("index" %in% names(obj$amplification))
  expect_true("cumulative" %in% names(obj$amplification))
})

test_that("amplification index has correct groups", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  obj <- aib_amplify(obj,    ref_group = "A", verbose = FALSE)
  idx <- obj$amplification$index
  expect_true(all(idx$group %in% c("B")))  # only non-ref groups
})


# =========================================================================
# aib_audit (pipeline)
# =========================================================================

test_that("aib_audit runs full pipeline without error", {
  d   <- make_panel(N = 200)
  obj <- aib_audit(d, id = "id", time = "time", group = "group",
                   decision = "decision", ref_group = "A", verbose = FALSE)
  expect_false(is.null(obj$bias))
  expect_false(is.null(obj$transitions))
  expect_false(is.null(obj$amplification))
})

test_that("aib_audit accepts aibias object directly", {
  d   <- make_panel(N = 200)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj2 <- aib_audit(obj, ref_group = "A", verbose = FALSE)
  expect_s3_class(obj2, "aibias")
})


# =========================================================================
# S3 methods
# =========================================================================

test_that("print.aibias runs without error", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_output(print(obj), regexp = "AIBias")
})

test_that("summary.aibias runs after full pipeline", {
  d   <- make_panel(N = 200)
  obj <- aib_audit(d, id = "id", time = "time", group = "group",
                   decision = "decision", ref_group = "A", verbose = FALSE)
  expect_output(summary(obj), regexp = "Audit Summary")
})

test_that("plot.aibias returns ggplot for trajectory", {
  skip_if_not_installed("ggplot2")
  d   <- make_panel(N = 200)
  obj <- aib_audit(d, id = "id", time = "time", group = "group",
                   decision = "decision", ref_group = "A", verbose = FALSE)
  p   <- plot(obj, type = "trajectory")
  expect_s3_class(p, "ggplot")
})

test_that("plot.aibias returns ggplot for heatmap", {
  skip_if_not_installed("ggplot2")
  d   <- make_panel(N = 200)
  obj <- aib_audit(d, id = "id", time = "time", group = "group",
                   decision = "decision", ref_group = "A", verbose = FALSE)
  p   <- plot(obj, type = "heatmap")
  expect_s3_class(p, "ggplot")
})

test_that("plot.aibias returns ggplot for transition", {
  skip_if_not_installed("ggplot2")
  d   <- make_panel(N = 200)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_transition(obj, ref_group = "A", verbose = FALSE)
  p   <- plot(obj, type = "transition")
  expect_s3_class(p, "ggplot")
})

test_that("plot.aibias returns ggplot for amplification", {
  skip_if_not_installed("ggplot2")
  d   <- make_panel(N = 200)
  obj <- aib_audit(d, id = "id", time = "time", group = "group",
                   decision = "decision", ref_group = "A", verbose = FALSE)
  p   <- plot(obj, type = "amplification")
  expect_s3_class(p, "ggplot")
})


# =========================================================================
# aib_bootstrap
# =========================================================================

test_that("aib_bootstrap populates bootstrap slot", {
  d   <- make_panel(N = 100)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  obj <- aib_bootstrap(obj, B = 50, seed = 1, verbose = FALSE)
  expect_false(is.null(obj$bootstrap))
  expect_true(all(c("B_lo", "B_hi") %in% names(obj$bootstrap$ci)))
})

test_that("bootstrap CI contains observed estimate", {
  d   <- make_panel(N = 200, seed = 5)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  obj <- aib_describe(obj, ref_group = "A", verbose = FALSE)
  obj <- aib_bootstrap(obj, B = 100, seed = 1, verbose = FALSE)
  ci  <- obj$bootstrap$ci
  # The observed B_raw should lie within [B_lo, B_hi] for most waves
  n_within <- sum(ci$B_raw >= ci$B_lo & ci$B_raw <= ci$B_hi, na.rm = TRUE)
  expect_gte(n_within / sum(!is.na(ci$B_lo)), 0.8)
})


# =========================================================================
# Edge cases
# =========================================================================

test_that("aib_build handles factor time variable", {
  d        <- make_panel()
  d$time   <- factor(d$time)
  obj      <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_equal(obj$meta$n_times, 5)
})

test_that("aib_describe errors if wrong ref_group", {
  d   <- make_panel()
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  expect_error(aib_describe(obj, ref_group = "NOTEXIST", verbose = FALSE))
})

test_that("aib_panel_info returns correct unit counts", {
  d   <- make_panel(N = 120)
  obj <- aib_build(d, "id", "time", "group", "decision", verbose = FALSE)
  pi  <- aib_panel_info(obj)
  expect_equal(sum(pi$n_units), 120)
})
