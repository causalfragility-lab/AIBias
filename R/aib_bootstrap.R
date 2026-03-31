#' Bootstrap confidence intervals for bias trajectories
#'
#' @description
#' Computes bootstrap confidence intervals for the bias trajectory
#' and cumulative burden by resampling units (cluster bootstrap).
#'
#' @param object An `aibias` object with `$bias` populated.
#' @param B Integer. Number of bootstrap replicates. Default 500.
#' @param conf Numeric. Confidence level. Default 0.95.
#' @param ref_group Character. Reference group.
#' @param seed Integer. Random seed for reproducibility.
#' @param verbose Logical.
#'
#' @return The `aibias` object with `$bootstrap` populated.
#'
#' @details
#' Uses the cluster (unit-level) bootstrap to preserve the panel
#' structure. Units are resampled with replacement; all their
#' time observations are retained.
#'
#' @examples
#' \donttest{
#' data(lending_panel)
#' obj <- aib_build(lending_panel, "applicant_id", "year", "race", "approved")
#' obj <- aib_describe(obj, ref_group = "White")
#' obj <- aib_bootstrap(obj, B = 200, seed = 42)
#' }
#'
#' @export
aib_bootstrap <- function(object,
                          B         = 500,
                          conf      = 0.95,
                          ref_group = NULL,
                          seed      = NULL,
                          verbose   = TRUE) {
  .check_aibias(object)
  if (is.null(object$bias))
    abort("Run `aib_describe()` before bootstrapping.")

  if (!is.null(seed)) set.seed(seed)
  ref_group <- .resolve_ref_group(object, ref_group)

  data    <- object$data
  id_col  <- object$meta$id
  t_col   <- object$meta$time_int
  grp_col <- object$meta$group
  dec_col <- object$meta$decision
  ids     <- unique(data[[id_col]])
  alpha   <- 1 - conf

  if (verbose) cli_alert_info("Running {B} bootstrap replicates...")

  boot_results <- vector("list", B)
  for (b in seq_len(B)) {
    # Cluster-resample units
    ids_b    <- sample(ids, length(ids), replace = TRUE)
    data_b   <- do.call(rbind, lapply(seq_along(ids_b), function(i) {
      sub <- data[data[[id_col]] == ids_b[i], ]
      sub[[id_col]] <- paste0(i, "_boot")
      sub
    }))

    # Rates
    rates_b <- data_b |>
      group_by(.data[[grp_col]], .data[[t_col]]) |>
      summarise(rate = mean(.data[[dec_col]], na.rm = TRUE), .groups = "drop") |>
      rename(group = 1, time = 2)

    ref_r   <- rates_b |> filter(.data$group == ref_group) |>
      select("time", rate_ref = "rate")
    traj_b  <- rates_b |> filter(.data$group != ref_group) |>
      left_join(ref_r, by = "time") |>
      mutate(B_raw = .data$rate - .data$rate_ref, rep = b)

    boot_results[[b]] <- traj_b
  }

  boot_df <- bind_rows(boot_results)

  # ---- Percentile CIs by group and time ----------------------------------
  ci <- boot_df |>
    group_by(.data$group, .data$time) |>
    summarise(
      B_mean = mean(.data$B_raw, na.rm = TRUE),
      B_lo   = quantile(.data$B_raw, alpha / 2,       na.rm = TRUE),
      B_hi   = quantile(.data$B_raw, 1 - alpha / 2,   na.rm = TRUE),
      .groups = "drop"
    )

  # ---- Merge with observed trajectory ------------------------------------
  traj_obs <- object$bias$trajectory |>
    select("group", "time", "B_raw")

  ci_merged <- traj_obs |>
    left_join(ci |> select("group", "time", "B_lo", "B_hi"),
              by = c("group", "time"))

  object$bootstrap <- list(
    ci          = ci_merged,
    boot_draws  = boot_df,
    B           = B,
    conf        = conf,
    ref_group   = ref_group
  )

  if (verbose) cli_alert_success("Bootstrap complete ({conf*100}% CI).")
  invisible(object)
}
