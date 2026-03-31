#' Describe longitudinal disparity trajectories
#'
#' @description
#' Computes group decision rate trajectories, raw and standardized bias
#' trajectories, and cumulative bias burden.
#'
#' @param object An `aibias` object from [AIBias::aib_build()].
#' @param ref_group Character. Reference group label. If NULL, uses the
#'   first group level.
#' @param weights Numeric vector of time weights for cumulative burden.
#'   Length must equal the number of time points. If NULL, equal weights.
#' @param verbose Logical. Print summary output.
#'
#' @return The `aibias` object with `$bias` populated. The bias element is
#'   a list with components:
#'   - `trajectory`: raw bias trajectory \eqn{B_{g,r}(t)}
#'   - `trajectory_smd`: standardized bias trajectory \eqn{B^*_{g,r}(t)}
#'   - `cumulative`: cumulative bias burden \eqn{CB_{g,r}(T)}
#'   - `slope`: first differences \eqn{\Delta B_{g,r}(t)}
#'   - `curvature`: second differences \eqn{\Delta^2 B_{g,r}(t)}
#'   - `ref_group`: reference group used
#'
#' @examples
#' data(lending_panel)
#' obj <- aib_build(lending_panel, "applicant_id", "year", "race", "approved")
#' obj <- aib_describe(obj, ref_group = "White")
#' obj$bias$cumulative
#'
#' @export
aib_describe <- function(object,
                         ref_group = NULL,
                         weights   = NULL,
                         verbose   = TRUE) {
  .check_aibias(object)

  ref_group <- .resolve_ref_group(object, ref_group)
  rates     <- object$rates
  groups    <- setdiff(levels(object$meta$groups |> as.factor()), ref_group)

  # Overall decision rate by time (for SMD denominator)
  overall <- object$data |>
    group_by(.data[[object$meta$time_int]]) |>
    summarise(
      pi_overall = mean(.data[[object$meta$decision]], na.rm = TRUE),
      .groups    = "drop"
    ) |>
    rename(time = 1)

  ref_rates <- rates |>
    filter(.data$group == ref_group) |>
    select("time", rate_ref = "rate")

  # ---- Compute trajectory per non-reference group -------------------------
  traj_list <- lapply(groups, function(g) {
    g_rates <- rates |>
      filter(.data$group == g) |>
      select("time", rate_g = "rate")

    traj <- g_rates |>
      left_join(ref_rates,  by = "time") |>
      left_join(overall,    by = "time") |>
      mutate(
        group         = g,
        ref_group     = ref_group,
        B_raw         = .data$rate_g - .data$rate_ref,
        denom_smd     = sqrt(.data$pi_overall * (1 - .data$pi_overall)),
        B_smd         = ifelse(.data$denom_smd > 0,
                               .data$B_raw / .data$denom_smd,
                               NA_real_),
        slope         = c(NA_real_, diff(.data$B_raw)),
        curvature     = c(NA_real_, NA_real_, diff(diff(.data$B_raw)))
      ) |>
      select("group", "ref_group", "time",
             "rate_g", "rate_ref",
             "B_raw", "B_smd",
             "slope", "curvature")
    traj
  })

  trajectory <- bind_rows(traj_list)

  # ---- Cumulative bias burden -------------------------------------------
  T       <- max(trajectory$time)
  wt_arg  <- weights
  if (is.null(wt_arg)) wt_arg <- rep(1, T)
  if (length(wt_arg) != T) {
    abort("`weights` must have length equal to number of time points ({T}).")
  }
  wt_df <- tibble(time = seq_len(T), wt = wt_arg)

  cumulative <- trajectory |>
    left_join(wt_df, by = "time") |>
    group_by(.data$group, .data$ref_group) |>
    summarise(
      CB_raw         = sum(.data$wt * .data$B_raw, na.rm = TRUE),
      CB_normalized  = sum(.data$wt * .data$B_raw, na.rm = TRUE) /
                         sum(.data$wt, na.rm = TRUE),
      CB_smd         = sum(.data$wt * .data$B_smd, na.rm = TRUE),
      peak_disparity = max(abs(.data$B_raw), na.rm = TRUE),
      time_to_peak   = .data$time[which.max(abs(.data$B_raw))],
      .groups        = "drop"
    )

  # ---- Bias persistence & instability -----------------------------------
  persistence <- trajectory |>
    group_by(.data$group) |>
    summarise(
      instability = mean(abs(.data$slope), na.rm = TRUE),
      volatility  = var(.data$slope, na.rm = TRUE),
      .groups     = "drop"
    )

  # ---- Store in object ---------------------------------------------------
  object$bias <- list(
    trajectory     = trajectory,
    cumulative     = cumulative,
    persistence    = persistence,
    ref_group      = ref_group,
    weights        = wt_arg
  )

  if (verbose) {
    cli_h2("Bias Trajectories (ref: {ref_group})")
    for (g in groups) {
      cb <- cumulative |> filter(.data$group == g) |> pull(.data$CB_normalized)
      pk <- cumulative |> filter(.data$group == g) |> pull(.data$peak_disparity)
      dir <- if (cb < 0) "disadvantaged" else "advantaged"
      cli_li("Group {g}: CB = {round(cb, 3)} ({dir}), peak |B| = {round(pk, 3)}")
    }
  }

  invisible(object)
}


#' Compute bias persistence above a threshold
#'
#' @param object An `aibias` object with `$bias` populated.
#' @param threshold Numeric. Minimum absolute disparity to count. Default 0.05.
#' @return A tibble with group-level persistence counts.
#' @export
aib_persistence <- function(object, threshold = 0.05) {
  .check_aibias(object)
  if (is.null(object$bias))
    abort("Run `aib_describe()` first.")

  object$bias$trajectory |>
    group_by(.data$group) |>
    summarise(
      T_total      = n(),
      T_persistent = sum(abs(.data$B_raw) > threshold, na.rm = TRUE),
      frac_persist = .data$T_persistent / .data$T_total,
      .groups      = "drop"
    )
}
