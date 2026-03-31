#' Compute group-specific decision transition matrices and disparities
#'
#' @description
#' Estimates group-specific Markov transition probabilities
#' \eqn{p_g^{ab}(t) = Pr(D_{it} = b \mid D_{i,t-1} = a, A_i = g)}
#' and derives transition disparities, advantage retention, and recovery gaps.
#'
#' @param object An `aibias` object from [AIBias::aib_build()].
#' @param ref_group Character. Reference group. If NULL, uses first level.
#' @param verbose Logical. Print summary.
#'
#' @return The `aibias` object with `$transitions` populated. Contains:
#'   - `probs`: Transition probabilities by group and time
#'   - `pooled`: Pooled transition probabilities (time-averaged)
#'   - `disparity`: Transition disparity \eqn{\Delta^{ab}_{g,r}(t)}
#'   - `recovery_gap`: Disparity in 0->1 transitions (recovery)
#'   - `retention_gap`: Disparity in 1->1 transitions (retention)
#'   - `matrices`: Named list of 2x2 transition matrices per group
#'
#' @examples
#' data(lending_panel)
#' obj <- aib_build(lending_panel, "applicant_id", "year", "race", "approved")
#' obj <- aib_transition(obj, ref_group = "White")
#' obj$transitions$pooled
#'
#' @export
aib_transition <- function(object,
                           ref_group = NULL,
                           verbose   = TRUE) {
  .check_aibias(object)

  ref_group <- .resolve_ref_group(object, ref_group)
  data      <- object$data
  id_col    <- object$meta$id
  time_col  <- object$meta$time_int
  grp_col   <- object$meta$group
  dec_col   <- object$meta$decision

  # ---- Build lag structure ------------------------------------------------
  lagged <- data |>
    arrange(.data[[id_col]], .data[[time_col]]) |>
    group_by(.data[[id_col]]) |>
    mutate(
      D_lag  = dplyr::lag(.data[[dec_col]]),
      T_curr = .data[[time_col]]
    ) |>
    ungroup() |>
    filter(!is.na(.data$D_lag))

  # ---- Estimate transition probabilities by group x time ------------------
  trans_long <- lagged |>
    group_by(.data[[grp_col]], .data$T_curr, .data$D_lag) |>
    summarise(
      n_total  = n(),
      n_pos    = sum(.data[[dec_col]], na.rm = TRUE),
      p_01_or_11 = mean(.data[[dec_col]], na.rm = TRUE),
      .groups  = "drop"
    ) |>
    rename(group = 1, time = 2, from_state = 3)

  # Separate 0->1 (recovery) and 1->1 (retention)
  trans_wide <- trans_long |>
    mutate(
      trans_type = ifelse(.data$from_state == 0, "p01", "p11")
    ) |>
    select("group", "time", "trans_type",
           "n_total", "n_pos", "p_01_or_11") |>
    pivot_wider(
      names_from  = "trans_type",
      values_from = c("n_total", "n_pos", "p_01_or_11")
    )

  # ---- Pooled (time-averaged) transition probs per group ------------------
  pooled <- lagged |>
    group_by(.data[[grp_col]], .data$D_lag) |>
    summarise(
      n_total     = n(),
      n_pos       = sum(.data[[dec_col]], na.rm = TRUE),
      p_transition = mean(.data[[dec_col]], na.rm = TRUE),
      .groups     = "drop"
    ) |>
    rename(group = 1, from_state = 2)

  # ---- Transition disparities relative to ref group ----------------------
  ref_trans <- trans_wide |>
    filter(.data$group == ref_group) |>
    select("time",
           p01_ref = "p_01_or_11_p01",
           p11_ref = "p_01_or_11_p11")

  disparity <- trans_wide |>
    filter(.data$group != ref_group) |>
    left_join(ref_trans, by = "time") |>
    mutate(
      delta_01 = .data$p_01_or_11_p01 - .data$p01_ref,  # recovery gap
      delta_11 = .data$p_01_or_11_p11 - .data$p11_ref   # retention gap
    ) |>
    select("group", "time", "delta_01", "delta_11",
           p01_g = "p_01_or_11_p01", p11_g = "p_01_or_11_p11",
           "p01_ref", "p11_ref")

  # ---- Pooled disparity summaries ----------------------------------------
  pooled_ref <- pooled |>
    filter(.data$group == ref_group) |>
    select("from_state", p_ref = "p_transition")

  recovery_gap <- pooled |>
    filter(.data$group != ref_group, .data$from_state == 0) |>
    left_join(pooled_ref |> filter(.data$from_state == 0), by = "from_state") |>
    mutate(recovery_gap = .data$p_transition - .data$p_ref) |>
    select("group", p_recovery_g = "p_transition",
           p_recovery_ref = "p_ref", "recovery_gap")

  retention_gap <- pooled |>
    filter(.data$group != ref_group, .data$from_state == 1) |>
    left_join(pooled_ref |> filter(.data$from_state == 1), by = "from_state") |>
    mutate(retention_gap = .data$p_transition - .data$p_ref) |>
    select("group", p_retention_g = "p_transition",
           p_retention_ref = "p_ref", "retention_gap")

  # ---- Build 2x2 matrices per group (pooled) -----------------------------
  matrices <- lapply(levels(data[[grp_col]]), function(g) {
    pg  <- pooled |> filter(.data$group == g)
    p01 <- pg |> filter(.data$from_state == 0) |> pull(.data$p_transition)
    p11 <- pg |> filter(.data$from_state == 1) |> pull(.data$p_transition)
    if (length(p01) == 0) p01 <- NA_real_
    if (length(p11) == 0) p11 <- NA_real_
    matrix(
      c(1 - p01, p01, 1 - p11, p11),
      nrow = 2, byrow = TRUE,
      dimnames = list(c("from_0", "from_1"), c("to_0", "to_1"))
    )
  })
  names(matrices) <- levels(data[[grp_col]])

  # ---- State distribution evolution --------------------------------------
  state_evol <- .compute_state_evolution(matrices, data, grp_col, dec_col,
                                          time_col, object$meta$n_times)

  # ---- Markov amplification operator A_state(T) --------------------------
  groups_nref <- setdiff(levels(data[[grp_col]]), ref_group)
  amp_state <- lapply(groups_nref, function(g) {
    dists <- state_evol |>
      filter(.data$group %in% c(g, ref_group)) |>
      pivot_wider(names_from = "group", values_from = "p_favorable")
    total_divergence <- sum(abs(dists[[g]] - dists[[ref_group]]), na.rm = TRUE)
    tibble(group = g, A_state = total_divergence)
  }) |> bind_rows()

  object$transitions <- list(
    probs        = trans_wide,
    pooled       = pooled,
    disparity    = disparity,
    recovery_gap  = recovery_gap,
    retention_gap = retention_gap,
    matrices     = matrices,
    state_evol   = state_evol,
    amp_state    = amp_state,
    ref_group    = ref_group
  )

  if (verbose) {
    cli_h2("Transition Analysis (ref: {ref_group})")
    for (g in groups_nref) {
      rg <- recovery_gap  |> filter(.data$group == g) |> pull(.data$recovery_gap)
      rtn <- retention_gap |> filter(.data$group == g) |> pull(.data$retention_gap)
      if (length(rg)  == 0) rg  <- NA
      if (length(rtn) == 0) rtn <- NA
      cli_li("Group {g}: recovery gap = {round(rg,3)}, retention gap = {round(rtn,3)}")
    }
  }

  invisible(object)
}


# ---- Internal: state distribution evolution ---------------------------------

.compute_state_evolution <- function(matrices, data, grp_col, dec_col,
                                      time_col, n_times) {
  groups <- names(matrices)
  purrr::map_dfr(groups, function(g) {
    # Empirical initial distribution
    t1_data <- data |>
      filter(.data[[grp_col]] == g, .data[[time_col]] == 1)
    p0 <- mean(t1_data[[dec_col]], na.rm = TRUE)
    v  <- c(1 - p0, p0)  # (P(D=0), P(D=1))
    M  <- matrices[[g]]

    rows <- vector("list", n_times)
    for (t in seq_len(n_times)) {
      rows[[t]] <- tibble(group = g, time = t, p_favorable = v[2])
      v <- v %*% M
    }
    bind_rows(rows)
  })
}
