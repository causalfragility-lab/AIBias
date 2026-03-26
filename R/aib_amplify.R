#' Compute dynamic bias amplification indices
#'
#' @description
#' Estimates the amplification index \eqn{A_{g,r}(t) = B_{g,r}(t|1) - B_{g,r}(t|0)},
#' which measures how conditioning on prior decision state changes the group
#' disparity at time \eqn{t}. Non-zero amplification indicates that prior
#' decisions are shaping current disparities—the hallmark of compounding bias.
#'
#' @param object An `aibias` object from [AIBias::aib_build()]. Recommended to run
#'   [AIBias::aib_transition()] first.
#' @param ref_group Character. Reference group.
#' @param verbose Logical.
#'
#' @return The `aibias` object with `$amplification` populated. Contains:
#'   - `lagged_disparity`: \eqn{B_{g,r}(t|d)} for d equal to 0 or 1
#'   - `index`: Amplification index \eqn{A_{g,r}(t)}
#'   - `cumulative`: \eqn{A_{g,r}(T)} summed over time
#'   - `matrix_norm`: \eqn{\sum_t \|P_g(t) - P_r(t)\|}
#'
#' @details
#' A decision system exhibits **bias amplification** if:
#' 1. \eqn{|B_{g,r}(t)| > |B_{g,r}(s)|} for some \eqn{t > s} (disparity grows), AND
#' 2. \eqn{A_{g,r}(t) \neq 0} (prior decisions drive current disparity), OR
#' 3. \eqn{P_g(t) \neq P_r(t)} (transition matrices are unequal)
#'
#' @examples
#' data(lending_panel)
#' obj <- aib_build(lending_panel, "applicant_id", "year", "race", "approved")
#' obj <- aib_transition(obj, ref_group = "White")
#' obj <- aib_amplify(obj, ref_group = "White")
#' obj$amplification$index
#'
#' @export
aib_amplify <- function(object,
                        ref_group = NULL,
                        verbose   = TRUE) {
  .check_aibias(object)

  ref_group <- .resolve_ref_group(object, ref_group)
  data      <- object$data
  id_col    <- object$meta$id
  time_col  <- object$meta$time_int
  grp_col   <- object$meta$group
  dec_col   <- object$meta$decision
  groups    <- setdiff(levels(data[[grp_col]]), ref_group)

  # ---- Build lag structure ------------------------------------------------
  lagged <- data |>
    arrange(.data[[id_col]], .data[[time_col]]) |>
    group_by(.data[[id_col]]) |>
    mutate(D_lag = dplyr::lag(.data[[dec_col]])) |>
    ungroup() |>
    filter(!is.na(.data$D_lag))

  # ---- Lagged group-conditional decision rates ----------------------------
  # B_{g,r}(t | d) for d in {0, 1}
  lagged_rates <- lagged |>
    group_by(.data[[grp_col]], .data[[time_col]], .data$D_lag) |>
    summarise(
      rate_cond = mean(.data[[dec_col]], na.rm = TRUE),
      n         = n(),
      .groups   = "drop"
    ) |>
    rename(group = 1, time = 2, d_lag = 3)

  ref_lagged <- lagged_rates |>
    filter(.data$group == ref_group) |>
    select("time", "d_lag", rate_ref_cond = "rate_cond")

  # ---- Compute B(t|d) and amplification index per group ------------------
  amp_list <- lapply(groups, function(g) {
    g_lag <- lagged_rates |>
      filter(.data$group == g) |>
      left_join(ref_lagged, by = c("time", "d_lag")) |>
      mutate(
        B_cond = .data$rate_cond - .data$rate_ref_cond,
        group  = g
      )

    # A(t) = B(t|1) - B(t|0)
    amp_by_time <- g_lag |>
      select("group", "time", "d_lag", "B_cond") |>
      pivot_wider(names_from  = "d_lag",
                  values_from = "B_cond",
                  names_prefix = "B_d") |>
      mutate(
        A_index = .data$B_d1 - .data$B_d0,
        amplifying = abs(.data$A_index) > 1e-10
      )

    amp_by_time
  })

  amp_index <- bind_rows(amp_list)

  # ---- Cumulative amplification -------------------------------------------
  cumulative_amp <- amp_index |>
    group_by(.data$group) |>
    summarise(
      A_cumulative   = sum(.data$A_index, na.rm = TRUE),
      A_mean         = mean(.data$A_index, na.rm = TRUE),
      A_max          = max(abs(.data$A_index), na.rm = TRUE),
      T_amplifying   = sum(.data$amplifying, na.rm = TRUE),
      .groups        = "drop"
    )

  # ---- Matrix norm amplification (if transitions computed) ----------------
  matrix_norm <- NULL
  if (!is.null(object$transitions)) {
    mats    <- object$transitions$matrices
    times   <- unique(lagged[[time_col]])
    mat_ref <- mats[[ref_group]]

    matrix_norm <- lapply(groups, function(g) {
      mat_g  <- mats[[g]]
      norm_t <- if (all(!is.na(mat_g)) && all(!is.na(mat_ref))) {
        sum(abs(mat_g - mat_ref))
      } else NA_real_
      tibble(group = g, matrix_L1_norm = norm_t)
    }) |> bind_rows()
  }

  # ---- Narrative interpretation -------------------------------------------
  narratives <- lapply(groups, function(g) {
    ca  <- cumulative_amp |> filter(.data$group == g)
    rg  <- object$transitions$recovery_gap  |> filter(.data$group == g) |>
             pull(.data$recovery_gap)
    rtn <- object$transitions$retention_gap |> filter(.data$group == g) |>
             pull(.data$retention_gap)
    if (length(rg)  == 0 || is.na(rg[1]))  rg  <- NA_real_
    if (length(rtn) == 0 || is.na(rtn[1])) rtn <- NA_real_
    rg  <- rg[1]; rtn <- rtn[1]
    A   <- ca$A_cumulative

    msgs <- character(0)
    if (!is.na(rg) && rg < -0.05)
      msgs <- c(msgs, sprintf(
        "Recovery disparity = %.3f: group %s is less likely to regain favorable status after an adverse decision.",
        rg, g))
    if (!is.na(rtn) && rtn < -0.05)
      msgs <- c(msgs, sprintf(
        "Retention disparity = %.3f: group %s is less likely to retain favorable status across periods.",
        rtn, g))
    if (!is.null(A) && !is.na(A) && abs(A) > 0.1)
      msgs <- c(msgs, sprintf(
        "Cumulative amplification A = %.3f: prior decisions compound current disparity.",
        A))

    tibble(group = g, narrative = paste(msgs, collapse = " | "))
  }) |> bind_rows()

  object$amplification <- list(
    lagged_disparity = bind_rows(amp_list |> lapply(\(x) {
      x |> select("group", "time", B_d0 = "B_d0", B_d1 = "B_d1")
    })),
    index        = amp_index,
    cumulative   = cumulative_amp,
    matrix_norm  = matrix_norm,
    narratives   = narratives,
    ref_group    = ref_group
  )

  if (verbose) {
    cli_h2("Amplification Analysis (ref: {ref_group})")
    for (g in groups) {
      ca <- cumulative_amp |> filter(.data$group == g)
      direction <- if (ca$A_mean < 0) "compounding against" else "compounding toward"
      cli_li("Group {g}: A_cumulative = {round(ca$A_cumulative,3)} ({direction} group {g})")
    }
  }

  invisible(object)
}

