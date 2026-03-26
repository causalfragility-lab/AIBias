# ============================================================
# utils_checks.R - validation helpers
# ============================================================

.check_aibias <- function(x, call = caller_env()) {
  if (!inherits(x, "aibias")) {
    abort(
      c("Expected an `aibias` object.",
        "i" = "Use `aib_build()` to create one."),
      call = call
    )
  }
  invisible(TRUE)
}

.resolve_ref_group <- function(object, ref_group) {
  groups <- object$meta$groups
  if (is.null(ref_group)) {
    ref_group <- as.character(groups[1])
    inform(c(
      "No `ref_group` specified.",
      "i" = paste0("Using '", ref_group, "' as reference group.")
    ))
  }
  if (!ref_group %in% as.character(groups)) {
    abort(c(
      paste0("ref_group '", ref_group, "' not found in data."),
      "i" = paste0("Available groups: ", paste(groups, collapse = ", "))
    ))
  }
  ref_group
}


# ============================================================
# utils_panel.R - panel structure helpers
# ============================================================

#' Check panel balance
#'
#' @param object An `aibias` object.
#' @return A tibble summarizing observation counts per unit.
#' @export
aib_panel_info <- function(object) {
  .check_aibias(object)
  data   <- object$data
  id_col <- object$meta$id
  t_col  <- object$meta$time_int
  grp_col <- object$meta$group

  per_unit <- data |>
    group_by(.data[[id_col]], .data[[grp_col]]) |>
    summarise(n_obs = n(), .groups = "drop")

  summary_tbl <- per_unit |>
    group_by(.data[[grp_col]]) |>
    summarise(
      n_units    = n(),
      min_obs    = min(.data$n_obs),
      max_obs    = max(.data$n_obs),
      mean_obs   = round(mean(.data$n_obs), 2),
      balanced   = min(.data$n_obs) == max(.data$n_obs),
      .groups    = "drop"
    )
  summary_tbl
}


# ============================================================
# utils_stats.R - small statistical helpers
# ============================================================

# Proportion SE using Wald formula
.prop_se <- function(p, n) sqrt(p * (1 - p) / n)

# Two-proportion z-test (one-sided: g vs r)
.prop_ztest <- function(p_g, n_g, p_r, n_r) {
  p_pool <- (p_g * n_g + p_r * n_r) / (n_g + n_r)
  se     <- sqrt(p_pool * (1 - p_pool) * (1/n_g + 1/n_r))
  z      <- (p_g - p_r) / se
  list(z = z, p_value = 2 * stats::pnorm(-abs(z)))
}
