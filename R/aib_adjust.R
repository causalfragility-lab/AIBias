#' Covariate-adjusted bias trajectories
#'
#' @description
#' Estimates covariate-adjusted bias trajectories by fitting a model for
#' \eqn{Pr(D_{it} = 1 \mid X_{it}, A_i, t)} and computing marginal
#' predicted disparities by group and time.
#'
#' @param object An `aibias` object from [AIBias::aib_build()].
#' @param formula A one-sided formula specifying covariates, e.g.
#'   `~ age + income + prior_score`. The group and time variables are
#'   added automatically.
#' @param method One of `"glm"`, `"gam"`, or `"mixed"`.
#'   - `"glm"`: logistic regression
#'   - `"gam"`: generalized additive model (requires mgcv)
#'   - `"mixed"`: GLMM with random intercept for id (requires lme4)
#' @param ref_group Character. Reference group.
#' @param verbose Logical.
#'
#' @return The `aibias` object with `$adjusted` populated, containing:
#'   - `trajectory`: adjusted bias trajectory
#'   - `marginal_rates`: marginal predicted rates by group and time
#'   - `model`: the fitted model object
#'   - `formula_used`: the full formula passed to the model
#'
#' @examples
#' data(lending_panel)
#' obj <- aib_build(lending_panel, "applicant_id", "year", "race", "approved")
#' obj <- aib_adjust(obj, formula = ~ income + credit_score, method = "glm",
#'                   ref_group = "White")
#'
#' @export
aib_adjust <- function(object,
                       formula,
                       method    = c("glm", "gam", "mixed"),
                       ref_group = NULL,
                       verbose   = TRUE) {
  .check_aibias(object)
  method    <- match.arg(method)
  ref_group <- .resolve_ref_group(object, ref_group)

  data    <- object$data
  dec_col <- object$meta$decision
  grp_col <- object$meta$group
  id_col  <- object$meta$id
  t_col   <- object$meta$time_int
  groups  <- levels(data[[grp_col]])

  # ---- Build full formula -------------------------------------------------
  covariate_terms <- as.character(formula)[length(as.character(formula))]
  full_formula_chr <- paste0(
    dec_col, " ~ ", grp_col, " * factor(", t_col, ")",
    if (nchar(trimws(covariate_terms)) > 0) paste0(" + ", covariate_terms) else ""
  )
  full_formula <- as.formula(full_formula_chr)

  if (verbose) cli_alert_info("Fitting {method} model: {full_formula_chr}")

  # ---- Fit model ----------------------------------------------------------
  fit <- switch(method,
    glm   = {
      stats::glm(full_formula, data = data, family = stats::binomial())
    },
    gam   = {
      if (!requireNamespace("mgcv", quietly = TRUE))
        abort("Package 'mgcv' required for method = 'gam'. Install it first.")
      mgcv::gam(full_formula, data = data, family = stats::binomial())
    },
    mixed = {
      if (!requireNamespace("lme4", quietly = TRUE))
        abort("Package 'lme4' required for method = 'mixed'. Install it first.")
      re_formula <- as.formula(paste0(
        dec_col, " ~ ", grp_col, " * factor(", t_col, ")",
        if (nchar(trimws(covariate_terms)) > 0)
          paste0(" + ", covariate_terms)
        else "",
        " + (1 | ", id_col, ")"
      ))
      lme4::glmer(re_formula, data = data, family = stats::binomial(),
                  control = lme4::glmerControl(optimizer = "bobyqa"))
    }
  )

  # ---- Marginal predictions by group and time ----------------------------
  times  <- sort(unique(data[[t_col]]))

  marginal <- purrr::map_dfr(groups, function(g) {
    purrr::map_dfr(times, function(t) {
      pred_data        <- data
      pred_data[[grp_col]] <- factor(g, levels = groups)
      pred_data[[t_col]]   <- t

      preds <- tryCatch(
        stats::predict(fit, newdata = pred_data, type = "response"),
        error = function(e) rep(NA_real_, nrow(pred_data))
      )

      tibble(
        group          = g,
        time           = t,
        rate_adjusted  = mean(preds, na.rm = TRUE)
      )
    })
  })

  # ---- Adjusted bias trajectory ------------------------------------------
  ref_adj <- marginal |>
    filter(.data$group == ref_group) |>
    select(.data$time, rate_ref_adj = .data$rate_adjusted)

  traj_adj <- marginal |>
    filter(.data$group != ref_group) |>
    left_join(ref_adj, by = "time") |>
    mutate(
      B_adj      = .data$rate_adjusted - .data$rate_ref_adj,
      ref_group  = ref_group
    )

  object$adjusted <- list(
    trajectory     = traj_adj,
    marginal_rates = marginal,
    model          = fit,
    method         = method,
    formula_used   = full_formula_chr,
    ref_group      = ref_group
  )

  if (verbose) cli_alert_success("Adjustment complete.")
  invisible(object)
}
