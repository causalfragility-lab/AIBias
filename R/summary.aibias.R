#' Print an aibias object
#'
#' @param x An `aibias` object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}, called for its side effect of printing a concise summary of the audit object to the console.
#' @export
print.aibias <- function(x, ...) {
  cat("\n")
  cat("== AIBias Audit Object ==\n")
  cat("  Units       :", x$meta$n_units, "\n")
  cat("  Time points :", x$meta$n_times,
      "(", paste(x$meta$raw_times, collapse = ", "), ")\n")
  cat("  Groups      :", paste(x$meta$groups, collapse = ", "), "\n")
  cat("  Decision    :", x$meta$decision, "\n")
  if (!is.null(x$meta$outcome))
    cat("  Outcome     :", x$meta$outcome, "\n")
  cat("\n  Components populated:\n")
  components <- c("bias", "transitions", "amplification", "adjusted", "bootstrap")
  for (comp in components) {
    status <- if (!is.null(x[[comp]])) "\u2713" else "\u2715"
    cat("   ", status, comp, "\n")
  }
  cat("\n")
  invisible(x)
}

#' Summarize an aibias object
#'
#' @description
#' Produces a comprehensive audit summary including trajectory statistics,
#' transition gaps, amplification indices, and narrative interpretation.
#'
#' @param object An `aibias` object.
#' @param ... Ignored.
#' @return Invisibly returns \code{object}, called for its side effect of printing a comprehensive audit summary including trajectory statistics, transition gaps, amplification indices, and narrative interpretation to the console.
#' @export
summary.aibias <- function(object, ...) {
  cat("\n")
  cat("== AIBias Audit Summary ==\n")

  cat(sprintf("  Panel: %d units x %d waves | %d groups\n",
              object$meta$n_units, object$meta$n_times,
              length(object$meta$groups)))
  cat(sprintf("  Decision variable : '%s'\n", object$meta$decision))
  cat("\n")

  # ---- Decision rates by group ------------------------------------------
  cli_h2("Decision Rates by Group and Time")
  rates_wide <- object$rates |>
    select("group", "time", "rate") |>
    pivot_wider(names_from = "time", values_from = "rate",
                names_prefix = "t=")
  rates_wide[, -1] <- round(rates_wide[, -1], 3)
  print(as.data.frame(rates_wide), row.names = FALSE)
  cat("\n")

  # ---- Bias trajectories ------------------------------------------------
  if (!is.null(object$bias)) {
    cli_h2("Bias Trajectory Summary")
    ref <- object$bias$ref_group
    cat(sprintf("  Reference group: %s\n\n", ref))

    cb <- object$bias$cumulative |>
      select("group", "CB_normalized", "peak_disparity", "time_to_peak")
    cb[, -1] <- round(cb[, -1], 3)
    print(as.data.frame(cb), row.names = FALSE)
    cat("\n")
  }

  # ---- Transitions -------------------------------------------------------
  if (!is.null(object$transitions)) {
    cli_h2("Transition Gaps (pooled, vs reference)")
    rg  <- object$transitions$recovery_gap
    rtn <- object$transitions$retention_gap

    if (nrow(rg) > 0 && nrow(rtn) > 0) {
      gaps <- left_join(
        rg  |> select("group", "p_recovery_g",  "recovery_gap"),
        rtn |> select("group", "p_retention_g", "retention_gap"),
        by = "group"
      )
      gaps[, -1] <- round(gaps[, -1], 3)
      print(as.data.frame(gaps), row.names = FALSE)
    } else {
      cat("  (Insufficient data for transition gaps)\n")
    }
    cat("\n")
  }

  # ---- Amplification -----------------------------------------------------
  if (!is.null(object$amplification)) {
    cli_h2("Amplification Index Summary")
    amp_tbl <- object$amplification$cumulative
    amp_tbl[, -1] <- round(amp_tbl[, -1], 3)
    print(as.data.frame(amp_tbl), row.names = FALSE)
    cat("\n")

    cli_h2("Narrative Interpretation")
    for (i in seq_len(nrow(object$amplification$narratives))) {
      row <- object$amplification$narratives[i, ]
      g   <- row$group
      msg <- row$narrative
      if (nchar(trimws(msg)) > 0) {
        cat(sprintf("  [%s] %s\n", g, msg))
      } else {
        cat(sprintf("  [%s] No strong amplification detected.\n", g))
      }
    }
    cat("\n")
  }

  invisible(object)
}
