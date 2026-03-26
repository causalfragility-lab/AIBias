#' Run the full AIBias audit pipeline
#'
#' @description
#' A convenience wrapper that runs the complete audit pipeline:
#' `aib_describe()` → `aib_transition()` → `aib_amplify()`.
#' Optionally runs `aib_bootstrap()` for uncertainty quantification.
#'
#' @param object An `aibias` object from [AIBias::aib_build()], OR a data frame
#'   (in which case `...` arguments are passed to [AIBias::aib_build()]).
#' @param ref_group Character. Reference group.
#' @param bootstrap Logical. Run bootstrap CIs? Default FALSE.
#' @param B Integer. Bootstrap replicates if `bootstrap = TRUE`.
#' @param seed Integer. Random seed.
#' @param verbose Logical.
#' @param ... If `object` is a data frame, additional arguments passed
#'   to [AIBias::aib_build()].
#'
#' @return A fully-populated `aibias` object.
#'
#' @examples
#' data(lending_panel)
#' result <- aib_audit(lending_panel,
#'                     id        = "applicant_id",
#'                     time      = "year",
#'                     group     = "race",
#'                     decision  = "approved",
#'                     ref_group = "White")
#' \donttest{
#' summary(result)
#' plot(result, type = "trajectory")
#' }
#'
#' @export
aib_audit <- function(object,
                      ref_group = NULL,
                      bootstrap = FALSE,
                      B         = 500,
                      seed      = NULL,
                      verbose   = TRUE,
                      ...) {

  # Allow passing a data frame directly
  if (is.data.frame(object)) {
    if (verbose) cli_h1("AIBias Full Audit")
    object <- aib_build(object, verbose = verbose, ...)
  } else {
    .check_aibias(object)
    if (verbose) cli_h1("AIBias Full Audit")
  }

  object <- aib_describe(object,   ref_group = ref_group, verbose = verbose)
  object <- aib_transition(object, ref_group = ref_group, verbose = verbose)
  object <- aib_amplify(object,    ref_group = ref_group, verbose = verbose)

  if (bootstrap) {
    object <- aib_bootstrap(object, B = B, ref_group = ref_group,
                             seed = seed, verbose = verbose)
  }

  if (verbose) {
    cli_h1("Audit complete")
    cli_alert_info("Use summary(object) for a full report.")
    cli_alert_info("Use plot(object, type = 'trajectory') to visualize.")
  }

  invisible(object)
}
