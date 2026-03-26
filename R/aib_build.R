#' Build an AIBias audit object
#'
#' @description
#' Constructs the core `aibias` S3 object from a panel dataset. Validates
#' the panel structure and prepares internal data for downstream analysis.
#'
#' @param data A data frame in long (panel) format.
#' @param id Character. Name of the unit identifier column.
#' @param time Character. Name of the time/wave column (integer or factor).
#' @param group Character. Name of the protected group column.
#' @param decision Character. Name of the binary decision column (0/1).
#' @param outcome Character or NULL. Optional downstream outcome column.
#' @param verbose Logical. Print validation messages. Default TRUE.
#'
#' @return An object of class `"aibias"`.
#'
#' @details
#' The function expects a balanced or unbalanced panel where:
#' - `id` indexes units observed over multiple periods
#' - `time` is an ordered index (will be coerced to integer rank)
#' - `group` is a categorical variable indicating protected group membership
#' - `decision` is a binary 0/1 variable (1 = favorable decision)
#'
#' @examples
#' data(lending_panel)
#' obj <- aib_build(lending_panel,
#'                  id       = "applicant_id",
#'                  time     = "year",
#'                  group    = "race",
#'                  decision = "approved")
#'
#' @export
aib_build <- function(data,
                      id,
                      time,
                      group,
                      decision,
                      outcome  = NULL,
                      verbose  = TRUE) {

  # ---- Input validation ------------------------------------------------
  data  <- as_tibble(data)
  cols  <- c(id, time, group, decision)
  if (!is.null(outcome)) cols <- c(cols, outcome)

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    abort(c(
      "Missing columns in `data`:",
      paste0("  x ", missing_cols)
    ))
  }

  # Coerce decision to integer 0/1
  dec_vals <- unique(data[[decision]])
  if (!all(dec_vals %in% c(0L, 1L, 0, 1, TRUE, FALSE, NA))) {
    abort(c(
      "`decision` column must be binary (0/1).",
      paste0("  Found values: ", paste(dec_vals, collapse = ", "))
    ))
  }
  data[[decision]] <- as.integer(as.logical(data[[decision]]))

  # Coerce time to sorted integer ranks
  raw_times       <- sort(unique(data[[time]]))
  time_map        <- setNames(seq_along(raw_times), as.character(raw_times))
  data$.time_int  <- time_map[as.character(data[[time]])]

  # Factor-encode group
  data[[group]] <- as.factor(data[[group]])
  groups        <- levels(data[[group]])

  # Basic panel summary
  n_units  <- length(unique(data[[id]]))
  n_times  <- length(raw_times)
  n_groups <- length(groups)

  if (verbose) {
    cli_h1("AIBias: Building audit object")
    cli_alert_info("Units       : {n_units}")
    cli_alert_info("Time points : {n_times} ({paste(raw_times, collapse = ', ')})")
    cli_alert_info("Groups      : {n_groups} ({paste(groups, collapse = ', ')})")
    cli_alert_info("Decision    : '{decision}' (mean = {round(mean(data[[decision]], na.rm=TRUE), 3)})")
    if (!is.null(outcome))
      cli_alert_info("Outcome     : '{outcome}'")
  }

  # ---- Compute base rates by group x time --------------------------------
  rates <- .compute_rates(data, group, decision, ".time_int")

  # ---- Build object ------------------------------------------------------
  obj <- structure(
    list(
      data     = data,
      meta     = list(
        id        = id,
        time      = time,
        time_int  = ".time_int",
        time_map  = time_map,
        group     = group,
        decision  = decision,
        outcome   = outcome,
        groups    = groups,
        n_units   = n_units,
        n_times   = n_times,
        raw_times = raw_times
      ),
      rates          = rates,
      bias           = NULL,
      transitions    = NULL,
      amplification  = NULL,
      adjusted       = NULL,
      bootstrap      = NULL,
      diagnostics    = list()
    ),
    class = "aibias"
  )

  if (verbose) cli_alert_success("Object built successfully.")
  obj
}


# -------------------------------------------------------------------------
# Internal helpers
# -------------------------------------------------------------------------

.compute_rates <- function(data, group, decision, time_int) {
  data |>
    group_by(.data[[group]], .data[[time_int]]) |>
    summarise(
      n          = n(),
      n_positive = sum(.data[[decision]], na.rm = TRUE),
      rate       = mean(.data[[decision]], na.rm = TRUE),
      .groups    = "drop"
    ) |>
    rename(group = 1, time = 2)
}
