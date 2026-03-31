#' AIBias: Longitudinal Bias Auditing for Sequential Decision Systems
#'
#' @description
#' AIBias treats algorithmic bias as a longitudinal process on repeated
#' decisions, not a one-time snapshot. The package provides tools for:
#'
#' - **Trajectory analysis**: How group-level decision disparities evolve over time
#' - **Transition analysis**: Where compounding occurs in decision sequences
#' - **Amplification detection**: Whether prior decisions worsen later inequality
#'
#' ## Core Workflow
#'
#' ```r
#' # Build the audit object
#' obj <- aib_build(data, id = "unit_id", time = "wave", group = "group",
#'                  decision = "approved")
#'
#' # Run the full audit pipeline
#' result <- aib_audit(obj, ref_group = "majority")
#'
#' # Or step by step:
#' desc   <- aib_describe(obj)
#' trans  <- aib_transition(obj)
#' amp    <- aib_amplify(obj)
#'
#' plot(obj, type = "trajectory")
#' summary(obj)
#' ```
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr filter mutate select group_by summarise left_join n
#'   arrange ungroup distinct pull across all_of rename bind_rows
#' @importFrom tidyr pivot_wider pivot_longer complete
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_tile geom_hline
#'   geom_point geom_col geom_text facet_wrap scale_fill_gradient2
#'   scale_color_manual scale_fill_manual scale_color_brewer labs theme
#'   theme_minimal element_text element_blank scale_x_continuous
#'   scale_y_continuous coord_cartesian
#' @importFrom rlang sym enquo quo_name abort warn inform caller_env .data
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning
#'   cli_h1 cli_h2 cli_ul cli_li
#' @importFrom purrr map map_dfr map2 imap reduce
#' @importFrom tibble tibble as_tibble
#' @importFrom stats plogis qnorm quantile sd var weighted.mean
#'   as.formula setNames binomial glm predict
## usethis namespace: end
NULL
