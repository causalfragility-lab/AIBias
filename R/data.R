#' Synthetic Lending Panel Dataset
#'
#' @description
#' A synthetic panel dataset simulating loan application decisions over six
#' years for applicants from three racial groups. Designed to illustrate
#' longitudinal bias analysis with AIBias.
#'
#' The data are generated so that Black and Hispanic applicants face lower
#' approval rates, lower recovery probabilities after denial, and lower
#' retention probabilities after approval — producing compounding disparities
#' over time.
#'
#' @format A data frame with 3,600 rows and 6 columns:
#' \describe{
#'   \item{applicant_id}{Character. Unique applicant identifier.}
#'   \item{year}{Integer. Year of application (2015–2020).}
#'   \item{race}{Factor. Racial group: White, Black, Hispanic.}
#'   \item{income}{Numeric. Annual income (thousands USD).}
#'   \item{credit_score}{Numeric. Credit score (300–850).}
#'   \item{approved}{Integer. Loan approval decision (1 = approved, 0 = denied).}
#' }
#'
#' @details
#' Transition parameters used in data generation:
#'
#' | Group    | P(approve | prev approved) | P(approve | prev denied) |
#' |----------|------------------------|---------------------|
#' | White    | 0.82                   | 0.65                |
#' | Black    | 0.62                   | 0.38                |
#' | Hispanic | 0.68                   | 0.44                |
#'
#' @source Synthetic data generated via `data-raw/lending_panel.R`.
#'
#' @examples
#' data(lending_panel)
#' head(lending_panel)
#' table(lending_panel$race, lending_panel$year)
"lending_panel"
