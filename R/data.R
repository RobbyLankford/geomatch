#' Simulated Data by Geography
#'
#' These data sets provide simulated geographic level features. They are toy
#' data sets used for internal testing, examples, and demonstrations. Note
#' that this data is simulated and, therefore, **not real**.
#'
#' `simulated_zips` is simulated at a ZIP code level and `simulated_states` is
#' simulated at a state level.
#' @name simulated_geographies
NULL

#' @rdname simulated_geographies
#' @format ## `simulated_zips`
#' A `tibble` with 30 rows and 8 columns:
#' \describe{
#'   \item{zip}{Five-digit ZIP code (possibly not a real ZIP code)}
#'   \item{med_income}{Median income}
#'   \item{pop_density}{Population density}
#'   \item{pct_college}{Percent of population with a college degree}
#'   \item{avg_age}{Average age of population}
#'   \item{pct_bcollar}{Percent of population with blue collar job}
#'   \item{prior_spend}{Amount of marketing spend in prior quarter}
#'   \item{brand_aware}{Percent of population aware of the brand}
#' }
"simulated_zips"

#' @name simulated_geographies
#' @format ## `simulated_states`
#' A `tibble` with 30 rows and 8 columns (7 are the same as `simulated_zips`):
#' \describe{
#'   \item{state}{Two-digit state abbreviation}
#' }
"simulated_states"
