#' Match Geographies Based on Statistical Similarity
#'
#' This function takes a `data.frame` of geographies (*e.g.*, U.S. States, ZIP
#' codes, *etc.*), where each row is a unique geography, and features about
#' those geographies (*e.g.*, population density, GDP, *etc.*), and, for one
#' specified geography, calculates the "most similar" geography to it. That is,
#' given a `data.frame` of U.S. States, and various features about those States,
#' and specifying one specific state (say, Massachusetts), this function will
#' calculate a similarity metric (*e.g.*, Euclidean Distance) and return, from
#' the remaining States, the State most similar to it.
#'
#' @details
#' ## Feature Format Requirements
#' Note that, in order to calculate the similarity metric, all features must be
#' numeric. Non-numeric features will be ignored. Note also that the column
#' of geographic labels must be character. If a non-character column is
#' supplied, it will be converted to character.
#'
#' ## Available Similarity Metrics
#' Currently, Euclidean Distance is used as the similarity metric. Future
#' updates will include additional metrics to choose.
#'
#' @param .data A `data.frame` or [tibble][tibble::tibble-package] with one
#'   character column of unique geographic labels and at least one numeric
#'   column of a feature about those geographies.
#' @param id_col The (unquoted) column name of geographic labels.
#' @param target The target geographic label to which a match will be made.
#' @param covariates The (quoted) column, or columns (in the form `c(...)`)
#'   of features to use for calculating the similarity metric.
#' @param .matches (Optional) The number of geographies matched to `target` to
#'   return. Defaults to 1.
#' @param .scale (Optional) Whether to normalize the `covariates` before
#'   calculating the similarity metric. Defaults to `FALSE`.
#'
#' @returns A `.matches` + 1 row [tibble][tibble::tibble-package] of `id_col`,
#'   `covariates`, and an additional `.distance` column that contains the
#'   calculated similarity metric.
#'
#' @examples
#' \dontrun{
#' geo_match(simulated_states, state, "CT", c("avg_age", "pop_density"))
#'
#' geo_match(
#'   simulated_states, state, "CT", c("avg_age", "pop_density"), .matches = 3
#' )
#'
#' geo_match(
#'   simulated_states, state, "CT", c("avg_age", "pop_density"), .scale = 3
#' )
#' }
#'
#' @export
geo_match <- function(.data,
                      id_col,
                      target,
                      covariates,
                      .matches = 1,
                      .scale   = FALSE) {

  id_col_quo <- enquo(id_col)

  if (quo_is_missing(id_col_quo)) {
    cli_abort(c(
            "Missing function argument.",
      "i" = "Please specify a value for argument {.arg id_col}."
    ))
  }

  id_col_str  <- as_name(id_col_quo)

  if (!(id_col_str %in% colnames(.data))) {
    cli_abort(c(
            "ID column {.val {id_col_str}} not found.",
      "i" = "Please specify a column name that exists in {.arg .data}."
    ))
  }

  id_col_vals <- pull_(.data, id_col_str)

  if (is_missing(target)) {
    cli_abort(c(
            "Missing function argument.",
      "i" = "Please specify a value for argument {.arg target}."
    ))
  }

  if (!(target %in% id_col_vals)) {
    cli_abort(c(
            "Target ID '{target}' not found.",
      "i" = "Please specify a value found in column {.var {id_col_str}}."
    ))
  }

  if (is_bare_numeric(id_col_vals)) {
    cli_alert_info("Target variable {.var {id_col_str}} is numeric.")
    cli_alert_info("Casting target variable {.var {id_col_str}} to character.")

    .data <- mutate_(.data, id_col_str, as.character(id_col_vals))
  }

  if (is_missing(.matches)) .matches <- 1

  if (.matches < 1) {
    cli_abort(c(
            "Argument {.arg .matches} too small.",
      "i" = "Please specify a value of at least {.val {as.integer(1)}}."
    ))
  }

  max_int <- length(id_col_vals)

  if (.matches >= max_int) {
    cli_abort(c(
            "Argument {.arg .matches} too large.",
      "i" = "Please specify a value less than {.val {max_int}}."
    ))
  }

  covariates_quo <- enquo(covariates)

  if (quo_is_missing(covariates_quo)) {
    cli_alert_warning("Argument {.arg covariates} is missing.")
    cli_alert_info("All numeric columns will be used as covariates.")

    numeric_mask_lgl <- sapply(.data, is_bare_numeric)

    if (!any(numeric_mask_lgl)) {
      cli_abort(c(
              "No numeric columns found.",
        "i" = "At least one potential covariate column must be numeric."
      ))
    }

    numeric_cols_chr <- colnames(.data)[numeric_mask_lgl]
    numeric_data_tbl <- select_(.data, numeric_cols_chr)

    covariates_chr <- colnames(numeric_data_tbl)
  } else {
    covar_names_chr <- names(eval_select(covariates, data = .data))
    check_mask_lgl  <- sapply(.data[covar_names_chr], is_bare_numeric)

    if (!all(check_mask_lgl)) {
      cli_abort(c(
              "Columns specified in {.var covariates} are not all numeric.",
        "i" = "Please specify only numeric columns as covariates."
      ))
    }

    covariates_chr <- covar_names_chr
  }

  match_data_tbl <- select_(.data, c(id_col_str, covariates_chr))

  if (.scale) {
    match_mtrx <- as.matrix(remove_(match_data_tbl, id_col_str))
    scale_mtrx <- scale(match_mtrx, center = TRUE, scale = TRUE)

    match_data_tbl <- as_tibble(
      cbind(select_(.data, id_col_str), as.data.frame(scale_mtrx))
    )
  }

  target_mask_lgl <- pull_(match_data_tbl, id_col_str) == target
  target_row_tbl  <- filter_(match_data_tbl, target_mask_lgl)
  target_vars_tbl <- remove_(target_row_tbl, id_col_str)

  control_mask_lgl <- pull_(match_data_tbl, id_col_str) != target
  control_rows_tbl <- filter_(match_data_tbl, control_mask_lgl)
  control_vars_tbl <- remove_(control_rows_tbl, id_col_str)

  data_tbl <- rbind(target_vars_tbl, control_vars_tbl)
  dist_num <- apply(data_tbl, 1, function(row) {
    sqrt(sum((row - target_vars_tbl) ^ 2))
  })

  target_mask_lgl <- pull_(.data, id_col_str) == target
  target_row_tbl  <- filter_(.data, target_mask_lgl)

  control_mask_lgl <- pull_(.data, id_col_str) != target
  control_rows_tbl <- filter_(.data, control_mask_lgl)

  out_tbl <- rbind(target_row_tbl, control_rows_tbl)
  out_tbl <- mutate_(out_tbl, ".distance", dist_num)
  out_tbl <- arrange_(out_tbl, ".distance", .direction = "asc")

  filter_(out_tbl, 1:(.matches + 1))
}
