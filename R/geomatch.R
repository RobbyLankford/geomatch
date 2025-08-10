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

  check_id_col(.data, {{ id_col }})

  id_col_str <- arg_to_str({{ id_col }})

  check_target(.data, {{ target }}, id_col_str)

  id_col_vals <- pull_(.data, id_col_str)

  .data <- validate_id_col(.data, id_col_str)

  covariates_chr <- validate_covariates(.data, {{ covariates }})

  .matches <- validate_matches(.data, .matches, id_col_str)

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


# Helpers ---------------------------------------------------------------------

#' Check `id_col` Argument
#'
#' This function performs initial checks for the `id_col` argument, ensuring
#' that the argument is not missing and that it is a column that exists in
#' `.data`.
#'
#' @return `TRUE`, if `id_col` is valid, otherwise an error is thrown.
#'
#' @template param-data-check
#' @template param-id_col-check-unquote
#' @template param-call
#'
#' @noRd
check_id_col <- function(.data, id_col, .call = caller_env()) {
  check_missing_arg({{ id_col }}, name = "id_col", .call = .call)

  id_col_str <- arg_to_str({{ id_col }})

  check_exists_in(id_col_str, colnames(.data), name = "id_col", .call = .call)

  invisible(TRUE)
}


#' Check `target` Argument
#'
#' This function performs initial checks for the `target` argument, ensuring
#' that the argument is not missing and that it exists in the `id_col` column.
#'
#' @return `TRUE`, if `target` is valid, otherwise an error is thrown.
#'
#' @template param-data-check
#' @template param-target-check
#' @template param-id_col-check-quoted
#' @template param-call
#'
#' @noRd
check_target <- function(.data, target, id_col, .call = caller_env()) {
  check_missing_arg({{ target }}, name = "target", .call = .call)
  check_exists_in(target, .data[[id_col]], name = "target")

  invisible(TRUE)
}


#' Validate `id_col` Argument
#'
#' This function validates the value of `id_col`, ensuring that it is a
#' character value. If it is numeric, a warning will be thrown, and `id_col`
#' will be converted to character type.
#'
#' @return `.data`, with `id_col` converted to character type, if necessary.
#'
#' @template param-data-check
#' @template param-id_col-check-quoted
#' @template param-call
#'
#' @noRd
validate_id_col <- function(.data, id_col, .call = caller_env()) {
  id_col_vals <- .data[[id_col]]

  if (is_bare_numeric(id_col_vals)) {
    cli_warn(c(
            "Target variable {.var {id_col}} is numeric.",
      "i" = "Automatically casting {.var {id_col}} to character."
    ), call = .call)

    ret_tbl <- mutate_(.data, id_col, as.character(id_col_vals))
  } else {
    ret_tbl <- .data
  }

  ret_tbl
}


#' Check & Validate `covariate` Argument
#'
#' This function performs initial checks for and validates the value of
#' `covariates`, ensuring that, if it is missing, all numeric variables in
#' `.data` will be used as covariates. Further checks will be done to make sure
#' that there are numeric columns that can be used as covariates. If no numeric
#' data is available to use for covariates, an error is thrown.
#'
#' @param covariates The original value (one or multiple variables) originally
#'   passed to `covariates`.
#'
#' @return A character vector of variable names to use as covariates.
#'
#' @template param-data-check
#' @template param-call
#'
#' @noRd
validate_covariates <- function(.data, covariates, .call = caller_env()) {
  covariates_quo <- enquo(covariates)

  if (quo_is_missing(covariates_quo)) {
    cli_warn(c(
            "Argument {.arg covariates} is missing.",
      "i" = "Will attempt to use all numeric columns as covariates."
    ), call = .call)

    numeric_mask_lgl <- sapply(.data, is_bare_numeric)

    if (!any(numeric_mask_lgl)) {
      cli_abort(c(
              "No numeric columns found.",
        "i" = "Please use a data set that has at least one numeric covariate."
      ), call = .call)
    }

    numeric_cols_chr <- colnames(.data)[numeric_mask_lgl]
    numeric_data_tbl <- select_(.data, numeric_cols_chr)
    covar_names_chr  <- colnames(numeric_data_tbl)
  } else {
    covar_names_chr <- names(eval_select(covariates, data = .data))
    check_mask_lgl  <- sapply(.data[covar_names_chr], is_bare_numeric)

    if (!all(check_mask_lgl)) {
      cli_abort(c(
              "Columns specified in {.var covariates} are not all numeric.",
        "i" = "Please specify only numeric columns as covariates."
      ), call = .call)
    }
  }

  covar_names_chr
}


#' Validate `.matches` Argument
#'
#' This function validates the value of `.matches`, ensuring that it is a value
#' between some minimum (1) and maximum (one less than the number of IDs).
#'
#' @param .matches The numeric value originally passed to `.matches`.
#'
#' @return A numeric value of `.matches`, which may be updated from than the
#'   original value if it is not a valid value.
#'
#' @template param-data-check
#' @template param-id_col-check-quoted
#' @template param-call
#'
#' @noRd
validate_matches <- function(.data, .matches, id_col, .call = caller_env()) {
  min_int <- as.integer(1)
  max_int <- length(.data[[id_col]]) - 1

  matches_quo <- enquo(.matches)

  if (quo_is_missing(matches_quo)) {
    cli_warn(c(
            "No value given for argument {.arg .matches}.",
      "i" = "Setting {.arg .matches} to default value of {.val {min_int}}."
    ), call = .call)

    .matches <- min_int
  }

  if (.matches < 1) {
    cli_warn(c(
            "Argument {.arg .matches} cannot be less than {.val {min_int}}.",
      "i" = "Setting {.arg .matches} to default value of {.val {min_int}}."
    ), call = .call)

    .matches <- min_int
  }

  if (.matches > max_int) {
    cli_warn(c(
            "Argument {.arg .matches} cannot be greater than {.val {max_int}}.",
      "i" = "Setting {.arg .matches} to maximum value of {.val {max_int}}."
    ), call = .call)

    .matches <- max_int
  }

  .matches
}
