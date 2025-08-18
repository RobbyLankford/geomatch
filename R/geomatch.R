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
#' @param .keep (Optional) Whether to keep all columns in `.data` in the output
#'   (`TRUE`) or just the `id_col` and `covariates` (`FALSE`). Defaults to
#'   `FALSE`.
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
                      .scale   = FALSE,
                      .keep    = FALSE) {

  UseMethod("geo_match")
}

#' @rdname geo_match
#' @export
geo_match.data.frame <- function(.data,
                                 id_col,
                                 target,
                                 covariates,
                                 .matches = 1,
                                 .scale   = FALSE,
                                 .keep    = FALSE) {

  #> need `id_col` to be a column in `.data`
  check_id_col(.data, {{ id_col }})

  id_col_str <- arg_to_str({{ id_col }})

  #> need `target` to be a (character) value in `id_col`
  check_target(.data, {{ target }}, id_col_str)

  .data <- validate_id_col(.data, id_col_str)

  #> need at least one numeric `covariate`
  covariates_chr <- validate_covariates(.data, {{ covariates }})

  #> need `.matches` to be at least one but less that total number of IDs
  .matches <- validate_matches(.data, .matches, id_col_str)

  scores_tbl <- calc_match_scores(
    .data, id_col_str, target, covariates_chr,

    .scale = .scale,
    .func  = calc_euclidean_dist,
    .keep  = .keep
  )

  extract_num_matches(scores_tbl, .matches = .matches)
}


# Helpers ---------------------------------------------------------------------

#' Check `id_col` Argument
#'
#' This function performs initial checks for the `id_col` argument, ensuring
#' that the argument is not missing and that it is a column that exists in
#' `.data`.
#'
#' @param id_col The unquoted value originally passed to `id_col`.
#'
#' @templateVar param id_col
#' @template param-data-check
#' @template param-call
#' @template return-validated
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
#' @templateVar param target
#' @template param-data-check
#' @template param-target-check
#' @template param-id_col-quoted
#' @template param-call
#' @template return-validated
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
#' @template param-id_col-quoted
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
#' @return A numeric value of `.matches`, which may be updated from than the
#'   original value if it is not a valid value.
#'
#' @template param-data-check
#' @template param-matches-check
#' @template param-id_col-quoted
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


#' Normalize a Numeric Data Set
#'
#' This function normalizes (centers and scales) a numeric data set and
#' reorganizes it to have the `id_col` as the first column and all other
#' columns following.
#'
#' @template param-data-match
#' @template param-id_col-quoted
#' @template return-tibble
#'
#' @noRd
scale_data <- function(.data, id_col) {
  id_vals <- pull_(.data, id_col)

  match_mtrx <- as.matrix(remove_(.data, id_col))
  scale_mtrx <- scale(match_mtrx, center = TRUE, scale = TRUE)

  vec_cbind(tibble({{ id_col }} := id_vals), as.data.frame(scale_mtrx))
}


#' Format Data for Matching
#'
#' This function formats a data set to prepare it for matching. It subsets
#' the data set to just the `id_col` and `covariates` columns, and applies
#' scaling, if desired.
#'
#' @template param-data-check
#' @template param-id_col-quoted
#' @template param-scale-check
#' @template return-tibble
#'
#' @noRd
create_match_data <- function(.data, id_col, covariates, .scale) {
  match_data_tbl <- select_(.data, c(id_col, covariates))

  if (.scale) match_data_tbl <- scale_data(match_data_tbl, id_col)

  match_data_tbl
}


#' Filter a Data Set Based on a Predicate
#'
#' This function filters a data set to return only those rows where the value
#' in a column (`id_col`) is equal to some specified value (`target`).
#'
#' @param .negate (Optional) Whether to return all rows where `id_col` is equal
#'   to `target` (`FALSE`) or where `id_col` is not equal to `target` (`TRUE`).
#'   (Defaults to `FALSE`).
#'
#' @template param-id_col-quoted
#' @template param-target-check
#' @template return-tibble
#'
#' @noRd
extract_rows <- function(.data, id_col, target, .negate = FALSE) {
  if (.negate) {
    mask_lgl <- pull_(.data, id_col) != target
  } else {
    mask_lgl <- pull_(.data, id_col) == target
  }

  filter_(.data, mask_lgl)
}


#' Order Target and Control Rows
#'
#' This function helps prepare data for matching by ordering the target and
#' row columns. Functions that calculate distance in this package assume that
#' the target row is the first row.
#'
#' @template param-data-match
#' @template param-id_col-quoted
#' @template param-target-check
#' @template return-tibble
#'
#' @noRd
order_target_control <- function(.data, id_col, target) {
  target_rows  <- extract_rows(.data, id_col, target)
  control_rows <- extract_rows(.data, id_col, target, .negate = TRUE)

  vec_rbind(target_rows, control_rows)
}


#' Apply Scoring Function
#'
#' This function applies a generic scoring function to a data set. The scoring
#' function must work on rows and must assume that the target row is the first
#' row.
#'
#' @template param-data-match
#' @template param-id_col-quoted
#' @template param-target-check
#' @template param-func
#'
#' @noRd
apply_score_func <- function(.data, id_col, target, .func) {
  data_tbl <- order_target_control(.data, id_col, target)

  .func(remove_(data_tbl, id_col))
}


#' Calculate Euclidean Distance
#'
#' This function calculates the Euclidean distance between a target row and
#' all other rows. It assumes that the target row is the first row.
#'
#' @template param-data-match
#' @template return-tibble
#'
#' @noRd
calc_euclidean_dist <- function(.data) {

  #> need target to be top row
  target_row <- vec_slice(.data, 1)

  apply(.data, 1, function(row) {
    sqrt(sum((row - target_row) ^ 2))
  })
}


#' Calculate Match Scores
#'
#' This function takes in all the checked and validated inputs to `geo_match`,
#' and a distance calculation function, formats the data, and runs to the
#' distance calculation function to return the match scores.
#'
#' @param covariates A character vector of (quoted) covariate column names.
#' @param .keep The `TRUE`/`FALSE` value originally passed to `.keep`.
#'
#' @template param-data-check
#' @template param-id_col-quoted
#' @template param-target-check
#' @template param-scale-check
#' @template param-func
#' @template return-tibble
#'
#' @noRd
calc_match_scores <- function(.data,
                              id_col,
                              target,
                              covariates,
                              .scale,
                              .func,
                              .keep) {

  match_data_tbl <- create_match_data(.data, id_col, covariates, .scale)
  distances_num  <- apply_score_func(match_data_tbl, id_col, target, .func)

  out_tbl <- order_target_control(.data, id_col, target)
  out_tbl <- mutate_(out_tbl, ".distance", distances_num)

  if (!.keep) {
    out_tbl <- select_(out_tbl, c(colnames(match_data_tbl), ".distance"))
  }

  arrange_(out_tbl, ".distance", .direction = "asc")
}


#' Extract The Number of Desired Matches
#'
#' This function filters the final, post-[calc_match_scores()] data set to
#' have only the desired number of matches.
#'
#' @template param-data
#' @template param-matches-check
#' @template return-tibble
#'
#' @noRd
extract_num_matches <- function(.data, .matches = 1) {
  matches_num <- .matches + 1

  filter_(.data, 1:matches_num)
}
