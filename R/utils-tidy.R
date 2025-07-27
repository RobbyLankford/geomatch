# Base R/`vctrs` implementations of `dplyr` functions
# Idea taken "Writing performance code with tidy tools" by Simon Couch
# https://www.tidyverse.org/blog/2023/04/performant-packages/

#' Internal Implementation of `dplyr::select`
#'
#' @param .data A `data.frame` or `tibble`.
#' @param ... Quoted column names in `.data`.
#' @param .call Used for error handling.
#'
#' @return A `tibble`
#'
#' @noRd
select_ <- function(.data, ..., .call = caller_env()) {
  check_cols_exist(.data, ..., .call = .call)

  as_tibble(.data[c(...)])
}


#' Internal Implementation of `dplyr::select` (Specifically for Removing)
#'
#' @param .data A `data.frame` or `tibble`.
#' @param ... Quoted column names in `.data`.
#' @param .call Used for error handling.
#'
#' @return A `tibble`
#'
#' @noRd
remove_ <- function(.data, ..., .call = caller_env()) {
  check_cols_exist(.data, ..., .call = .call)

  as_tibble(.data[setdiff(colnames(.data), c(...))])
}


#' Internal Implementation of `dplyr::pull`
#'
#' @param .data A `data.frame` or `tibble`.
#' @param col Quoted column name in `.data`.
#' @param .call Used for error handling.
#'
#' @return A `tibble`.
#'
#' @noRd
pull_ <- function(.data, col, .call = caller_env()) {
  check_cols_exist(.data, col, .call = .call)

  as_tibble(.data[[col]])
}


#' Internal Implementation of `dplyr::filter` Using `vctrs::vec_slice`
#'
#' @inherit vctrs::vec_slice
#'
#' @noRd
filter_ <- vec_slice


#' Internal Implementation of `dplyr::mutate`
#'
#' @param .data A `data.frame` or `tibble`.
#' @param col The quoted name of the (possibly new) column.
#' @param value The value to assign to the column `col`.
#'
#' @return A `tibble`.
#'
#' @noRd
mutate_ <- function(.data, col, value) {
  .data[[col]] <- value

  as_tibble(.data)
}


#' Internal Implementation of `dplyr::arrange`
#'
#' @param .data A `data.frame` or `tibble`.
#' @param col The quoted name of the column to sort.
#' @param .direction (Optional) Whether to sort the column in ascending or
#'   descending order. Defaults to "asc" for ascending. Can also be "desc" for
#'   descending.
#'
#' @return A `tibble`.
#'
#' @noRd
arrange_ <- function(.data,
                     col,
                     .direction = c("asc", "desc"),
                     .call = caller_env()) {

  check_cols_exist(.data, col, .call = .call)

  if (!(.direction[[1]] %in% c("asc", "desc"))) {
    cli_abort(
      "Argument {.arg .direction} must be one of 'asc' or 'desc'.",
      .call = .call
    )
  }

  .direction <- .direction[[1]]

  cols_order_chr <- colnames(.data)

  to_sort_tbl <- select_(.data, c(col, setdiff(cols_order_chr, col)))
  sorted_tbl  <- vec_sort(to_sort_tbl, direction = .direction)

  select_(sorted_tbl, cols_order_chr)
}


# Helpers ---------------------------------------------------------------------
check_cols_exist <- function(.data, ..., .call = caller_env()) {
  if (!(all(c(...) %in% colnames(.data)))) {
    cli_abort("Not all column names found in {.arg .data}", call = .call)
  }

  invisible(TRUE)
}
