#' Check for Missing Function Argument
#'
#' @param arg The value of a function argument (should be passed using {{ }}).
#' @param name The name of the argument to use in the error message.
#' @param .call
#'
#' @return `TRUE`, invisibly, if argument is not missing, otherwise an error
#'   is thrown.
#'
#' @template param-call
#'
#' @noRd
check_missing_arg <- function(arg, name, .call = caller_env()) {
  arg_quo <- enquo(arg)

  if (quo_is_missing(arg_quo)) {
    cli_abort(c(
            "Missing function argument.",
      "i" = "Please specify a value for argument {.arg {name}}."
    ), call = .call)
  }

  invisible(TRUE)
}


#' Check if Item Exists in a Set of Values
#'
#' @param item The item to attempt to find in `values`.
#' @param values The values in which to search for `item`.
#' @param name The name of the set of values to use in the error message.
#'
#' @return `TRUE`, invisibly, if `item` is found in `values`, otherwise an
#'   error is thrown.
#'
#' @template param-call
#'
#' @noRd
check_exists_in <- function(item, values, name, .call = caller_env()) {
  vals_quo <- enquo(values)

  if (quo_is_call(vals_quo)) {
    vals_chr <- expr_to_str({{ values }})
  } else {
    vals_chr <- as_label(vals_quo)
  }

  if (!(item %in% values)) {
    cli_abort(c(
            "Value {.val {item}} not found in {.arg {vals_chr}}.",
      "i" = "Please specify a value for {.arg {name}} that exists."
    ), call = .call)
  }

  invisible(TRUE)
}


# Helpers ---------------------------------------------------------------------

arg_to_str <- function(arg) as_name(enquo(arg))

expr_to_str <- function(.expr) as_label(quo_get_expr(enquo(.expr)))
