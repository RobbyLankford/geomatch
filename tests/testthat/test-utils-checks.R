test_that("missing function argument throws error and informs user", {
  test_func <- function(x) {
    check_missing_arg({{ x }}, name = "x")
  }

  expect_error(test_func(), regex = "Missing function argument")

  expect_no_error(test_func(x = 1))
  expect_invisible(test_func(x = 1))
  expect_true(test_func(x = 1))
})

test_that("`item` not existing in `values` throws error and informs user", {
  vals <- c("a", "b", "c")

  expect_error(check_exists_in("d", vals, name = "item"), regexp = "not found")

  expect_no_error(check_exists_in("a", vals, name = "item"))
  expect_invisible(check_exists_in("a", vals, name = "item"))
  expect_true(check_exists_in("a", vals, name = "item"))
})


# Helpers ---------------------------------------------------------------------
test_that("helper `arg_to_string` converts function argument to string", {
  temp_func <- function(x) {
    arg_to_str({{ x }})
  }

  expect_equal(temp_func(test), "test")
})

test_that("helper `call_to_str` converts expression to string", {
  temp_func <- function(call) {
    call_to_str({{ call }})
  }

  expect_equal(temp_func(names(simulated_states)), "names(simulated_states)")
})
