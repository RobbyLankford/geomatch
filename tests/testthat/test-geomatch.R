# Check Input Handling --------------------------------------------------------

## `id_col` -------------------------------------------------------------------
test_that("missing `id_col` argument throws error and informs user", {
  expect_error(geo_match(simulated_states), regexp = "id_col")
})

test_that("non-existant `id_col` argument throws error and informs user", {
  expect_error(geo_match(simulated_states, zip), regexp = "not found")
})

## `target` -------------------------------------------------------------------
test_that("missing `target` argument throws error and informs user", {
  expect_error(geo_match(simulated_states, state), regexp = "target")
})

test_that("non-existant `target` argument throws error and informs user", {
  expect_error(geo_match(simulated_states, state, "PR"), regexp = "not found")
})

test_that_cli("numeric `target` informs user and casts to character", {
  local_edition(3)

  data_num_tbl <- mutate_(
    simulated_zips, "zip", as.numeric(simulated_zips[["zip"]])
  )

  expect_snapshot(geo_match(data_num_tbl, zip, 25081, c("avg_age")))
  expect_type(
    pull_(
      suppressMessages(geo_match(data_num_tbl, zip, 25081, c("avg_age"))),
      "zip"
    ),

    "character"
  )
})

## `covariate` ----------------------------------------------------------------
test_that_cli("missing `covariate` argument informs user of correction", {
  local_edition(3)

  expect_snapshot(geo_match(simulated_states, state, "CT"))
})

test_that("having no numeric columns throws error and informs user", {
  data_no_num_tbl <- select_(simulated_states, c("state", "avg_age"))

  data_no_num_tbl <- mutate_(
    data_no_num_tbl, "avg_age", as.character(data_no_num_tbl[["avg_age"]])
  )

  expect_error(geo_match(data_no_num_tbl, state, "CT"))
  expect_error(geo_match(data_no_num_tbl, state, "CT", c("avg_age")))
})

test_that("having some numeric covariates ignores non-numeric", {
  data_some_num_tbl <- select_(
    simulated_states, c("state", "avg_age", "pop_density")
  )

  data_some_num_tbl <- mutate_(
    data_some_num_tbl, "avg_age", as.character(data_some_num_tbl[["avg_age"]])
  )

  expect_no_error(geo_match(data_some_num_tbl, state, "CT"))
})

test_that("specifying a non-numeric covariate throws error and informs user", {
  data_some_num_tbl <- select_(
    simulated_states, c("state", "avg_age", "pop_density")
  )

  data_some_num_tbl <- mutate_(
    data_some_num_tbl, "avg_age", as.character(data_some_num_tbl[["avg_age"]])
  )

  expect_error(
    geo_match(data_some_num_tbl, state, "CT", c("avg_age", "pop_density"))
  )
})

## `.matches`
test_that("specifying too few matches throws error and informs user", {
  expect_error(
    geo_match(
      simulated_states, state, "CT", c("avg_age", "pop_density"),

      .matches = 0
    )
  )
})

test_that("specifying too many matches throws error and informs user", {
  too_many_int <- nrow(simulated_states)

  expect_error(
    geo_match(
      simulated_states, state, "CT", c("avg_age", "pop_density"),

      .matches = too_many_int
    )
  )
})


# Check Function Output -------------------------------------------------------
test_that("function output is formatted properly", {
  result_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density")
  )

  #> should be a `tbl`
  expect_s3_class(result_tbl, "tbl_df")

  #> should have one additional column named `.distance`
  expect_equal(ncol(result_tbl), ncol(simulated_states) + 1)

  expect_setequal(
    colnames(result_tbl),
    c(colnames(simulated_states), ".distance")
  )

  #> should have one row for target and additional row for each of `.matches`
  expect_equal(nrow(result_tbl), 1 + 1)
})

test_that("changing `.match` results correct number of matches", {
  result_3_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density"), .matches = 3
  )

  result_7_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density"), .matches = 7
  )

  result_10_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density"), .matches = 10
  )

  #> should have `.matches` + 1 rows
  expect_equal(nrow(result_3_tbl), 3 + 1)
  expect_equal(nrow(result_7_tbl), 7 + 1)
  expect_equal(nrow(result_10_tbl), 10 + 1)
})

test_that("setting `.scale = TRUE` scales data before calculating distance", {
  result_no_scale_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density"), .scale = FALSE
  )

  result_scale_tbl <- geo_match(
    simulated_states, state, "CT", c("avg_age", "pop_density"), .scale = TRUE
  )

  expect_lt(
    result_scale_tbl[[".distance"]][[2]],
    result_no_scale_tbl[[".distance"]][[2]]
  )
})
