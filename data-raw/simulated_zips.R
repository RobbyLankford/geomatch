library(tibble)

set.seed(1914)

num_row_int <- 30

simulated_zips <- tibble(
  zip = sprintf("%05d", sample(10000:99999, num_row_int, replace = FALSE)),

  med_income  = round(rnorm(num_row_int, mean = 50000, sd = 15000)),
  pop_density = round(rnorm(num_row_int, mean = 1500, sd = 500)),
  pct_college = round(runif(num_row_int, min = 0.2, max = 0.8), 2),
  avg_age     = round(rnorm(num_row_int, mean = 37, sd = 5), 1),
  pct_bcollar = round(runif(num_row_int, min = 0.1, max = 0.7), 2),
  prior_spend = round(rnorm(num_row_int, mean = 250e3, sd = 50e3)),
  brand_aware = round(runif(num_row_int, min = 0.3, max = 0.9), 2)
)

usethis::use_data(simulated_zips, overwrite = TRUE)
