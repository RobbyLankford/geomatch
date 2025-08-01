# numeric `target` informs user and casts to character [plain]

    Code
      geo_match(data_num_tbl, zip, 25081, c("avg_age"))
    Message
      i Target variable `zip` is numeric.
      i Casting target variable `zip` to character.
    Output
      # A tibble: 2 x 9
        zip   med_income pop_density pct_college avg_age pct_bcollar prior_spend
        <chr>      <dbl>       <dbl>       <dbl>   <dbl>       <dbl>       <dbl>
      1 25081      39983        1754        0.58    46.1        0.39      295502
      2 67206      51577        1020        0.76    44.7        0.16      306547
      # i 2 more variables: brand_aware <dbl>, .distance <dbl>

# numeric `target` informs user and casts to character [ansi]

    Code
      geo_match(data_num_tbl, zip, 25081, c("avg_age"))
    Message
      [36mi[39m Target variable `zip` is numeric.
      [36mi[39m Casting target variable `zip` to character.
    Output
      [90m# A tibble: 2 x 9[39m
        zip   med_income pop_density pct_college avg_age pct_bcollar prior_spend
        [3m[90m<chr>[39m[23m      [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m
      [90m1[39m 25081      [4m3[24m[4m9[24m983        [4m1[24m754        0.58    46.1        0.39      [4m2[24m[4m9[24m[4m5[24m502
      [90m2[39m 67206      [4m5[24m[4m1[24m577        [4m1[24m020        0.76    44.7        0.16      [4m3[24m[4m0[24m[4m6[24m547
      [90m# i 2 more variables: brand_aware <dbl>, .distance <dbl>[39m

# numeric `target` informs user and casts to character [unicode]

    Code
      geo_match(data_num_tbl, zip, 25081, c("avg_age"))
    Message
      ℹ Target variable `zip` is numeric.
      ℹ Casting target variable `zip` to character.
    Output
      # A tibble: 2 × 9
        zip   med_income pop_density pct_college avg_age pct_bcollar prior_spend
        <chr>      <dbl>       <dbl>       <dbl>   <dbl>       <dbl>       <dbl>
      1 25081      39983        1754        0.58    46.1        0.39      295502
      2 67206      51577        1020        0.76    44.7        0.16      306547
      # ℹ 2 more variables: brand_aware <dbl>, .distance <dbl>

# numeric `target` informs user and casts to character [fancy]

    Code
      geo_match(data_num_tbl, zip, 25081, c("avg_age"))
    Message
      [36mℹ[39m Target variable `zip` is numeric.
      [36mℹ[39m Casting target variable `zip` to character.
    Output
      [90m# A tibble: 2 × 9[39m
        zip   med_income pop_density pct_college avg_age pct_bcollar prior_spend
        [3m[90m<chr>[39m[23m      [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m
      [90m1[39m 25081      [4m3[24m[4m9[24m983        [4m1[24m754        0.58    46.1        0.39      [4m2[24m[4m9[24m[4m5[24m502
      [90m2[39m 67206      [4m5[24m[4m1[24m577        [4m1[24m020        0.76    44.7        0.16      [4m3[24m[4m0[24m[4m6[24m547
      [90m# ℹ 2 more variables: brand_aware <dbl>, .distance <dbl>[39m

# missing `covariate` argument informs user of correction [plain]

    Code
      geo_match(simulated_states, state, "CT")
    Message
      ! Argument `covariates` is missing.
      i All numeric columns will be used as covariates.
    Output
      # A tibble: 2 x 9
        state med_income pop_density pct_college avg_age pct_bcollar prior_spend
        <chr>      <dbl>       <dbl>       <dbl>   <dbl>       <dbl>       <dbl>
      1 CT         48481        1772        0.25    42.8        0.22      188553
      2 AZ         56036        1890        0.27    44.4        0.52      192168
      # i 2 more variables: brand_aware <dbl>, .distance <dbl>

# missing `covariate` argument informs user of correction [ansi]

    Code
      geo_match(simulated_states, state, "CT")
    Message
      [33m![39m Argument `covariates` is missing.
      [36mi[39m All numeric columns will be used as covariates.
    Output
      [90m# A tibble: 2 x 9[39m
        state med_income pop_density pct_college avg_age pct_bcollar prior_spend
        [3m[90m<chr>[39m[23m      [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m
      [90m1[39m CT         [4m4[24m[4m8[24m481        [4m1[24m772        0.25    42.8        0.22      [4m1[24m[4m8[24m[4m8[24m553
      [90m2[39m AZ         [4m5[24m[4m6[24m036        [4m1[24m890        0.27    44.4        0.52      [4m1[24m[4m9[24m[4m2[24m168
      [90m# i 2 more variables: brand_aware <dbl>, .distance <dbl>[39m

# missing `covariate` argument informs user of correction [unicode]

    Code
      geo_match(simulated_states, state, "CT")
    Message
      ! Argument `covariates` is missing.
      ℹ All numeric columns will be used as covariates.
    Output
      # A tibble: 2 × 9
        state med_income pop_density pct_college avg_age pct_bcollar prior_spend
        <chr>      <dbl>       <dbl>       <dbl>   <dbl>       <dbl>       <dbl>
      1 CT         48481        1772        0.25    42.8        0.22      188553
      2 AZ         56036        1890        0.27    44.4        0.52      192168
      # ℹ 2 more variables: brand_aware <dbl>, .distance <dbl>

# missing `covariate` argument informs user of correction [fancy]

    Code
      geo_match(simulated_states, state, "CT")
    Message
      [33m![39m Argument `covariates` is missing.
      [36mℹ[39m All numeric columns will be used as covariates.
    Output
      [90m# A tibble: 2 × 9[39m
        state med_income pop_density pct_college avg_age pct_bcollar prior_spend
        [3m[90m<chr>[39m[23m      [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m       [3m[90m<dbl>[39m[23m
      [90m1[39m CT         [4m4[24m[4m8[24m481        [4m1[24m772        0.25    42.8        0.22      [4m1[24m[4m8[24m[4m8[24m553
      [90m2[39m AZ         [4m5[24m[4m6[24m036        [4m1[24m890        0.27    44.4        0.52      [4m1[24m[4m9[24m[4m2[24m168
      [90m# ℹ 2 more variables: brand_aware <dbl>, .distance <dbl>[39m

