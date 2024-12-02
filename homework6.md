Homework6
================
Yunjia Liu
2024-12-02

## Probelm 1

Load the dataset

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## using cached file: /Users/veronica/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:17:42.269536 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

A function to calculate the r-squared and log(beta0 \* beta1)

``` r
compute_stats = function(data) {
  model <- lm(tmax ~ tmin, data = data)
  glance_model <- glance(model)
  coef_model <- coef(model)
  
  r_squared <- glance_model$r.squared
  log_beta0_beta1 <- log(abs(coef_model[1] * coef_model[2]))
  
  return(c(r_squared = r_squared, log_beta0_beta1 = log_beta0_beta1))
}
```

``` r
boot_sample = function(df) {
  sample_frac(df, replace = TRUE)
}
```

Perform bootstrap

``` r
n_boot = 5000
set.seed(123)

boot_straps =
  tibble(strap_number = 1:n_boot) |>
  mutate(
    strap_sample = map(strap_number, ~ boot_sample(weather_df)),  # Create bootstrap samples
    stats = map(strap_sample, compute_stats)  # Compute r^2 and log(beta0 * beta1)
  )
```

``` r
# Extract results into a tidy format
boot_results <- boot_straps %>%
  unnest_wider(stats)  # Unpack the results
```

## Problem 2

## Problem 3
