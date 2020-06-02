Bird Community Metrics
================

``` r
library(tidyverse)
library(neonstore)
library(lubridate)
```

``` r
brd_countdata <- neonstore::neon_read("brd_countdata-expanded", col_types = vroom::cols(.default = "c"))
```

``` r
richness <- brd_countdata %>%
  mutate(year = lubridate::year(startDate)) %>%
  select(scientificName, year, siteID) %>% 
  distinct() %>%
  count(siteID, year)
richness
```

    ## # A tibble: 163 x 3
    ##    siteID  year     n
    ##    <chr>  <dbl> <int>
    ##  1 ABBY    2017    52
    ##  2 ABBY    2018    46
    ##  3 ABBY    2019    49
    ##  4 BARR    2017    29
    ##  5 BARR    2018    24
    ##  6 BARR    2019    32
    ##  7 BART    2015    40
    ##  8 BART    2016    39
    ##  9 BART    2017    35
    ## 10 BART    2018    37
    ## # … with 153 more rows

## Let’s make a forecast:

``` r
forecast <- richness %>% group_by(siteID) %>% summarize(forecast = mean(n), sd = sd(n))
readr::write_csv(forecast, "bird_richness.csv")
```
