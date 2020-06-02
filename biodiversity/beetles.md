Beetle Community Metrics
================

``` r
library(tidyverse)
library(neonstore)
library(lubridate)
```

``` r
# neonstore::neon_download("DP1.10022.001")
bet_sorting <- neonstore::neon_read("bet_sorting")
```

``` r
richness <- bet_sorting %>%
  mutate(year = lubridate::year(collectDate)) %>%
  select(scientificName, year, siteID) %>% 
  distinct() %>%
  count(siteID, year)
richness
```

    ## # A tibble: 215 x 3
    ##    siteID  year     n
    ##    <chr>  <dbl> <int>
    ##  1 ABBY    2016    19
    ##  2 ABBY    2017    37
    ##  3 ABBY    2018    32
    ##  4 ABBY    2019    36
    ##  5 BARR    2017     2
    ##  6 BARR    2018     3
    ##  7 BARR    2019     7
    ##  8 BART    2014    14
    ##  9 BART    2015    25
    ## 10 BART    2016    24
    ## # … with 205 more rows

## Let’s forecast richness at each site in the following year\!

``` r
forecast <- richness %>% group_by(siteID) %>% summarize(forecast = mean(n), sd = sd(n))
readr::write_csv(forecast, "beetle_richness.csv")
```
