Zooplankton Community Metrics
================

``` r
library(tidyverse)
library(neonstore)
library(lubridate)
```

``` r
# neonstore::neon_download("DP1.20219.001")
zoo <- neonstore::neon_read("zoo_taxonomyRaw-expanded")
```

``` r
richness <- zoo %>%
  mutate(year = lubridate::year(collectDate)) %>%
  select(scientificName, year, siteID) %>% 
  distinct() %>%
  count(siteID, year)
richness
```

    ## # A tibble: 36 x 3
    ##    siteID  year     n
    ##    <chr>  <dbl> <int>
    ##  1 BARC    2014    21
    ##  2 BARC    2015    37
    ##  3 BARC    2016    44
    ##  4 BARC    2017    43
    ##  5 BARC    2018    34
    ##  6 BARC    2019    25
    ##  7 CRAM    2015    37
    ##  8 CRAM    2016    49
    ##  9 CRAM    2017    44
    ## 10 CRAM    2018    37
    ## # … with 26 more rows

## Let’s forecast richness at each site in the following year\!

``` r
forecast <- richness %>% group_by(siteID) %>% summarize(forecast = mean(n), sd = sd(n))
readr::write_csv(forecast, "zoo_richness.csv")
```
