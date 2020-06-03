Beetles
================

``` r
library(lubridate)
library(tidyverse)
```

``` r
# neonstore::neon_download("DP1.10022.001")
bet_sorting <- neonstore::neon_read("bet_sorting")
```

    ## Rows: 134,013
    ## Columns: 29
    ## Delimiter: ","
    ## chr  [22]: uid, namedLocation, domainID, siteID, plotID, trapID, sampleID, sampleCode, sub...
    ## dbl  [ 1]: individualCount
    ## lgl  [ 1]: sampleCondition
    ## date [ 5]: setDate, collectDate, processingDate, etOHChangeDate, identifiedDate
    ## 
    ## Use `spec()` to retrieve the guessed column specification
    ## Pass a specification to the `col_types` argument to quiet this message

``` r
sites <- neonstore:::neon_sites() 
```

``` r
bet_ts <- bet_sorting %>% 
  select(collectDate, siteID, domainID, scientificName, individualCount) %>%
  mutate(month = format(collectDate, "%Y-%m")) %>% 
  mutate(month = as.Date(paste(month, "01", sep="-"))) %>%
  group_by(scientificName, month, siteID, domainID) %>%
  summarize(count = sum(individualCount, na.rm = TRUE)) %>%
  ungroup()
```

## Counts

First let’s take a quick look at the raw counts data. This is less
meaningful than abundance, since we would need to account for detection
probability, and effort is not entirely constant over time or
necessarily equal across sites. Details of the estimation of beetle
density will need to take into account the specifics of the pitfall
sampling design. For now, let’s consider only the raw counts, which are
much simpler to work with and free from assumptions required to estimate
abundance:

To start, here is cumulative counts across all beetles: shows an
increase which is no doubt connected to increased sampling effort as
sites come online, along with an obvious seasonal pattern…

``` r
totals <- bet_ts %>% 
  group_by(month) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

totals %>% ggplot(aes(month, count)) + geom_line() + geom_point()
```

![](beetles_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
bysite <- bet_ts %>% 
  group_by(month, siteID, domainID) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  ungroup()

domains <- sites %>% select(domainCode, domainName) %>% distinct()
bet_domain <- bysite %>% left_join(domains, by = c(domainID = "domainCode"))
```

``` r
bet_domain %>% ggplot(aes(month, count)) + geom_line() + geom_point() + 
  facet_wrap(~domainName, ncol = 3, scale = "free_y")
```

![](beetles_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
