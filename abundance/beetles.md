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

-----

``` r
## now the fun starts
library(nimbleEcology)
```

    ## Loading required package: nimble

    ## nimble version 0.9.1 is loaded.
    ## For more information on NIMBLE and a User Manual,
    ## please visit http://R-nimble.org.

    ## 
    ## Attaching package: 'nimble'

    ## The following object is masked from 'package:stats':
    ## 
    ##     simulate

    ## Loading nimbleEcology. 
    ## Registering the following user-defined functions:
    ##  dOcc, dDynOcc, dCJS, dHMM, dDHMM, dNmixture.
    ## Please note that prior to nimbleEcology version 0.3.0, dHMM and dDHMM
    ## contained a bug that gave incorrect results.

## Estimating abundance

> Ground beetles are sampled using pitfall traps (16 oz deli containers
> filled with 150 or 250 mL of propylene glycol). Four traps are
> deployed in each of 10 plots at each terrestrial NEON site (40 traps
> per site), with traps arrayed approximately 20 meters from the center
> of the plot in each of the four cardinal directions. Sampling occurs
> biweekly throughout the growing season (when temperatures are above 4
> degrees C).

Sum all beetles in the trap

``` r
trap_totals <- bet_sorting %>% 
  select(collectDate, siteID, plotID, trapID, scientificName, individualCount) %>%
  group_by(collectDate, trapID, plotID, siteID) %>%
  summarize(count = sum(individualCount, na.rm = TRUE)) %>%
  ungroup()
```

Actually, let’s start with 1 site, and sum the 4 traps in each plot,
leaving us with just the 10 plots per collectionDate

``` r
ornl <- bet_sorting %>% 
  filter(siteID == "ORNL") %>%
  group_by(plotID, collectDate) %>%
  summarize(count = sum(individualCount, na.rm = TRUE)) %>%
  ungroup() %>% arrange(collectDate)
```

Okay, here’s 10 plots for the first date:

``` r
d1 <- ornl %>% filter(collectDate == as.Date("2014-06-03"))
d1
```

    ## # A tibble: 10 x 3
    ##    plotID   collectDate count
    ##    <chr>    <date>      <dbl>
    ##  1 ORNL_001 2014-06-03      5
    ##  2 ORNL_002 2014-06-03      5
    ##  3 ORNL_003 2014-06-03      4
    ##  4 ORNL_004 2014-06-03      2
    ##  5 ORNL_006 2014-06-03     15
    ##  6 ORNL_021 2014-06-03      5
    ##  7 ORNL_027 2014-06-03     13
    ##  8 ORNL_029 2014-06-03      3
    ##  9 ORNL_031 2014-06-03      6
    ## 10 ORNL_032 2014-06-03     18

``` r
len <- length(d1$count)
dat <- d1$count
lambda <- mean(d1$count) * 100
prob <- rep(.01, len)
```

``` r
# Define code for a nimbleModel
 nc <- nimbleCode({
   x[1:5] ~ dNmixture_v(lambda, prob = prob[1:5],
                        Nmin = -1, Nmax = -1, len = 5)

   lambda ~ dunif(0, 1000)

   for (i in 1:5) {
     prob[i] ~ dunif(0, 1)
   }
 })

# Build the model
nmix <- nimbleModel(nc,
                    data = list(x = dat),
                    inits = list(lambda = lambda,
                                 prob = prob))
```

    ## defining model...

    ## building model...

    ## setting data and initial values...

    ## running calculate on model (any error reports that follow may simply reflect missing values in model variables) ... 
    ## checking model sizes and dimensions...
    ## model building finished.

``` r
# Calculate log probability of data from the model
nmix$calculate()
```

    ## [1] -23.42438

and now time for some max likelihood or MCMC estimation…
