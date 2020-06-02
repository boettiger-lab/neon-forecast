
The NEON bird sampling protocol is clearly designed with modern
distance-sampling based protocols in mind.

``` r
library(lubridate)
library(tidyverse)
library(Distance)

## remotes::install_github("cboettig/neonstore")
```

``` r
## ick, manually force columns bc vroom is confused about NEON's
## non-standard timestamp notation 
s <- cols(
  uid = col_character(),
  namedLocation = col_character(),
  domainID = col_character(),
  siteID = col_character(),
  plotID = col_character(),
  plotType = col_character(),
  pointID = col_character(),
  startDate = col_character(),
  eventID = col_character(),
  pointCountMinute = col_double(),
  targetTaxaPresent = col_character(),
  taxonID = col_character(),
  scientificName = col_character(),
  taxonRank = col_character(),
  vernacularName = col_character(),
  family = col_character(),
  nativeStatusCode = col_character(),
  observerDistance = col_double(),
  detectionMethod = col_character(),
  visualConfirmation = col_character(),
  sexOrAge = col_character(),
  clusterSize = col_double(),
  clusterCode = col_character(),
  identifiedBy = col_character()
)

brd_countdata <- neonstore::neon_read("brd_countdata", col_types = s)
```

Let’s re-arrange the data slightly so that is more suitable for input in
the widely used `Distance` package.

First, let’s make all `pointID`s identify unique points in space by
including the ID of the plot (which includes the siteID already) in the
`pointID`

If we want annual estimates of the abundance of each species, maybe we
should round the `startDate` to year, allowing us to treat the all
visits to the same point in the same year as the effort that year.

Lastly, we’ll ignore the issue of Area for the moment and consider
density estimates alone, which should be reasonable for forecasting
(after all the Area, whatever it is, will remain fixed.)

``` r
bird_ds <- brd_countdata %>% 
  mutate(pointID = paste(plotID, pointID, sep="-")) %>%
  mutate(year = lubridate::year(startDate), Area = 1)
```

Not really sure on this, but I’m going to set sampling effort as the
number of visits to a given point?

``` r
effort <- bird_ds %>% 
  select(pointID,year) %>% 
  distinct() %>% 
  count(pointID)
```

Join the effort data back on by pointID

``` r
ds_data <- bird_ds %>%
  left_join(rename(effort, Effort = n), by = "pointID")
```

Now rename columns to match `ds` columns, or reflect grouping variables
(species, year).

``` r
ds_data <- 
  ds_data  %>% 
  select(scientificName, 
         year,
         distance = observerDistance, 
         Sample.Label = eventID, 
         Region.Label = plotID, 
         size = clusterSize)
```

## Selecting focal species

``` r
brd_countdata %>% select(siteID, scientificName) %>%
  distinct() %>% count(scientificName, sort = TRUE) %>% head()
```

    ## # A tibble: 6 x 2
    ##   scientificName         n
    ##   <chr>              <int>
    ## 1 <NA>                  47
    ## 2 Aves sp.              41
    ## 3 Zenaida macroura      37
    ## 4 Buteo jamaicensis     36
    ## 5 Turdus migratorius    36
    ## 6 Molothrus ater        34

``` r
brd_countdata %>% select(plotID, scientificName) %>%
  distinct() %>% count(scientificName, sort = TRUE) %>% head()
```

    ## # A tibble: 6 x 2
    ##   scientificName            n
    ##   <chr>                 <int>
    ## 1 <NA>                    588
    ## 2 Aves sp.                368
    ## 3 Zenaida macroura        340
    ## 4 Molothrus ater          293
    ## 5 Corvus brachyrhynchos   277
    ## 6 Cardinalis cardinalis   255

Cool, morning doves are common across sites and plots, let’s try
something with those.

``` r
doves <- ds_data %>% 
  filter(scientificName == "Zenaida macroura", year == "2015") %>% 
  as.data.frame()
```

``` r
abund <- Distance::ds(doves, transect = "point")
```

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 2709.462

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 2683.766

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 2649.626

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 2638.611

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## AIC= 2638.286

    ## Fitting half-normal key function with cosine(2,3,4,5,6) adjustments

    ## AIC= 2633.49

    ## No survey area information supplied, only estimating detection function.

Let’s hack the `Distance::ds` print method to provide a `data.frame`
return object instead…

``` r
## Tidy the `ds` return object
 estimate_table <- function(abund) {
  abund_summary <- summary(abund)
  x <- abund_summary$ds
  if(!is.null(x$Nhat)){
    parameters = data.frame(Estimate = c(x$average.p,x$Nhat))
    row.names(parameters) = c("Average p", "N in covered region")
    if(!is.null(x$average.p.se)){
      parameters$SE = c(x$average.p.se,x$Nhat.se)
      parameters$CV = parameters$SE/parameters$Estimate
    }
  } else {
    parameters = data.frame(Estimate = c(x$average.p))
    row.names(parameters) = c("Average p")
    if(!is.null(x$average.p.se)){
      parameters$SE = c(x$average.p.se)
      parameters$CV = parameters$SE/parameters$Estimate
    }
  }
  out <- tibble::rownames_to_column(parameters)
out
}
```

We can not get a table summary of the results:

``` r
estimate_table(abund)
```

    ## # A tibble: 2 x 4
    ##   rowname              Estimate        SE    CV
    ##   <chr>                   <dbl>     <dbl> <dbl>
    ## 1 Average p              0.0373   0.00384 0.103
    ## 2 N in covered region 5791.     710.      0.123

Now we can do multiple years at once:

``` r
estimate_ds <- function(df, ...){
    m <- ds(as.data.frame(df), transect = "point")
    estimate_table(m)
}

doves_abund <- ds_data %>% 
  filter(scientificName == "Zenaida macroura") %>% 
  group_by(year) %>%
dplyr::group_modify(estimate_ds, keep = TRUE)
```

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 997.455

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 998.324

    ## 
    ## Half-normal key function selected.

    ## No survey area information supplied, only estimating detection function.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 2709.462

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 2683.766

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 2649.626

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 2638.611

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## Error : 
    ## gosolnp-->Could not find a feasible starting point...exiting

    ## 
    ## 
    ## Error in model fitting, returning: half-normal key function with cosine(2,3,4) adjustments

    ## 
    ##   Error: Error in -lt$value : invalid argument to unary operator

    ## No survey area information supplied, only estimating detection function.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 3971.918

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 3862.931

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 3852.575

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 3848.261

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## AIC= 3850.085

    ## 
    ## Half-normal key function with cosine(2,3,4) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 13060.585

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 13000.977

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 13001.695

    ## 
    ## Half-normal key function with cosine(2) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 13517.31

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 13441.698

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 13242.039

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 13110.588

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## AIC= 13484.609

    ## 
    ## Half-normal key function with cosine(2,3,4) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 11827.127

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 11693.325

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 11677.009

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 11672.18

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## AIC= 11674.167

    ## 
    ## Half-normal key function with cosine(2,3,4) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

``` r
doves_abund
```

    ## # A tibble: 12 x 5
    ##     year rowname               Estimate          SE     CV
    ##    <dbl> <chr>                    <dbl>       <dbl>  <dbl>
    ##  1  2013 Average p               0.111     0.0100   0.0897
    ##  2  2013 N in covered region   718.       99.4      0.138 
    ##  3  2015 Average p               0.0494    0.00425  0.0860
    ##  4  2015 N in covered region  4371.      475.       0.109 
    ##  5  2016 Average p               0.0544    0.00411  0.0755
    ##  6  2016 N in covered region  6336.      582.       0.0919
    ##  7  2017 Average p               0.0513    0.00120  0.0234
    ##  8  2017 N in covered region 22142.      824.       0.0372
    ##  9  2018 Average p               0.0211    0.000396 0.0188
    ## 10  2018 N in covered region 52997.     1855.       0.0350
    ## 11  2019 Average p               0.0502    0.00234  0.0466
    ## 12  2019 N in covered region 20623.     1146.       0.0556
