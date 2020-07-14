
The NEON bird sampling protocol is clearly designed with modern
distance-sampling based protocols in mind.

``` r
library(lubridate)
library(tidyverse)
library(Distance)
library(knitr)

## remotes::install_github("cboettig/neonstore")

# use cache since model fitting can take a while
opts_chunk$set(cache=TRUE)
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

Now rename columns to match `ds` columns, or reflect grouping variables
and interesting covariates.

``` r
ds_data <- 
  bird_ds  %>% 
  mutate(Region.Label = paste(siteID, year, sep="-")) %>%
  select(scientificName, 
         year,
         siteID,
         plotID,
         Region.Label,
         obs_id = identifiedBy,
         detection_method = detectionMethod,
         distance = observerDistance, 
         Sample.Label = eventID,
         size = clusterSize) %>%
  mutate(obs_id = as.factor(obs_id),
         Effort = 1,
         detection_method = as.factor(detection_method))
```

`eventID` gives identifies visit to a point sampler at a given time,
this seems like a reasonable `Sample.Label` (if one wanted to ignore
temporal differences `plotID` would be acceptable to identify only the
point sampler location).

`Region.Label` will give the level at which we wish to estimate
abundance. A reasonable target for this might be a Neon site in a given
year, this ensures replication at each location (since there are
multiple points) so variance estimation will likely be better behaved.

`Effort` is number of visits to sample unit (`Sample.Label`), so is 1
for all entries.

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

Let’s mourning doves as they are common across sites and plots.

``` r
doves <- ds_data %>% 
  filter(scientificName == "Zenaida macroura") %>% 
  as.data.frame()
```

It seems reasonable to pool data over all years and expect detection to
be similar (we should check this at some point by adding a covariate for
year\!)

First plot the histogram of distances:

``` r
hist(doves$distance, xlab="Distance (m)", breaks=20)
```

![](birds_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Fit a simple model, truncating at 500m to begin with:

``` r
df_dove_hn_cos_500 <- Distance::ds(doves, transect = "point", truncation=500)
```

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 44894.034

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 44429.404

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 44404.07

    ## Fitting half-normal key function with cosine(2,3,4) adjustments

    ## AIC= 44382.826

    ## Fitting half-normal key function with cosine(2,3,4,5) adjustments

    ## AIC= 44383.468

    ## 
    ## Half-normal key function with cosine(2,3,4) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

Plot that (both the detection function and PDF of distances):

``` r
par(mfrow=c(1,2))
plot(df_dove_hn_cos_500)
plot(df_dove_hn_cos_500, pdf=TRUE)
```

![](birds_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

PDF makes it look like there is some overfitting here – those extra
adjustment terms are probably overfitting the tail.

Rough rule of thumb that we want probability of detection to be
\(\approx 0.15\) at the truncation distance, so we can truncate a lot
more here:

``` r
df_dove_hn_cos_150 <- Distance::ds(doves, transect = "point", truncation=150)
```

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 26338.709

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 26325.007

    ## Fitting half-normal key function with cosine(2,3) adjustments

    ## AIC= 26325.772

    ## 
    ## Half-normal key function with cosine(2) adjustments selected.

    ## No survey area information supplied, only estimating detection function.

Plot that:

``` r
par(mfrow=c(1,2))
plot(df_dove_hn_cos_150)
plot(df_dove_hn_cos_150, pdf=TRUE)
```

![](birds_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Maybe an issue here that the 2nd bin in this histograms looks too tall.
This may be because observers are flushing birds? One way around this is
to bin the distances.

Then try to fit the model again using the `cutpoints=` argument
(expanding the truncation a wee bit):

``` r
df_dove_hn_cos_200_bin <- Distance::ds(doves, transect = "point", truncation=200,
                                       cutpoints=seq(0, 200, by=20))
```

    ## Warning in create.bins(data, cutpoints): Some distances were outside bins and
    ## have been removed.

    ## Starting AIC adjustment term selection.

    ## Fitting half-normal key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 14310.748

    ## Fitting half-normal key function with cosine(2) adjustments

    ## AIC= 14312.143

    ## 
    ## Half-normal key function selected.

    ## No survey area information supplied, only estimating detection function.

``` r
par(mfrow=c(1,2))
plot(df_dove_hn_cos_200_bin)
plot(df_dove_hn_cos_200_bin, pdf=TRUE)
```

![](birds_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

That looks better behaved.

Could also try with hazard-rate detection function:

``` r
df_dove_hr_cos_200_bin <- Distance::ds(doves, transect = "point", truncation=200,
                                       key="hr", cutpoints=seq(0, 200, by=20))
```

    ## Warning in create.bins(data, cutpoints): Some distances were outside bins and
    ## have been removed.

    ## Starting AIC adjustment term selection.

    ## Fitting hazard-rate key function

    ## Key only model: not constraining for monotonicity.

    ## AIC= 14254.22

    ## Fitting hazard-rate key function with cosine(2) adjustments

    ## AIC= 14254.642

    ## 
    ## Hazard-rate key function selected.

    ## No survey area information supplied, only estimating detection function.

``` r
par(mfrow=c(1,2))
plot(df_dove_hr_cos_200_bin)
plot(df_dove_hr_cos_200_bin, pdf=TRUE)
```

![](birds_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Compare via AIC?

``` r
AIC(df_dove_hn_cos_200_bin, df_dove_hr_cos_200_bin)
```

    ## # A tibble: 2 x 2
    ##      df    AIC
    ##   <dbl>  <dbl>
    ## 1     1 14311.
    ## 2     2 14254.

Not much in it\! What about goodness of fit?

``` r
ds.gof(df_dove_hn_cos_200_bin)
```

    ## 
    ## Goodness of fit results for ddf object
    ## 
    ## Chi-square tests
    ##                 [0,20]    (20,40]    (40,60]   (60,80]  (80,100] (100,120]
    ## Observed  91.000000000 239.000000 353.000000 577.00000 571.00000  346.0000
    ## Expected  90.787293126 258.366983 387.495907 463.09183 482.14101  452.6652
    ## Chisquare  0.000498354   1.451734   3.070917  28.01836  16.37679   25.1344
    ##            (120,140]  (140,160]    (160,180] (180,200]     Total
    ## Observed  328.000000 277.000000 2.310000e+02  214.0000 3227.0000
    ## Expected  389.826141 310.922476 2.310643e+02  160.6388 3227.0000
    ## Chisquare   9.805581   3.701033 1.791689e-05   17.7256  105.2849
    ## 
    ## P = 0 with 8 degrees of freedom

``` r
ds.gof(df_dove_hr_cos_200_bin)
```

    ## 
    ## Goodness of fit results for ddf object
    ## 
    ## Chi-square tests
    ##              [0,20]     (20,40]    (40,60]    (60,80]   (80,100] (100,120]
    ## Observed  91.000000 239.0000000 353.000000 577.000000 571.000000 346.00000
    ## Expected  81.938318 245.8149115 407.703426 517.654448 504.864767 427.83474
    ## Chisquare  1.002145   0.1889349   7.339808   6.803563   8.663447  15.65306
    ##            (120,140]    (140,160]    (160,180]  (180,200]      Total
    ## Observed  328.000000 277.00000000 231.00000000 214.000000 3227.00000
    ## Expected  346.725067 279.57627293 227.43863858 187.449410 3227.00000
    ## Chisquare   1.011257   0.02374015   0.05576579   3.760662   44.50239
    ## 
    ## P = 1.708e-07 with 7 degrees of freedom

# Covariates

What if group size affects detectability?

``` r
df_dove_hn_size_200_bin <- Distance::ds(doves, transect = "point", truncation=200,
                                        formula=~size, cutpoints=seq(0, 200, by=20))
```

    ## Warning in create.bins(data, cutpoints): Some distances were outside bins and
    ## have been removed.

    ## Model contains covariate term(s): no adjustment terms will be included.

    ## Fitting half-normal key function

    ## AIC= 14309.722

    ## No survey area information supplied, only estimating detection function.

``` r
df_dove_hr_size_200_bin <- Distance::ds(doves, transect = "point", truncation=200,
                                        key="hr", formula=~size,
                                        cutpoints=seq(0, 200, by=20))
```

    ## Warning in create.bins(data, cutpoints): Some distances were outside bins and
    ## have been removed.

    ## Model contains covariate term(s): no adjustment terms will be included.

    ## Fitting hazard-rate key function

    ## AIC= 14254.312

    ## No survey area information supplied, only estimating detection function.

(We *could* think about other covariates here. `obs_id` would be an
option but it has 52 levels, that model takes a very long time to fit.)

Which of those looks best via AIC?

``` r
AIC(df_dove_hn_cos_200_bin, df_dove_hr_cos_200_bin, df_dove_hr_size_200_bin, df_dove_hn_size_200_bin)
```

    ## # A tibble: 4 x 2
    ##      df    AIC
    ##   <dbl>  <dbl>
    ## 1     1 14311.
    ## 2     2 14254.
    ## 3     3 14254.
    ## 4     2 14310.

Plot,

``` r
par(mfrow=c(2,2))
plot(df_dove_hr_size_200_bin, main="hr_size", pdf=TRUE)
plot(df_dove_hn_size_200_bin, main="hn_size", pdf=TRUE)
plot(df_dove_hr_cos_200_bin, main="hr", pdf=TRUE)
plot(df_dove_hn_cos_200_bin, main="hn", pdf=TRUE)
```

![](birds_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# Results tables

We can compare multiple models:

``` r
res_tab <- summarize_ds_models(df_dove_hn_cos_200_bin, df_dove_hr_cos_200_bin,
                               df_dove_hr_size_200_bin, df_dove_hn_size_200_bin, output="plain")
```

And `kable` them:

``` r
kable(res_tab)
```

|   | Model                        | Key function | Formula | Chi^2 p-value | Average detectability | se(Average detectability) |  Delta AIC |
| :- | :--------------------------- | :----------- | :------ | ------------: | --------------------: | ------------------------: | ---------: |
| 2 | df\_dove\_hr\_cos\_200\_bin  | Hazard-rate  | \~1     |         2e-07 |             0.3938328 |                 0.0121846 |  0.0000000 |
| 3 | df\_dove\_hr\_size\_200\_bin | Hazard-rate  | \~size  |         1e-07 |             0.3938030 |                 0.0122457 |  0.0918791 |
| 4 | df\_dove\_hn\_size\_200\_bin | Half-normal  | \~size  |         0e+00 |             0.3501822 |                 0.0077644 | 55.5018771 |
| 1 | df\_dove\_hn\_cos\_200\_bin  | Half-normal  | \~1     |         0e+00 |             0.3507792 |                 0.0073099 | 56.5285287 |

# Per year abundance estimates

We can do this in various ways (including via setting the `Region.Label`
appropriately above and estimate at the same time as fitting).
`Distance` also provides `dht2` which allows for more complex abundance
estimation. Let’s use `df_dove_hr_cos_200_bin` as our detection function
for these estimates.

``` r
# set the stratum areas to just be the covered area for that year
doves_areas <- doves %>%
  select(Region.Label, Sample.Label, Effort) %>%
  distinct() %>%
  mutate(Area=sum(Effort) * df_dove_hn_cos_200_bin$ddf$meta.data$width^2 * pi)
# only 1 value here so we can cheat
doves$Area <- doves_areas$Area[1]


year_site_abund <- dht2(df_dove_hn_cos_200_bin, flatfile=doves,
                        strat_formula=~Region.Label)
```

    ## Warning in `[<-.data.frame`(`*tmp*`, flatfile$Sample.Label %in% sl_diff, :
    ## provided 13 variables to replace 12 variables

    ## Warning in dht2(ddf = structure(list(call = ddf(dsmodel = ~cds(key = "hn", : One
    ## or more strata have only one transect, cannot calculate empirical encounter rate
    ## variance

    ## Warning in `[<-.data.frame`(`*tmp*`, flatfile$Sample.Label %in% sl_diff, :
    ## provided 13 variables to replace 12 variables

    ## Warning in dht2(df_dove_hn_cos_200_bin, flatfile = doves, strat_formula =
    ## ~Region.Label): One or more strata have only one transect, cannot calculate
    ## empirical encounter rate variance

Plot some trends?

``` r
library(ggplot2)
year_site_abund_plot <- as.data.frame(attr(year_site_abund, "density"))
year_site_abund_plot$year <- as.numeric(sub("^.{4}-", "", year_site_abund_plot$Region.Label))
```

    ## Warning: NAs introduced by coercion

``` r
year_site_abund_plot$site <- sub("-\\d{4}", "", year_site_abund_plot$Region.Label)

ggplot(year_site_abund_plot) +
  geom_line(aes(x=year, y=Density, colour=site, group=site)) +
  theme_minimal()
```

    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](birds_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->
