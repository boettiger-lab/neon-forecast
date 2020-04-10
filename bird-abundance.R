
library(lubridate)
library(tidyverse)
library(contentid) 
library(Distance)



## We'll access the data by content identifier for data reproducibility
#f <- contenturi::resolve("hash://sha256/758f1d665d5a25f48e9fa52f3eda7bc320003187c70a3c12a8bfcd13824be3af")
brd_countdata <- contenturi::retrieve(
  "hash://sha256/3544e9345cc9ff9e235ff49e2d446dfea1ce5fb2be2c82c66f4e58516bf8a3bd")
## Tidy the input data
brd_df <- read_csv(brd_countdata) %>% 
  mutate(pointID = paste(plotID, pointID, sep="-"))

bird_ds <- brd_df %>% mutate(pointID = paste(plotID, pointID, sep="-")) # easier when pointID is unique
effort <- bird_ds %>% select(pointID,startDate) %>% distinct() %>% count(pointID)

ds_data <- bird_ds %>%
  left_join(rename(effort, Effort = n), by = "pointID") %>%
  select(family, startDate,
         distance = observerDistance, Sample.Label = eventID, 
         Region.Label = plotID, size = clusterSize) %>% 
  mutate(Area = 1, year = lubridate::year(startDate)) 


## Tidy the `ds` return object
 estimate_table <- function(abund) {
  abund_summary <- summary(abund)
  x <- abund_summary$ds
  if(!is.null(x$Nhat)){
    parameters=data.frame(Estimate=c(x$average.p,x$Nhat))
    row.names(parameters)=c("Average p", "N in covered region")
    if(!is.null(x$average.p.se)){
      parameters$SE=c(x$average.p.se,x$Nhat.se)
      parameters$CV=parameters$SE/parameters$Estimate
    }
  }else{
    parameters=data.frame(Estimate=c(x$average.p))
    row.names(parameters)=c("Average p")
    if(!is.null(x$average.p.se)){
      parameters$SE=c(x$average.p.se)
      parameters$CV=parameters$SE/parameters$Estimate
    }
  }
parameters
}

 

all_groups <-  ds_data %>% 
  as.data.frame(stringsAsFactors = TRUE) %>% 
  group_by(family, year) 
## more than 10 observations in a group please
good_groups <- inner_join(all_groups, 
                          all_groups %>% count() %>% filter(n > 10))

n <- good_groups %>% count() %>% dim() 
p1 <- dplyr::progress_estimated(n[1])

blank <- data.frame(rowname = NA, Estimate = NA, SE = NA, CV = NA)

estimate_ds <- function(df, ...){
    p1$tick()$print()
    tryCatch({
      suppressMessages({
    m <- ds(as.data.frame(df), transect = "point")
      })
    out <- estimate_table(m)
    out <- tibble::rownames_to_column(out)
    out}, 
    error = function(e) blank, finally = blank)
  }


## how many groups?
n[1]

## Test

ex <- ds_data %>% filter(family %in% "Emberizidae") %>% as.data.frame(stringsAsFactors = TRUE)
m <- ds(ex, transect = "point")
# estimate_ds(ex) # will use up a tick!


## and here we go!
  
abund <- good_groups %>% 
  #filter(family %in% "Emberizidae") %>%  ## testing on a subset  -- involves several failures!
  dplyr::group_modify(estimate_ds, keep = TRUE)

saveRDS(abund, "abund.rds")
write_csv(abund, "abund.csv")
