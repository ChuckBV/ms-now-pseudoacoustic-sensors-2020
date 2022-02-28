#============================================================================#
# script1b-clean-sticky-trap-dat.R
#
# Upload sticky trap data and merge it onto the skeleton onto "data_skeleton"
# produced in script1a
# 
# 1. Load skeleton data and trapping data sets into Global Memory (line 17)
# 2. Match dates and randomizations for monitoring intervals (line 54)
# 3. Merge re-randomization and interval information and save (line 95)
# 
#============================================================================#

library(tidyverse)
library(lubridate)
library(FSA) # for se()

#-- 1. Load skeleton data and trapping data sets into Global Memory ---------

skeleton <- read_csv("./data-intermediate/data_skeleton.csv")
#    Reduce to observations for sticky traps  
skeleton_wing <- skeleton %>% 
  filter(TrapType == "Wing")

### Load sticky trap data 
sticky <- readr::read_csv("./data-raw/y20_farmsense_wingtrap_counts.csv")


### Rename sticky trap variables and change variable types
sticky <- sticky %>% 
  rename(StartDate = Start_date,
         EndDate = End_date) %>% 
  # for consistency with meta df
  mutate(StartDate = as.Date(mdy(StartDate)),
         EndDate = as.Date(mdy(EndDate)))
  # for consistency and utility

sticky$TrapType <- "Wing"

### Head current data
sticky
# A tibble: 357 x 7
#   PlotID Lure_type Start_date End_date   Count Re_randomization TrapType
#    <dbl> <chr>     <date>     <date>     <dbl>            <dbl> <chr>   
# 1     13 Phero     2020-07-02 2020-07-09    15                1 Wing    
# 2     14 PPO       2020-07-02 2020-07-09    17                1 Wing    
# 3     15 Peterson  2020-07-02 2020-07-09     0                1 Wing  

#    Replace Re_randomization with code used in skeleton
sticky$randCode <- letters[sticky$Re_randomization]
sticky$Re_randomization <- NULL

sticky2 <- sticky[sticky$randCode != "a",]

#-- 2. Match dates and randomizations for monitoring intervals --------------

### Get date information
intervals <- read_csv("./data-intermediate/intervals.csv")
intervals
# A tibble: 17 x 4
#   randCode intervalID StartDate  EndDate   
#   <chr>         <dbl> <date>     <date>    
# 1 a                 1 2020-07-02 2020-07-09
# 2 a                 2 2020-07-09 2020-07-10
# 3 b                 3 2020-07-10 2020-07-16


#-- 3. Merge re-randomization and interval information and save -------------

### merge Re_randomization and intervalID into meta data frame
skeleton_wing <- left_join(skeleton_wing,intervals)
skeleton_wing
# A tibble: 2,751 x 11
#   RandStart  randCode intervalID block PlotID TrapType Lure  SerialCode  nObs StartDate  EndDate   
#   <date>     <chr>         <dbl> <dbl>  <dbl> <chr>    <chr> <chr>      <int> <date>     <date>    
# 1 2020-07-10 b                 3     1     14 Wing     Phero NA            21 2020-07-10 2020-07-16
# 2 2020-07-10 b                 3     1     14 Wing     Phero NA            21 2020-07-16 2020-07-22
# 3 2020-07-10 b                 3     1     14 Wing     Phero NA            21 2020-07-22 2020-07-29

#    Note-- skeleton_wing$SerialCode is NA (missing) for all observation in this
#    data set. SerialCode is an identifier for FarmSense monitoring, and there
#    are no FarmSense data in this data frame. The variable is nonetheless 
#    retained because it might simplify a subsequent marger between wing trap
#    and FarmSenze data

### Merge Intervals back into Sticky Trap data set

skeleton_wing <- skeleton_wing %>% 
  filter(randCode %in% c("b","c","d")) %>% 
  filter(intervalID %in% 3:16) %>% 
  arrange(RandStart,randCode,intervalID,PlotID) %>% 
  select(RandStart,randCode,intervalID,PlotID,TrapType,Lure,SerialCode,StartDate,EndDate)

### Drop iteration 1
sticky2 <- sticky[sticky$randCode != "a",]

sticky2 <- select(sticky2, -c(randCode,TrapType))
#    Initial query demonstrates that these are duplicated in both sources

sticky2 <- left_join(skeleton_wing,sticky2,
                     by = c("PlotID","StartDate","EndDate"))
sticky2
# A tibble: 2,688 x 12
#   RandStart  randCode intervalID PlotID TrapType Lure  SerialCode StartDate  EndDate    Lure_type Count
#   <date>     <chr>         <dbl>  <dbl> <chr>    <chr> <chr>      <date>     <date>     <chr>     <dbl>
# 1 2020-07-10 b                 3     14 Wing     Phero NA         2020-07-10 2020-07-16 Phero         0
# 2 2020-07-10 b                 3     14 Wing     Phero NA         2020-07-16 2020-07-22 Phero         0
# 3 2020-07-10 b                 3     14 Wing     Phero NA         2020-07-22 2020-07-29 Phero         0

### Output needed data sets

write.csv(sticky2,"./data-intermediate/sticky_trap_counts_by_interval.csv", row.names = FALSE)

