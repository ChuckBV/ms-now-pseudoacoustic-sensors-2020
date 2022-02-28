#============================================================================#
# script1c-clean-farmsense-dat.R
#
# Upload FarmSense data and merge it onto the skeleton onto "data_skeleton"
# produced in script1a
#
# 1. Load skeleton and Farmsense data sets (line 19)
# 2. Load Farmsense data and perform initial examination (line 72) 
# 3. Adjust date values to moth biology(line 145)
# 4. Summarize and output count sums by sensor and monitoring interval (line 223)
# 
#============================================================================#

library(tidyverse)
library(lubridate)
library(timechange)
library(FSA) # for se()

#-- 1. Load skeleton and Farmsense data sets --------------------------------

### Load skeleton data set

skeleton <- read_csv("./data-intermediate/data_skeleton.csv")
#    Reduce to observations for sticky traps  
skeleton_fsense <- skeleton %>% 
  filter(TrapType == "Farmsense")

#    Examine and verify    
skeleton_fsense
# A tibble: 315 x 8
#   RandStart  randCode intervalID block PlotID TrapType  Lure    SerialCode
#   <date>     <chr>         <dbl> <dbl>  <dbl> <chr>     <chr>   <chr>     
# 1 2020-07-10 b                 3     1     11 Farmsense Ovibait FS-352    
# 2 2020-07-10 b                 4     1     11 Farmsense Ovibait FS-352    
# 3 2020-07-10 b                 5     1     11 Farmsense Ovibait FS-352   

skeleton_fsense %>% 
  filter(intervalID > 2 & intervalID < 17) %>% 
  group_by(intervalID,PlotID,TrapType,SerialCode,Lure) %>% 
  summarise(nObs = n())
# A tibble: 294 x 6
# Groups:   intervalID, PlotID, TrapType, SerialCode [294]
#   intervalID PlotID TrapType  SerialCode Lure     nObs
#        <dbl>  <dbl> <chr>     <chr>      <chr>   <int>
# 1          3     11 Farmsense FS-352     Ovibait     1
# 2          3     12 Farmsense FS-354     Phero       1
# 3          3     13 Farmsense FS-328     PPO         1

### Import FarmSense sensor data. Have column names ready

fsvars <- c("SerialCode","Location","Insect","dt_utc","degC","rh","lat","long")
#   Good column names to replace unsuitable defaults

fscounts <- read_csv("./data-raw/captures_2020-07-01_2020-11-20_1605756009922.csv")
#   Import final data

colnames(fscounts) <- fsvars
#   Replace unsuitable column names

fscounts$dt_utc <- time_force_tz(fscounts$dt_utc, tz = "UTC") 
#    Specify UTC in POSIXct datetime value

fscounts <- fscounts[ ,c(1,4:6)] 
#   Drop lat/long because all NA

fscounts
# A tibble: 2,149 x 4
#   SerialCode dt_utc               degC    rh
#   <chr>      <dttm>              <dbl> <dbl>
# 1 FS-322     2020-07-09 03:50:04  21.1  79.8
# 2 FS-322     2020-07-10 13:12:10  14.7  78.2
# 3 FS-322     2020-07-11 13:19:40  16.6  76.8

#-- 2. Load Farmsense data and perform initial examination ------------------

### The Farmsense data are not initially linked to the attractant used. 
### Load the raw data, and examine the distribution of time of events in 
### the sensor by hour, day, and Month

### Get local time into the data set in POSIXct
fscounts$dt_pdt <- time_force_tz(time_at_tz(fscounts$dt_utc, tz = "America/Los_Angeles"))

fscounts$date_pdt <- as.Date(fscounts$dt_pdt)
#    Get date-only value associated with local time

fscounts$hr_pdt <- hour(fscounts$dt_pdt)
#    Get hour associated with local time as separate variable

fscounts
# A tibble: 2,149 x 7
#     SerialCode dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt
#     <chr>      <dttm>              <dbl> <dbl> <dttm>              <date>      <int>
#   1 FS-322     2020-07-09 03:50:04  21.1  79.8 2020-07-08 20:50:04 2020-07-08     20

### Trim to on or after July 10 because these are the data that will be used
fscounts <- fscounts %>% # drops 208 obs
  filter(date_pdt > as.Date("2020-07-10"))

### Examine distribution of count events
ggplot(fscounts, aes(x = hr_pdt)) +
  geom_bar() +
  theme_bw() +
  xlab("Hour of Day (Pacific Daylight Time)") +
  ylab("Number of Count Events") +
  theme(axis.text.x = element_text(color = "black", size = 12), #angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))
### Indicates spike at th 6AM hour. Further explore by month

y <- fscounts # y is a temperary data frame for this exploration

### Get Month as a separate variable, and get separate plots of events by hour
### for each month
y$Month <- month(y$date_pdt,
                 label = TRUE,
                 abbr = TRUE)

ggplot(y, aes(x = hr_pdt)) +
  geom_bar() +
  facet_wrap(vars(Month), ncol = 2) +
  theme_bw() +
  xlab("Hour of Day (Pacific Daylight Time)") +
  ylab("Number of Count Events") 

### The spike is much more evident in July than in other months. Examine by
### day

y %>% 
  filter(Month == "Jul") %>% # drops 1941 to 727 obs
  mutate(Day = mday(date_pdt)) %>%
  ggplot(., aes(x = Day)) +
    geom_bar()

### The number of events per day is generally in the range of 20-50 per day
### for all days in July

#-- 3. Adjust date values to moth biology -----------------------------------

### Note that raw data from FarmSense is datatime (down to second), but for 
### sticky traps it is date (weekly intervals). Date can obviously be extracted
### from datetime, but this should be adjusted for the fact that moths from 
### pre-dawn hours are recorded with the pre-change monitoring date interal and
### those captured after sunset but before midnight are recorded with the 
### following monitoring interval.

### Merge intervals onto FarmSense data

### Create a date value such that calendar date is considered sun-up to sun-up.
### Date math is proving problematic in tidyverse functions, so will split 
### data set, modify part, then re-join (all base R)

sort(unique(fscounts$hr_pdt)) # observe range
# [1]  0  1  2  3  4  5  6  7 19 20 21 22 23

fscounts$eve_morn <- ifelse(fscounts$hr_pdt < 12,"am","pm")
#    Create additional variable to distinguish desired split

x <- split(fscounts,fscounts$eve_morn) # 1852 obs am, 89 obs pm
#    Temporary object x is a list

x$pm$date_pdt <- x$pm$date_pdt + 1
#    Set all pm readings to next calendar date

x$am[x$am$eve_morn == "am", ]
# A tibble: 1,852 x 8
#   SerialCode dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt eve_morn
#   <chr>      <dttm>              <dbl> <dbl> <dttm>              <date>      <int> <chr>   
# 1 FS-322     2020-07-11 13:19:40  16.6  76.8 2020-07-11 06:19:40 2020-07-11      6 am      
# 2 FS-322     2020-07-12 09:11:49  21.8  49.0 2020-07-12 02:11:49 2020-07-12      2 am      
# 3 FS-322     2020-07-12 09:14:35  21.8  48.8 2020-07-12 02:14:35 2020-07-12      2 am    
#    Showing no change

x$pm[x$pm$eve_morn == "pm", ]
# A tibble: 89 x 8
#   SerialCode dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt eve_morn
#   <chr>      <dttm>              <dbl> <dbl> <dttm>              <date>      <int> <chr>   
# 1 FS-322     2020-07-14 04:38:18  22.8  54.4 2020-07-13 21:38:18 2020-07-14     21 pm      
# 2 FS-322     2020-07-14 04:40:35  22.7  53.9 2020-07-13 21:40:35 2020-07-14     21 pm   
#    Confirming that date_pdt was shifted forward

fscounts2 <- rbind(x$am,x$pm)
#    Put the two part back together again

### Hardcode monitoring intervals for Farmsense data
fscounts3 <- fscounts2 %>% 
  mutate(intervalID = case_when(date_pdt <= "2020-07-16" ~ 3,
                                date_pdt <= "2020-07-22" ~ 4,
                                date_pdt <= "2020-07-29" ~ 5,
                                date_pdt <= "2020-08-06" ~ 6,
                                date_pdt <= "2020-08-12" ~ 7,
                                date_pdt <= "2020-08-20" ~ 8,
                                date_pdt <= "2020-08-27" ~ 9,
                                date_pdt <= "2020-09-03" ~ 10,
                                date_pdt <= "2020-09-11" ~ 11,
                                date_pdt <= "2020-09-16" ~ 12,
                                date_pdt <= "2020-09-17" ~ 13,
                                date_pdt <= "2020-09-23" ~ 14,
                                date_pdt <= "2020-10-21" ~ 15,
                                date_pdt <= "2020-10-28" ~ 16,
                                TRUE ~ 0))

#fscounts3$intervalID <- as.integer(fscounts3$intervalID)

sort(unique(fscounts3$intervalID))
# [1]  3  4  5  6  7  8  9 10 11 12 13 14 15 16
#    Nothing left out

fscounts3
# A tibble: 1,941 x 9
#   SerialCode dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt eve_morn intervalID
#   <chr>      <dttm>              <dbl> <dbl> <dttm>              <date>      <int> <chr>         <int>
# 1 FS-322     2020-07-11 13:19:40  16.6  76.8 2020-07-11 06:19:40 2020-07-11      6 am                3
# 2 FS-322     2020-07-12 09:11:49  21.8  49.0 2020-07-12 02:11:49 2020-07-12      2 am                3

#-- 4. Summarize and output count sums by sensor and monitoring interval ----

fscount_sums <- fscounts3 %>% 
  group_by(intervalID,SerialCode) %>% 
  summarise(Count = n())
fscount_sums
# A tibble: 152 x 3
# Groups:   intervalID [14]
#   intervalID SerialCode Count
#        <dbl> <chr>      <int>
# 1          3 FS-322        12
# 2          3 FS-326        27
# 3          3 FS-330        18

### Merge on trap meta data from skeleton_fsense

fscount_sums2 <- left_join(skeleton_fsense,fscount_sums)

fscount_sums2
# A tibble: 315 x 9
#   RandStart  randCode intervalID block PlotID TrapType  Lure     SerialCode Count
#   <date>     <chr>         <dbl> <dbl>  <dbl> <chr>     <chr>    <chr>      <int>
# 1 2020-07-10 b                 3     1     11 Farmsense Peterson FS-352         1
# 2 2020-07-10 b                 4     1     11 Farmsense Peterson FS-352        NA
# 3 2020-07-10 b                 5     1     11 Farmsense Peterson FS-352        NA
# 4 2020-07-10 b                 6     1     11 Farmsense Peterson FS-352         1

### Further clean-up: NAs and 0 conflated, block here but not in sticky trap
### data set, and StartDate and EndDate in the sticky data set but not this one

#    Replace NA with 0
fscount_sums2$Count[is.na(fscount_sums2$Count)] <- 0
fscount_sums2
# A tibble: 315 x 9
#   RandStart  randCode intervalID block PlotID TrapType  Lure    SerialCode Count
#   <date>     <chr>         <dbl> <dbl>  <dbl> <chr>     <chr>   <chr>      <dbl>
# 1 2020-07-10 b                 3     1     11 Farmsense Ovibait FS-352         1
# 2 2020-07-10 b                 4     1     11 Farmsense Ovibait FS-352         0
# 3 2020-07-10 b                 5     1     11 Farmsense Ovibait FS-352         0

#    Drop block
fscount_sums2$block <- NULL

#    Merge in StartDate and Stopdate
intervals <- read_csv("./data-intermediate/intervals.csv")
intervals
# A tibble: 17 x 4
#   randCode intervalID StartDate  EndDate   
#   <chr>         <dbl> <date>     <date>    
# 1 a                 1 2020-07-02 2020-07-09
# 2 a                 2 2020-07-09 2020-07-10
# 3 b                 3 2020-07-10 2020-07-16
# 4 b                 4 2020-07-16 2020-07-22

fscount_sums3 <- left_join(intervals,fscount_sums2)

fscount_sums3 <- fscount_sums3 %>% 
  select(RandStart,randCode,intervalID,PlotID,TrapType,Lure,SerialCode,StartDate,EndDate,Count)

fscount_sums3
# A tibble: 317 x 10
#   RandStart  randCode intervalID PlotID TrapType  Lure    SerialCode StartDate  EndDate    Count
#   <date>     <chr>         <dbl>  <dbl> <chr>     <chr>   <chr>      <date>     <date>     <dbl>
# 1 NA         a                 1     NA NA        NA      NA         2020-07-02 2020-07-09    NA
# 2 NA         a                 2     NA NA        NA      NA         2020-07-09 2020-07-10    NA
# 3 2020-07-10 b                 3     11 Farmsense Ovibait FS-352     2020-07-10 2020-07-16     1

### Output all and grouped data sets
write.csv(fscounts3,"./data-intermediate/fsense_all_obs.csv",row.names = FALSE)
write.csv(fscount_sums3,"./data-intermediate/fsense_by_interval.csv",row.names = FALSE)
