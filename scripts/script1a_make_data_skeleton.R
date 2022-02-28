#===========================================================================#
# script1a_make_data_skeleton.R
#
# There are 7 replicates of 6 treats for total of 42 traps. The orignal
# randomization order within replicate blocks was re-randomized three times.
# Trapping intervals were generally 1 week, and there were 17 intervals.
# This script expands a data frame to include one observation for each 
# level of randomization x monitoring interval x trap. Actual counts from 
# sticky traps and the FarmSense devices will be merged on in subsequent
# scripts.
#
# 1. Load metadata file (line 23)
# 3. Break apart, clean/verify data (line 44)
# 4. Join iterations back into 1 dataframe, clean names, export (line 107)
#
#===========================================================================#

sessionInfo()

library(tidyverse)
library(lubridate)

#-- 1. Load metadata file ---------------------------------------------------

### Provides information about treatment, trap position, etc.

meta <- readr::read_csv("./data-raw/y20-farmsense-randomizations.csv")
meta$StartDate <- as.Date(mdy(meta$StartDate))
meta 
# A tibble: 168 x 8
#   PosID PlotID StartDate  SerialCode block TrtCode TrapType  Lure    
#   <dbl>  <dbl> <date>     <chr>      <dbl> <chr>   <chr>     <chr>   
# 1     1     11 2020-07-02 FS-354         1 D       Farmsense Phero
# 2     2     12 2020-07-02 FS-328         1 F       Farmsense PPO     
# 3     3     13 2020-07-02 NA             1 A       Wing      Phero

unique(meta$StartDate)
# [1] "2020-07-02" "2020-07-10" "2020-09-17" "2020-10-21"
#    Extract start dates for re-randomizations

meta <- meta[meta$StartDate != as.Date("2020-07-02"), ]
#    Drop first iteration, not enough data to be useful

#-- 2. Extract and save monitoring intervals from sticky trap data ----------

### Load sticky trap data
sticky <- readr::read_csv("./data-raw/y20_farmsense_wingtrap_counts.csv")

### Rename sticky trap variables and change variable types
sticky <- sticky %>% 
  rename(StartDate = Start_date,
         EndDate = End_date) %>% 
  # for consistency with meta df
  mutate(StartDate = as.Date(mdy(StartDate)),
         EndDate = as.Date(mdy(EndDate)))

sticky$TrapType <- "Wing" # Change to trap description desired in reports

#    Replace Re_randomization with code used in skeleton
sticky$randCode <- letters[sticky$Re_randomization]
sticky$Re_randomization <- NULL

#sticky2 <- sticky[sticky$randCode != "a",] 
# Do not drop first randomization until later

### Head current data

sticky
# A tibble: 357 x 7
#   PlotID Lure_type StartDate  EndDate    Count TrapType randCode
#    <dbl> <chr>     <date>     <date>     <dbl> <chr>    <chr>   
# 1     13 Phero     2020-07-02 2020-07-09    15 Wing     a       
# 2     14 PPO       2020-07-02 2020-07-09    17 Wing     a       
# 3     15 Peterson  2020-07-02 2020-07-09     0 Wing     a  

### Get date information

intervals <- sticky %>% 
  group_by(randCode,StartDate,EndDate) %>% 
  summarise(nObs = sum(!is.na(Count)))

intervals
# A tibble: 17 x 4
# Groups:   randCode, StartDate [17]
#   randCode StartDate  EndDate     nObs
#   <chr>    <date>     <date>     <int>
# 1 a        2020-07-02 2020-07-09    21
# 2 a        2020-07-09 2020-07-10    21
# 3 b        2020-07-10 2020-07-16    21

### Add randomization information
intervals <- intervals %>% 
  mutate(Re_randomization = case_when(StartDate < as.Date("2020-07-10") ~ 1,
                                      StartDate < as.Date("2020-09-17") ~ 2,
                                      StartDate < as.Date("2020-10-21") ~ 3,
                                      TRUE ~ 4)) %>% 
  select(c(4,1:3))

intervals$intervalID <- seq.int(nrow(intervals))
#    Adds an index number for each monitoring interval
intervals$nObs <- NULL
#    drop what is no longer needed

intervals <- intervals[,c(1,4,2,3)]
intervals
# A tibble: 17 x 4
# Groups:   randCode, StartDate [17]
#   randCode intervalID StartDate  EndDate   
#   <chr>         <int> <date>     <date>    
# 1 a                 1 2020-07-02 2020-07-09
# 2 a                 2 2020-07-09 2020-07-10
# 3 b                 3 2020-07-10 2020-07-16

write.csv(intervals,"./data-intermediate/intervals.csv", row.names = FALSE)

#-- 3. Break apart, clean/verify data on weeks in randomized iterations -----

### Want to use to split function to split the meta data frame by iterations.
### Split function requires a factor for grouping, and prefer not to 
### redefine StartDate as a factor because more useful in date format. Therefore
### add intervalCode and define that as a factor

meta <- meta %>% 
  mutate(randCode = case_when(StartDate == as.Date("2020-07-10") ~ "b",
                              StartDate == as.Date("2020-09-17") ~ "c",
                              StartDate == as.Date("2020-10-21") ~ "d",
                              TRUE ~ "wtf")) %>% # wtf is error code 
  select(9,1:8)

unique(meta$randCode)
# [1] "b" "c" "d" 
meta$randCode <- factor(meta$randCode, ordered = TRUE, levels = c("b","c","d"))

### Get weeks per randomization from the intervals data set, created in
### script1b
intervals %>% 
  group_by(randCode) %>% 
  summarise(nObs = n(),
            lo = min(intervalID),
            hi = max(intervalID))
# A tibble: 4 x 4
#   randCode  nObs    lo    hi
#   <chr>    <int> <int> <int>
# 1 a            2     1     2
# 2 b           11     3    13
# 3 c            1    14    14
# 4 d            3    15    17

### Split into list of three data frames

x <- split(meta, f = meta$randCode)
str(x)
# List of 3
# $ b: tibble[,9] [42 x 9] (S3: tbl_df/tbl/data.frame) | and $ c and $ d, many more lines

### Replace each line in the data fame 11 times
x$b <- x$b %>% 
  slice(rep(1:n(), each = 11)) %>% 
  group_by(PlotID) %>% 
  mutate(intevalID = row_number() + 2)

View(x$b)

x$c <- x$c %>% 
  arrange(PlotID) %>% 
  mutate(intevalID = 14)

View(x$c)

x$d <- x$d %>% 
  slice(rep(1:n(), each = 3)) %>% 
  group_by(PlotID) %>% 
  mutate(intevalID = row_number()+14)

View(x$d)

#-- 4. Join iterations back into 1 dataframe, clean names, export -----------

### Zip back together with do.call()

meta2 <- do.call("rbind", x)
meta2
# A tibble: 2,142 x 10
# Groups:   PlotID [42]
#   randCode PosID PlotID StartDate  SerialCode block TrtCode TrapType  Lure     intevalID
#   <ord>    <dbl>  <dbl> <date>     <chr>      <dbl> <chr>   <chr>     <chr>        <int>
# 1 b            1     11 2020-07-10 FS-352         1 E       Farmsense Peterson         1
# 2 b            1     11 2020-07-10 FS-352         1 E       Farmsense Peterson         2
# 3 b            1     11 2020-07-10 FS-352         1 E       Farmsense Peterson         3

### Change Lure treatment from "Peterson" to "Ovibait"
meta2$Lure[meta2$Lure == "Peterson"] <- "Ovibait"

View(meta2)
#    rename StartDate using base R
meta2$RandStart <- meta2$StartDate
meta2$StartDate <- NULL

meta2 <- meta2 %>% 
  rename(intervalID = intevalID) %>% 
  select(RandStart,randCode,intervalID,block,PlotID,TrapType,Lure,SerialCode)

### Save to data-intermediate

write.csv(meta2,"./data-intermediate/data_skeleton.csv",row.names = FALSE)
