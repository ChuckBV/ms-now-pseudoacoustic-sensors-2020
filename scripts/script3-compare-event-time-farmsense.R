#============================================================================#
# script3-compare-event-time-farmsense.R
#
# Examine if time of capture differs between attractants and months
# 1. Load FarmSense and merge to data skeleton
# 2. Use plots to examine the dat
# 
# 
#============================================================================#

library(tidyverse)
library(lubridate)
library(DescTools)

#-- 1. Load FarmSense and merge to data skeleton ----------------------------

fs_all <- read_csv("./data-intermediate/fsense_all_obs.csv")
fs_all
# A tibble: 1,941 x 9
#   SerialCode dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt eve_morn intervalID
#   <chr>      <dttm>              <dbl> <dbl> <dttm>              <date>      <dbl> <chr>         <dbl>
# 1 FS-322     2020-07-11 13:19:40  16.6  76.8 2020-07-11 06:19:40 2020-07-11      6 am                3
# 2 FS-322     2020-07-12 09:11:49  21.8  49.0 2020-07-12 02:11:49 2020-07-12      2 am                3
# 3 FS-322     2020-07-12 09:14:35  21.8  48.8 2020-07-12 02:14:35 2020-07-12      2 am                3

### Load data_skeleton and extract look-up key for Lure for each SerialCode
### (examine by randomization)

skel <- read_csv("./data-intermediate/data_skeleton.csv")

skel2 <- skel %>% 
  filter(TrapType == "Farmsense") %>% 
  group_by(SerialCode,randCode,Lure) %>% 
  summarise(nObs = n())

skel2 %>% 
  group_by(SerialCode,Lure) %>% 
  summarise(nObs = n()) %>% 
  filter(nObs != 3)
#  Confirms Farmsense device kept the same lure

skel2 <- skel2 %>% 
  group_by(SerialCode,Lure) %>% 
  summarise(nObs = n()) %>% 
  select(-nObs)

fs_all2 <- left_join(skel2,fs_all)
fs_all2
# A tibble: 1,942 x 10
# Groups:   SerialCode [21]
#   SerialCode Lure  dt_utc               degC    rh dt_pdt              date_pdt   hr_pdt eve_morn intervalID
#   <chr>      <chr> <dttm>              <dbl> <dbl> <dttm>              <date>      <dbl> <chr>         <dbl>
# 1 FS-322     PPO   2020-07-11 13:19:40  16.6  76.8 2020-07-11 06:19:40 2020-07-11      6 am                3
# 2 FS-322     PPO   2020-07-12 09:11:49  21.8  49.0 2020-07-12 02:11:49 2020-07-12      2 am                3
# 3 FS-322     PPO   2020-07-12 09:14:35  21.8  48.8 2020-07-12 02:14:35 2020-07-12      2 am                3

fs_all2$hr_pdt <- factor(fs_all2$hr_pdt, ordered = TRUE, levels = c(19,20,21,22,23,0,1,2,3,4,5,6,7))
#   Converts hour to an ordered factor, with order from beginning to end of
#   the night

x <- fs_all2[fs_all2$degC > 5, ]
#   Drops ca. 40 of 1960 records in which temperature was -1C

#-- 2. Use plots to examine the data ----------------------------------------

### time all events

counts_all <- x %>% 
  filter(!is.na(hr_pdt)) %>%            # drops obs w NA in critical field
  group_by(hr_pdt) %>%  
  summarise(Count = n()) %>%            # Gets events per hour 
  mutate(hr_of_nt = seq_along(hr_pdt))  # Converts hour of clock to start sundown

counts_all

ggplot(counts_all, aes(x = hr_of_nt, y = Count)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  xlab("Hour of night") +
  ylab("Total detection events") +
  theme(axis.text.x = element_text(color = "black", size = 12), #angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))

### By lure and month

fs_all2$mnth <- month(fs_all2$date_pdt, label = TRUE, abbr = TRUE)
fs_all2

counts_x_grp <- fs_all2 %>% 
  filter(!is.na(hr_pdt) & mnth != "Oct") %>% 
  group_by(Lure,mnth,hr_pdt) %>% 
  summarise(Count = n()) %>% 
  mutate(hr_of_nt = seq_along(hr_pdt))


p5 <- ggplot(counts_x_grp, aes(x = hr_pdt, y = Count)) +
  geom_bar(stat = "identity") +
  facet_grid(Lure ~ mnth, scales = "free_y") +
  theme_bw() +
  #xlab("Hour of day") +
  ylab("Total detection events") +
  scale_x_discrete("Hour of day", labels =  c("19" = "19","20" = "","21" = "21","22" = "","23" = "23","0" = "","1" = "1","2" = "","3" = "3","4" = "","5" = "5","6" = "","7" = "7")) +
  theme(axis.text.x = element_text(color = "black", size = 12), 
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))

p5

ggsave(filename = "y20_farmsense_detections_by_hour_grid.jpg", 
       plot = p5, device = "jpg", path = "./results/", 
       dpi = 300, width = 5.83, height = 5.83, units = "in") 


