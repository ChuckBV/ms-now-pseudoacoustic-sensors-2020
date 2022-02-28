#============================================================================#
# script2-compare.R
#
# Load wing trap and FarmSence interval sums, add into same data set, and
# use plots to observe traps
#
# 1. Load data sets(line 17)
# 2. Plot mean and SE by interval for each level of TrapType*Lure (line 50)
#   - Vertically aligned weekly mean/SE for pheromone, ovibait, ppo
# 3. Summary and stats for ovibait (Peterson) traps (line 50)
#   - Determines mean and SE up to 8/12 and Welch t-test stats  
# 4. Summary and stats for pheromone traps (line 174)
#   - Scatter plot and spearman correlation stats
# 5. Summary and stats for PPO traps (line 216)
#   - Scatter plot and spearman correlation stats
# 6. Use ggpubr to get the two correlation plots into single figure (line 249)
# 
#============================================================================#

library(tidyverse)
library(lubridate)
library(FSA)
library(ggpubr)

#-- 1. Load data sets -------------------------------------------------------

sticky <- read_csv("./data-intermediate/sticky_trap_counts_by_interval.csv")
sticky$Lure_type <- NULL 
#    Duplicate and not matched in FarmSense data set
sticky$block <- NULL
#    Not matched in FarmSense data set
sticky$SerialCode <- as.character(sticky$SerialCode)
#    All NA in stick and therefore data type logical, but chr in Farmsense

fsense <- read_csv("./data-intermediate/fsense_by_interval.csv")

fsense <- fsense %>% 
  filter(!is.na(TrapType))

sticky
# A tibble: 315 x 10
#   RandStart  randCode intervalID PlotID TrapType Lure  SerialCode StartDate  EndDate    Count
#   <date>     <chr>         <dbl>  <dbl> <chr>    <chr> <chr>      <date>     <date>     <dbl>
# 1 2020-07-10 b                 3     14 Wing     Phero NA         2020-07-10 2020-07-16     0
# 2 2020-07-10 b                 4     14 Wing     Phero NA         2020-07-16 2020-07-22     0
# 3 2020-07-10 b                 5     14 Wing     Phero NA         2020-07-22 2020-07-29     0

fsense
# A tibble: 315 x 10
#   RandStart  randCode intervalID PlotID TrapType  Lure    SerialCode StartDate  EndDate    Count
#   <date>     <chr>         <dbl>  <dbl> <chr>     <chr>   <chr>      <date>     <date>     <dbl>
# 1 2020-07-10 b                 3     11 Farmsense Ovibait FS-352     2020-07-10 2020-07-16     1
# 2 2020-07-10 b                 3     12 Farmsense Phero   FS-354     2020-07-10 2020-07-16     1
# 3 2020-07-10 b                 3     13 Farmsense PPO     FS-328     2020-07-10 2020-07-16     0

all <- rbind(sticky,fsense)

#-- 2. Plot mean and SE by interval for each level of TrapType*Lure ----

trap_mns <- all %>% 
  group_by(TrapType,Lure,intervalID,EndDate) %>% 
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count, na.rm = TRUE),
            sem = se(Count))
trap_mns
# A tibble: 90 x 7
# Groups:   TrapType, Lure, intervalID [90]
#   TrapType  Lure    intervalID EndDate     nObs    mn   sem
#   <chr>     <chr>        <dbl> <date>     <int> <dbl> <dbl>
# 1 Farmsense Ovibait          3 2020-07-16     7 18.7  8.15 
# 2 Farmsense Ovibait          4 2020-07-22     7 13.7  6.77 
# 3 Farmsense Ovibait          5 2020-07-29     7 18.4  7.30 

p1 <- ggplot() +
  geom_point(data = trap_mns, mapping = aes(x = EndDate, y = mn, color = TrapType)) +
  geom_line(data = trap_mns, mapping = aes(x = EndDate, y = mn, color = TrapType)) + 
  geom_errorbar(data = trap_mns, 
                mapping = aes(x = EndDate, 
                              y = mn, 
                              ymax = mn + sem, 
                              ymin = mn - sem, 
                              color = TrapType),
                width = 1.0) +
  facet_grid(Lure ~ ., scales = "free_y", ) +
  theme_bw() +
  xlab("") +
  ylab("NOW/trap/week") +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))

p1 

ggsave(filename = "y20_farmsense_vs_sticky_wkly.jpg", 
       plot = p1, device = "jpg", path = "./results/", 
       dpi = 300, width = 5.83, height = 5.83, units = "in") 

#-- 3. Summary and stats for ovibait (Peterson) traps -----------------------

### Examine Peterson to determine the range of interest

trap_mns %>% 
  filter(Lure == "Ovibait")
# A tibble: 29 x 7
# Groups:   TrapType, Lure, intervalID [29]
#   TrapType  Lure    intervalID EndDate     nObs    mn   sem
#   <chr>     <chr>        <dbl> <date>     <int> <dbl> <dbl>
# 1 Farmsense Ovibait          3 2020-07-16     7 18.7  8.15 
# 2 Farmsense Ovibait          4 2020-07-22     7 13.7  6.77 
# 3 Farmsense Ovibait          5 2020-07-29     7 18.4  7.30 
# 4 Farmsense Ovibait          6 2020-08-06     7 22.4  8.26 
# 5 Farmsense Ovibait          7 2020-08-12     7 12.1  4.40 
# 6 Farmsense Ovibait          8 2020-08-20     7  3.71 1.13 
# 7 Farmsense Ovibait          9 2020-08-27     7  1.29 0.522
# 8 Farmsense Ovibait         10 2020-09-03     7  1    0.436
# 9 Farmsense Ovibait         11 2020-09-11     7  0    0     

### Ovibait, 5-week trap totals

bait_trap_sums <- all %>% 
  mutate(replicate = PlotID%/%10) %>% 
  # re-extract replicate block from plotID
  arrange(intervalID,replicate) %>% 
  # conenient for visual confirmation 
  filter(Lure == "Ovibait" & EndDate <= as.Date("2020-08-12")) %>% 
  group_by(TrapType,replicate) %>% 
  summarise(nObs = n(),
            total = sum(Count, na.rm = TRUE))
bait_trap_sums
# A tibble: 14 x 4
# Groups:   TrapType [2]
#   TrapType  replicate  nObs total
#   <chr>         <dbl> <int> <dbl>
#  1 Farmsense         1     5     3
#  2 Farmsense         2     5     3
#  3 Farmsense         3     5    68
#  4 Farmsense         4     5   148
#  5 Farmsense         5     5    98
#  6 Farmsense         6     5   227
#  7 Farmsense         7     5    51
#  8 Wing              1     5     0
#  9 Wing              2     5     1
# 10 Wing              3     5     0
# 11 Wing              4     5     3
# 12 Wing              5     5     0
# 13 Wing              6     5     2
# 14 Wing              7     5     2

bait_trap_sums %>% 
  group_by(TrapType) %>% 
  summarise(nObs = sum(!is.na(total)),
            mn = mean(total, na.rm = TRUE),
            sem = se(total))
# A tibble: 2 x 4
# TrapType   nObs    mn    sem
#   <chr>     <int> <dbl>  <dbl>
# 1 Farmsense     7 85.4  30.5  
# 2 Wing          7  1.14  0.459

t.test(total ~ TrapType,
       data = bait_trap_sums,
       var.equal = FALSE)
# 
# Welch Two Sample t-test
# 
# data:  total by TrapType
# t = 2.7588, df = 6.0027, p-value = 0.03289
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   9.537056 159.034372
# sample estimates:
#   mean in group Farmsense      mean in group Wing 
# 85.428571                1.142857 

#-- 4. Summary and stats for pheromone traps ------------------

phero_plot <- all %>% 
  filter(Lure == "Phero") %>% 
  mutate(replicate = PlotID%/%10) %>% 
  arrange(intervalID,replicate) %>% 
  select(intervalID,replicate,TrapType,Count) %>% 
  pivot_wider(names_from = "TrapType", values_from = "Count")

p2 <- ggplot(phero_plot, aes(x = Wing, y = Farmsense)) +
  geom_point() +
  theme_bw() +
  xlab("Wing trap with pheromone") +
  ylab("FarmSense with pheromone") +
  theme(axis.text.x = element_text(color = "black", size = 12), #angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))

p2 

ggsave(filename = "y20_farmsense_vs_sticky_phero_scatter.jpg", 
       plot = p2, device = "jpg", path = "./results/", 
       dpi = 300, width = 2.83, height = 2.83, units = "in") 


cor.test(phero_plot$Wing,phero_plot$Farmsense,
         method = "spearman")
# Spearman's rank correlation rho
# 
# data:  phero_plot$Wing and phero_plot$Farmsense
# S = 119897, p-value = 0.01953
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.2355917 

#-- 5. Summary and stats for PPO traps --------------------------------------

ppo_plot <- all %>% 
  filter(Lure == "PPO") %>% 
  mutate(replicate = PlotID%/%10) %>% 
  arrange(intervalID,replicate) %>% 
  select(intervalID,replicate,TrapType,Count) %>% 
  pivot_wider(names_from = "TrapType", values_from = "Count")

p3 <- ggplot(ppo_plot, aes(x = Wing, y = Farmsense)) +
  geom_point() +
  theme_bw() +
  xlab("Wing trap with PPO") +
  ylab("FarmSense with PPO") +
  theme(axis.text.x = element_text(color = "black", size = 12), #angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 12),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10))

p3

ggsave(filename = "y20_farmsense_vs_sticky_ppo_scatter.jpg", 
       plot = p3, device = "jpg", path = "./results/", 
       dpi = 300, width = 2.83, height = 2.83, units = "in") 


cor.test(ppo_plot$Wing,ppo_plot$Farmsense,
         method = "spearman")

#-- 6. Use ggpubr to get the two correlation plots into single figure -------

p2p3 <- ggarrange(p2,p3,
                  ncol = 2,
                  nrow = 1)

p2p3

ggsave(filename = "y20_farmsense_2cor_scatter_plots.jpg", 
       plot = p2p3, device = "jpg", path = "./results/", 
       dpi = 300, width = 5.83, height = 2.83, units = "in")

# Spearman's rank correlation rho
# 
# data:  ppo_plot$Wing and ppo_plot$Farmsense
# S = 82402, p-value = 7.907e-07
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.474641 

