# 2020 Comparison of Farmsense and sticky traps when monitoring with pheromone, PPO, and ovibait

~/ChuckBV/ms-now-pseudoaccounstic-sensors-2020/README.md

## Overview

The objective of the current study is to examine correlation of trap activity 
as determined by FarmSense monitors when using pheromone, PPO, and pistachio 
ovibait as bait. The study used an RBCD arrangement, with sections of row-ends 
in a pistachio plot serving as replicate blocks.

The experiment is described in greater detail in "farmsense_report.pdf".
A visual representation of the plot is found in 
"farmsense_experiment_2020_06_30b.jpg", and the Google Earth file
"mwoolf-farmsense-expt-2020-07-01.kml" provides latitude/longitdue 
coordinates.

The Pheromone used has been Trece L2-l, and the PPO used has been the 
non-commercial pouches previously provided by a benefactor.

Replicate blocks were arranged along the east edge of the part of the Mike 
Woolf pistachio block stradling I-5 near Huron (this test was on the west of
I-5) (see diagram in "./doc/"). The traps were placed in groups of groups of 
6 serving as replicate blocks, with 7 such repliate blocks present. On various 
dates, traps were re-randomized.


## Key dates

All wing traps and farmsense devices were in place by 9 July 2020. Traps were 
serviced weekly until 23 September 2020, at which time they were removed by 
owner request to avoid interference with harvest activities. Trapping resumed 
on 14 October 2020 for two more weekly trapping intervals, and traps were 
again removed on 4 November 2020.Monitoring intervals are summarized by 
script 1; see "./data/intervals.csv"

## Data

***./data-raw*** contains three csv data sets. "y20-farmsense-randomizations.csv"
is a listing of trap positions within replicate blocks for replicates in time,
which are distinguished by StartDate. "y20_farmsense_wingtrap_counts.csv" 
contains weekly count data for traditional wing traps with a glue liner. 
"captures_2020-07-01_2020-11-20_1605756009922.csv" has one record with time 
stamp and monitor identifier for each FarmSense detection event. These data
were downloaded from the FarmSense server. 

***./data-intermediate*** contains five csv data sets. "intervals.csv" is a 
listing of start and end dates for weekly monitoring periods, obtained
from a summary of "y20_farmsense_wingtrap_counts.csv". "data_skeleton.csv"
was obtained by merging the randomizations and intervals data set, and serves
as a scafold to merge wing trap and farmsense data into common data sets.
"sticky_trap_counts_by_interval.csv" and "fsense_by_interval.csv" are the 
results of such merges. They have the same data struction and can be 
merged vertically {"rbind()"} into a common data set to facilitate comparisons.
"fsense_all_obs.csv" replicate data but retains individual events to allow
comparisons of the hour that the events were observed.

## Script functions and output

script1a creates the data skeleton, while script1b and script1c merge
on wing trap and FarmSense data, respectively. script1c outputs both the
"fsense_by_interval.csv" and the "fsense_all_obs.csv" data sets.

script4 compares sticky trap counts over monitoring periods with sums of
FarmSense sensor events per period at the level of individaul traps or
sensors. sript4 output includes Figure 2 (a 3 x 1 grid of weekly mean and 
standard error of counts for traps or sensors baited with pheromone, ovibait,
or PPO) and Figure 3 (scatter plots of Farm Sense events vs. trap counts for
the same replicate and week). Statistics output byt script4 include a 
 - unequal variance t-test for ovibait
 - Spearman correlation for pheromone
 - Spearman correlatio for PPO

script5 produces vertical bar charts of counts by hour of the night in July,
Aug, and Sep for ovibait, pheromone, and PPO

