# FarmSense 2020: ovibait summary and stats

## Which weeks had counts?

```
trap_mns %>% 
  filter(Lure == "Peterson") %>% 
  ***Shows date where FarmSense trails off***

A tibble: 28 x 7
Groups:   TrapType, Lure, intervalID [28]
TrapType  Lure     intervalID EndDate     nObs    mn   sem
<chr>     <chr>         <dbl> <date>     <int> <dbl> <dbl>
1 Farmsense Peterson          3 2020-07-16     7 18.7  8.15 
2 Farmsense Peterson          4 2020-07-22     7 13.7  6.77 
3 Farmsense Peterson          5 2020-07-29     7 18.4  7.30 
4 Farmsense Peterson          6 2020-08-06     7 22.4  8.26 
5 Farmsense Peterson          7 2020-08-12     7 12.1  4.40 
6 Farmsense Peterson          8 2020-08-20     7  3.71 1.13 
7 Farmsense Peterson          9 2020-08-27     7  1.29 0.522
8 Farmsense Peterson         10 2020-09-03     7  1    0.436
9 Farmsense Peterson         11 2020-09-11     7  0    0    
```

### Ovibait, 5-week trap totals

```
bait_trap_sums <- all %>% 
  mutate(replicate = PlotID%/%10) %>% 
  ***re-extract replicate block from plotID***
  arrange(intervalID,replicate) %>% 
  ***convenient for visual confirmation*** 
  filter(Lure == "Peterson" & EndDate <= as.Date("2020-08-12")) %>% 
  group_by(TrapType,replicate) %>% 
  summarise(nObs = n(),
            total = sum(Count, na.rm = TRUE))
bait_trap_sums

A tibble: 14 x 4
Groups:   TrapType [2]
  TrapType  replicate  nObs total
  <chr>         <dbl> <int> <dbl>
 1 Farmsense         1     5     3
 2 Farmsense         2     5     3
 3 Farmsense         3     5    68
 4 Farmsense         4     5   148
 5 Farmsense         5     5    98
 6 Farmsense         6     5   227
 7 Farmsense         7     5    51
 8 Wing              1     5     0
 9 Wing              2     5     1
10 Wing              3     5     0
11 Wing              4     5     3
12 Wing              5     5     0
13 Wing              6     5     2
14 Wing              7     5     2
```

## Mean and SE by Device

```
bait_trap_sums %>% 
  group_by(TrapType) %>% 
  summarise(nObs = sum(!is.na(total)),
            mn = mean(total, na.rm = TRUE),
            sem = se(total))
A tibble: 2 x 4
TrapType   nObs    mn    sem
  <chr>     <int> <dbl>  <dbl>
1 Farmsense     7 85.4  30.5  
2 Wing          7  1.14  0.459
```

## Welch unequal variance t-test

```
t.test(total ~ TrapType,
       data = bait_trap_sums,
       var.equal = FALSE)
 
Welch Two Sample t-test

data:  total by TrapType
t = 2.7588, df = 6.0027, p-value = 0.03289
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  9.537056 159.034372
sample estimates:
  mean in group Farmsense      mean in group Wing 
                85.428571                1.142857 
```