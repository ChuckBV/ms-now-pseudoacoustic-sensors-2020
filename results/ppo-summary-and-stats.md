# FarmSense 2020: ppo summary and stats

## Plot

See "y20_farmsense_vs_sticky_ppo_scatter.jpg"

## Spearman Correlation 

```
cor.test(ppo_plot$Wing,ppo_plot$Farmsense,
         method = "spearman")

Spearman's rank correlation rho

data:  ppo_plot$Wing and ppo_plot$Farmsense
S = 82402, p-value = 7.907e-07
alternative hypothesis: true rho is not equal to 0
sample estimates:
     rho 
0.474641 
```