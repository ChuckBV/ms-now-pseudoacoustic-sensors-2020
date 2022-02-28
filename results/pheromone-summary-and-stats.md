# FarmSense 2020: pheromone summary and stats

## Plot

See "y20_farmsense_vs_sticky_phero_scatter.jpg"

## Spearman Correlation 

```
cor.test(phero_plot$Wing,phero_plot$Farmsense,
         method = "spearman")
		 
 Spearman's rank correlation rho
 
 data:  phero_plot$Wing and phero_plot$Farmsense
 S = 119897, p-value = 0.01953
 alternative hypothesis: true rho is not equal to 0
 sample estimates:
       rho 
 0.2355917 
```