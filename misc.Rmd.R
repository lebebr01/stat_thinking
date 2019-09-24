
## Scatterplots


### 2d Scatterplots
```{r scatter}
gf_point(wage ~ age, data = CPS85) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```

### Add Color and Shapes

```{r scatter-color}
gf_point(wage ~ age, data = CPS85, color = ~ sex) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```


```{r scatter-shape}
gf_point(wage ~ age, data = CPS85, shape = ~ married) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```

```{r scatter-shape-color}
gf_point(wage ~ age, data = CPS85, color = ~ sex, shape = ~ married) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```


### Faceting
```{r scatter-facet}
gf_point(wage ~ age, data = CPS85, color = ~ sex) %>%
  gf_facet_wrap(~ married) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```

### Jittered Points
```{r jitter}
gf_jitter(wage ~ age, data = CPS85) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```


## Hex Bins
```{r hex}
gf_hex(wage ~ age, data = CPS85) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```

### Adjust color
```{r hex-color}
gf_hex(wage ~ age, data = CPS85) %>%
  gf_refine(scale_fill_gradientn(colors  = rev(brewer.pal(9, "YlGnBu")),
                                 values = c(0, exp(seq(-5, 0, length.out = 100))))) %>%
  gf_theme(theme_bw()) %>%
  gf_labs(x = "Age (in years)",
          y = "Wage ($ per hour)")
```




### Label Interesting Points

May need some new data for this.


## Additional Resources
