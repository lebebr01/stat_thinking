# Prediction for individuals    


## Comparison of classification / linear model  


## Compared linear model with median
### Skewed Data - Inference
In one example, a skewed distribution was transformed prior to conducting the analysis with a regression tree. Another approach could be to use a more robust statistic such as the median. One limitation of the median, is that a linear regression model as we have covered so far, does not allow you to fit the model while using the median.


```r
library(tidyverse)
library(ggformula)
library(mosaic)
library(broom)
library(statthink)

# Set theme for plots
theme_set(theme_statthinking())

college_score <- read_csv("https://raw.githubusercontent.com/lebebr01/statthink/master/data-raw/College-scorecard-clean.csv", guess_max = 10000)
head(college_score)
```

```
## [90m# A tibble: 6 x 17[39m
##   instnm city  stabbr preddeg region locale adm_rate actcmmid  ugds costt4_a
##   [3m[90m<chr>[39m[23m  [3m[90m<chr>[39m[23m [3m[90m<chr>[39m[23m  [3m[90m<chr>[39m[23m   [3m[90m<chr>[39m[23m  [3m[90m<chr>[39m[23m     [3m[90m<dbl>[39m[23m    [3m[90m<dbl>[39m[23m [3m[90m<dbl>[39m[23m    [3m[90m<dbl>[39m[23m
## [90m1[39m Alaba… Norm… AL     Bachel… South… City:…    0.903       18  [4m4[24m824    [4m2[24m[4m2[24m886
## [90m2[39m Unive… Birm… AL     Bachel… South… City:…    0.918       25 [4m1[24m[4m2[24m866    [4m2[24m[4m4[24m129
## [90m3[39m Unive… Hunt… AL     Bachel… South… City:…    0.812       28  [4m6[24m917    [4m2[24m[4m2[24m108
## [90m4[39m Alaba… Mont… AL     Bachel… South… City:…    0.979       18  [4m4[24m189    [4m1[24m[4m9[24m413
## [90m5[39m The U… Tusc… AL     Bachel… South… City:…    0.533       28 [4m3[24m[4m2[24m387    [4m2[24m[4m8[24m836
## [90m6[39m Aubur… Mont… AL     Bachel… South… City:…    0.825       22  [4m4[24m211    [4m1[24m[4m9[24m892
## [90m# … with 7 more variables: costt4_p [3m[90m<dbl>[90m[23m, tuitionfee_in [3m[90m<dbl>[90m[23m,[39m
## [90m#   tuitionfee_out [3m[90m<dbl>[90m[23m, debt_mdn [3m[90m<dbl>[90m[23m, grad_debt_mdn [3m[90m<dbl>[90m[23m, female [3m[90m<dbl>[90m[23m,[39m
## [90m#   bachelor_degree [3m[90m<dbl>[90m[23m[39m
```


```r
adm_model <- lm(adm_rate ~ preddeg, data = college_score)
coef(adm_model)
```

```
##               (Intercept)    preddegBachelor Degree preddegCertificate Degree 
##                0.72296993               -0.05170254                0.02193828
```

Prior to doing the median, we can bootstrap the mean difference from the model above.


```r
resample_admrate <- function(...) {
  college_resample <- college_score %>%
    sample_n(nrow(college_score), replace = TRUE)

  college_resample %>%
    lm(adm_rate ~ preddeg, data = .) %>%
    tidy(.) %>%
    select(term, estimate)
}

resample_admrate()
```

```
## [90m# A tibble: 3 x 2[39m
##   term                      estimate
##   [3m[90m<chr>[39m[23m                        [3m[90m<dbl>[39m[23m
## [90m1[39m (Intercept)                 0.716 
## [90m2[39m preddegBachelor Degree     -[31m0[39m[31m.[39m[31m0[39m[31m50[4m7[24m[39m
## [90m3[39m preddegCertificate Degree   0.017[4m4[24m
```


```r
admrate_coef <- map(1:10000, resample_admrate) %>%
  bind_rows()

admrate_coef %>%
  gf_density(~ estimate) %>% 
  gf_facet_wrap(~ term, scales = 'free_x')
```

<img src="08-prediction_files/figure-html/replicate-adm-rate-1.png" width="672" />

### Bootstrap Median
he bootstrap for the median will take much of a similar process as before, the major difference being that a model will not be fitted. Instead, we will compute statistics for the median of each group, take differences of the median to represent the median difference between the groups and then replicate.

1. Resample the observed data available, with replacement
2. Estimate median for each group.
3. Calculate median difference between the groups
4. Repeat steps 1 - 3 many times
5. Explore the distribution of median differences from the many resampled data sets.


```r
resample_admrate_median <- function(...) {
  college_resample <- college_score %>%
    sample_n(nrow(college_score), replace = TRUE)

  med_est <- college_resample %>%
    df_stats(adm_rate ~ preddeg, median) %>%
    spread(preddeg, median_adm_rate)

  names(med_est) <- c("Associate", "Bachelor", "Certificate")
  
  med_est %>% 
    mutate(bachelor_associate = Bachelor - Associate,
           certificate_associate = Certificate - Associate,
           bachelor_certificate = Bachelor - Certificate) %>%
    gather(Associate:bachelor_certificate, 
                 key = "Term", 
                 value = "Median_Difference")
}

resample_admrate_median()
```

```
##                    Term Median_Difference
## 1             Associate           0.78430
## 2              Bachelor           0.68965
## 3           Certificate           0.79540
## 4    bachelor_associate          -0.09465
## 5 certificate_associate           0.01110
## 6  bachelor_certificate          -0.10575
```


```r
admrate_median <- map(1:10000, resample_admrate_median) %>%
  bind_rows()

admrate_median %>%
    filter(Term %in% c('bachelor_associate', 'certificate_associate', 'bachelor_certificate')) %>%
    gf_density(~ Median_Difference) %>% 
    gf_facet_wrap(~ Term, scales = 'free_x')
```

<img src="08-prediction_files/figure-html/replicate-adm-median-1.png" width="672" />


