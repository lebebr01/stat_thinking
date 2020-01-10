# Prediction for individuals    


## Comparison of classification / linear model  


## Compared linear model with median
### Skewed Data - Inference
In one example, a skewed distribution was transformed prior to conducting the analysis with a regression tree. Another approach could be to use a more robust statistic such as the median. One limitation of the median, is that a linear regression model as we have covered so far, does not allow you to fit the model while using the median.


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✔ ggplot2 3.2.1     ✔ purrr   0.3.3
## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
## ✔ readr   1.3.1     ✔ forcats 0.4.0
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(ggformula)
```

```
## Loading required package: ggstance
```

```
## 
## Attaching package: 'ggstance'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     geom_errorbarh, GeomErrorbarh
```

```
## 
## New to ggformula?  Try the tutorials: 
## 	learnr::run_tutorial("introduction", package = "ggformula")
## 	learnr::run_tutorial("refining", package = "ggformula")
```

```r
library(mosaic)
```

```
## Loading required package: lattice
```

```
## Loading required package: mosaicData
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
```

```
## Registered S3 method overwritten by 'mosaic':
##   method                           from   
##   fortify.SpatialPolygonsDataFrame ggplot2
```

```
## 
## The 'mosaic' package masks several functions from core packages in order to add 
## additional features.  The original behavior of these functions should not be affected by this.
## 
## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.
```

```
## 
## Attaching package: 'mosaic'
```

```
## The following object is masked from 'package:Matrix':
## 
##     mean
```

```
## The following objects are masked from 'package:dplyr':
## 
##     count, do, tally
```

```
## The following object is masked from 'package:purrr':
## 
##     cross
```

```
## The following object is masked from 'package:ggplot2':
## 
##     stat
```

```
## The following objects are masked from 'package:stats':
## 
##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
##     quantile, sd, t.test, var
```

```
## The following objects are masked from 'package:base':
## 
##     max, mean, min, prod, range, sample, sum
```

```r
library(broom)
library(statthink)

# Set theme for plots
theme_set(theme_statthinking())

college_score <- read_csv("https://raw.githubusercontent.com/lebebr01/statthink/master/data-raw/College-scorecard-clean.csv", guess_max = 10000)
```

```
## Parsed with column specification:
## cols(
##   instnm = col_character(),
##   city = col_character(),
##   stabbr = col_character(),
##   preddeg = col_character(),
##   region = col_character(),
##   locale = col_character(),
##   adm_rate = col_double(),
##   actcmmid = col_double(),
##   ugds = col_double(),
##   costt4_a = col_double(),
##   costt4_p = col_double(),
##   tuitionfee_in = col_double(),
##   tuitionfee_out = col_double(),
##   debt_mdn = col_double(),
##   grad_debt_mdn = col_double(),
##   female = col_double(),
##   bachelor_degree = col_double()
## )
```

```r
head(college_score)
```

```
## # A tibble: 6 x 17
##   instnm city  stabbr preddeg region locale adm_rate actcmmid  ugds costt4_a
##   <chr>  <chr> <chr>  <chr>   <chr>  <chr>     <dbl>    <dbl> <dbl>    <dbl>
## 1 Alaba… Norm… AL     Bachel… South… City:…    0.903       18  4824    22886
## 2 Unive… Birm… AL     Bachel… South… City:…    0.918       25 12866    24129
## 3 Unive… Hunt… AL     Bachel… South… City:…    0.812       28  6917    22108
## 4 Alaba… Mont… AL     Bachel… South… City:…    0.979       18  4189    19413
## 5 The U… Tusc… AL     Bachel… South… City:…    0.533       28 32387    28836
## 6 Aubur… Mont… AL     Bachel… South… City:…    0.825       22  4211    19892
## # … with 7 more variables: costt4_p <dbl>, tuitionfee_in <dbl>,
## #   tuitionfee_out <dbl>, debt_mdn <dbl>, grad_debt_mdn <dbl>, female <dbl>,
## #   bachelor_degree <dbl>
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
## # A tibble: 3 x 2
##   term                      estimate
##   <chr>                        <dbl>
## 1 (Intercept)                 0.738 
## 2 preddegBachelor Degree     -0.0666
## 3 preddegCertificate Degree   0.0119
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
## 1             Associate           0.78720
## 2              Bachelor           0.68815
## 3           Certificate           0.76670
## 4    bachelor_associate          -0.09905
## 5 certificate_associate          -0.02050
## 6  bachelor_certificate          -0.07855
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


