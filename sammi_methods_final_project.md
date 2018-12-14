sammi\_methods\_final\_project
================
Samantha Brown
12/11/2018

**Load Data**

``` r
cancer_data <- read_csv("./data/Cancer_Registry.csv") %>%
        janitor::clean_names()
```

### Data Exploration

1.  75% of the variable `pct_some_col18_24` is missing. Therefore, we will remove it from the dataset.

2.  20% of the variable `pct_private_coverage_alone` is missing. Therefore, we will remove it from the dataset.

3.  `med_income` and `poverty_percent` are highly correlated with a correlation coefficient of -78.9%. To account for this, we will remove `poverty_percent` from the dataset.

4.  `median_age_female` and `median_age_male` are highly correlated with a correlation coefficient of 93.37%. Thus, we will only consider `median_age` for our data analysis.

5.  `avg_ann_count` and `avg_deaths_per_year` have a high correlation coefficient of 93.94%. Both of these variables are also highly correlated with `pop_est2015`. We will remove `avg_ann_count` and `avg_deaths_per_year`.

``` r
cor(cancer_data$pct_private_coverage, cancer_data$pct_public_coverage)
```

    ## [1] -0.7200115

``` r
cor(cancer_data$pct_emp_priv_coverage, cancer_data$pct_private_coverage)
```

    ## [1] 0.8274588

``` r
cor(cancer_data$pct_public_coverage_alone, cancer_data$pct_public_coverage)
```

    ## [1] 0.8658328

``` r
tidy_data = cancer_data %>% 
  select(-pct_some_col18_24, -pct_private_coverage_alone, -poverty_percent, -median_age_female, -median_age_male, -avg_ann_count, -avg_deaths_per_year, -pct_public_coverage, -pct_hs25_over, -pct_employed16_over, -pct_married_households, -pct_emp_priv_coverage, -binned_inc, -pct_hs18_24, -pct_no_hs18_24, -pct_bach_deg18_24) %>% 
  separate(geography, into = c("county", "state"), sep = ", ") %>% 
   mutate(region = case_when(  
            state %in% c("Montana", "Arizona","Idaho","Colorado", "New Mexico","Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington") ~ "West",
            state %in% c("Illinois","Indiana", "Michigan","Iowa","Ohio", "Kansas","Minnesota","Missouri","Wisconsin", "Nebraska","North Dakota","South Dakota") ~ "Midwest",
            state %in% c("District of Columbia", "Delaware","Georgia",
            "Maryland", "North Carolina","Florida","South Carolina",
            "West Virginia","Kentucky", "Alabama","Virginia","Mississippi", "Arkansas","Tennessee","Louisiana","Oklahoma","Texas") ~ "South",
             state %in%  c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania") ~ "Northeast" ), region = as.factor(region) %>% fct_relevel(., "West", "Midwest", "Northeast", "South"))

tidy_data = tidy_data %>%
  select(-county, -state) %>% 
  mutate(pct_non_black = pct_asian + pct_white + pct_other_race) %>% 
  select(-pct_asian, -pct_white, -pct_other_race, -pct_private_coverage, -pct_public_coverage_alone, -pct_black)
```

Start with full model

``` r
mult.fit <- lm(target_death_rate ~ ., data = tidy_data)
```

``` r
library(broom)

## Forward elimination
## Run regression on all individual variables 

fit1 = lm(target_death_rate ~ incidence_rate, data = tidy_data)
tidy(fit1)
```

    ## # A tibble: 2 x 5
    ##   term           estimate std.error statistic   p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)      76.2     3.72         20.5 1.61e- 87
    ## 2 incidence_rate    0.229   0.00823      27.8 2.07e-151

``` r
fit2 = lm(target_death_rate ~ med_income, data = tidy_data)
tidy(fit2)
```

    ## # A tibble: 2 x 5
    ##   term           estimate std.error statistic   p.value
    ##   <chr>             <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)  225.       1.83          123.  0.       
    ## 2 med_income    -0.000988 0.0000377     -26.2 2.04e-136

``` r
fit3 = lm(target_death_rate ~ pop_est2015, data = tidy_data)
tidy(fit3)
```

    ## # A tibble: 2 x 5
    ##   term            estimate  std.error statistic  p.value
    ##   <chr>              <dbl>      <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  180.        0.523         344.   0.      
    ## 2 pop_est2015   -0.0000101 0.00000152     -6.67 2.94e-11

``` r
fit4 = lm(target_death_rate ~ study_per_cap, data = tidy_data)
tidy(fit4)
```

    ## # A tibble: 2 x 5
    ##   term            estimate std.error statistic p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    179.       0.524       341.     0    
    ## 2 study_per_cap   -0.00117  0.000949     -1.23   0.219

``` r
fit5 = lm(target_death_rate ~ median_age, data = tidy_data)
tidy(fit5)
```

    ## # A tibble: 2 x 5
    ##   term         estimate std.error statistic p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept) 179.         0.711    251.      0    
    ## 2 median_age    0.00268    0.0111     0.241   0.809

``` r
fit6 <- lm(target_death_rate ~ avg_household_size, data = tidy_data)
tidy(fit6)
```

    ## # A tibble: 2 x 5
    ##   term               estimate std.error statistic p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)          185.        2.95     62.6   0     
    ## 2 avg_household_size    -2.39      1.17     -2.04  0.0416

``` r
fit7 <- lm(target_death_rate ~ percent_married, data = tidy_data)
tidy(fit7)
```

    ## # A tibble: 2 x 5
    ##   term            estimate std.error statistic  p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)       234.      3.67        63.8 0.      
    ## 2 percent_married    -1.07    0.0703     -15.3 7.92e-51

``` r
fit8 = lm(target_death_rate ~ pct_bach_deg25_over, data = tidy_data)
tidy(fit8)
```

    ## # A tibble: 2 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           212.      1.17       181.  0.       
    ## 2 pct_bach_deg25_over    -2.50    0.0815     -30.6 5.66e-180

``` r
fit9 = lm(target_death_rate ~ pct_unemployed16_over, data = tidy_data)
tidy(fit9)
```

    ## # A tibble: 2 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             155.       1.16      134.  0.       
    ## 2 pct_unemployed16_over     3.04     0.135      22.6 2.47e-104

``` r
fit13 <- lm(target_death_rate ~ pct_non_black, data = tidy_data)
tidy(fit13)
```

    ## # A tibble: 2 x 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    219.       2.76        79.5 0.      
    ## 2 pct_non_black   -0.467    0.0312     -14.9 9.95e-49

``` r
fit14 <- lm(target_death_rate ~ birth_rate, data = tidy_data)
tidy(fit14)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic    p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)   186.       1.51     123.   0         
    ## 2 birth_rate     -1.22     0.252     -4.84 0.00000135

``` r
fit15 <- lm(target_death_rate ~ region, data = tidy_data)
tidy(fit15)
```

    ## # A tibble: 4 x 5
    ##   term            estimate std.error statistic  p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)        158.       1.25    126.   0.      
    ## 2 regionMidwest       16.5      1.49     11.1  5.52e-28
    ## 3 regionNortheast     14.5      2.14      6.76 1.68e-11
    ## 4 regionSouth         30.9      1.43     21.6  1.44e-96

``` r
## Begin forward elimination -- Step 1

forward1 = lm(target_death_rate ~ pct_bach_deg25_over, data = tidy_data)
tidy(forward1)
```

    ## # A tibble: 2 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           212.      1.17       181.  0.       
    ## 2 pct_bach_deg25_over    -2.50    0.0815     -30.6 5.66e-180

``` r
fit1 = update(forward1, . ~ . +incidence_rate)
tidy(fit1)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          112.      3.34         33.6 3.85e-211
    ## 2 pct_bach_deg25_over   -2.41    0.0710      -34.0 4.54e-215
    ## 3 incidence_rate         0.219   0.00702      31.3 1.60e-186

``` r
fit2 = update(forward1, . ~ . +med_income)
tidy(fit2)
```

    ## # A tibble: 3 x 5
    ##   term                   estimate std.error statistic  p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)          222.       1.77         126.   0.      
    ## 2 pct_bach_deg25_over   -1.87     0.114        -16.5  1.81e-58
    ## 3 med_income            -0.000396 0.0000510     -7.76 1.13e-14

``` r
fit3 = update(forward1, . ~ . +pop_est2015)
tidy(fit3)
```

    ## # A tibble: 3 x 5
    ##   term                     estimate  std.error statistic   p.value
    ##   <chr>                       <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          212.         1.18          179.   0.       
    ## 2 pct_bach_deg25_over   -2.54       0.0853        -29.7  7.22e-171
    ## 3 pop_est2015            0.00000225 0.00000140      1.61 1.08e-  1

``` r
fit4 = update(forward1, . ~ . +study_per_cap)
tidy(fit4)
```

    ## # A tibble: 3 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          212.       1.17        181.   0.       
    ## 2 pct_bach_deg25_over   -2.51     0.0819      -30.7  2.13e-180
    ## 3 study_per_cap          0.00161  0.000835      1.93 5.33e-  2

``` r
fit5 = update(forward1, . ~ . +median_age)
tidy(fit5)
```

    ## # A tibble: 3 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          212.        1.26      169.    0.       
    ## 2 pct_bach_deg25_over   -2.50      0.0815    -30.6   6.28e-180
    ## 3 median_age            -0.00337   0.00971    -0.348 7.28e-  1

``` r
fit6 = update(forward1, . ~ . +avg_household_size)
tidy(fit6)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           217.      2.78       77.9  0.       
    ## 2 pct_bach_deg25_over    -2.50    0.0815    -30.6  8.43e-180
    ## 3 avg_household_size     -1.95    1.02       -1.90 5.70e-  2

``` r
fit7 = update(forward1, . ~ . +percent_married)
tidy(fit7)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          256.       3.30        77.4 0.       
    ## 2 pct_bach_deg25_over   -2.38     0.0794     -30.0 2.10e-173
    ## 3 percent_married       -0.881    0.0621     -14.2 2.80e- 44

``` r
fit8 = update(forward1, . ~ . +pct_unemployed16_over)
tidy(fit8)
```

    ## # A tibble: 3 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             192.      1.85       103.  0.       
    ## 2 pct_bach_deg25_over      -2.06    0.0852     -24.1 5.27e-118
    ## 3 pct_unemployed16_over     1.84    0.133       13.8 2.79e- 42

``` r
fit12 = update(forward1, . ~ . +pct_non_black)
tidy(fit12)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          241.       2.54        94.8 0.       
    ## 2 pct_bach_deg25_over   -2.36     0.0801     -29.5 5.02e-168
    ## 3 pct_non_black         -0.357    0.0278     -12.8 8.91e- 37

``` r
fit13 = update(forward1, . ~ . +birth_rate)
tidy(fit13)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           223.      1.77      126.   0.       
    ## 2 pct_bach_deg25_over    -2.56    0.0809    -31.6  8.94e-190
    ## 3 birth_rate             -1.83    0.220      -8.34 1.15e- 16

``` r
fit14 = update(forward1, . ~ . +region)
tidy(fit14)
```

    ## # A tibble: 5 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           192.      1.72      111.   0.       
    ## 2 pct_bach_deg25_over    -2.13    0.0821    -26.0  1.85e-134
    ## 3 regionMidwest          12.0     1.35        8.89 1.01e- 18
    ## 4 regionNortheast        16.0     1.94        8.27 2.03e- 16
    ## 5 regionSouth            22.0     1.34       16.5  2.34e- 58

``` r
## Step 2

forward2 <- update(forward1, . ~ . +incidence_rate)
tidy(forward2)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          112.      3.34         33.6 3.85e-211
    ## 2 pct_bach_deg25_over   -2.41    0.0710      -34.0 4.54e-215
    ## 3 incidence_rate         0.219   0.00702      31.3 1.60e-186

``` r
fit1 = update(forward2, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 4 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          123.       3.45           35.7 4.94e-233
    ## 2 pct_bach_deg25_over   -1.71     0.0985        -17.3 2.64e- 64
    ## 3 incidence_rate         0.222    0.00691        32.2 1.59e-195
    ## 4 med_income            -0.000448 0.0000441     -10.2 7.84e- 24

``` r
fit2 = update(forward2, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 4 x 5
    ##   term                estimate  std.error statistic   p.value
    ##   <chr>                  <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          1.12e+2 3.35          33.5   2.98e-210
    ## 2 pct_bach_deg25_over -2.43e+0 0.0744       -32.6   1.99e-200
    ## 3 incidence_rate       2.19e-1 0.00702       31.2   5.63e-186
    ## 4 pop_est2015          7.25e-7 0.00000122     0.595 5.52e-  1

``` r
fit3 = update(forward2, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 4 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          112.        3.35        33.5   8.60e-210
    ## 2 pct_bach_deg25_over   -2.41      0.0714     -33.7   2.62e-212
    ## 3 incidence_rate         0.220     0.00704     31.2   1.12e-185
    ## 4 study_per_cap         -0.000251  0.000729    -0.344 7.31e-  1

``` r
fit4 = update(forward2, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 4 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          113.        3.36       33.5   3.01e-210
    ## 2 pct_bach_deg25_over   -2.41      0.0710    -34.0   3.52e-215
    ## 3 incidence_rate         0.220     0.00702    31.3   1.25e-186
    ## 4 median_age            -0.00795   0.00845    -0.942 3.46e-  1

``` r
fit5 = update(forward2, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 4 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          108.      4.23        25.7  1.51e-131
    ## 2 pct_bach_deg25_over   -2.41    0.0709     -34.0  2.84e-215
    ## 3 incidence_rate         0.221   0.00706     31.2  3.58e-186
    ## 4 avg_household_size     1.36    0.897        1.51 1.30e-  1

``` r
fit6 = update(forward2, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 4 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          151.      4.50         33.6 4.39e-211
    ## 2 pct_bach_deg25_over   -2.33    0.0696      -33.4 4.80e-209
    ## 3 incidence_rate         0.209   0.00689      30.4 1.70e-177
    ## 4 percent_married       -0.687   0.0548      -12.5 2.93e- 35

``` r
fit7 = update(forward2, . ~ . +pct_unemployed16_over)
tidy(fit7)
```

    ## # A tibble: 4 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             99.4     3.40         29.2 1.33e-165
    ## 2 pct_bach_deg25_over     -2.06    0.0744      -27.6 5.04e-150
    ## 3 incidence_rate           0.211   0.00686      30.8 1.74e-181
    ## 4 pct_unemployed16_over    1.51    0.117        12.9 2.87e- 37

``` r
fit11 = update(forward2, . ~ . +pct_non_black)
tidy(fit11)
```

    ## # A tibble: 4 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          140.      3.91         35.9 2.15e-235
    ## 2 pct_bach_deg25_over   -2.29    0.0697      -32.9 1.80e-203
    ## 3 incidence_rate         0.214   0.00685      31.3 1.16e-186
    ## 4 pct_non_black         -0.312   0.0242      -12.9 5.78e- 37

``` r
fit12 = update(forward2, . ~ . +birth_rate)
tidy(fit12)
```

    ## # A tibble: 4 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          121.      3.67        33.0  1.15e-204
    ## 2 pct_bach_deg25_over   -2.45    0.0709     -34.6  4.21e-221
    ## 3 incidence_rate         0.215   0.00703     30.5  1.26e-178
    ## 4 birth_rate            -1.11    0.194       -5.73 1.12e-  8

``` r
fit13 = update(forward2, . ~ . +region)
tidy(fit13)
```

    ## # A tibble: 6 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          101.      3.33       30.4   1.47e-177
    ## 2 pct_bach_deg25_over   -2.06    0.0719    -28.7   1.54e-160
    ## 3 incidence_rate         0.214   0.00702    30.4   1.06e-177
    ## 4 regionMidwest          5.58    1.21        4.63  3.79e-  6
    ## 5 regionNortheast        1.39    1.77        0.789 4.30e-  1
    ## 6 regionSouth           15.4     1.19       12.9   2.32e- 37

``` r
## Step 3
forward3 = update(forward2, . ~ . + region)
tidy(forward3)
```

    ## # A tibble: 6 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          101.      3.33       30.4   1.47e-177
    ## 2 pct_bach_deg25_over   -2.06    0.0719    -28.7   1.54e-160
    ## 3 incidence_rate         0.214   0.00702    30.4   1.06e-177
    ## 4 regionMidwest          5.58    1.21        4.63  3.79e-  6
    ## 5 regionNortheast        1.39    1.77        0.789 4.30e-  1
    ## 6 regionSouth           15.4     1.19       12.9   2.32e- 37

``` r
fit1 = update(forward3, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 7 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          111.       3.48          31.8  1.45e-191
    ## 2 pct_bach_deg25_over   -1.52     0.0964       -15.8  3.33e- 54
    ## 3 incidence_rate         0.214    0.00694       30.9  2.96e-182
    ## 4 regionMidwest          6.18     1.19           5.18 2.39e-  7
    ## 5 regionNortheast        2.77     1.75           1.58 1.15e-  1
    ## 6 regionSouth           14.9      1.18          12.7  8.47e- 36
    ## 7 med_income            -0.000360 0.0000434     -8.29 1.66e- 16

``` r
fit2 = update(forward3, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 7 x 5
    ##   term                estimate  std.error statistic   p.value
    ##   <chr>                  <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          1.02e+2 3.34          30.4   2.08e-177
    ## 2 pct_bach_deg25_over -2.08e+0 0.0748       -27.8   8.94e-152
    ## 3 incidence_rate       2.13e-1 0.00702       30.4   3.14e-177
    ## 4 regionMidwest        5.66e+0 1.21           4.68  3.00e-  6
    ## 5 regionNortheast      1.35e+0 1.77           0.762 4.46e-  1
    ## 6 regionSouth          1.55e+1 1.19          13.0   1.87e- 37
    ## 7 pop_est2015          9.09e-7 0.00000118     0.769 4.42e-  1

``` r
fit3 = update(forward3, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 7 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          102.        3.34        30.4   3.74e-177
    ## 2 pct_bach_deg25_over   -2.07      0.0722     -28.6   7.94e-160
    ## 3 incidence_rate         0.213     0.00704     30.3   4.06e-176
    ## 4 regionMidwest          5.54      1.21         4.59  4.62e-  6
    ## 5 regionNortheast        1.41      1.77         0.796 4.26e-  1
    ## 6 regionSouth           15.4       1.19        13.0   2.06e- 37
    ## 7 study_per_cap          0.000446  0.000705     0.632 5.27e-  1

``` r
fit4 = update(forward3, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 7 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          102.        3.35       30.3   2.34e-176
    ## 2 pct_bach_deg25_over   -2.07      0.0719    -28.7   1.48e-160
    ## 3 incidence_rate         0.214     0.00702    30.4   1.04e-177
    ## 4 regionMidwest          5.58      1.21        4.63  3.82e-  6
    ## 5 regionNortheast        1.42      1.77        0.804 4.22e-  1
    ## 6 regionSouth           15.4       1.19       12.9   2.58e- 37
    ## 7 median_age            -0.00480   0.00813    -0.591 5.54e-  1

``` r
fit5 = update(forward3, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 7 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           99.7     4.18       23.9   1.73e-115
    ## 2 pct_bach_deg25_over   -2.07    0.0719    -28.7   1.41e-160
    ## 3 incidence_rate         0.214   0.00705    30.3   6.33e-177
    ## 4 regionMidwest          5.68    1.22        4.67  3.08e-  6
    ## 5 regionNortheast        1.48    1.77        0.834 4.04e-  1
    ## 6 regionSouth           15.5     1.19       13.0   1.98e- 37
    ## 7 avg_household_size     0.558   0.875       0.638 5.23e-  1

``` r
fit6 = update(forward3, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 7 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          135.      4.53       29.8   8.83e-172
    ## 2 pct_bach_deg25_over   -2.01    0.0707    -28.4   4.82e-158
    ## 3 incidence_rate         0.206   0.00693    29.7   2.41e-170
    ## 4 regionMidwest          7.51    1.20        6.28  3.91e- 10
    ## 5 regionNortheast        0.333   1.74        0.192 8.48e-  1
    ## 6 regionSouth           14.5     1.17       12.4   2.23e- 34
    ## 7 percent_married       -0.603   0.0558    -10.8   1.08e- 26

``` r
fit7 = update(forward3, . ~ . +pct_unemployed16_over)
tidy(fit7)
```

    ## # A tibble: 7 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             88.4     3.43        25.8  8.98e-133
    ## 2 pct_bach_deg25_over     -1.74    0.0752     -23.1  5.08e-109
    ## 3 incidence_rate           0.203   0.00691     29.4  7.05e-168
    ## 4 regionMidwest            9.57    1.22         7.83 6.75e- 15
    ## 5 regionNortheast          3.00    1.73         1.74 8.28e-  2
    ## 6 regionSouth             16.1     1.17        13.8  4.83e- 42
    ## 7 pct_unemployed16_over    1.46    0.120       12.1  4.30e- 33

``` r
fit10 = update(forward3, . ~ . +pct_non_black)
tidy(fit10)
```

    ## # A tibble: 7 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          122.      4.13        29.5  1.20e-168
    ## 2 pct_bach_deg25_over   -2.04    0.0712     -28.7  7.02e-161
    ## 3 incidence_rate         0.210   0.00696     30.1  9.48e-175
    ## 4 regionMidwest          6.51    1.20         5.44 5.76e-  8
    ## 5 regionNortheast        2.09    1.75         1.20 2.31e-  1
    ## 6 regionSouth           13.4     1.20        11.1  2.57e- 28
    ## 7 pct_non_black         -0.214   0.0258      -8.28 1.79e- 16

``` r
fit11 = update(forward3, . ~ . +birth_rate)
tidy(fit11)
```

    ## # A tibble: 7 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          108.      3.66       29.5   4.66e-168
    ## 2 pct_bach_deg25_over   -2.10    0.0722    -29.1   3.34e-164
    ## 3 incidence_rate         0.211   0.00703    30.0   1.91e-173
    ## 4 regionMidwest          5.58    1.20        4.64  3.60e-  6
    ## 5 regionNortheast        0.608   1.77        0.344 7.31e-  1
    ## 6 regionSouth           14.9     1.19       12.5   7.68e- 35
    ## 7 birth_rate            -0.803   0.190      -4.23  2.41e-  5

``` r
## Step 4
forward4 = update(forward3, . ~ . + pct_unemployed16_over)
tidy(forward4)
```

    ## # A tibble: 7 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             88.4     3.43        25.8  8.98e-133
    ## 2 pct_bach_deg25_over     -1.74    0.0752     -23.1  5.08e-109
    ## 3 incidence_rate           0.203   0.00691     29.4  7.05e-168
    ## 4 regionMidwest            9.57    1.22         7.83 6.75e- 15
    ## 5 regionNortheast          3.00    1.73         1.74 8.28e-  2
    ## 6 regionSouth             16.1     1.17        13.8  4.83e- 42
    ## 7 pct_unemployed16_over    1.46    0.120       12.1  4.30e- 33

``` r
fit1 = update(forward4, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 8 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            96.1      3.71          25.9  3.49e-134
    ## 2 pct_bach_deg25_over    -1.42     0.0953       -14.9  1.13e- 48
    ## 3 incidence_rate          0.205    0.00688       29.8  1.96e-171
    ## 4 regionMidwest           9.48     1.22           7.79 9.25e- 15
    ## 5 regionNortheast         3.71     1.73           2.15 3.17e-  2
    ## 6 regionSouth            15.7      1.16          13.5  2.90e- 40
    ## 7 pct_unemployed16_over   1.28     0.124         10.3  1.83e- 24
    ## 8 med_income             -0.000237 0.0000443     -5.36 9.05e-  8

``` r
fit2 = update(forward4, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 8 x 5
    ##   term                      estimate  std.error statistic   p.value
    ##   <chr>                        <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            88.0        3.45           25.5  3.20e-130
    ## 2 pct_bach_deg25_over    -1.71       0.0791        -21.6  1.62e- 96
    ## 3 incidence_rate          0.204      0.00691        29.5  5.09e-168
    ## 4 regionMidwest           9.51       1.22            7.78 1.01e- 14
    ## 5 regionNortheast         3.09       1.73            1.79 7.43e-  2
    ## 6 regionSouth            16.0        1.17           13.8  7.45e- 42
    ## 7 pct_unemployed16_over   1.48       0.122          12.2  3.15e- 33
    ## 8 pop_est2015            -0.00000131 0.00000117     -1.12 2.62e-  1

``` r
fit3 = update(forward4, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 8 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            88.4       3.44        25.7   4.97e-132
    ## 2 pct_bach_deg25_over    -1.74      0.0756     -23.0   4.54e-108
    ## 3 incidence_rate          0.203     0.00693     29.3   7.33e-167
    ## 4 regionMidwest           9.56      1.23         7.80  8.29e- 15
    ## 5 regionNortheast         3.00      1.73         1.74  8.27e-  2
    ## 6 regionSouth            16.1       1.17        13.8   4.96e- 42
    ## 7 pct_unemployed16_over   1.46      0.120       12.1   5.32e- 33
    ## 8 study_per_cap           0.000105  0.000689     0.152 8.79e-  1

``` r
fit4 = update(forward4, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 8 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            88.6       3.44       25.7   2.77e-132
    ## 2 pct_bach_deg25_over    -1.74      0.0752    -23.1   4.23e-109
    ## 3 incidence_rate          0.203     0.00691    29.4   6.53e-168
    ## 4 regionMidwest           9.57      1.22        7.83  6.73e- 15
    ## 5 regionNortheast         3.04      1.73        1.75  7.95e-  2
    ## 6 regionSouth            16.1       1.17       13.8   5.48e- 42
    ## 7 pct_unemployed16_over   1.46      0.120      12.1   3.91e- 33
    ## 8 median_age             -0.00607   0.00794    -0.764 4.45e-  1

``` r
fit5 = update(forward4, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             90.1     4.16       21.7   7.90e- 97
    ## 2 pct_bach_deg25_over     -1.73    0.0754    -23.0   3.04e-108
    ## 3 incidence_rate           0.203   0.00695    29.2   2.09e-165
    ## 4 regionMidwest            9.49    1.23        7.72  1.53e- 14
    ## 5 regionNortheast          2.92    1.73        1.68  9.23e-  2
    ## 6 regionSouth             16.1     1.17       13.8   6.85e- 42
    ## 7 pct_unemployed16_over    1.47    0.121      12.1   4.14e- 33
    ## 8 avg_household_size      -0.625   0.860      -0.726 4.68e-  1

``` r
fit6 = update(forward4, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.      5.43        20.5  7.54e- 88
    ## 2 pct_bach_deg25_over     -1.79    0.0755     -23.8  1.56e-114
    ## 3 incidence_rate           0.202   0.00688     29.3  3.61e-166
    ## 4 regionMidwest            9.64    1.22         7.92 3.19e- 15
    ## 5 regionNortheast          1.96    1.73         1.13 2.58e-  1
    ## 6 regionSouth             15.4     1.17        13.2  1.35e- 38
    ## 7 pct_unemployed16_over    1.07    0.139        7.73 1.46e- 14
    ## 8 percent_married         -0.351   0.0641      -5.48 4.54e-  8

``` r
fit10 = update(forward4, . ~ . +pct_non_black)
tidy(fit10)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            98.5      4.80        20.5  9.60e- 88
    ## 2 pct_bach_deg25_over    -1.77     0.0760     -23.3  7.19e-111
    ## 3 incidence_rate          0.203    0.00690     29.4  6.48e-168
    ## 4 regionMidwest           9.42     1.22         7.71 1.72e- 14
    ## 5 regionNortheast         3.07     1.73         1.78 7.55e-  2
    ## 6 regionSouth            15.2      1.20        12.6  1.22e- 35
    ## 7 pct_unemployed16_over   1.26     0.136        9.27 3.33e- 20
    ## 8 pct_non_black          -0.0869   0.0289      -3.00 2.69e-  3

``` r
fit11 = update(forward4, . ~ . +birth_rate)
tidy(fit11)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)             93.9     3.76        24.9  4.05e-125
    ## 2 pct_bach_deg25_over     -1.77    0.0757     -23.4  1.24e-111
    ## 3 incidence_rate           0.201   0.00692     29.1  1.38e-164
    ## 4 regionMidwest            9.49    1.22         7.78 1.01e- 14
    ## 5 regionNortheast          2.33    1.74         1.34 1.80e-  1
    ## 6 regionSouth             15.6     1.17        13.4  1.29e- 39
    ## 7 pct_unemployed16_over    1.43    0.120       11.9  6.78e- 32
    ## 8 birth_rate              -0.656   0.186       -3.52 4.30e-  4

``` r
## Step 5
forward5 = update(forward4, . ~ . + percent_married)
tidy(forward5)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.      5.43        20.5  7.54e- 88
    ## 2 pct_bach_deg25_over     -1.79    0.0755     -23.8  1.56e-114
    ## 3 incidence_rate           0.202   0.00688     29.3  3.61e-166
    ## 4 regionMidwest            9.64    1.22         7.92 3.19e- 15
    ## 5 regionNortheast          1.96    1.73         1.13 2.58e-  1
    ## 6 regionSouth             15.4     1.17        13.2  1.35e- 38
    ## 7 pct_unemployed16_over    1.07    0.139        7.73 1.46e- 14
    ## 8 percent_married         -0.351   0.0641      -5.48 4.54e-  8

``` r
fit1 = update(forward5, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 9 x 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.       5.42          20.7  7.45e- 89
    ## 2 pct_bach_deg25_over     -1.54     0.0997       -15.5  4.73e- 52
    ## 3 incidence_rate           0.203    0.00688       29.5  1.01e-168
    ## 4 regionMidwest            9.56     1.21           7.87 4.87e- 15
    ## 5 regionNortheast          2.73     1.74           1.57 1.16e-  1
    ## 6 regionSouth             15.2      1.16          13.1  4.75e- 38
    ## 7 pct_unemployed16_over    1.03     0.139          7.39 1.87e- 13
    ## 8 percent_married         -0.270    0.0674        -4.01 6.23e-  5
    ## 9 med_income              -0.000179 0.0000465     -3.84 1.27e-  4

``` r
fit2 = update(forward5, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 9 x 5
    ##   term                       estimate  std.error statistic   p.value
    ##   <chr>                         <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.         5.43           20.6  3.77e- 88
    ## 2 pct_bach_deg25_over     -1.75       0.0791        -22.2  5.22e-101
    ## 3 incidence_rate           0.202      0.00688        29.3  2.04e-166
    ## 4 regionMidwest            9.56       1.22            7.85 5.70e- 15
    ## 5 regionNortheast          2.06       1.73            1.19 2.34e-  1
    ## 6 regionSouth             15.3        1.17           13.1  3.39e- 38
    ## 7 pct_unemployed16_over    1.09       0.139           7.85 5.86e- 15
    ## 8 percent_married         -0.365      0.0645         -5.65 1.74e-  8
    ## 9 pop_est2015             -0.00000207 0.00000117     -1.77 7.71e-  2

``` r
fit3 = update(forward5, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 9 x 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.        5.43        20.5   7.97e- 88
    ## 2 pct_bach_deg25_over     -1.79      0.0759     -23.6   2.04e-113
    ## 3 incidence_rate           0.202     0.00690     29.2   1.64e-165
    ## 4 regionMidwest            9.66      1.22         7.92  3.26e- 15
    ## 5 regionNortheast          1.95      1.73         1.13  2.60e-  1
    ## 6 regionSouth             15.4       1.17        13.2   1.61e- 38
    ## 7 pct_unemployed16_over    1.07      0.139        7.73  1.48e- 14
    ## 8 percent_married         -0.352     0.0643      -5.48  4.49e-  8
    ## 9 study_per_cap           -0.000156  0.000688    -0.227 8.21e-  1

``` r
fit4 = update(forward5, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 9 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.        5.43       20.5   7.62e- 88
    ## 2 pct_bach_deg25_over     -1.79      0.0756    -23.8   1.58e-114
    ## 3 incidence_rate           0.202     0.00689    29.3   3.80e-166
    ## 4 regionMidwest            9.64      1.22        7.92  3.21e- 15
    ## 5 regionNortheast          1.98      1.73        1.14  2.53e-  1
    ## 6 regionSouth             15.4       1.17       13.2   1.40e- 38
    ## 7 pct_unemployed16_over    1.07      0.139       7.74  1.37e- 14
    ## 8 percent_married         -0.350     0.0643     -5.44  5.71e-  8
    ## 9 median_age              -0.00306   0.00792    -0.386 7.00e-  1

``` r
fit5 = update(forward5, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 9 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            114.      5.97       19.1   1.32e- 76
    ## 2 pct_bach_deg25_over     -1.79    0.0757    -23.7   9.70e-114
    ## 3 incidence_rate           0.201   0.00692    29.0   1.66e-163
    ## 4 regionMidwest            9.54    1.22        7.80  8.17e- 15
    ## 5 regionNortheast          1.85    1.74        1.07  2.86e-  1
    ## 6 regionSouth             15.3     1.17       13.1   2.08e- 38
    ## 7 pct_unemployed16_over    1.08    0.139       7.78  1.02e- 14
    ## 8 percent_married         -0.353   0.0641     -5.51  3.99e-  8
    ## 9 avg_household_size      -0.759   0.856      -0.887 3.75e-  1

``` r
fit9 = update(forward5, . ~ . +pct_non_black)
tidy(fit9)
```

    ## # A tibble: 9 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.        5.60       20.0   1.63e- 83
    ## 2 pct_bach_deg25_over     -1.80      0.0759    -23.6   1.30e-113
    ## 3 incidence_rate           0.202     0.00689    29.3   4.23e-166
    ## 4 regionMidwest            9.63      1.22        7.90  3.82e- 15
    ## 5 regionNortheast          1.98      1.74        1.14  2.54e-  1
    ## 6 regionSouth             15.3       1.20       12.8   1.64e- 36
    ## 7 pct_unemployed16_over    1.07      0.142       7.49  9.17e- 14
    ## 8 percent_married         -0.345     0.0752     -4.58  4.79e-  6
    ## 9 pct_non_black           -0.00582   0.0338     -0.172 8.64e-  1

``` r
fit10 = update(forward5, . ~ . +birth_rate)
tidy(fit10)
```

    ## # A tibble: 9 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            116.      5.57       20.7   1.71e- 89
    ## 2 pct_bach_deg25_over     -1.82    0.0760    -24.0   1.27e-116
    ## 3 incidence_rate           0.200   0.00690    29.0   2.86e-163
    ## 4 regionMidwest            9.57    1.22        7.87  4.74e- 15
    ## 5 regionNortheast          1.40    1.74        0.807 4.20e-  1
    ## 6 regionSouth             15.0     1.17       12.8   1.15e- 36
    ## 7 pct_unemployed16_over    1.06    0.139       7.68  2.21e- 14
    ## 8 percent_married         -0.337   0.0642     -5.24  1.68e-  7
    ## 9 birth_rate              -0.584   0.186      -3.14  1.68e-  3

``` r
## Step 6

forward6 = update(forward5, . ~ . + med_income)
tidy(forward6)
```

    ## # A tibble: 9 x 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.       5.42          20.7  7.45e- 89
    ## 2 pct_bach_deg25_over     -1.54     0.0997       -15.5  4.73e- 52
    ## 3 incidence_rate           0.203    0.00688       29.5  1.01e-168
    ## 4 regionMidwest            9.56     1.21           7.87 4.87e- 15
    ## 5 regionNortheast          2.73     1.74           1.57 1.16e-  1
    ## 6 regionSouth             15.2      1.16          13.1  4.75e- 38
    ## 7 pct_unemployed16_over    1.03     0.139          7.39 1.87e- 13
    ## 8 percent_married         -0.270    0.0674        -4.01 6.23e-  5
    ## 9 med_income              -0.000179 0.0000465     -3.84 1.27e-  4

``` r
fit2 = update(forward6, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 10 x 5
    ##    term                       estimate  std.error statistic   p.value
    ##    <chr>                         <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            112.         5.42           20.7  5.09e- 89
    ##  2 pct_bach_deg25_over     -1.52       0.101         -15.1  8.34e- 50
    ##  3 incidence_rate           0.203      0.00688        29.5  9.40e-169
    ##  4 regionMidwest            9.50       1.21            7.82 7.38e- 15
    ##  5 regionNortheast          2.78       1.74            1.60 1.11e-  1
    ##  6 regionSouth             15.2        1.17           13.0  8.96e- 38
    ##  7 pct_unemployed16_over    1.04       0.139           7.48 9.84e- 14
    ##  8 percent_married         -0.283      0.0681         -4.16 3.31e-  5
    ##  9 med_income              -0.000171   0.0000469      -3.64 2.79e-  4
    ## 10 pop_est2015             -0.00000151 0.00000118     -1.28 2.00e-  1

``` r
fit3 = update(forward6, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            112.       5.42         20.7   7.78e- 89
    ##  2 pct_bach_deg25_over     -1.54     0.100       -15.4   2.16e- 51
    ##  3 incidence_rate           0.203    0.00690      29.5   4.06e-168
    ##  4 regionMidwest            9.58     1.22          7.88  4.64e- 15
    ##  5 regionNortheast          2.73     1.74          1.57  1.17e-  1
    ##  6 regionSouth             15.2      1.17         13.1   5.98e- 38
    ##  7 pct_unemployed16_over    1.03     0.139         7.39  1.90e- 13
    ##  8 percent_married         -0.272    0.0675       -4.02  5.89e-  5
    ##  9 med_income              -0.000179 0.0000466    -3.85  1.21e-  4
    ## 10 study_per_cap           -0.000253 0.000687     -0.368 7.13e-  1

``` r
fit4 = update(forward6, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            112.       5.42         20.7   7.38e- 89
    ##  2 pct_bach_deg25_over     -1.54     0.0998      -15.5   5.14e- 52
    ##  3 incidence_rate           0.203    0.00689      29.5   1.03e-168
    ##  4 regionMidwest            9.56     1.21          7.87  4.91e- 15
    ##  5 regionNortheast          2.76     1.74          1.59  1.13e-  1
    ##  6 regionSouth             15.2      1.16         13.1   4.96e- 38
    ##  7 pct_unemployed16_over    1.03     0.139         7.40  1.69e- 13
    ##  8 percent_married         -0.268    0.0676       -3.96  7.59e-  5
    ##  9 med_income              -0.000179 0.0000466    -3.85  1.21e-  4
    ## 10 median_age              -0.00380  0.00790      -0.481 6.30e-  1

``` r
fit5 = update(forward6, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            112.       5.97        18.7    3.36e- 74
    ##  2 pct_bach_deg25_over     -1.54     0.100      -15.4    2.43e- 51
    ##  3 incidence_rate           0.203    0.00694     29.3    2.54e-166
    ##  4 regionMidwest            9.56     1.22         7.84   6.26e- 15
    ##  5 regionNortheast          2.74     1.75         1.57   1.17e-  1
    ##  6 regionSouth             15.2      1.17        13.1    5.06e- 38
    ##  7 pct_unemployed16_over    1.03     0.140        7.34   2.65e- 13
    ##  8 percent_married         -0.270    0.0678      -3.98   6.98e-  5
    ##  9 med_income              -0.000179 0.0000480   -3.73   1.92e-  4
    ## 10 avg_household_size       0.0341   0.880        0.0387 9.69e-  1

``` r
fit8 = update(forward6, . ~ . +pct_non_black)
tidy(fit8)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            113.       5.59         20.2   4.47e- 85
    ##  2 pct_bach_deg25_over     -1.54     0.0998      -15.5   4.51e- 52
    ##  3 incidence_rate           0.203    0.00689      29.5   9.97e-169
    ##  4 regionMidwest            9.52     1.22          7.82  6.96e- 15
    ##  5 regionNortheast          2.83     1.75          1.62  1.06e-  1
    ##  6 regionSouth             15.1      1.20         12.6   1.84e- 35
    ##  7 pct_unemployed16_over    1.01     0.143         7.05  2.16e- 12
    ##  8 percent_married         -0.247    0.0792       -3.12  1.83e-  3
    ##  9 med_income              -0.000181 0.0000468    -3.87  1.09e-  4
    ## 10 pct_non_black           -0.0189   0.0339       -0.556 5.78e-  1

``` r
fit9 = update(forward6, . ~ . +birth_rate)
tidy(fit9)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.       5.56          20.8  2.83e- 90
    ##  2 pct_bach_deg25_over     -1.58     0.100        -15.7  1.10e- 53
    ##  3 incidence_rate           0.202    0.00690       29.2  9.36e-166
    ##  4 regionMidwest            9.49     1.21           7.82 7.06e- 15
    ##  5 regionNortheast          2.18     1.75           1.25 2.12e-  1
    ##  6 regionSouth             14.9      1.17          12.7  3.30e- 36
    ##  7 pct_unemployed16_over    1.02     0.139          7.35 2.59e- 13
    ##  8 percent_married         -0.258    0.0674        -3.82 1.34e-  4
    ##  9 med_income              -0.000174 0.0000465     -3.75 1.81e-  4
    ## 10 birth_rate              -0.563    0.185         -3.04 2.42e-  3

``` r
## Step 7

forward7 = update(forward6, . ~ . + birth_rate)
tidy(forward7)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.       5.56          20.8  2.83e- 90
    ##  2 pct_bach_deg25_over     -1.58     0.100        -15.7  1.10e- 53
    ##  3 incidence_rate           0.202    0.00690       29.2  9.36e-166
    ##  4 regionMidwest            9.49     1.21           7.82 7.06e- 15
    ##  5 regionNortheast          2.18     1.75           1.25 2.12e-  1
    ##  6 regionSouth             14.9      1.17          12.7  3.30e- 36
    ##  7 pct_unemployed16_over    1.02     0.139          7.35 2.59e- 13
    ##  8 percent_married         -0.258    0.0674        -3.82 1.34e-  4
    ##  9 med_income              -0.000174 0.0000465     -3.75 1.81e-  4
    ## 10 birth_rate              -0.563    0.185         -3.04 2.42e-  3

``` r
fit2 = update(forward7, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 11 x 5
    ##    term                       estimate  std.error statistic   p.value
    ##    <chr>                         <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.         5.56           20.9  1.94e- 90
    ##  2 pct_bach_deg25_over     -1.56       0.101         -15.4  2.09e- 51
    ##  3 incidence_rate           0.202      0.00690        29.2  8.68e-166
    ##  4 regionMidwest            9.43       1.21            7.77 1.07e- 14
    ##  5 regionNortheast          2.22       1.75            1.27 2.03e-  1
    ##  6 regionSouth             14.8        1.17           12.7  6.10e- 36
    ##  7 pct_unemployed16_over    1.04       0.139           7.43 1.37e- 13
    ##  8 percent_married         -0.271      0.0682         -3.97 7.25e-  5
    ##  9 med_income              -0.000166   0.0000469      -3.55 3.91e-  4
    ## 10 birth_rate              -0.563      0.185          -3.04 2.42e-  3
    ## 11 pop_est2015             -0.00000151 0.00000118     -1.28 2.00e-  1

``` r
fit3 = update(forward7, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 11 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.       5.56         20.8   3.06e- 90
    ##  2 pct_bach_deg25_over     -1.57     0.101       -15.6   5.02e- 53
    ##  3 incidence_rate           0.202    0.00692      29.2   4.44e-165
    ##  4 regionMidwest            9.51     1.22          7.82  6.96e- 15
    ##  5 regionNortheast          2.18     1.75          1.25  2.13e-  1
    ##  6 regionSouth             14.9      1.17         12.7   3.94e- 36
    ##  7 pct_unemployed16_over    1.02     0.139         7.35  2.63e- 13
    ##  8 percent_married         -0.259    0.0675       -3.83  1.29e-  4
    ##  9 med_income              -0.000175 0.0000465    -3.76  1.75e-  4
    ## 10 birth_rate              -0.561    0.185        -3.03  2.49e-  3
    ## 11 study_per_cap           -0.000204 0.000686     -0.297 7.66e-  1

``` r
fit4 = update(forward7, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 11 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.       5.56         20.8   2.74e- 90
    ##  2 pct_bach_deg25_over     -1.58     0.100       -15.7   1.19e- 53
    ##  3 incidence_rate           0.202    0.00690      29.2   9.32e-166
    ##  4 regionMidwest            9.49     1.21          7.82  7.12e- 15
    ##  5 regionNortheast          2.21     1.75          1.27  2.06e-  1
    ##  6 regionSouth             14.9      1.17         12.7   3.47e- 36
    ##  7 pct_unemployed16_over    1.02     0.139         7.36  2.33e- 13
    ##  8 percent_married         -0.255    0.0676       -3.77  1.63e-  4
    ##  9 med_income              -0.000175 0.0000465    -3.76  1.73e-  4
    ## 10 birth_rate              -0.564    0.185        -3.04  2.38e-  3
    ## 11 median_age              -0.00407  0.00789      -0.515 6.06e-  1

``` r
fit5 = update(forward7, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 11 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            115.       6.06         19.0   3.46e- 76
    ##  2 pct_bach_deg25_over     -1.57     0.101       -15.6   6.94e- 53
    ##  3 incidence_rate           0.202    0.00695      29.1   6.12e-164
    ##  4 regionMidwest            9.52     1.22          7.82  7.46e- 15
    ##  5 regionNortheast          2.23     1.75          1.27  2.04e-  1
    ##  6 regionSouth             14.9      1.17         12.7   3.26e- 36
    ##  7 pct_unemployed16_over    1.02     0.140         7.27  4.54e- 13
    ##  8 percent_married         -0.256    0.0678       -3.77  1.68e-  4
    ##  9 med_income              -0.000178 0.0000479    -3.71  2.11e-  4
    ## 10 birth_rate              -0.567    0.186        -3.05  2.32e-  3
    ## 11 avg_household_size       0.261    0.882         0.296 7.67e-  1

``` r
fit6 = update(forward7, . ~ . +pct_non_black)
tidy(fit6)
```

    ## # A tibble: 11 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            118.       5.78          20.3  2.47e- 86
    ##  2 pct_bach_deg25_over     -1.58     0.100        -15.8  7.67e- 54
    ##  3 incidence_rate           0.202    0.00690       29.2  7.35e-166
    ##  4 regionMidwest            9.41     1.21           7.74 1.31e- 14
    ##  5 regionNortheast          2.33     1.75           1.33 1.84e-  1
    ##  6 regionSouth             14.6      1.21          12.1  9.08e- 33
    ##  7 pct_unemployed16_over    0.983    0.143          6.87 7.61e- 12
    ##  8 percent_married         -0.212    0.0798        -2.65 7.99e-  3
    ##  9 med_income              -0.000179 0.0000467     -3.83 1.28e-  4
    ## 10 birth_rate              -0.596    0.188         -3.17 1.54e-  3
    ## 11 pct_non_black           -0.0368   0.0344        -1.07 2.84e-  1

``` r
## Predicted model from forward elimination: 
mult_forward_fit = lm(target_death_rate ~ pct_bach_deg25_over + incidence_rate + region + pct_unemployed16_over + percent_married + med_income + birth_rate, data = tidy_data)

tidy(mult_forward_fit)
```

    ## # A tibble: 10 x 5
    ##    term                     estimate std.error statistic   p.value
    ##    <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)            116.       5.56          20.8  2.83e- 90
    ##  2 pct_bach_deg25_over     -1.58     0.100        -15.7  1.10e- 53
    ##  3 incidence_rate           0.202    0.00690       29.2  9.36e-166
    ##  4 regionMidwest            9.49     1.21           7.82 7.06e- 15
    ##  5 regionNortheast          2.18     1.75           1.25 2.12e-  1
    ##  6 regionSouth             14.9      1.17          12.7  3.30e- 36
    ##  7 pct_unemployed16_over    1.02     0.139          7.35 2.59e- 13
    ##  8 percent_married         -0.258    0.0674        -3.82 1.34e-  4
    ##  9 med_income              -0.000174 0.0000465     -3.75 1.81e-  4
    ## 10 birth_rate              -0.563    0.185         -3.04 2.42e-  3

``` r
HH::vif(mult.fit)
```

    ##        incidence_rate            med_income           pop_est2015 
    ##              1.139895              2.686550              1.187071 
    ##         study_per_cap            median_age    avg_household_size 
    ##              1.043709              1.009143              1.134738 
    ##       percent_married   pct_bach_deg25_over pct_unemployed16_over 
    ##              2.483326              2.411837              1.962519 
    ##            birth_rate         regionMidwest       regionNortheast 
    ##              1.106677              2.640044              1.617178 
    ##           regionSouth         pct_non_black 
    ##              2.848598              2.256071

``` r
summary(mult_forward_fit)
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ pct_bach_deg25_over + incidence_rate + 
    ##     region + pct_unemployed16_over + percent_married + med_income + 
    ##     birth_rate, data = tidy_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -122.864  -11.109   -0.475   10.618  133.334 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            1.158e+02  5.558e+00  20.842  < 2e-16 ***
    ## pct_bach_deg25_over   -1.577e+00  1.002e-01 -15.734  < 2e-16 ***
    ## incidence_rate         2.016e-01  6.897e-03  29.234  < 2e-16 ***
    ## regionMidwest          9.487e+00  1.213e+00   7.823 7.06e-15 ***
    ## regionNortheast        2.181e+00  1.747e+00   1.248 0.212022    
    ## regionSouth            1.488e+01  1.169e+00  12.731  < 2e-16 ***
    ## pct_unemployed16_over  1.020e+00  1.388e-01   7.347 2.59e-13 ***
    ## percent_married       -2.578e-01  6.741e-02  -3.824 0.000134 ***
    ## med_income            -1.744e-04  4.651e-05  -3.749 0.000181 ***
    ## birth_rate            -5.627e-01  1.854e-01  -3.035 0.002424 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.66 on 3037 degrees of freedom
    ## Multiple R-squared:  0.4997, Adjusted R-squared:  0.4982 
    ## F-statistic:   337 on 9 and 3037 DF,  p-value: < 2.2e-16
