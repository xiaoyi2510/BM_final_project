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
  select(-pct_asian, -pct_white, -pct_other_race)
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
fit10 = lm(target_death_rate ~ pct_private_coverage, data = tidy_data)
tidy(fit10)
```

    ## # A tibble: 2 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            243.      2.84        85.6 0.       
    ## 2 pct_private_coverage    -1.01    0.0436     -23.1 7.15e-109

``` r
fit11 = lm(target_death_rate ~ pct_public_coverage_alone, data = tidy_data)
tidy(fit11)
```

    ## # A tibble: 2 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 139.      1.48        94.0 0.       
    ## 2 pct_public_coverage_alone     2.04    0.0735      27.8 2.35e-151

``` r
fit12 = lm(target_death_rate ~ pct_black, data = tidy_data)
tidy(fit12)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  174.       0.573      304.  0.      
    ## 2 pct_black      0.491    0.0334      14.7 3.61e-47

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
fit9 = update(forward1, . ~ . +pct_private_coverage)
tidy(fit9)
```

    ## # A tibble: 3 x 5
    ##   term                 estimate std.error statistic  p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)           230.       2.75       83.9  0.      
    ## 2 pct_bach_deg25_over    -2.04     0.101     -20.2  5.34e-85
    ## 3 pct_private_coverage   -0.382    0.0513     -7.44 1.29e-13

``` r
fit10 = update(forward1, . ~ . +pct_public_coverage_alone)
tidy(fit10)
```

    ## # A tibble: 3 x 5
    ##   term                      estimate std.error statistic  p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                 180.      2.75        65.6 0.      
    ## 2 pct_bach_deg25_over          -1.73    0.0999     -17.4 2.09e-64
    ## 3 pct_public_coverage_alone     1.11    0.0881      12.6 1.05e-35

``` r
fit11 = update(forward1, . ~ . +pct_black)
tidy(fit11)
```

    ## # A tibble: 3 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          207.       1.22       170.  0.       
    ## 2 pct_bach_deg25_over   -2.35     0.0805     -29.3 5.20e-166
    ## 3 pct_black              0.363    0.0299      12.1 3.46e- 33

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
fit8 = update(forward2, . ~ . +pct_private_coverage)
tidy(fit8)
```

    ## # A tibble: 4 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.      3.63         37.3 1.26e-251
    ## 2 pct_bach_deg25_over    -1.66    0.0869      -19.1 3.19e- 77
    ## 3 incidence_rate          0.235   0.00689      34.1 1.48e-216
    ## 4 pct_private_coverage   -0.625   0.0442      -14.1 6.07e- 44

``` r
fit9 = update(forward2, . ~ . +pct_public_coverage_alone)
tidy(fit9)
```

    ## # A tibble: 4 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 83.2     3.86         21.6 3.40e- 96
    ## 2 pct_bach_deg25_over         -1.69    0.0865      -19.5 6.11e- 80
    ## 3 incidence_rate               0.217   0.00681      31.9 2.83e-193
    ## 4 pct_public_coverage_alone    1.06    0.0763       13.9 1.55e- 42

``` r
fit10 = update(forward2, . ~ . +pct_black)
tidy(fit10)
```

    ## # A tibble: 4 x 5
    ##   term                estimate std.error statistic   p.value
    ##   <chr>                  <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)          112.      3.28         34.1 2.65e-216
    ## 2 pct_bach_deg25_over   -2.31    0.0705      -32.7 9.40e-202
    ## 3 incidence_rate         0.212   0.00694      30.5 1.53e-178
    ## 4 pct_black              0.275   0.0263       10.5 3.33e- 25

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
forward3 = update(forward2, . ~ . + pct_private_coverage)
tidy(forward3)
```

    ## # A tibble: 4 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.      3.63         37.3 1.26e-251
    ## 2 pct_bach_deg25_over    -1.66    0.0869      -19.1 3.19e- 77
    ## 3 incidence_rate          0.235   0.00689      34.1 1.48e-216
    ## 4 pct_private_coverage   -0.625   0.0442      -14.1 6.07e- 44

``` r
fit1 = update(forward3, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 5 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.       3.63          37.5  1.25e-253
    ## 2 pct_bach_deg25_over    -1.50     0.0988       -15.2  1.71e- 50
    ## 3 incidence_rate          0.234    0.00689       33.9  1.79e-214
    ## 4 pct_private_coverage   -0.533    0.0521       -10.2  3.32e- 24
    ## 5 med_income             -0.000170 0.0000511     -3.33 8.76e-  4

``` r
fit2 = update(forward3, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 5 x 5
    ##   term                      estimate  std.error statistic   p.value
    ##   <chr>                        <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.         3.63           37.4  8.50e-252
    ## 2 pct_bach_deg25_over    -1.60       0.0923        -17.4  1.54e- 64
    ## 3 incidence_rate          0.236      0.00690        34.2  3.12e-217
    ## 4 pct_private_coverage   -0.640      0.0449        -14.2  1.24e- 44
    ## 5 pop_est2015            -0.00000227 0.00000120     -1.89 5.84e-  2

``` r
fit3 = update(forward3, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 5 x 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.         3.64       37.2    2.80e-250
    ## 2 pct_bach_deg25_over    -1.66       0.0871    -19.1    9.35e- 77
    ## 3 incidence_rate          0.235      0.00691    34.0    1.97e-215
    ## 4 pct_private_coverage   -0.625      0.0442    -14.1    6.64e- 44
    ## 5 study_per_cap          -0.0000389  0.000707   -0.0551 9.56e-  1

``` r
fit4 = update(forward3, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 5 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           136.        3.64       37.3   8.84e-251
    ## 2 pct_bach_deg25_over    -1.66      0.0869    -19.2   2.67e- 77
    ## 3 incidence_rate          0.235     0.00689    34.1   1.38e-216
    ## 4 pct_private_coverage   -0.624     0.0442    -14.1   7.60e- 44
    ## 5 median_age             -0.00579   0.00819    -0.708 4.79e-  1

``` r
fit5 = update(forward3, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 5 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           138.      4.61       30.0   3.34e-173
    ## 2 pct_bach_deg25_over    -1.65    0.0875    -18.9   1.89e- 75
    ## 3 incidence_rate          0.235   0.00692    33.9   3.77e-214
    ## 4 pct_private_coverage   -0.632   0.0449    -14.1   1.27e- 43
    ## 5 avg_household_size     -0.831   0.884      -0.941 3.47e-  1

``` r
fit6 = update(forward3, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 5 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           152.      4.44        34.2  5.74e-217
    ## 2 pct_bach_deg25_over    -1.81    0.0895     -20.2  1.56e- 85
    ## 3 incidence_rate          0.225   0.00703     32.0  2.00e-194
    ## 4 pct_private_coverage   -0.459   0.0513      -8.93 6.99e- 19
    ## 5 percent_married        -0.396   0.0631      -6.27 4.20e- 10

``` r
fit7 = update(forward3, . ~ . +pct_unemployed16_over)
tidy(fit7)
```

    ## # A tibble: 5 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            121.      4.32        28.1  6.43e-155
    ## 2 pct_bach_deg25_over     -1.69    0.0865     -19.5  6.91e- 80
    ## 3 incidence_rate           0.226   0.00702     32.2  1.01e-195
    ## 4 pct_private_coverage    -0.439   0.0539      -8.15 5.07e- 16
    ## 5 pct_unemployed16_over    0.843   0.142        5.95 2.96e-  9

``` r
fit8 = update(forward3, . ~ . +pct_public_coverage_alone)
tidy(fit8)
```

    ## # A tibble: 5 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                112.      7.26        15.4  2.49e- 51
    ## 2 pct_bach_deg25_over         -1.61    0.0878     -18.3  4.52e- 71
    ## 3 incidence_rate               0.228   0.00714     31.9  1.59e-192
    ## 4 pct_private_coverage        -0.369   0.0803      -4.59 4.59e-  6
    ## 5 pct_public_coverage_alone    0.529   0.138        3.82 1.36e-  4

``` r
fit9 = update(forward3, . ~ . +pct_black)
tidy(fit9)
```

    ## # A tibble: 5 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           132.      3.67        35.9  1.05e-235
    ## 2 pct_bach_deg25_over    -1.72    0.0868     -19.8  4.19e- 82
    ## 3 incidence_rate          0.228   0.00695     32.8  5.62e-202
    ## 4 pct_private_coverage   -0.525   0.0469     -11.2  1.74e- 28
    ## 5 pct_black               0.168   0.0275       6.09 1.26e-  9

``` r
fit10 = update(forward3, . ~ . +pct_non_black)
tidy(fit10)
```

    ## # A tibble: 5 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           147.      3.92        37.4  3.01e-252
    ## 2 pct_bach_deg25_over    -1.79    0.0881     -20.4  1.86e- 86
    ## 3 incidence_rate          0.228   0.00691     32.9  1.04e-203
    ## 4 pct_private_coverage   -0.456   0.0499      -9.13 1.21e- 19
    ## 5 pct_non_black          -0.194   0.0272      -7.12 1.33e- 12

``` r
fit11 = update(forward3, . ~ . +birth_rate)
tidy(fit11)
```

    ## # A tibble: 5 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           143.      3.90        36.8  1.21e-245
    ## 2 pct_bach_deg25_over    -1.71    0.0869     -19.7  4.15e- 81
    ## 3 incidence_rate          0.230   0.00691     33.3  5.09e-208
    ## 4 pct_private_coverage   -0.616   0.0440     -14.0  3.88e- 43
    ## 5 birth_rate             -1.01    0.188       -5.40 7.30e-  8

``` r
fit12 = update(forward3, . ~ . +region)
tidy(fit12)
```

    ## # A tibble: 7 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.      3.74        33.3  1.71e-207
    ## 2 pct_bach_deg25_over    -1.37    0.0893     -15.4  2.76e- 51
    ## 3 incidence_rate          0.225   0.00690     32.5  1.92e-199
    ## 4 pct_private_coverage   -0.621   0.0495     -12.5  3.45e- 35
    ## 5 regionMidwest          11.6     1.27         9.15 1.00e- 19
    ## 6 regionNortheast         5.22    1.75         2.98 2.86e-  3
    ## 7 regionSouth            16.0     1.16        13.7  1.17e- 41

``` r
## Step 4
forward4 = update(forward3, . ~ . + region)
tidy(forward4)
```

    ## # A tibble: 7 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.      3.74        33.3  1.71e-207
    ## 2 pct_bach_deg25_over    -1.37    0.0893     -15.4  2.76e- 51
    ## 3 incidence_rate          0.225   0.00690     32.5  1.92e-199
    ## 4 pct_private_coverage   -0.621   0.0495     -12.5  3.45e- 35
    ## 5 regionMidwest          11.6     1.27         9.15 1.00e- 19
    ## 6 regionNortheast         5.22    1.75         2.98 2.86e-  3
    ## 7 regionSouth            16.0     1.16        13.7  1.17e- 41

``` r
fit1 = update(forward4, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 8 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.       3.74          33.4  3.11e-208
    ## 2 pct_bach_deg25_over    -1.29     0.0982       -13.1  3.13e- 38
    ## 3 incidence_rate          0.224    0.00691       32.4  1.45e-197
    ## 4 pct_private_coverage   -0.557    0.0585        -9.52 3.28e- 21
    ## 5 regionMidwest          11.2      1.29           8.68 6.54e- 18
    ## 6 regionNortheast         5.22     1.75           2.99 2.83e-  3
    ## 7 regionSouth            15.8      1.17          13.5  1.75e- 40
    ## 8 med_income             -0.000103 0.0000506     -2.04 4.16e-  2

``` r
fit2 = update(forward4, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 8 x 5
    ##   term                      estimate  std.error statistic   p.value
    ##   <chr>                        <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.         3.74           33.3  1.09e-207
    ## 2 pct_bach_deg25_over    -1.34       0.0939        -14.2  1.35e- 44
    ## 3 incidence_rate          0.225      0.00691        32.5  1.28e-199
    ## 4 pct_private_coverage   -0.630      0.0501        -12.6  2.40e- 35
    ## 5 regionMidwest          11.6        1.27            9.13 1.26e- 19
    ## 6 regionNortheast         5.35       1.75            3.05 2.30e-  3
    ## 7 regionSouth            15.9        1.16           13.7  1.88e- 41
    ## 8 pop_est2015            -0.00000136 0.00000117     -1.17 2.43e-  1

``` r
fit3 = update(forward4, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 8 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.        3.75        33.3   3.84e-207
    ## 2 pct_bach_deg25_over    -1.37      0.0897     -15.3   5.90e- 51
    ## 3 incidence_rate          0.224     0.00693     32.4   5.94e-198
    ## 4 pct_private_coverage   -0.620     0.0495     -12.5   4.23e- 35
    ## 5 regionMidwest          11.6       1.27         9.12  1.35e- 19
    ## 6 regionNortheast         5.22      1.75         2.99  2.86e-  3
    ## 7 regionSouth            16.0       1.16        13.7   1.18e- 41
    ## 8 study_per_cap           0.000145  0.000688     0.210 8.33e-  1

``` r
fit4 = update(forward4, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 8 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           125.        3.76       33.2   1.74e-206
    ## 2 pct_bach_deg25_over    -1.37      0.0894    -15.4   2.63e- 51
    ## 3 incidence_rate          0.225     0.00690    32.5   2.05e-199
    ## 4 pct_private_coverage   -0.620     0.0495    -12.5   3.86e- 35
    ## 5 regionMidwest          11.6       1.27        9.15  1.04e- 19
    ## 6 regionNortheast         5.24      1.75        2.99  2.79e-  3
    ## 7 regionSouth            16.0       1.16       13.7   1.29e- 41
    ## 8 median_age             -0.00332   0.00793    -0.419 6.75e-  1

``` r
fit5 = update(forward4, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 8 x 5
    ##   term                 estimate std.error statistic    p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)           127.      4.61       27.5    8.74e-149
    ## 2 pct_bach_deg25_over    -1.37    0.0896    -15.2    1.62e- 50
    ## 3 incidence_rate          0.224   0.00692    32.4   10.00e-198
    ## 4 pct_private_coverage   -0.625   0.0498    -12.5    3.28e- 35
    ## 5 regionMidwest          11.6     1.27        9.06   2.19e- 19
    ## 6 regionNortheast         5.15    1.75        2.94   3.30e-  3
    ## 7 regionSouth            15.9     1.16       13.7    1.69e- 41
    ## 8 avg_household_size     -0.641   0.858      -0.747  4.55e-  1

``` r
fit6 = update(forward4, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 8 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           139.      4.50        30.9  9.21e-183
    ## 2 pct_bach_deg25_over    -1.50    0.0918     -16.4  6.19e- 58
    ## 3 incidence_rate          0.217   0.00698     31.1  7.13e-185
    ## 4 pct_private_coverage   -0.473   0.0554      -8.54 2.06e- 17
    ## 5 regionMidwest          11.3     1.26         8.97 5.10e- 19
    ## 6 regionNortheast         3.68    1.76         2.09 3.66e-  2
    ## 7 regionSouth            15.3     1.16        13.2  1.73e- 38
    ## 8 percent_married        -0.359   0.0621      -5.78 8.06e-  9

``` r
fit7 = update(forward4, . ~ . +pct_unemployed16_over)
tidy(fit7)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.      4.36        25.0  2.02e-125
    ## 2 pct_bach_deg25_over     -1.38    0.0886     -15.5  1.93e- 52
    ## 3 incidence_rate           0.215   0.00700     30.6  8.06e-180
    ## 4 pct_private_coverage    -0.427   0.0567      -7.52 7.04e- 14
    ## 5 regionMidwest           12.3     1.27         9.73 4.56e- 22
    ## 6 regionNortheast          5.06    1.74         2.92 3.57e-  3
    ## 7 regionSouth             16.2     1.15        14.0  1.93e- 43
    ## 8 pct_unemployed16_over    0.941   0.137        6.84 9.29e- 12

``` r
fit8 = update(forward4, . ~ . +pct_public_coverage_alone)
tidy(fit8)
```

    ## # A tibble: 8 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 86.2     7.59        11.4  2.72e- 29
    ## 2 pct_bach_deg25_over         -1.30    0.0897     -14.4  8.14e- 46
    ## 3 incidence_rate               0.214   0.00710     30.1  1.66e-174
    ## 4 pct_private_coverage        -0.202   0.0873      -2.31 2.10e-  2
    ## 5 regionMidwest               10.9     1.27         8.58 1.52e- 17
    ## 6 regionNortheast              3.68    1.76         2.09 3.63e-  2
    ## 7 regionSouth                 16.4     1.16        14.1  6.77e- 44
    ## 8 pct_public_coverage_alone    0.803   0.138        5.82 6.69e-  9

``` r
fit9 = update(forward4, . ~ . +pct_black)
tidy(fit9)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           124.       3.75        33.2  4.39e-206
    ## 2 pct_bach_deg25_over    -1.39     0.0902     -15.4  2.49e- 51
    ## 3 incidence_rate          0.224    0.00696     32.1  3.64e-195
    ## 4 pct_private_coverage   -0.607    0.0510     -11.9  5.97e- 32
    ## 5 regionMidwest          11.5      1.28         8.99 4.22e- 19
    ## 6 regionNortheast         5.06     1.75         2.88 3.99e-  3
    ## 7 regionSouth            15.5      1.25        12.4  1.44e- 34
    ## 8 pct_black               0.0334   0.0294       1.14 2.56e-  1

``` r
fit10 = update(forward4, . ~ . +pct_non_black)
tidy(fit10)
```

    ## # A tibble: 8 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           132.      4.18        31.5  4.14e-189
    ## 2 pct_bach_deg25_over    -1.45    0.0915     -15.9  1.72e- 54
    ## 3 incidence_rate          0.221   0.00694     31.9  1.58e-192
    ## 4 pct_private_coverage   -0.541   0.0536     -10.1  1.38e- 23
    ## 5 regionMidwest          11.3     1.27         8.90 9.00e- 19
    ## 6 regionNortheast         5.08    1.75         2.91 3.66e-  3
    ## 7 regionSouth            14.9     1.19        12.5  6.25e- 35
    ## 8 pct_non_black          -0.106   0.0276      -3.86 1.17e-  4

``` r
fit11 = update(forward4, . ~ . +birth_rate)
tidy(fit11)
```

    ## # A tibble: 8 x 5
    ##   term                 estimate std.error statistic   p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)           132.      4.03        32.6  1.36e-200
    ## 2 pct_bach_deg25_over    -1.40    0.0893     -15.7  1.58e- 53
    ## 3 incidence_rate          0.222   0.00691     32.1  4.42e-195
    ## 4 pct_private_coverage   -0.624   0.0494     -12.7  8.70e- 36
    ## 5 regionMidwest          11.7     1.27         9.21 6.02e- 20
    ## 6 regionNortheast         4.42    1.75         2.52 1.17e-  2
    ## 7 regionSouth            15.4     1.17        13.2  7.27e- 39
    ## 8 birth_rate             -0.842   0.185       -4.55 5.62e-  6

``` r
## Step 5
forward5 = update(forward4, . ~ . + pct_unemployed16_over)
tidy(forward5)
```

    ## # A tibble: 8 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.      4.36        25.0  2.02e-125
    ## 2 pct_bach_deg25_over     -1.38    0.0886     -15.5  1.93e- 52
    ## 3 incidence_rate           0.215   0.00700     30.6  8.06e-180
    ## 4 pct_private_coverage    -0.427   0.0567      -7.52 7.04e- 14
    ## 5 regionMidwest           12.3     1.27         9.73 4.56e- 22
    ## 6 regionNortheast          5.06    1.74         2.92 3.57e-  3
    ## 7 regionSouth             16.2     1.15        14.0  1.93e- 43
    ## 8 pct_unemployed16_over    0.941   0.137        6.84 9.29e- 12

``` r
fit1 = update(forward5, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 9 x 5
    ##   term                     estimate std.error statistic   p.value
    ##   <chr>                       <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.       4.36          25.0  5.00e-126
    ## 2 pct_bach_deg25_over     -1.29     0.0975       -13.3  3.70e- 39
    ## 3 incidence_rate           0.214    0.00701       30.5  4.13e-178
    ## 4 pct_private_coverage    -0.364    0.0646        -5.64 1.88e-  8
    ## 5 regionMidwest           11.9      1.28           9.25 4.06e- 20
    ## 6 regionNortheast          5.06     1.74           2.92 3.54e-  3
    ## 7 regionSouth             16.0      1.16          13.8  3.07e- 42
    ## 8 pct_unemployed16_over    0.940    0.137          6.84 9.46e- 12
    ## 9 med_income              -0.000102 0.0000502     -2.03 4.22e-  2

``` r
fit2 = update(forward5, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 9 x 5
    ##   term                       estimate  std.error statistic   p.value
    ##   <chr>                         <dbl>      <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.         4.36           24.9  6.45e-125
    ## 2 pct_bach_deg25_over     -1.32       0.0932        -14.2  1.82e- 44
    ## 3 incidence_rate           0.215      0.00701        30.7  2.23e-180
    ## 4 pct_private_coverage    -0.436      0.0569         -7.66 2.52e- 14
    ## 5 regionMidwest           12.3        1.26            9.71 5.58e- 22
    ## 6 regionNortheast          5.25       1.74            3.02 2.54e-  3
    ## 7 regionSouth             16.2        1.15           14.0  3.44e- 43
    ## 8 pct_unemployed16_over    0.964      0.138           6.98 3.49e- 12
    ## 9 pop_est2015             -0.00000211 0.00000116     -1.82 6.95e-  2

``` r
fit3 = update(forward5, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 9 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.         4.37       24.9    4.61e-125
    ## 2 pct_bach_deg25_over     -1.38       0.0891    -15.5    5.41e- 52
    ## 3 incidence_rate           0.215      0.00703    30.5    8.54e-179
    ## 4 pct_private_coverage    -0.427      0.0567     -7.52   7.18e- 14
    ## 5 regionMidwest           12.3        1.27        9.71   5.73e- 22
    ## 6 regionNortheast          5.06       1.74        2.92   3.57e-  3
    ## 7 regionSouth             16.2        1.16       14.0    2.06e- 43
    ## 8 pct_unemployed16_over    0.941      0.138       6.84   9.58e- 12
    ## 9 study_per_cap            0.0000184  0.000683    0.0270 9.78e-  1

``` r
fit4 = update(forward5, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 9 x 5
    ##   term                    estimate std.error statistic   p.value
    ##   <chr>                      <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            109.        4.37       25.0   2.74e-125
    ## 2 pct_bach_deg25_over     -1.38      0.0887    -15.5   1.70e- 52
    ## 3 incidence_rate           0.215     0.00700    30.6   8.42e-180
    ## 4 pct_private_coverage    -0.426     0.0567     -7.50  8.03e- 14
    ## 5 regionMidwest           12.3       1.27        9.73  4.77e- 22
    ## 6 regionNortheast          5.08      1.74        2.93  3.44e-  3
    ## 7 regionSouth             16.2       1.16       14.0   2.17e- 43
    ## 8 pct_unemployed16_over    0.943     0.138       6.85  8.60e- 12
    ## 9 median_age              -0.00460   0.00787    -0.585 5.59e-  1

``` r
fit5 = update(forward5, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 9 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            112.      5.04        22.2  1.03e-101
    ## 2 pct_bach_deg25_over     -1.37    0.0890     -15.4  1.98e- 51
    ## 3 incidence_rate           0.214   0.00703     30.4  2.06e-177
    ## 4 pct_private_coverage    -0.431   0.0568      -7.59 4.39e- 14
    ## 5 regionMidwest           12.2     1.27         9.62 1.35e- 21
    ## 6 regionNortheast          4.95    1.74         2.85 4.46e-  3
    ## 7 regionSouth             16.2     1.16        14.0  3.15e- 43
    ## 8 pct_unemployed16_over    0.952   0.138        6.91 5.87e- 12
    ## 9 avg_household_size      -1.04    0.854       -1.21 2.25e-  1

``` r
fit6 = update(forward5, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 9 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            122.      5.64        21.6  2.73e- 96
    ## 2 pct_bach_deg25_over     -1.46    0.0917     -16.0  4.08e- 55
    ## 3 incidence_rate           0.212   0.00703     30.1  1.77e-174
    ## 4 pct_private_coverage    -0.369   0.0589      -6.26 4.44e- 10
    ## 5 regionMidwest           12.0     1.27         9.47 5.44e- 21
    ## 6 regionNortheast          4.08    1.75         2.32 2.02e-  2
    ## 7 regionSouth             15.7     1.16        13.5  1.27e- 40
    ## 8 pct_unemployed16_over    0.751   0.147        5.11 3.46e-  7
    ## 9 percent_married         -0.237   0.0663      -3.58 3.53e-  4

``` r
fit7 = update(forward5, . ~ . +pct_public_coverage_alone)
tidy(fit7)
```

    ## # A tibble: 9 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 81.9     7.59        10.8  1.10e- 26
    ## 2 pct_bach_deg25_over         -1.32    0.0894     -14.8  1.13e- 47
    ## 3 incidence_rate               0.208   0.00715     29.1  2.21e-164
    ## 4 pct_private_coverage        -0.135   0.0876      -1.55 1.22e-  1
    ## 5 regionMidwest               11.6     1.27         9.17 8.85e- 20
    ## 6 regionNortheast              3.91    1.75         2.23 2.56e-  2
    ## 7 regionSouth                 16.5     1.15        14.3  6.73e- 45
    ## 8 pct_unemployed16_over        0.797   0.141        5.65 1.78e-  8
    ## 9 pct_public_coverage_alone    0.615   0.141        4.35 1.39e-  5

``` r
fit8 = update(forward5, . ~ . +pct_black)
tidy(fit8)
```

    ## # A tibble: 9 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            108.       4.40        24.6  1.18e-121
    ## 2 pct_bach_deg25_over     -1.36     0.0896     -15.2  3.41e- 50
    ## 3 incidence_rate           0.215    0.00701     30.7  4.02e-180
    ## 4 pct_private_coverage    -0.430    0.0568      -7.57 4.83e- 14
    ## 5 regionMidwest           12.5      1.28         9.83 1.89e- 22
    ## 6 regionNortheast          5.26     1.74         3.02 2.57e-  3
    ## 7 regionSouth             16.9      1.25        13.5  3.55e- 40
    ## 8 pct_unemployed16_over    1.01     0.147        6.88 7.30e- 12
    ## 9 pct_black               -0.0416   0.0312      -1.33 1.83e-  1

``` r
fit9 = update(forward5, . ~ . +pct_non_black)
tidy(fit9)
```

    ## # A tibble: 9 x 5
    ##   term                   estimate std.error statistic   p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            113.       5.21        21.8  9.07e- 98
    ## 2 pct_bach_deg25_over     -1.41     0.0912     -15.5  5.00e- 52
    ## 3 incidence_rate           0.214    0.00701     30.5  1.36e-178
    ## 4 pct_private_coverage    -0.409    0.0578      -7.07 1.97e- 12
    ## 5 regionMidwest           12.1      1.27         9.53 2.98e- 21
    ## 6 regionNortheast          5.01     1.74         2.89 3.90e-  3
    ## 7 regionSouth             15.7      1.19        13.2  1.59e- 38
    ## 8 pct_unemployed16_over    0.860    0.147        5.86 5.22e-  9
    ## 9 pct_non_black           -0.0462   0.0293      -1.58 1.15e-  1

``` r
fit10 = update(forward5, . ~ . +birth_rate)
tidy(fit10)
```

    ## # A tibble: 9 x 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)            116.      4.68        24.8  1.87e-123
    ## 2 pct_bach_deg25_over     -1.40    0.0887     -15.8  2.40e- 54
    ## 3 incidence_rate           0.213   0.00700     30.4  4.15e-177
    ## 4 pct_private_coverage    -0.440   0.0567      -7.76 1.16e- 14
    ## 5 regionMidwest           12.3     1.26         9.75 3.76e- 22
    ## 6 regionNortheast          4.37    1.74         2.51 1.21e-  2
    ## 7 regionSouth             15.7     1.16        13.6  9.44e- 41
    ## 8 pct_unemployed16_over    0.893   0.138        6.49 1.00e- 10
    ## 9 birth_rate              -0.738   0.185       -4.00 6.46e-  5

``` r
## Step 6

forward6 = update(forward5, . ~ . + pct_public_coverage_alone)
tidy(forward6)
```

    ## # A tibble: 9 x 5
    ##   term                      estimate std.error statistic   p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)                 81.9     7.59        10.8  1.10e- 26
    ## 2 pct_bach_deg25_over         -1.32    0.0894     -14.8  1.13e- 47
    ## 3 incidence_rate               0.208   0.00715     29.1  2.21e-164
    ## 4 pct_private_coverage        -0.135   0.0876      -1.55 1.22e-  1
    ## 5 regionMidwest               11.6     1.27         9.17 8.85e- 20
    ## 6 regionNortheast              3.91    1.75         2.23 2.56e-  2
    ## 7 regionSouth                 16.5     1.15        14.3  6.73e- 45
    ## 8 pct_unemployed16_over        0.797   0.141        5.65 1.78e-  8
    ## 9 pct_public_coverage_alone    0.615   0.141        4.35 1.39e-  5

``` r
fit1 = update(forward6, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 10 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                83.5       7.68          10.9  4.97e- 27
    ##  2 pct_bach_deg25_over        -1.27      0.0975       -13.0  1.10e- 37
    ##  3 incidence_rate              0.208     0.00715       29.1  5.36e-164
    ##  4 pct_private_coverage       -0.109     0.0898        -1.22 2.24e-  1
    ##  5 regionMidwest              11.4       1.29           8.86 1.32e- 18
    ##  6 regionNortheast             3.97      1.75           2.27 2.34e-  2
    ##  7 regionSouth                16.3       1.16          14.1  7.84e- 44
    ##  8 pct_unemployed16_over       0.804     0.141          5.69 1.36e-  8
    ##  9 pct_public_coverage_alone   0.583     0.143          4.07 4.87e-  5
    ## 10 med_income                 -0.0000673 0.0000508     -1.33 1.85e-  1

``` r
fit2 = update(forward6, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 10 x 5
    ##    term                          estimate  std.error statistic   p.value
    ##    <chr>                            <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                81.3        7.59           10.7  2.76e- 26
    ##  2 pct_bach_deg25_over        -1.26       0.0940        -13.4  5.40e- 40
    ##  3 incidence_rate              0.208      0.00715        29.1  6.36e-165
    ##  4 pct_private_coverage       -0.141      0.0876         -1.61 1.07e-  1
    ##  5 regionMidwest              11.6        1.27            9.14 1.16e- 19
    ##  6 regionNortheast             4.10       1.75            2.34 1.94e-  2
    ##  7 regionSouth                16.4        1.15           14.3  1.18e- 44
    ##  8 pct_unemployed16_over       0.819      0.141           5.79 7.65e-  9
    ##  9 pct_public_coverage_alone   0.624      0.141           4.41 1.05e-  5
    ## 10 pop_est2015                -0.00000227 0.00000116     -1.96 5.03e-  2

``` r
fit3 = update(forward6, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 10 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                81.9        7.60        10.8   1.41e- 26
    ##  2 pct_bach_deg25_over        -1.32       0.0898     -14.7   3.66e- 47
    ##  3 incidence_rate              0.208      0.00717     29.0   1.07e-163
    ##  4 pct_private_coverage       -0.135      0.0876      -1.54  1.23e-  1
    ##  5 regionMidwest              11.7        1.27         9.15  9.89e- 20
    ##  6 regionNortheast             3.91       1.75         2.23  2.57e-  2
    ##  7 regionSouth                16.5        1.15        14.3   7.39e- 45
    ##  8 pct_unemployed16_over       0.797      0.141        5.65  1.78e-  8
    ##  9 pct_public_coverage_alone   0.615      0.141        4.35  1.39e-  5
    ## 10 study_per_cap              -0.0000702  0.000682    -0.103 9.18e-  1

``` r
fit4 = update(forward6, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 10 x 5
    ##    term                       estimate std.error statistic   p.value
    ##    <chr>                         <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                82.1       7.60       10.8   9.90e- 27
    ##  2 pct_bach_deg25_over        -1.32      0.0894    -14.8   1.03e- 47
    ##  3 incidence_rate              0.208     0.00715    29.1   2.26e-164
    ##  4 pct_private_coverage       -0.135     0.0876     -1.55  1.22e-  1
    ##  5 regionMidwest              11.6       1.27        9.16  9.08e- 20
    ##  6 regionNortheast             3.93      1.75        2.24  2.49e-  2
    ##  7 regionSouth                16.5       1.15       14.3   7.57e- 45
    ##  8 pct_unemployed16_over       0.799     0.141       5.66  1.67e-  8
    ##  9 pct_public_coverage_alone   0.613     0.141       4.34  1.47e-  5
    ## 10 median_age                 -0.00390   0.00785    -0.497 6.19e-  1

``` r
fit5 = update(forward6, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                 84.3     8.24       10.2   3.39e- 24
    ##  2 pct_bach_deg25_over         -1.31    0.0896    -14.7   3.86e- 47
    ##  3 incidence_rate               0.208   0.00716    29.0   3.19e-163
    ##  4 pct_private_coverage        -0.144   0.0883     -1.63  1.04e-  1
    ##  5 regionMidwest               11.6     1.27        9.10  1.54e- 19
    ##  6 regionNortheast              3.86    1.75        2.20  2.77e-  2
    ##  7 regionSouth                 16.5     1.15       14.3   1.05e- 44
    ##  8 pct_unemployed16_over        0.806   0.142       5.69  1.38e-  8
    ##  9 pct_public_coverage_alone    0.603   0.142       4.24  2.26e-  5
    ## 10 avg_household_size          -0.641   0.857      -0.748 4.54e-  1

``` r
fit6 = update(forward6, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                95.2      8.59        11.1  5.43e- 28
    ##  2 pct_bach_deg25_over        -1.40     0.0928     -15.1  7.82e- 50
    ##  3 incidence_rate              0.206    0.00717     28.7  1.40e-160
    ##  4 pct_private_coverage       -0.0985   0.0882      -1.12 2.64e-  1
    ##  5 regionMidwest              11.4      1.27         8.96 5.68e- 19
    ##  6 regionNortheast             3.07     1.77         1.74 8.21e-  2
    ##  7 regionSouth                16.0      1.16        13.8  4.29e- 42
    ##  8 pct_unemployed16_over       0.631    0.150        4.22 2.54e-  5
    ##  9 pct_public_coverage_alone   0.581    0.141        4.10 4.15e-  5
    ## 10 percent_married            -0.217    0.0663      -3.27 1.08e-  3

``` r
fit7 = update(forward6, . ~ . +pct_black)
tidy(fit7)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                80.7      7.63        10.6  1.03e- 25
    ##  2 pct_bach_deg25_over        -1.30     0.0904     -14.4  2.19e- 45
    ##  3 incidence_rate              0.208    0.00715     29.1  8.37e-165
    ##  4 pct_private_coverage       -0.136    0.0876      -1.55 1.22e-  1
    ##  5 regionMidwest              11.9      1.28         9.28 3.07e- 20
    ##  6 regionNortheast             4.11     1.76         2.34 1.93e-  2
    ##  7 regionSouth                17.2      1.25        13.7  9.05e- 42
    ##  8 pct_unemployed16_over       0.871    0.150        5.82 6.58e-  9
    ##  9 pct_public_coverage_alone   0.622    0.141        4.40 1.13e-  5
    ## 10 pct_black                  -0.0460   0.0311      -1.48 1.39e-  1

``` r
fit8 = update(forward6, . ~ . +pct_non_black)
tidy(fit8)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                86.5      8.02        10.8  1.26e- 26
    ##  2 pct_bach_deg25_over        -1.36     0.0918     -14.8  9.43e- 48
    ##  3 incidence_rate              0.207    0.00716     28.9  6.60e-163
    ##  4 pct_private_coverage       -0.111    0.0887      -1.25 2.11e-  1
    ##  5 regionMidwest              11.4      1.28         8.94 6.48e- 19
    ##  6 regionNortheast             3.84     1.75         2.19 2.85e-  2
    ##  7 regionSouth                15.9      1.19        13.4  1.12e- 39
    ##  8 pct_unemployed16_over       0.704    0.151        4.68 3.01e-  6
    ##  9 pct_public_coverage_alone   0.625    0.141        4.42 1.03e-  5
    ## 10 pct_non_black              -0.0512   0.0292      -1.75 7.99e-  2

``` r
fit9 = update(forward6, . ~ . +birth_rate)
tidy(fit9)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                 90.0     7.88        11.4  1.34e- 29
    ##  2 pct_bach_deg25_over         -1.35    0.0895     -15.1  1.76e- 49
    ##  3 incidence_rate               0.207   0.00714     28.9  9.46e-163
    ##  4 pct_private_coverage        -0.166   0.0878      -1.89 5.91e-  2
    ##  5 regionMidwest               11.7     1.27         9.21 5.72e- 20
    ##  6 regionNortheast              3.34    1.75         1.90 5.69e-  2
    ##  7 regionSouth                 16.0     1.16        13.8  3.40e- 42
    ##  8 pct_unemployed16_over        0.762   0.141        5.40 7.22e-  8
    ##  9 pct_public_coverage_alone    0.576   0.141        4.08 4.71e-  5
    ## 10 birth_rate                  -0.683   0.185       -3.70 2.21e-  4

``` r
## Step 7

forward7 = update(forward6, . ~ . + birth_rate)
tidy(forward7)
```

    ## # A tibble: 10 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                 90.0     7.88        11.4  1.34e- 29
    ##  2 pct_bach_deg25_over         -1.35    0.0895     -15.1  1.76e- 49
    ##  3 incidence_rate               0.207   0.00714     28.9  9.46e-163
    ##  4 pct_private_coverage        -0.166   0.0878      -1.89 5.91e-  2
    ##  5 regionMidwest               11.7     1.27         9.21 5.72e- 20
    ##  6 regionNortheast              3.34    1.75         1.90 5.69e-  2
    ##  7 regionSouth                 16.0     1.16        13.8  3.40e- 42
    ##  8 pct_unemployed16_over        0.762   0.141        5.40 7.22e-  8
    ##  9 pct_public_coverage_alone    0.576   0.141        4.08 4.71e-  5
    ## 10 birth_rate                  -0.683   0.185       -3.70 2.21e-  4

``` r
fit1 = update(forward7, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 11 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                91.0       7.95          11.5  8.86e- 30
    ##  2 pct_bach_deg25_over        -1.31      0.0979       -13.4  1.40e- 39
    ##  3 incidence_rate              0.206     0.00714       28.9  1.79e-162
    ##  4 pct_private_coverage       -0.145     0.0902        -1.60 1.09e-  1
    ##  5 regionMidwest              11.5       1.28           8.95 6.13e- 19
    ##  6 regionNortheast             3.40      1.76           1.94 5.27e-  2
    ##  7 regionSouth                15.9       1.16          13.7  1.99e- 41
    ##  8 pct_unemployed16_over       0.768     0.141          5.44 5.81e-  8
    ##  9 pct_public_coverage_alone   0.552     0.143          3.85 1.19e-  4
    ## 10 birth_rate                 -0.667     0.185         -3.60 3.18e-  4
    ## 11 med_income                 -0.0000527 0.0000508     -1.04 3.00e-  1

``` r
fit2 = update(forward7, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 11 x 5
    ##    term                          estimate  std.error statistic   p.value
    ##    <chr>                            <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                89.4        7.88           11.3  3.26e- 29
    ##  2 pct_bach_deg25_over        -1.29       0.0941        -13.7  1.37e- 41
    ##  3 incidence_rate              0.207      0.00714        29.0  2.68e-163
    ##  4 pct_private_coverage       -0.172      0.0878         -1.96 5.04e-  2
    ##  5 regionMidwest              11.6        1.27            9.18 7.52e- 20
    ##  6 regionNortheast             3.53       1.76            2.01 4.45e-  2
    ##  7 regionSouth                16.0        1.16           13.8  6.04e- 42
    ##  8 pct_unemployed16_over       0.785      0.141           5.55 3.17e-  8
    ##  9 pct_public_coverage_alone   0.585      0.141           4.14 3.62e-  5
    ## 10 birth_rate                 -0.685      0.184          -3.71 2.07e-  4
    ## 11 pop_est2015                -0.00000230 0.00000116     -1.99 4.67e-  2

``` r
fit3 = update(forward7, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 11 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                90.0        7.89       11.4    1.74e- 29
    ##  2 pct_bach_deg25_over        -1.35       0.0900    -15.0    5.57e- 49
    ##  3 incidence_rate              0.207      0.00716    28.9    5.22e-162
    ##  4 pct_private_coverage       -0.166      0.0878     -1.89   5.92e-  2
    ##  5 regionMidwest              11.7        1.27        9.20   6.64e- 20
    ##  6 regionNortheast             3.34       1.75        1.90   5.70e-  2
    ##  7 regionSouth                16.0        1.16       13.8    3.62e- 42
    ##  8 pct_unemployed16_over       0.762      0.141       5.40   7.28e-  8
    ##  9 pct_public_coverage_alone   0.576      0.141       4.07   4.74e-  5
    ## 10 birth_rate                 -0.682      0.185      -3.70   2.22e-  4
    ## 11 study_per_cap              -0.0000238  0.000680   -0.0349 9.72e-  1

``` r
fit4 = update(forward7, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 11 x 5
    ##    term                       estimate std.error statistic   p.value
    ##    <chr>                         <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                90.2       7.89       11.4   1.20e- 29
    ##  2 pct_bach_deg25_over        -1.35      0.0896    -15.1   1.59e- 49
    ##  3 incidence_rate              0.207     0.00714    28.9   9.64e-163
    ##  4 pct_private_coverage       -0.166     0.0878     -1.89  5.91e-  2
    ##  5 regionMidwest              11.7       1.27        9.21  5.87e- 20
    ##  6 regionNortheast             3.36      1.75        1.92  5.54e-  2
    ##  7 regionSouth                16.0       1.16       13.8   3.84e- 42
    ##  8 pct_unemployed16_over       0.764     0.141       5.41  6.77e-  8
    ##  9 pct_public_coverage_alone   0.575     0.141       4.06  4.97e-  5
    ## 10 birth_rate                 -0.683     0.185      -3.70  2.18e-  4
    ## 11 median_age                 -0.00409   0.00783    -0.522 6.01e-  1

``` r
fit5 = update(forward7, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 11 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                 91.4     8.45       10.8   8.42e- 27
    ##  2 pct_bach_deg25_over         -1.35    0.0898    -15.0   5.55e- 49
    ##  3 incidence_rate               0.206   0.00716    28.8   7.50e-162
    ##  4 pct_private_coverage        -0.171   0.0884     -1.93  5.37e-  2
    ##  5 regionMidwest               11.6     1.27        9.17  8.72e- 20
    ##  6 regionNortheast              3.32    1.76        1.89  5.90e-  2
    ##  7 regionSouth                 16.0     1.16       13.8   4.26e- 42
    ##  8 pct_unemployed16_over        0.768   0.142       5.42  6.49e-  8
    ##  9 pct_public_coverage_alone    0.569   0.142       4.00  6.36e-  5
    ## 10 birth_rate                  -0.676   0.185      -3.65  2.65e-  4
    ## 11 avg_household_size          -0.403   0.857      -0.470 6.38e-  1

``` r
fit6 = update(forward7, . ~ . +percent_married)
tidy(fit6)
```

    ## # A tibble: 11 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                101.      8.77        11.6  2.55e- 30
    ##  2 pct_bach_deg25_over         -1.42    0.0928     -15.3  4.15e- 51
    ##  3 incidence_rate               0.205   0.00716     28.6  2.18e-159
    ##  4 pct_private_coverage        -0.130   0.0885      -1.47 1.42e-  1
    ##  5 regionMidwest               11.4     1.27         9.02 3.30e- 19
    ##  6 regionNortheast              2.62    1.77         1.48 1.38e-  1
    ##  7 regionSouth                 15.6     1.16        13.4  6.32e- 40
    ##  8 pct_unemployed16_over        0.614   0.149        4.11 4.08e-  5
    ##  9 pct_public_coverage_alone    0.548   0.142        3.87 1.11e-  4
    ## 10 birth_rate                  -0.634   0.185       -3.43 6.18e-  4
    ## 11 percent_married             -0.197   0.0664      -2.96 3.07e-  3

``` r
fit7 = update(forward7, . ~ . +pct_black)
tidy(fit7)
```

    ## # A tibble: 11 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                88.8      7.93        11.2  1.46e- 28
    ##  2 pct_bach_deg25_over        -1.33     0.0906     -14.7  3.56e- 47
    ##  3 incidence_rate              0.207    0.00715     29.0  4.12e-163
    ##  4 pct_private_coverage       -0.166    0.0878      -1.89 5.91e-  2
    ##  5 regionMidwest              11.9      1.28         9.32 2.24e- 20
    ##  6 regionNortheast             3.53     1.76         2.01 4.46e-  2
    ##  7 regionSouth                16.7      1.26        13.3  3.90e- 39
    ##  8 pct_unemployed16_over       0.831    0.150        5.55 3.10e-  8
    ##  9 pct_public_coverage_alone   0.583    0.141        4.12 3.87e-  5
    ## 10 birth_rate                 -0.676    0.185       -3.66 2.57e-  4
    ## 11 pct_black                  -0.0429   0.0310      -1.38 1.67e-  1

``` r
fit8 = update(forward7, . ~ . +pct_non_black)
tidy(fit8)
```

    ## # A tibble: 11 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                95.9      8.36        11.5  7.84e- 30
    ##  2 pct_bach_deg25_over        -1.39     0.0921     -15.1  5.89e- 50
    ##  3 incidence_rate              0.206    0.00715     28.7  5.95e-161
    ##  4 pct_private_coverage       -0.138    0.0888      -1.55 1.20e-  1
    ##  5 regionMidwest              11.4      1.27         8.96 5.67e- 19
    ##  6 regionNortheast             3.22     1.75         1.84 6.61e-  2
    ##  7 regionSouth                15.3      1.20        12.8  1.77e- 36
    ##  8 pct_unemployed16_over       0.649    0.151        4.30 1.76e-  5
    ##  9 pct_public_coverage_alone   0.586    0.141        4.14 3.52e-  5
    ## 10 birth_rate                 -0.718    0.185       -3.88 1.08e-  4
    ## 11 pct_non_black              -0.0616   0.0293      -2.10 3.55e-  2

``` r
## Step 8

forward8 = update(forward7, . ~ . + percent_married)
tidy(forward8)
```

    ## # A tibble: 11 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                101.      8.77        11.6  2.55e- 30
    ##  2 pct_bach_deg25_over         -1.42    0.0928     -15.3  4.15e- 51
    ##  3 incidence_rate               0.205   0.00716     28.6  2.18e-159
    ##  4 pct_private_coverage        -0.130   0.0885      -1.47 1.42e-  1
    ##  5 regionMidwest               11.4     1.27         9.02 3.30e- 19
    ##  6 regionNortheast              2.62    1.77         1.48 1.38e-  1
    ##  7 regionSouth                 15.6     1.16        13.4  6.32e- 40
    ##  8 pct_unemployed16_over        0.614   0.149        4.11 4.08e-  5
    ##  9 pct_public_coverage_alone    0.548   0.142        3.87 1.11e-  4
    ## 10 birth_rate                  -0.634   0.185       -3.43 6.18e-  4
    ## 11 percent_married             -0.197   0.0664      -2.96 3.07e-  3

``` r
fit1 = update(forward8, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 12 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                102.        8.77         11.6   2.36e- 30
    ##  2 pct_bach_deg25_over         -1.40      0.103       -13.6   9.96e- 41
    ##  3 incidence_rate               0.205     0.00716      28.6   2.48e-159
    ##  4 pct_private_coverage        -0.122     0.0904       -1.35  1.78e-  1
    ##  5 regionMidwest               11.4       1.28          8.86  1.32e- 18
    ##  6 regionNortheast              2.67      1.77          1.51  1.32e-  1
    ##  7 regionSouth                 15.6       1.17         13.4   1.28e- 39
    ##  8 pct_unemployed16_over        0.621     0.150         4.13  3.68e-  5
    ##  9 pct_public_coverage_alone    0.538     0.143         3.76  1.75e-  4
    ## 10 birth_rate                  -0.629     0.185        -3.39  7.04e-  4
    ## 11 percent_married             -0.191     0.0678       -2.81  4.97e-  3
    ## 12 med_income                  -0.0000233 0.0000518    -0.449 6.53e-  1

``` r
fit2 = update(forward8, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 12 x 5
    ##    term                           estimate  std.error statistic   p.value
    ##    <chr>                             <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                102.         8.76           11.6  2.03e- 30
    ##  2 pct_bach_deg25_over         -1.36       0.0965        -14.1  9.44e- 44
    ##  3 incidence_rate               0.205      0.00716        28.6  6.27e-160
    ##  4 pct_private_coverage        -0.135      0.0885         -1.52 1.28e-  1
    ##  5 regionMidwest               11.4        1.27            8.97 5.04e- 19
    ##  6 regionNortheast              2.79       1.77            1.58 1.15e-  1
    ##  7 regionSouth                 15.5        1.16           13.3  1.65e- 39
    ##  8 pct_unemployed16_over        0.630      0.150           4.21 2.59e-  5
    ##  9 pct_public_coverage_alone    0.556      0.141           3.93 8.75e-  5
    ## 10 birth_rate                  -0.634      0.185          -3.43 6.18e-  4
    ## 11 percent_married             -0.211      0.0667         -3.16 1.59e-  3
    ## 12 pop_est2015                 -0.00000264 0.00000116     -2.27 2.31e-  2

``` r
fit3 = update(forward8, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 12 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                101.        8.77        11.6   2.86e- 30
    ##  2 pct_bach_deg25_over         -1.42      0.0931     -15.3   1.12e- 50
    ##  3 incidence_rate               0.205     0.00718     28.5   6.66e-159
    ##  4 pct_private_coverage        -0.130     0.0886      -1.46  1.43e-  1
    ##  5 regionMidwest               11.5       1.27         9.02  3.38e- 19
    ##  6 regionNortheast              2.62      1.77         1.48  1.39e-  1
    ##  7 regionSouth                 15.6       1.16        13.4   7.34e- 40
    ##  8 pct_unemployed16_over        0.614     0.149        4.11  4.11e-  5
    ##  9 pct_public_coverage_alone    0.549     0.142        3.88  1.09e-  4
    ## 10 birth_rate                  -0.633     0.185       -3.42  6.35e-  4
    ## 11 percent_married             -0.198     0.0666      -2.97  2.99e-  3
    ## 12 study_per_cap               -0.000159  0.000681    -0.234 8.15e-  1

``` r
fit4 = update(forward8, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 12 x 5
    ##    term                        estimate std.error statistic   p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                101.        8.77       11.6   2.48e- 30
    ##  2 pct_bach_deg25_over         -1.42      0.0928    -15.3   4.09e- 51
    ##  3 incidence_rate               0.205     0.00716    28.6   2.32e-159
    ##  4 pct_private_coverage        -0.130     0.0886     -1.47  1.41e-  1
    ##  5 regionMidwest               11.4       1.27        9.02  3.33e- 19
    ##  6 regionNortheast              2.64      1.77        1.49  1.36e-  1
    ##  7 regionSouth                 15.6       1.16       13.4   6.64e- 40
    ##  8 pct_unemployed16_over        0.616     0.150       4.12  3.90e-  5
    ##  9 pct_public_coverage_alone    0.547     0.142       3.86  1.14e-  4
    ## 10 birth_rate                  -0.635     0.185      -3.43  6.11e-  4
    ## 11 percent_married             -0.195     0.0666     -2.93  3.36e-  3
    ## 12 median_age                  -0.00259   0.00784    -0.331 7.41e-  1

``` r
fit5 = update(forward8, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 12 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                103.      9.31       11.1   5.67e- 28
    ##  2 pct_bach_deg25_over         -1.42    0.0930    -15.3   1.17e- 50
    ##  3 incidence_rate               0.204   0.00718    28.5   2.08e-158
    ##  4 pct_private_coverage        -0.136   0.0891     -1.52  1.28e-  1
    ##  5 regionMidwest               11.4     1.27        8.97  5.27e- 19
    ##  6 regionNortheast              2.59    1.77        1.46  1.44e-  1
    ##  7 regionSouth                 15.6     1.16       13.4   8.30e- 40
    ##  8 pct_unemployed16_over        0.621   0.150       4.14  3.58e-  5
    ##  9 pct_public_coverage_alone    0.540   0.142       3.79  1.53e-  4
    ## 10 birth_rate                  -0.626   0.186      -3.37  7.52e-  4
    ## 11 percent_married             -0.198   0.0665     -2.98  2.94e-  3
    ## 12 avg_household_size          -0.471   0.856      -0.550 5.83e-  1

``` r
fit6 = update(forward8, . ~ . +pct_black)
tidy(fit6)
```

    ## # A tibble: 12 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                106.      8.84        11.9  3.95e- 32
    ##  2 pct_bach_deg25_over         -1.42    0.0926     -15.3  6.72e- 51
    ##  3 incidence_rate               0.205   0.00715     28.7  5.04e-160
    ##  4 pct_private_coverage        -0.107   0.0887      -1.21 2.28e-  1
    ##  5 regionMidwest               11.9     1.28         9.36 1.57e- 20
    ##  6 regionNortheast              2.69    1.77         1.53 1.27e-  1
    ##  7 regionSouth                 17.2     1.26        13.7  2.27e- 41
    ##  8 pct_unemployed16_over        0.711   0.152        4.67 3.07e-  6
    ##  9 pct_public_coverage_alone    0.548   0.141        3.88 1.07e-  4
    ## 10 birth_rate                  -0.583   0.185       -3.15 1.66e-  3
    ## 11 percent_married             -0.324   0.0766      -4.23 2.42e-  5
    ## 12 pct_black                   -0.119   0.0358      -3.32 9.26e-  4

``` r
fit7 = update(forward8, . ~ . +pct_non_black)
tidy(fit7)
```

    ## # A tibble: 12 x 5
    ##    term                       estimate std.error statistic   p.value
    ##    <chr>                         <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                102.       8.82       11.6   2.43e- 30
    ##  2 pct_bach_deg25_over         -1.43     0.0934    -15.3   5.87e- 51
    ##  3 incidence_rate               0.205    0.00716    28.6   3.87e-159
    ##  4 pct_private_coverage        -0.125    0.0889     -1.40  1.61e-  1
    ##  5 regionMidwest               11.4      1.27        8.93  6.98e- 19
    ##  6 regionNortheast              2.68     1.77        1.51  1.31e-  1
    ##  7 regionSouth                 15.4      1.20       12.9   7.53e- 37
    ##  8 pct_unemployed16_over        0.592    0.153       3.87  1.11e-  4
    ##  9 pct_public_coverage_alone    0.555    0.142       3.91  9.36e-  5
    ## 10 birth_rate                  -0.654    0.187      -3.49  4.89e-  4
    ## 11 percent_married             -0.170    0.0775     -2.19  2.84e-  2
    ## 12 pct_non_black               -0.0231   0.0341     -0.677 4.98e-  1

``` r
## Step 9

forward9 = update(forward8, . ~ . + pct_black)
tidy(forward9)
```

    ## # A tibble: 12 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                106.      8.84        11.9  3.95e- 32
    ##  2 pct_bach_deg25_over         -1.42    0.0926     -15.3  6.72e- 51
    ##  3 incidence_rate               0.205   0.00715     28.7  5.04e-160
    ##  4 pct_private_coverage        -0.107   0.0887      -1.21 2.28e-  1
    ##  5 regionMidwest               11.9     1.28         9.36 1.57e- 20
    ##  6 regionNortheast              2.69    1.77         1.53 1.27e-  1
    ##  7 regionSouth                 17.2     1.26        13.7  2.27e- 41
    ##  8 pct_unemployed16_over        0.711   0.152        4.67 3.07e-  6
    ##  9 pct_public_coverage_alone    0.548   0.141        3.88 1.07e-  4
    ## 10 birth_rate                  -0.583   0.185       -3.15 1.66e-  3
    ## 11 percent_married             -0.324   0.0766      -4.23 2.42e-  5
    ## 12 pct_black                   -0.119   0.0358      -3.32 9.26e-  4

``` r
fit1 = update(forward9, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 13 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                106.        8.84         11.9   3.75e- 32
    ##  2 pct_bach_deg25_over         -1.40      0.103       -13.6   1.02e- 40
    ##  3 incidence_rate               0.205     0.00715      28.6   5.76e-160
    ##  4 pct_private_coverage        -0.0999    0.0905       -1.10  2.70e-  1
    ##  5 regionMidwest               11.9       1.29          9.20  6.39e- 20
    ##  6 regionNortheast              2.74      1.77          1.55  1.22e-  1
    ##  7 regionSouth                 17.2       1.26         13.6   4.65e- 41
    ##  8 pct_unemployed16_over        0.717     0.153         4.69  2.86e-  6
    ##  9 pct_public_coverage_alone    0.540     0.143         3.78  1.63e-  4
    ## 10 birth_rate                  -0.579     0.186        -3.12  1.84e-  3
    ## 11 percent_married             -0.318     0.0780       -4.08  4.54e-  5
    ## 12 pct_black                   -0.118     0.0358       -3.31  9.53e-  4
    ## 13 med_income                  -0.0000201 0.0000518    -0.388 6.98e-  1

``` r
fit2 = update(forward9, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 13 x 5
    ##    term                           estimate  std.error statistic   p.value
    ##    <chr>                             <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                106.         8.83           12.0  3.30e- 32
    ##  2 pct_bach_deg25_over         -1.36       0.0964        -14.1  1.24e- 43
    ##  3 incidence_rate               0.205      0.00715        28.7  1.51e-160
    ##  4 pct_private_coverage        -0.112      0.0886         -1.26 2.08e-  1
    ##  5 regionMidwest               11.9        1.27            9.31 2.49e- 20
    ##  6 regionNortheast              2.86       1.77            1.62 1.06e-  1
    ##  7 regionSouth                 17.1        1.26           13.6  6.48e- 41
    ##  8 pct_unemployed16_over        0.725      0.152           4.77 1.92e-  6
    ##  9 pct_public_coverage_alone    0.556      0.141           3.94 8.48e-  5
    ## 10 birth_rate                  -0.583      0.185          -3.15 1.65e-  3
    ## 11 percent_married             -0.336      0.0768         -4.38 1.21e-  5
    ## 12 pct_black                   -0.118      0.0357         -3.29 1.02e-  3
    ## 13 pop_est2015                 -0.00000259 0.00000116     -2.23 2.56e-  2

``` r
fit3 = update(forward9, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 13 x 5
    ##    term                         estimate std.error statistic   p.value
    ##    <chr>                           <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                105.        8.84        11.9   4.44e- 32
    ##  2 pct_bach_deg25_over         -1.41      0.0929     -15.2   1.95e- 50
    ##  3 incidence_rate               0.205     0.00716     28.6   1.41e-159
    ##  4 pct_private_coverage        -0.106     0.0887      -1.20  2.31e-  1
    ##  5 regionMidwest               12.0       1.28         9.36  1.57e- 20
    ##  6 regionNortheast              2.68      1.77         1.52  1.29e-  1
    ##  7 regionSouth                 17.2       1.26        13.7   2.52e- 41
    ##  8 pct_unemployed16_over        0.711     0.152        4.67  3.08e-  6
    ##  9 pct_public_coverage_alone    0.549     0.141        3.89  1.04e-  4
    ## 10 birth_rate                  -0.582     0.185       -3.14  1.72e-  3
    ## 11 percent_married             -0.326     0.0768      -4.24  2.31e-  5
    ## 12 pct_black                   -0.119     0.0358      -3.32  9.10e-  4
    ## 13 study_per_cap               -0.000203  0.000680    -0.299 7.65e-  1

``` r
fit4 = update(forward9, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 13 x 5
    ##    term                        estimate std.error statistic   p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                106.        8.84       11.9   3.90e- 32
    ##  2 pct_bach_deg25_over         -1.42      0.0926    -15.3   6.70e- 51
    ##  3 incidence_rate               0.205     0.00715    28.7   5.48e-160
    ##  4 pct_private_coverage        -0.107     0.0887     -1.21  2.27e-  1
    ##  5 regionMidwest               11.9       1.28        9.35  1.59e- 20
    ##  6 regionNortheast              2.71      1.77        1.53  1.25e-  1
    ##  7 regionSouth                 17.2       1.26       13.7   2.44e- 41
    ##  8 pct_unemployed16_over        0.713     0.152       4.68  2.96e-  6
    ##  9 pct_public_coverage_alone    0.548     0.141       3.87  1.09e-  4
    ## 10 birth_rate                  -0.584     0.185      -3.15  1.65e-  3
    ## 11 percent_married             -0.323     0.0768     -4.20  2.73e-  5
    ## 12 pct_black                   -0.118     0.0358     -3.31  9.41e-  4
    ## 13 median_age                  -0.00224   0.00783    -0.286 7.75e-  1

``` r
fit5 = update(forward9, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 13 x 5
    ##    term                      estimate std.error statistic   p.value
    ##    <chr>                        <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                108.      9.42       11.5   6.40e- 30
    ##  2 pct_bach_deg25_over         -1.41    0.0928    -15.2   2.48e- 50
    ##  3 incidence_rate               0.204   0.00716    28.5   6.77e-159
    ##  4 pct_private_coverage        -0.115   0.0892     -1.29  1.99e-  1
    ##  5 regionMidwest               11.9     1.28        9.30  2.64e- 20
    ##  6 regionNortheast              2.65    1.77        1.50  1.34e-  1
    ##  7 regionSouth                 17.2     1.26       13.7   2.17e- 41
    ##  8 pct_unemployed16_over        0.723   0.153       4.73  2.34e-  6
    ##  9 pct_public_coverage_alone    0.536   0.142       3.77  1.65e-  4
    ## 10 birth_rate                  -0.571   0.186      -3.07  2.18e-  3
    ## 11 percent_married             -0.328   0.0768     -4.27  2.01e-  5
    ## 12 pct_black                   -0.121   0.0359     -3.37  7.64e-  4
    ## 13 avg_household_size          -0.698   0.858      -0.813 4.16e-  1

``` r
fit8 = update(forward9, . ~ . +pct_non_black)
tidy(fit8)
```

    ## # A tibble: 13 x 5
    ##    term                        estimate std.error statistic    p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 (Intercept)                121.        9.30      13.0     1.00e- 37
    ##  2 pct_bach_deg25_over         -1.49      0.0934   -16.0     2.95e- 55
    ##  3 incidence_rate               0.204     0.00712   28.6     1.21e-159
    ##  4 pct_private_coverage         0.00423   0.0909     0.0465  9.63e-  1
    ##  5 regionMidwest               12.0       1.27       9.47    5.46e- 21
    ##  6 regionNortheast              3.49      1.77       1.97    4.84e-  2
    ##  7 regionSouth                 17.9       1.26      14.2     1.89e- 44
    ##  8 pct_unemployed16_over        0.629     0.152      4.13    3.65e-  5
    ##  9 pct_public_coverage_alone    0.639     0.142      4.50    6.91e-  6
    ## 10 birth_rate                  -0.727     0.187     -3.90   10.00e-  5
    ## 11 percent_married             -0.242     0.0779    -3.11    1.90e-  3
    ## 12 pct_black                   -0.345     0.0564    -6.11    1.11e-  9
    ## 13 pct_non_black               -0.278     0.0537    -5.17    2.49e-  7

``` r
## Step 10

forward10 = update(forward9, . ~ . + pct_non_black)
tidy(forward10)
```

    ## # A tibble: 13 x 5
    ##    term                        estimate std.error statistic    p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 (Intercept)                121.        9.30      13.0     1.00e- 37
    ##  2 pct_bach_deg25_over         -1.49      0.0934   -16.0     2.95e- 55
    ##  3 incidence_rate               0.204     0.00712   28.6     1.21e-159
    ##  4 pct_private_coverage         0.00423   0.0909     0.0465  9.63e-  1
    ##  5 regionMidwest               12.0       1.27       9.47    5.46e- 21
    ##  6 regionNortheast              3.49      1.77       1.97    4.84e-  2
    ##  7 regionSouth                 17.9       1.26      14.2     1.89e- 44
    ##  8 pct_unemployed16_over        0.629     0.152      4.13    3.65e-  5
    ##  9 pct_public_coverage_alone    0.639     0.142      4.50    6.91e-  6
    ## 10 birth_rate                  -0.727     0.187     -3.90   10.00e-  5
    ## 11 percent_married             -0.242     0.0779    -3.11    1.90e-  3
    ## 12 pct_black                   -0.345     0.0564    -6.11    1.11e-  9
    ## 13 pct_non_black               -0.278     0.0537    -5.17    2.49e-  7

``` r
fit1 = update(forward10, . ~ . +med_income)
tidy(fit1)
```

    ## # A tibble: 14 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                122.        9.33         13.1   4.62e- 38
    ##  2 pct_bach_deg25_over         -1.44      0.103       -14.0   5.30e- 43
    ##  3 incidence_rate               0.204     0.00712      28.6   1.48e-159
    ##  4 pct_private_coverage         0.0326    0.0935        0.349 7.27e-  1
    ##  5 regionMidwest               11.8       1.28          9.19  7.14e- 20
    ##  6 regionNortheast              3.66      1.77          2.07  3.88e-  2
    ##  7 regionSouth                 17.9       1.26         14.1   5.05e- 44
    ##  8 pct_unemployed16_over        0.646     0.153         4.23  2.40e-  5
    ##  9 pct_public_coverage_alone    0.614     0.143         4.30  1.80e-  5
    ## 10 birth_rate                  -0.719     0.187        -3.85  1.22e-  4
    ## 11 percent_married             -0.220     0.0798       -2.76  5.86e-  3
    ## 12 pct_black                   -0.354     0.0568       -6.22  5.59e- 10
    ## 13 pct_non_black               -0.289     0.0545       -5.31  1.16e-  7
    ## 14 med_income                  -0.0000670 0.0000523    -1.28  2.00e-  1

``` r
fit2 = update(forward10, . ~ . +pop_est2015)
tidy(fit2)
```

    ## # A tibble: 14 x 5
    ##    term                           estimate  std.error statistic   p.value
    ##    <chr>                             <dbl>      <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                121.         9.30         13.0    1.52e- 37
    ##  2 pct_bach_deg25_over         -1.44       0.0974      -14.8    8.14e- 48
    ##  3 incidence_rate               0.204      0.00712      28.7    4.57e-160
    ##  4 pct_private_coverage        -0.00249    0.0909       -0.0274 9.78e-  1
    ##  5 regionMidwest               12.0        1.27          9.42   8.41e- 21
    ##  6 regionNortheast              3.60       1.77          2.04   4.14e-  2
    ##  7 regionSouth                 17.8        1.26         14.1    6.31e- 44
    ##  8 pct_unemployed16_over        0.644      0.152         4.23   2.44e-  5
    ##  9 pct_public_coverage_alone    0.643      0.142         4.54   5.98e-  6
    ## 10 birth_rate                  -0.724      0.187        -3.88   1.08e-  4
    ## 11 percent_married             -0.255      0.0782       -3.26   1.13e-  3
    ## 12 pct_black                   -0.338      0.0565       -5.98   2.43e-  9
    ## 13 pct_non_black               -0.271      0.0538       -5.03   5.26e-  7
    ## 14 pop_est2015                 -0.00000218 0.00000116   -1.88   5.95e-  2

``` r
fit3 = update(forward10, . ~ . +study_per_cap)
tidy(fit3)
```

    ## # A tibble: 14 x 5
    ##    term                          estimate std.error statistic   p.value
    ##    <chr>                            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                121.         9.31       13.0    1.16e- 37
    ##  2 pct_bach_deg25_over         -1.49       0.0938    -15.9    7.87e- 55
    ##  3 incidence_rate               0.204      0.00714    28.6    5.28e-159
    ##  4 pct_private_coverage         0.00427    0.0909      0.0470 9.62e-  1
    ##  5 regionMidwest               12.0        1.27        9.45   6.28e- 21
    ##  6 regionNortheast              3.48       1.77        1.97   4.86e-  2
    ##  7 regionSouth                 17.9        1.26       14.2    2.05e- 44
    ##  8 pct_unemployed16_over        0.629      0.152       4.13   3.66e-  5
    ##  9 pct_public_coverage_alone    0.639      0.142       4.50   6.93e-  6
    ## 10 birth_rate                  -0.727      0.187      -3.89   1.02e-  4
    ## 11 percent_married             -0.242      0.0782     -3.10   1.94e-  3
    ## 12 pct_black                   -0.345      0.0564     -6.11   1.14e-  9
    ## 13 pct_non_black               -0.278      0.0538     -5.16   2.62e-  7
    ## 14 study_per_cap               -0.0000294  0.000678   -0.0434 9.65e-  1

``` r
fit4 = update(forward10, . ~ . +median_age)
tidy(fit4)
```

    ## # A tibble: 14 x 5
    ##    term                        estimate std.error statistic   p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                121.        9.31      13.0    9.99e- 38
    ##  2 pct_bach_deg25_over         -1.49      0.0934   -16.0    2.97e- 55
    ##  3 incidence_rate               0.204     0.00712   28.6    1.33e-159
    ##  4 pct_private_coverage         0.00396   0.0909     0.0436 9.65e-  1
    ##  5 regionMidwest               12.0       1.27       9.47   5.55e- 21
    ##  6 regionNortheast              3.50      1.77       1.98   4.76e-  2
    ##  7 regionSouth                 17.9       1.26      14.2    2.04e- 44
    ##  8 pct_unemployed16_over        0.631     0.152      4.14   3.54e-  5
    ##  9 pct_public_coverage_alone    0.638     0.142      4.50   7.10e-  6
    ## 10 birth_rate                  -0.728     0.187     -3.90   9.90e-  5
    ## 11 percent_married             -0.241     0.0781    -3.09   2.05e-  3
    ## 12 pct_black                   -0.345     0.0564    -6.11   1.15e-  9
    ## 13 pct_non_black               -0.278     0.0537    -5.17   2.52e-  7
    ## 14 median_age                  -0.00203   0.00779   -0.261  7.94e-  1

``` r
fit5 = update(forward10, . ~ . +avg_household_size)
tidy(fit5)
```

    ## # A tibble: 14 x 5
    ##    term                        estimate std.error statistic   p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                126.        9.97      12.7    8.44e- 36
    ##  2 pct_bach_deg25_over         -1.49      0.0935   -15.9    1.07e- 54
    ##  3 incidence_rate               0.203     0.00714   28.5    4.50e-158
    ##  4 pct_private_coverage        -0.00574   0.0911    -0.0629 9.50e-  1
    ##  5 regionMidwest               11.9       1.27       9.39   1.19e- 20
    ##  6 regionNortheast              3.43      1.77       1.94   5.24e-  2
    ##  7 regionSouth                 18.0       1.26      14.2    1.34e- 44
    ##  8 pct_unemployed16_over        0.648     0.153      4.24   2.30e-  5
    ##  9 pct_public_coverage_alone    0.620     0.142      4.36   1.36e-  5
    ## 10 birth_rate                  -0.709     0.187     -3.79   1.52e-  4
    ## 11 percent_married             -0.246     0.0780    -3.16   1.59e-  3
    ## 12 pct_black                   -0.356     0.0570    -6.25   4.66e- 10
    ## 13 pct_non_black               -0.287     0.0541    -5.30   1.24e-  7
    ## 14 avg_household_size          -1.22      0.860     -1.42   1.55e-  1

``` r
## Predicted model from forward elimination: 
mult_forward_fit = lm(target_death_rate ~ pct_bach_deg25_over + incidence_rate + pct_private_coverage + region + pct_unemployed16_over + pct_public_coverage_alone + birth_rate + percent_married + pct_black + pct_non_black, data = tidy_data)

tidy(mult_forward_fit)
```

    ## # A tibble: 13 x 5
    ##    term                        estimate std.error statistic    p.value
    ##    <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 (Intercept)                121.        9.30      13.0     1.00e- 37
    ##  2 pct_bach_deg25_over         -1.49      0.0934   -16.0     2.95e- 55
    ##  3 incidence_rate               0.204     0.00712   28.6     1.21e-159
    ##  4 pct_private_coverage         0.00423   0.0909     0.0465  9.63e-  1
    ##  5 regionMidwest               12.0       1.27       9.47    5.46e- 21
    ##  6 regionNortheast              3.49      1.77       1.97    4.84e-  2
    ##  7 regionSouth                 17.9       1.26      14.2     1.89e- 44
    ##  8 pct_unemployed16_over        0.629     0.152      4.13    3.65e-  5
    ##  9 pct_public_coverage_alone    0.639     0.142      4.50    6.91e-  6
    ## 10 birth_rate                  -0.727     0.187     -3.90   10.00e-  5
    ## 11 percent_married             -0.242     0.0779    -3.11    1.90e-  3
    ## 12 pct_black                   -0.345     0.0564    -6.11    1.11e-  9
    ## 13 pct_non_black               -0.278     0.0537    -5.17    2.49e-  7
