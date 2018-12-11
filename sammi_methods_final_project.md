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
cancer_data = cancer_data %>% 
  select(-pct_some_col18_24, -pct_private_coverage_alone, -poverty_percent, -median_age_female, -median_age_male, -avg_ann_count, -avg_deaths_per_year, -pct_public_coverage, -pct_hs25_over) %>% 
  separate(geography, into = c("county", "state"), sep = ", ") %>%
        mutate(
                binned_inc_lowerb = str_split_fixed(binned_inc, ", ", 2)[ ,1] %>% parse_number(), 
                binned_inc_upperb = str_split_fixed(binned_inc, ", ", 2)[ ,2] %>% parse_number(), 
                binned_inc_mean = (binned_inc_lowerb + binned_inc_upperb)/2)
```

Start with full model

``` r
mult.fit <- lm(target_death_rate ~ ., data = cancer_data)
```
