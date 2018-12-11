sammi\_methods\_final\_project
================
Samantha Brown
12/11/2018

Load Data

``` r
cancer_data <- read_csv("./data/Cancer_Registry.csv") %>%
        janitor::clean_names()
```

### Data Exploration

-   75% of the variable `pct_some_col18_24` is missing. Therefore, we will remove it from the dataset.

-   20% of the variable `pct_private_coverage_alone` is missing. Therefore, we will remove it from the dataset.

-   `med_income` and `poverty_percent` are highly correlated with a correlation coefficient of -78.9%. To account for this, we will remove `poverty_percent` from the dataset.

-   `median_age_female` and `median_age_male` are highly correlated with a correlation coefficient of 93.37%. Thus, we will only consider `median_age` for our data analysis.

``` r
cancer_data = cancer_data %>% 
  select(-pct_some_col18_24, -pct_private_coverage_alone, -poverty_percent, -median_age_female, -median_age_male)
```

Start with full model

``` r
##mult.fit <- lm(target_death_rate ~ ., data = cancer_data)
##summary(mult.fit)
```
