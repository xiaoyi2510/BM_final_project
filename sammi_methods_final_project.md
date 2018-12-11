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
mult.fit <- lm(target_death_rate ~ ., data = cancer_data)
```
