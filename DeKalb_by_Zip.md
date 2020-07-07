DeKalb County by Zipcode
================

``` r
#Load required libraries

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.1     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.1     ✓ dplyr   1.0.0
    ## ✓ tidyr   1.1.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
#Import and tidy data from DeKalb County Board of Health 
#https://www.dekalbhealth.net/covid-19dekalb/

dekalb <- read_lines("Dekalb.txt", skip_empty_rows = TRUE)
dekalb <- dekalb[dekalb != "\t"]
dekalb <- dekalb[6:(length(dekalb))]
dekalb <- matrix(dekalb, ncol = 5, byrow = TRUE) %>%
  as_tibble() %>%
  select(V1, V3) %>%
  rename(ZIP = V1,
         current_count = V3,)
```

    ## Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
    ## Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
#Import data from US Census American Community Survey 2018 5-year estimate

zcta <-
  read_csv("ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv",
           skip = 1) %>%
  mutate(ZIP = str_extract(`Geographic Area Name`,
                           ".....$")) %>%
  select(ZIP, `Estimate!!Households!!Median income (dollars)`)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Estimate!!Households!!Total` = col_double(),
    ##   `Margin of Error!!Households MOE!!Total` = col_double(),
    ##   `Estimate!!Families!!Total` = col_double(),
    ##   `Margin of Error!!Families MOE!!Total` = col_double(),
    ##   `Estimate!!Married-couple families!!Total` = col_double(),
    ##   `Margin of Error!!Married-couple families MOE!!Total` = col_double(),
    ##   `Estimate!!Nonfamily households!!Total` = col_double(),
    ##   `Margin of Error!!Nonfamily households MOE!!Total` = col_double()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 280 parsing failures.
    ##   row                                      col expected actual                                                         file
    ## 29477 Estimate!!Households!!Total              a double   null 'ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv'
    ## 29477 Margin of Error!!Households MOE!!Total   a double   null 'ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv'
    ## 29477 Estimate!!Families!!Total                a double   null 'ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv'
    ## 29477 Margin of Error!!Families MOE!!Total     a double   null 'ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv'
    ## 29477 Estimate!!Married-couple families!!Total a double   null 'ACSST5Y2018.S1901_data_with_overlays_2020-07-04T190723.csv'
    ## ..... ........................................ ........ ...... ............................................................
    ## See problems(...) for more details.

``` r
#Import population data from 2010 Census

zcta_pop <- read_csv("DECENNIALSF12010.P1_data_with_overlays_2020-07-06T171622.csv",
                     skip = 1) %>%
  mutate(ZIP = str_extract(`Geographic Area Name`, "\\d\\d\\d\\d\\d")) %>%
  select(ZIP, total_population = Total)
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   `Geographic Area Name` = col_character(),
    ##   Total = col_double()
    ## )

``` r
#Join ACS data with local COVID-19 data and specify column types

dekalb <- dekalb %>%
  inner_join(zcta) %>%
  mutate(
    current_count = as.numeric(current_count),
    median_income = as.numeric(`Estimate!!Households!!Median income (dollars)`)
  ) %>%
  select(!(`Estimate!!Households!!Median income (dollars)`))
```

    ## Joining, by = "ZIP"

``` r
#Add population data to Dekalb tibble and calculate per capita data

dekalb <- dekalb %>%
  inner_join(zcta_pop) %>%
  mutate(cases_per_thousand = current_count / (total_population / 1000))
```

    ## Joining, by = "ZIP"

``` r
#clear redundant tibbles from memory

remove(zcta)
remove(zcta_pop)
```

``` r
#Now let's make a plot to see if there is a correlation between income and disease

ggplot(data = dekalb,
       mapping = aes(median_income, cases_per_thousand)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](DeKalb_by_Zip_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#It looks like there's a correlation. Let's try modeling it

mod_covid <- lm(cases_per_thousand ~ median_income, dekalb)
summary(mod_covid)
```

    ## 
    ## Call:
    ## lm(formula = cases_per_thousand ~ median_income, data = dekalb)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.9186 -2.1517 -0.8885  1.9821  7.8445 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    1.255e+01  2.153e+00   5.829 2.55e-06 ***
    ## median_income -9.295e-05  3.099e-05  -2.999  0.00552 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.363 on 29 degrees of freedom
    ## Multiple R-squared:  0.2367, Adjusted R-squared:  0.2104 
    ## F-statistic: 8.993 on 1 and 29 DF,  p-value: 0.005516
