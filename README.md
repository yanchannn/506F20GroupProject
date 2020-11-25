# README
Stats 506 F20 Group Project -- Group 7


## Code organization

- `Group7.Rmd`, `Group.html`: the write-up for this project
- `master.csv`: the dataset downloaded from [Kaggle](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016)
- The source code for:
   + R: [R](https://github.com/aravind1338/506F20GroupProject/tree/main/R) folder
   + Stata: [Stata](https://github.com/aravind1338/506F20GroupProject/tree/main/STATA) folder
   + Python: [Python](https://github.com/aravind1338/506F20GroupProject/tree/main/Python) folder

## Group members:

[Aravind Mantravadi](https://github.com/aravind1338/Stats506_public): R

[Yan Chen](https://github.com/yanchannn/Stats506_public): Stata

[Yingyi Yang](https://github.com/YingyiYang/Stats506_public): Python


## Overview

Regression models tell us about the effect of predictor variables on the response variable, but to find out the effect of specific predictors on the response, we need to use linear combinations and draw inferences.

## Data description

The 1985-2006 Suicide Rate data for 101 different countries and 6 age groups, found on [Kaggle](https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016)

Key variables: 

|  Variable               | Description                                         |
| ----------------------- | ----------------------------------------------------|
| `country` | 101 unique countries                  |
|`year`        | 1985 ~ 2016                                |
|`sex`          | female, male                                |
|`age`          | 5-14 years, 15-24 years, 25-34 years, 35-54 years, 55-74 years, 75+ years |
|`suicides_no` | suicides count  |
|`population`   | population of each subgroup |
|`gdp_per_capita`| gdp per capita |


## Core Examples

We used linear and non-linear combinations of predictor variables to draw inferences.

### Linear Combinations

* Do people become less likely to suicide in recent decade?
* Do males or females tend to have higher suicide counts for the same age group? 
* Are teenage females more likely to suicide than retired females? 

To answer these questions, we will build a poisson regression model and explore the effect of age, gender and year in pairs on the suicide count as a linear combination.


### Non-linear combinations

* Are the expected suicide counts in a particular year larger than another year?
* What are the expected suicide counts when the interaction between two subgroups are considered?

To answer these questions, we have implemented delta method to compute the effect of age, gender and year on the suicide count using non-linear combinations, i.e. the ratio and product of the pair in comparison. 

