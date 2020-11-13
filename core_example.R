## R Script for the Group Project, Stats 506 F20
##
## Linear and non-linear combinations of regression coefficients
## Data source: GLobal suicide rates from 
## https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016
##  
##
## Author: Aravind Mantravadi, amantrav@umich.edu
## Updated: November 13, 2020

#! Limit lines to 79 characters with rare exceptions. 
# 79: -------------------------------------------------------------------------


    ## Import libraries ##

library(tidyverse)
library(biostat3)
library(doBy)
#base_wd = getwd()
#dataset = read.csv(file.path(base_wd, "/506F20GroupProject/master.csv"))

dataset = read.csv(file="master.csv")



# variable transormation
## create factor levels for age, sex and year
dataset$age = factor(dataset$age)
dataset$sex = factor(dataset$sex)
dataset$year = factor(dataset$year)


## Create the Poisson regression model

model = glm(suicides_no ~ year + sex + age + gdp_per_capita...., offset 
            = log(population),
            family = poisson(link = "log"), data=dataset)

#summary(model)


        ##  Find estimates, std error and confidence intervals ##

# Variance-covariance matrix
matrix = vcov(model)


lcom <- function(var_list, code_list, vcov_matrix=matrix) {
  # Function to calculate estimate, std error and confidence intervals
  # Inputs: var_list is a list of variables to compare (3 items expected)
  #         code_list is the list of corresponding factor codes for var_list
  # Output: A list containing the estimate, std error, lower and upper limits
  #         for the linear combination
  #         intercept + code_list[1]*var_list[1] + code_list[2]*var_list[2]
  #         + code_list[3]*var_list[3]
  
  # list of the coefficients for var_list variables
  coefficients = c(model$coefficients[var_list[1]], 
                   model$coefficients[var_list[2]], 
                   model$coefficients[var_list[3]])
  
  intercept = model$coefficients[1]
  
  ## Find the estimate, lc_age_gender ##
  lc_age_gender = Map('*', code_list, coefficients)
  lc_age_gender = Reduce('+', lc_age_gender) + intercept
  
  # Get the test statistic using a t distribution #
  alpha = 0.05
  df = df.residual(model)
  tcr = qt(1-alpha/2, df)
  
  
  ## Get the variance of the estiamate and sqrt to find std error ##
  ## First get sum of the Var(i) terms
  
  intercept_variance = vcov_matrix["(Intercept)", "(Intercept)"]
  variances = code_list[1]^2*vcov_matrix[var_list[1], var_list[1]] + 
    code_list[2]^2*vcov_matrix[var_list[2], var_list[2]] + 
    code_list[2]^2*vcov_matrix[var_list[3], var_list[3]] + 
    intercept_variance
  
  
  ## Sum of covariance terms
  covariance = 2*code_list[1]*vcov_matrix["(Intercept)", var_list[1]] + 
    2*code_list[2]*vcov_matrix["(Intercept)", var_list[2]] + 
    2*code_list[3]*vcov_matrix["(Intercept)", var_list[3]] + 
    2*code_list[1]*code_list[2]*vcov_matrix[var_list[1], var_list[2]] + 
    2*code_list[1]*code_list[3]*vcov_matrix[var_list[1], var_list[3]] + 
    2*code_list[2]*code_list[3]*vcov_matrix[var_list[2], var_list[3]]

  
  final_variance = variances + covariance
  
  std_error = sqrt(final_variance)
  
  
  ## Find confidence intervals ##
  lower = lc_age_gender - tcr*std_error
  upper = lc_age_gender + tcr*std_error
  
  remove_names = unname(c(lc_age_gender, std_error, lower, upper))
  names(remove_names) = c("estimate", "se", "lower", "upper")
  
  return(remove_names)
  
}


# Binary factor codes are used to encode the variable #

male_25_34_2015 = lcom(c("sexmale", "age25-34 years", "year2015"), c(1, 1, 1))
male_25_34_2005 = lcom(c("sexmale", "age25-34 years", "year2005"), c(1, 1, 1))


male_35_54_2015 = lcom(c("sexmale", "age35-54 years", "year2015"), c(1, 1, 1))
female_35_54_2015 = lcom(c("sexmale", "age35-54 years", "year2015"), c(0, 1, 1))


male_75_2015 = lcom(c("sexmale", "age75+ years", "year2015"), c(1, 1, 1))
male_5_14_2015 = lcom(c("sexmale", "age5-14 years", "year2015"), c(1, 1, 1))



create_dataframe <- function(c1, c2, l1, l2) {
  # A function to create a dataframe with columns c1 and c2
  # Input: lists c1 and c2 that will become columns, l1, l2 are the extra names
  # Output: A datframe with column names equal to the names of the list items
  
  # For plotting, create a column with the names of the columns
  # for ex. "25-34 Males, 2015", "25-34 Males, 2005"
  type = c(l1, l2)
  
  
  temp = do.call(rbind, Map(data.frame, A=c1, B=c2))
  transpose = t(as.matrix(temp))
  
  final_df = as.data.frame(transpose)
  final_df = cbind(final_df, type)
  
  return(final_df)
  
}


plot_ci <- function(dataset, xvar="estimate", yvar="type", lower="lower", 
                    upper="upper", xlab="LC CI", ylab="LC groups",
                    title="Linear Combination (LC) CI's") {
  # Function to create a confidence interval plot using
  # ggplot for the given variable
  # Inputs: 
  #    dataset - the dataset containing columns xvar, yvar,
  #              lower, upper
  #    xvar - the x-axis variable in question - (string) column in dataset
  #    yvar - the y-axis variable in question - type in this script
  #    lower - the lower confidence limit - (string) column in dataset
  #    upper - the upper confidence limit - (string) column in dataset
  #    xlab - the label that should be on the x-axis - related to var
  #    ylab - the label that should be on the y-axis - census division
  # Output: a plot of the CI of the linear combination.
  
  
  dataset %>%
    ggplot(aes_string(x = xvar, y = yvar)) +
    geom_point(position = position_dodge(width = 0.4)) +
    geom_errorbarh(aes_string(xmin = lower, xmax = upper),
                   position = position_dodge(width = 0.4)) +
    labs(x = xlab, y = ylab) + ggtitle("Linear Combination (LC) CI")
}


# 
#       ## Create plots for each of the pairs of combinations ##
# 
# 
# # 25-34 males, 2005 vs 2015 #
# df1 = create_dataframe(male_25_34_2015, male_25_34_2005,
#                        "25-34 Males, 2015", "25-34 Males, 2005")
# 
# plot_ci(dataset=df1)
# 
# 
# # 35-54 in 2015, male vs female #
# 
# df2 = create_dataframe(male_35_54_2015, female_35_54_2015,
#                        "35-54 Males, 2015", "35-54 Females, 2015")
# 
# plot_ci(dataset=df2)
# 
# 
# # male in 2015, 75+ vs 5-14 #
# 
# df3 = create_dataframe(male_75_2015, male_5_14_2015,
#                        "75+ Males, 2015", "5-14 Males, 2015")
# 
# plot_ci(dataset=df3)





## Check results using lincom and esticon;
## The confidence intervals match


#lin_comb = lincom(model, "(Intercept) + 1*sexmale + 1*age25-34 years + 1*year2015")


##  TESTING ESTICON FUNCTION ##
    # Answers match #
# 
# 
# lambda1 = integer(39)
# lambda1[1] = 1 # set intercept
# lambda1[33] = 1 # set sexmale = 1
# lambda1[34] = 1 # set age25-34 years = 1
# lambda1[31] = 1 # set year2015 = 1
# 
# 
# estimates = esticon(model, lambda1)

