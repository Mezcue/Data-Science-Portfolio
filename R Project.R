## Load and install required packages
library(dplyr)
library(mosaic)
library(ggplot2)

## Data Setup
myDataLocation <- "~/Desktop/NU Data Science Course/GItHub/R-Project/consumer_data.csv"
myData <- read.csv(myDataLocation)

## Data Structure
str(myData)

## Displaying top and bottom of data
head(myData, 6)
tail(myData, 6)

## Creating a frequency table
tally(~ region, data = myData)

tally(~ employmentSector, data = myData)

## Categorizing a numeric variable
consumerData <- myData ## Keeping original data

consumerData$household_category <- factor(
  consumerData$household,
  levels = 2:10,  # Assuming levels start from 2 and go up to 10
  labels = c("XX Small", "X Small", "Small", "Low Medium", "Medium", "High Medium", "Large", "X Large", "XX Large")
)

str(consumerData)
tally(consumerData$household)
tally(consumerData$household_category)

## Example of getting descriptive statistics
houseMean <- aggregate(household ~ 1, consumerData, FUN = mean)
houseMedian <- aggregate(household ~ 1, consumerData, FUN = median)
houseMode <- aggregate(household ~ 1, consumerData, FUN = mode)
houseSD <- aggregate(household ~ 1, consumerData, FUN = sd)

houseMean
houseMedian
houseMode
houseSD


## Handling Missing data
consumerData_missing <- is.na(consumerData)
consumerData_cleaned <- filter(consumerData, is.na(age) == FALSE) ## won't always remove it

select(consumerData, age)
select(consumerData_cleaned, age)
tally(~ age, data = consumerData_cleaned)

## Counting dropped observations when removing na from age.
print(nrow(consumerData) - nrow(consumerData_cleaned)) ## 156 obs were dropped


NA_dropped <- sum(is.na(consumerData)) - sum(is.na(consumerData_cleaned)) ## different method
#NA_dropped ## 156 observations were dropped
cat("Number of observations dropped:", NA_dropped, "\n")

## Creating a basic histogram of the age variable
gf_histogram(~ age, data = consumerData_cleaned)

#Q11. Draw a random sample of 10 observations from your filter_dat dataframe in a new dataframe. ----
random_sample <- sample(consumerData_cleaned, 10)
head(random_sample, 10)

#Q12. Create a histogram of the age variable from the random sample. ----
gf_histogram(~ age, data = consumerData_cleaned)
gf_histogram(~ age, data = random_sample)

#Q13. What do you notice is different between the random sample and dataset histograms? (Respond with code and short written response) ----
## The appearance somewhat has a similiar bellcurve shape, positive skew, tail going to the right.
## The age ranges up to 60 in the random sample, vs 80 in the larger dataset.
## The count ranges significantly less in the random sample vs the larger data set (obviously)
## Ourliers are more identifiable in the larger dataset. 
## Very obvious difference given the data ranges/ data points in the larger dataset vs the random sample

#Q14. Create a new variable of the income variable, called income3 with 3 levels, and assign levels and labels to its values. ----
str(consumerData_cleaned) ## income
tally(~ income, data = consumerData_cleaned)

consumerData_cleaned <- consumerData_cleaned |>
  mutate(
    income3 = case_when(
      income <= 33 ~ "Low Income",
      income <= 66 ~ "Medium Income",
      TRUE ~ "High Income"
    )
  )

str(consumerData_cleaned)
tally(~ income3, data = consumerData_cleaned)

#Q15. Use the aggregate() function to compute the mean and standard deviation of three quantitative variables, by one categorical variable ----
summary(consumerData_cleaned)

## I calculated the mean and sd of age, household, and kids by financialStability. 

mean_household <- aggregate(household ~ financialStability, consumerData_cleaned, FUN = mean)
sd_household <- aggregate(household ~ financialStability, consumerData_cleaned, FUN = sd)
mean_household
sd_household

mean_age <- aggregate(age ~ financialStability, consumerData_cleaned, FUN = mean)
sd_age <- aggregate(age ~ financialStability, consumerData_cleaned, FUN = sd)
mean_age
sd_age

mean_kids <- aggregate(kids ~ financialStability, consumerData_cleaned, FUN = mean)
sd_kids <- aggregate(kids ~ financialStability, consumerData_cleaned, FUN = sd)
mean_kids 
sd_kids