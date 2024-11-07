#-------------------------------------------------------------------------------
# Being new to GitHub

#### Navigate to your project directory (if not already there)
# cd path/to/R-Project

#### Stage all changes
#git add .

#### Commit with a message describing your changes
#git commit -m "Update project files"

#### Push to GitHub
#git push origin main
#-------------------------------------------------------------------------------

# ----
## Load and install required packages
library(dplyr)
library(mosaic)
library(ggplot2)

# ----
## Data Setup
myDataLocation <- "~/Desktop/NU Data Science Course/GItHub/R-Project/consumer_data.csv"
myData <- read.csv(myDataLocation)

# ----
## Data Structure
str(myData)

# ----
## Displaying top and bottom of data
head(myData, 6)
tail(myData, 6)

# ----
## Creating a frequency table
tally(~ region, data = myData)

tally(~ employmentSector, data = myData)

# ----
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

# ----
## Example of getting descriptive statistics
houseMean <- aggregate(household ~ 1, consumerData, FUN = mean)
houseMedian <- aggregate(household ~ 1, consumerData, FUN = median)
houseMode <- aggregate(household ~ 1, consumerData, FUN = mode)
houseSD <- aggregate(household ~ 1, consumerData, FUN = sd)

houseMean
houseMedian
houseMode
houseSD

# ----
## Handling Missing data
consumerData_missing <- is.na(consumerData)
consumerData_cleaned <- filter(consumerData, is.na(age) == FALSE) ## won't always remove it

select(consumerData, age)
select(consumerData_cleaned, age)
tally(~ age, data = consumerData_cleaned)

## Counting dropped observations when removing na from age.
print(nrow(consumerData) - nrow(consumerData_cleaned)) ## 156 obs were dropped

# ----
## Creating a basic histogram of the age variable
gf_histogram(~ age, data = consumerData_cleaned)

# ----
## Creating a new variable
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

# ----
## Calculating the mean and sd of age, household, and kids by financialStability. 

# Using summary()
summary(consumerData_cleaned)

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