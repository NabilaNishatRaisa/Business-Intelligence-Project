##--------------------------------------------------------------##
## Install Necessary Packages / Libraries   --------------------##
##--------------------------------------------------------------##
packages <- c("dplyr", "readr", "lubridate", "tidyr", "scales", "stargazer")
for (package in packages) {
  if (!package %in% row.names(installed.packages())) {
    install.packages(package, repos = "https://cran.rstudio.com")
  }
}
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(scales)
library(stargazer)

rm (package, packages)

##--------------------------------------------------------------##
## READ IN DATA    ---------------------------------------------##
##--NOTE:                                                       ##
##    This is the one please you will need to change the code.   ##
##    Enter the directories where you have saved the data below.##
##    For Mac, use the following syntax                         ##
##    credit <- read_csv("~Downloads/Credit.csv")               ##
##    titanic <- read_csv("~Downloads/Titanic.csv")             ##
##    movies <- read_csv("~Downloads/Movies.csv")               ##
##--------------------------------------------------------------##
credit <- read_csv("C:/R data for class/Credit.csv")
titanic <- read_csv("C:/R data for class/Titanic.csv")
movies <- read_csv("C:/R data for class/Movies.csv")

##--------------------------------------------------------------##
## Predict 'Annual Spend' variables in 'Credit'                 ##
##--------------------------------------------------------------##

#Build a linear model using all variables in data set
ann_spend <- lm(Annual_Spend ~ . , 
                 data=credit)

summary(ann_spend)

stargazer(ann_spend, 
          type="text", 
          intercept.bottom = FALSE, 
          report = 'vcp*', 
          digits=3)

#Re-Run on just variables that were significant
ann_spend <- lm(Annual_Spend ~ Annual_Income + HH_Size + Years_postHS + TV_week + Over_Limit,
                 data=credit)

summary(ann_spend)

stargazer(ann_spend, 
          type="text", 
          intercept.bottom = FALSE, 
          report = 'vcp*', 
          digits=3)

#Just coeff
ann_spend_coeff <- as.data.frame(ann_spend$coefficients)

##------------------------------------------------------------------##
## Predict 'Probability of Survival' using variables in 'Titanic'   ##
##------------------------------------------------------------------##

#Model using all variables in data set except PassengerId & Name
prob_surv<- glm(Survived ~ . -PassengerId -Name, 
                family = binomial(link="logit"), #set the estimation model to logit 
                data=titanic)

summary(prob_surv)

stargazer(prob_surv, 
          type="text", 
          intercept.bottom = FALSE, 
          report = 'vcp*', 
          digits=3)

#Re-Run on just variables that were significant
prob_surv <- glm(Survived ~ Age + Sibling.Spouse + Female + Upper.Class,
                 family = binomial(link="logit"),  
                 data=titanic)

summary(prob_surv)

stargazer(prob_surv, 
          type="text", 
          intercept.bottom = FALSE, 
          report = 'vcp*', 
          digits=3)

#Just coeff
pSurv_coeff <- as.data.frame(prob_surv$coefficients)

##----------------------------------------------------------------------##
## Predict 'Probability of Winning Oscar' using variables in 'Movies'   ##
##----------------------------------------------------------------------##

#Model using all variables in data set except Title


#Re-Run on just variables that were significant


#Just coeff

