# this R file is used to try on CHAID library
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
# setwd("C:/")

library('CHAID')
library('partykit')
library('grid')
library('libcoin')
library(readxl)

helpme <- read_excel("helpme.xlsx", sheet = "Sheet1", range = "A1:G300")
show (helpme)
attach(helpme)

# building the model
att <- factor(Attrition)
ag <- factor(Age)
el <- factor(EducationLevel)
gndr <- factor(Gender)
pr <- factor(PerformanceRating)
pay <- factor(PaymentType)
marstat <- factor(MaritalStatus)

ctrl <- chaid_control(minsplit = 2, minbucket = 2, alpha2 = .05, alpha4 = .05)
chaidmar <- chaid(marstat ~ pr + el + gndr +pay + att + ag, data = helpme, control = ctrl)

print(chaidmar)
plot(chaidmar)

plot(chaidmar, type = 'simple')

# evaluation of the model
library(gmodels)
trainToBishan <- predict(chaidmar, helpme)
CrossTable(helpme$MaritalStatus, trainToBishan,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('actual default', 'predicted default'))