# Adam Hereth
# using K nearest neighbor model to predict heart disease

library(tidyverse)
library(readxl)
library(kknn)
setwd("C:/Users/Adam/Desktop/Spring 2021/Data Science CMSC 462/files")
## Classification with *knn*
Heart = read_excel("Heart Disease.xlsx")

# Recode the HeartDisease as a factor
Heart$HeartDisease = factor(Heart$HeartDisease,
levels = c("Yes","No"), labels = c("Yes","No"))

# Let's try k = 8
KNNHeart = kknn(HeartDisease ~ ChestPain + Age, train = Heart, test = Heart, k = 8)

# Add predicted values to our dataset
Heart = Heart %>% mutate(HeartDiseaseKNN = KNNHeart$fitted.values)

# Let's take a look at the result
Heart %>% select(HeartDisease, HeartDiseaseKNN)

Heart %>% group_by(HeartDisease,HeartDiseaseKNN) %>% summarise(Patients = n())


# Optimal k
set.seed(10) # This sets a start value for generating random numbers

KNNHeartOptimal = train.kknn(HeartDisease ~ ChestPain + Age,
data = Heart, kmax = 20) # max k we are interested in trying

# Now we can view the results with the summary function
summary(KNNHeartOptimal)

