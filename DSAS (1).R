#FINAL PROJECT - GROUP 7: DSAS
#MOVIE DATASET PROJECT

#------------------------------------------------
#INITIALIZATION

#Setting up the workspace for our file
library(readr)
setwd("/Users/mahee/Downloads")
Merged <- read_csv("DSAS.csv")

#Understanding our Merged.csv file
str(Merged)
summary(Merged)

#Dropping column x16
Merged <- Merged[,-c(16)]

#---------------------------------------------------
#CONVERTING ALL VARIABLE TYPES

#Convert to factor type
Merged$genre = as.factor(Merged$genre)
Merged$rating = as.factor(Merged$rating)

#convert genre into 4 categories
Merged$genre = as.character(Merged$genre)
clean_movies = c("Comedy", "Action", "Drama")
Merged$genre[!(Merged$genre %in% clean_movies)] = "Other"
Merged$genre = as.factor(Merged$genre)

#convert rating into 4 categories
Merged$rating = as.character(Merged$rating)
rating_to_keep = c("R","PG-13","PG", "G")
Merged$rating[!(Merged$rating %in% rating_to_keep)] = "Other"
Merged$rating = as.factor(Merged$rating)
levels(Merged$rating)

#drop other from rating
Merged$rating = as.character(Merged$rating)
Merged <- subset(Merged, rating != "Other")
Merged$rating = as.factor(Merged$rating)
levels(Merged$rating)
str(Merged)

#----------------------------------------------------------
#DROPPING ALL UNNECESSARY COLUMNS
Merged1 <- Merged[,-c(2,3,4,7,9,11,12,13,14,15)] 
Merged.c <- Merged[,-c(3,4,7,9,11,12,13,14,15)] 
str(Merged1)
str(Merged.c)

#drop budget = 0 from both datasets 
Merged.c <- subset(Merged.c, budget != "0")
Merged1 <- subset(Merged1, budget != "0")

#-----------------------------------------
#RANDOM FOREST

#We ran the random forest

set.seed(1)
train.index = sample(1:nrow(Merged1),nrow(Merged1)*0.8)
train.rev = Merged1[train.index,]
test.rev = Merged1[-train.index,]

#Reproducibility method

library(randomForest)
regr.model = randomForest((gross)~. ,data = Merged1)
regr.model

#Setting parameters so that graphs are visible
par("mar") 
par(mar=c(1,1,1,1))
plot(regr.model,main="Error as ntree increases")

#Running random forest
regr.model = randomForest((gross)~. ,data = train.rev, ntree=180, importance = TRUE)
regr.model

#Seeing the most important variables in the plot
importance(regr.model)
varImpPlot(regr.model,main="Variable Importance Plots")

#Finding MSE
pred.y = predict(regr.model,test.rev)
mse = mean((pred.y-test.rev$gross)^2)
mse
sqrt(mse)

#-------------------------------------------
#Random Forest for Method 2 with Company + Limits

#Using this complex model to compare with baseline
Merged.c <- Merged.c[ which(Merged.c$gross >= 1000000 & Merged.c$gross <=100000000), ]
Merged.c <- Merged.c[ which(Merged.c$runtime <=180), ]
Merged.c

#Adding company and changing the type
Merged.c$company = as.character(Merged.c$company)
keep_company = c("Universal Pictures", "Warner Bros.", "Paramount Pictures", "Twentieth Century Fox Film Corporation", "New Line Cinema","Columbia Pictures Corporation","Touchstone Pictures", "Columbia Pictures", "Metro-Goldwyn-Mayer (MGM)", "DreamWorks")
Merged.c$company[!(Merged.c$company %in% keep_company)] = "Other"
Merged.c <- subset(Merged.c, company != "Other")
Merged.c$company = as.factor(Merged.c$company)
str(Merged.c)

#Random Forest for this method
set.seed(1)
library(randomForest)
str(Merged.c)
train.index = sample(1:nrow(Merged.c),nrow(Merged.c)*0.8)
train.rev = Merged.c[train.index,]
test.rev = Merged.c[-train.index,]

#Running the random forest formula
reg1.model = randomForest((gross)~. ,data = Merged.c)
reg1.model

#Check margins
par("mar")
plot(reg1.model,main="Error as ntree increases")    

#Finding the random forest based on 150 trees
reg1.model = randomForest((gross)~. ,data = train.rev, ntree=150, importance = TRUE)
reg1.model

#Most important variables in the plot
importance(reg1.model)
varImpPlot(reg1.model,main="Variable Importance Plots")

#Predicting based on the model and finding RMSE
pred= predict(reg1.model,test.rev)
mse = mean((pred-test.rev$gross)^2)
sqrt(mse)

#--------------------------------------------------------------------------
#CREATING DUMMY VARIABLES

#Changing variables to use for linear regression and KNN (dummy variables)
#Convert rating to dummy variables for Merged1 
Merged1$g.rating = ifelse(Merged1$rating == "G", 1, 0)
Merged1$pg.rating = ifelse(Merged1$rating == "PG", 1, 0)
Merged1$r.rating = ifelse(Merged1$rating == "R", 1, 0)
Merged1$pg.13rating = ifelse(Merged1$rating == "PG-13", 1, 0)

#Convert genre to dummy variables for Merged1 
Merged1$g.action = ifelse(Merged1$genre == "Action", 1, 0)
Merged1$g.comedy = ifelse(Merged1$genre == "Comedy", 1, 0)
Merged1$g.drama = ifelse(Merged1$genre == "Drama", 1, 0)
Merged1$g.other = ifelse(Merged1$genre == "Other", 1, 0)

#Convert rating to dummy variables for Merged.c
Merged.c$g.rating = ifelse(Merged.c$rating == "G", 1, 0)
Merged.c$pg.rating = ifelse(Merged.c$rating == "PG", 1, 0)
Merged.c$r.rating = ifelse(Merged.c$rating == "R", 1, 0)
Merged.c$pg.13rating = ifelse(Merged.c$rating == "PG-13", 1, 0)

#convert genre to dummy variables for Merged.c 
Merged.c$g.action = ifelse(Merged.c$genre == "Action", 1, 0)
Merged.c$g.comedy = ifelse(Merged.c$genre == "Comedy", 1, 0)
Merged.c$g.drama = ifelse(Merged.c$genre == "Drama", 1, 0)
Merged.c$g.other = ifelse(Merged.c$genre == "Other", 1, 0)

#------------------------------------------------------------
#EXPLORATORY DATA ANALYSIS

#EDA budget vs revenue graph
x=Merged$budget
y=Merged$gross 
plot(x,y,xlab="Budget",ylab="Revenue",main="Revenue vs Budget")

#EDA budget vs revenue graph
x=Merged$runtime
y=Merged$gross 
plot(x,y,xlab="Runtime",ylab="Revenue",main="Revenue vs Runtime")

#Removing any remaining 0's from budget so that they are no longer causing outliers
Merged.c <- subset(Merged.c, budget != "0")
Merged1 <- subset(Merged1, budget != "0")

#-------------------------------------------------------------
#LINEAR REGRESSION MODEL FOR BASELINE DATA

#Building a linear regression model
model = lm(gross~budget+runtime+rating+genre,data=Merged1) 
summary(model) 

#Testing assumptions
#plot(model) will produce 4 plots, so prepare a proper display
par(mfrow=c(2,2))

#plot(model) will generate useful diagnostic plots
plot(model)

#Multicollinearity
install.packages("car")
library(car) # Import car package
vif(model)

#Other considerations
#Interaction Terms
model1 = lm(gross~budget*runtime,data=Merged1)
summary(model1)

#Feature Selection
install.packages("leaps")
library(leaps)

# Apply forward selection to model with all variables 
#(the ~. represents all predictors)
model_fwd = regsubsets(gross~., data=Merged1, nvmax=NULL, method="forward") #fwd for all the variables in our dataset

#summary(model_fwd) : fwd for all the variables in our dataset
summary(model_fwd)
plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2")

#Store summary output
model_fwd_summary = summary(model_fwd)

# Display best subset by adjr2
which.max(model_fwd_summary$adjr2) 
summary(model_fwd)$which[7,]

#Best model given RMSE (lowest)
best_model_fwd1 = lm(gross~budget+g.drama+pg.rating+r.rating+runtime+g.rating+g.action, data=Merged1)
summary(best_model_fwd1)

#----------------------------------------------------------
#KNN FOR BASELINE MODEL

# installing all necessary packages
install.packages('caret')
install.packages('tidyverse')
library(caret)
library(tidyverse)

#Checking all the Merged1 columns
str(Merged1)
Merged1 <- Merged1[c("budget", "genre", "rating", "runtime", "g.rating", "pg.rating", "r.rating", "pg.13rating", "g.action", "g.comedy", "g.drama", "g.other", "gross")]


# Normalization
normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# normalize numeric values of budget and runtime
norm.budget = normalize(Merged1$budget)
norm.runtime = normalize (Merged1$runtime)

#Binding all the correct columns for norm.merged
norm.merged = cbind(Merged1[,5:13],norm.budget, norm.runtime)
summary(norm.merged)

#Create training and testing set
set.seed(1) # for reproducibility purposes
train.index = sample(1:nrow(norm.merged),nrow(norm.merged)*0.80)
train = norm.merged[train.index,]
test = norm.merged[-train.index,]
library(class)

#looking at 5-fold CV
ctrl <- trainControl(method="repeatedcv",repeats = 5)
knnFit <- train(gross ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 15)
knnFit
plot(knnFit)

knnPredict <- predict(knnFit,newdata = test)
predictions <- knnFit %>% predict(test)
head(predictions)

RMSE(predictions, test$gross)


#----------------------------------------------------
#CREATING COMPLEX MODEL WITH LIMITS FOR BUDGET AND RUNTIME + ADDING COMPANY 

#Checking levels for company
levels(Merged.c$company)

#Creating the set order of columns
Merged.c <- Merged.c[c("budget", "genre", "company", "rating", "runtime", "g.rating", "pg.rating", "r.rating", "pg.13rating", "g.action", "g.comedy", "g.drama", "g.other", "gross")]

#converting the top 10 companies to dummy variables
Merged.c$cp = ifelse(Merged.c$company == "Columbia Pictures", 1, 0)
Merged.c$cpc = ifelse(Merged.c$company == "Columbia Pictures Corporation", 1, 0)
Merged.c$dw = ifelse(Merged.c$company == "DreamWorks", 1, 0)
Merged.c$mgm = ifelse(Merged.c$company == "Metro-Goldwyn-Mayer (MGM)", 1, 0)
Merged.c$nlc = ifelse(Merged.c$company == "New Line Cinema", 1, 0)
Merged.c$pp = ifelse(Merged.c$company == "Paramount Pictures", 1, 0)
Merged.c$tp = ifelse(Merged.c$company == "Touchstone Pictures", 1, 0)
Merged.c$tc = ifelse(Merged.c$company == "Twentieth Century Fox Film Corporation", 1, 0)
Merged.c$up = ifelse(Merged.c$company == "Universal Pictures", 1, 0)
Merged.c$wb = ifelse(Merged.c$company == "Warner Bros.", 1, 0)

#Checking Merged.c with updates
str(Merged.c)

#-----------------------------------------------------------
#LINEAR REGRESSION FOR THE COMPLEX MODEL W/ COMPANY + LIMITS

#Building a linear regression model with company column and outliers removed 
model100 = lm(gross~budget+runtime+rating+genre+company,data=Merged.c) 
summary(model100) 

# Testing assumptions
# plot(model) will produce 4 plots, so prepare a proper display
par(mfrow=c(2,2))

# plot(model) will generate useful diagnostic plots
plot(model100)

# Multicollinearity
install.packages("car")
library(car) # Import car package
vif(model100)

#Feature Selection
install.packages("leaps")
library(leaps)

#Apply forward selection to model with all variables 
model_fwd100 = regsubsets(gross~budget+runtime+rating+genre+company, data=Merged.c, nvmax=NULL, method="forward")

#summary(model_fwd) : fwd for all the variables in our dataset
summary(model_fwd100)

#plot(model_fwd, scale="adjr2", main="Forward Selection: AdjR2")
plot(model_fwd100, scale="adjr2", main="Forward Selection: AdjR2")

#Store summary output
model_fwd_summary100 = summary(model_fwd100)

#Display best subset by adjr2
which.max(model_fwd_summary100$adjr2) 
summary(model_fwd100)$which[15,]

#Plotting the best model
best_model_fwd100 = lm(gross~budget+runtime+pg.rating+r.rating+g.drama+cpc+mgm+pp+tc+pg.13rating+ dw+nlc+up+tp+wb, data=Merged.c)
summary(best_model_fwd100)

#------------------------------------------------
#KNN FOR COMPLEX MODEL WITH TOP 10 COMPANIES + LIMITS

#Ensuring Merged.C looks good
str(Merged.c)
summary(Merged.c)

# Normalization
normalize = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

#Normalize all numeric values (budget + runtime) 
norm.budgetupdate = normalize(Merged.c$budget)
norm.runtimeupdate = normalize(Merged.c$runtime)
summary(Merged.c)

#Binding all of the correct columns to the Merged.c
norm.mergedupdate = cbind(Merged.c[,6:24],norm.budgetupdate, norm.runtimeupdate)
summary(norm.mergedupdate)

#For reproducibility purposes
set.seed(1) 
train.index = sample(1:nrow(norm.mergedupdate),nrow(norm.mergedupdate)*0.80)
train = norm.mergedupdate[train.index,]
test = norm.mergedupdate[-train.index,]
library(class)

#Finding the optimal k with 5-fold testing
ctrl <- trainControl(method="repeatedcv",repeats = 5)
knnFit1 <- train(gross ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"),  tuneLength = 15)
knnFit1
plot(knnFit1)

#Using predict function for Merged.c
knnPredict <- predict(knnFit1,newdata = test)
prediction <- knnFit1 %>% predict(test)
head(prediction)

#Finding RMSE value for Merged.c
RMSE(prediction, test$gross)