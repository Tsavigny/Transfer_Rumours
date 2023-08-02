if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, dplyr,rpart, caret, rpart.plot, pdp, randomForest,readr, readr, corrplot, tidyverse, rsample, recipes, ISLR, glmnet)

Analysis_Data <- read_csv("C:/Users/Titoran/Documents/Outlet_Analysis_Data.csv")
df <- Analysis_Data

#--------------Data Cleaning-------------#
#Looking at the data
summary(df)

#Removing unwanted columns
df <- df[,-c(1)]

df$Outcome <- as.integer(as.logical(df$Outcome))

#Turning columns into factors
df[,c(1:3)] <- lapply(df[,c(1:3)], factor)

#--------------Data Partition-------------#
set.seed(123)
trainRowNumbers <- createDataPartition(df$Outcome, p = 0.8, list = FALSE)
training <- df[trainRowNumbers,]
testing <- df[-trainRowNumbers,]

#-------------- GLM -------------#
df["Outlet_The Athletic"] <- NULL
df["League_Süper Lig"] <- NULL

model <- glm(Outcome ~.,family=binomial(link='logit'),data=training)
summary(model)

anova(model, test="Chisq")

fitted.results <- predict(model,newdata= testing,type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results), testing$Outcome)

#--------------Logistic Regression-------------#

ctrl <- trainControl(method = "cv", number = 5)

param_grid <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0, 1, by = 0.1))

model <- train(Outcome ~ ., data = training , method = "glmnet", trControl = ctrl, tuneGrid = param_grid)

best_params <- model$bestTune
print(best_params)

final_model <- glmnet(Outcome ~ ., data = training, alpha = best_params$alpha, lambda = best_params$lambda)

predictions <- predict(final_model, newdata = test_data, type = "response")

#--------------Logistic Regression-------------#

ctrl <- trainControl(method = "repeatedcv",  # Cross-validation method
                     number = 5,  # Number of folds
                     repeats = 2,  # Number of repetitions
                     savePredictions = "final")  # Save predictions for tuning

param_grid <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0, 1, by = 0.1))

model <- train(Outcome ~ ., data = training , method = "glmnet", trControl = ctrl, tuneGrid = param_grid)

coef_scores <- coef(model$finalModel, s = model$bestTune$lambda)

length(coef_scores)

p_values <- coef_scores[, "Pr(>|z|)"]

#predictions <- ifelse(predict(model, newdata = testing)  > 0.5,1,0 )

predictions <- predict(model, newdata = testing)
confusionMatrix(as.factor(predictions), as.factor(testing$Outcome))
