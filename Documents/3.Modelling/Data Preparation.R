if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, rpart, caret, rpart.plot, pdp, randomForest, readr, corrplot, pROC, ISLR, dplyr, stringr)

#--------------Data Cleaning-------------#
Analysis_Data <- read_csv("C:/Users/Titoran/Documents/2.Cleaning/Analysis_Data.csv")
df <- Analysis_Data

#Looking at the data
summary(df)

#Removing unwanted columns
df <- df[,-c(1)]

#Missing values check
sum(is.na(df))

#Correlation plot
M = cor(df[,c(5,7,8,9)])
corrplot(M, method = "number")

df$Outcome <- as.factor(as.integer(as.logical(df$Outcome)))

#Turning columns into factors
df[,c(1,2,3,4,6,9)] <- lapply(df[,c(1,2,3,4,6,9)], factor)

colnames(df)[8] ="Market_Value"
#--------------Data Partition-------------#
set.seed(123)
trainRowNumbers <- createDataPartition(df$Outcome, p = 0.8, list = FALSE)
training <- df[trainRowNumbers,]
testing <- df[-trainRowNumbers,]



#--------------Regression Data Cleaning-------------#
Reg_Data <- read_csv("C:/Users/Titoran/Documents/2.Cleaning/Regression_Data.csv")
df <- Reg_Data

#Removing unwanted columns
df <- df[,-c(1)]

n = ncol(df)
#Int
df[,c(5:n)] <- sapply(df[,c(5:n)],as.numeric)

#Turning columns into factors
df[,c(4:n)] <- lapply(df[,c(4:n)], factor)

df$Outcome <- as.factor(as.integer(as.logical(df$Outcome)))

colnames(df)[1] ="Market_Value"

#--------------Data Partition-------------#
set.seed(123)
trainRowNumbers <- createDataPartition(df$Outcome, p = 0.8, list = FALSE)
training <- df[trainRowNumbers,]
testing <- df[-trainRowNumbers,]





