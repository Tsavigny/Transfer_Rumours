if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, gridExtra, caret, readr, dplyr)

#-------------- Control -------------#
#5 fold cross validation
control <- trainControl(method = "repeatedcv",  # Cross-validation method
                        number = 5,  # Number of folds
                        repeats = 2,  # Number of repetitions
)  

#-------------- SVM Linear -------------#
set.seed(119)
svm_linear <- train(Outcome ~. ,
                    data = training,
                    method = "svmLinear",
                    trControl = control,
                    preProc = c("center", "scale"),
                    tuneLength = 10)

#-------------- SVM Radial -------------#
#Tunelength tests for different values of the tuning parameter (Kernel in this case)
set.seed(200)
svm_radial <- train(Outcome ~. ,
                    data = training,
                    method = "svmRadial",
                    trControl = control,
                    preProc = c("center", "scale"),
                    tuneLength = 10)

#-------------- SVM Polynomial -------------#
set.seed(991)
svm_poly <- train(Outcome ~. ,
                  data = training,
                  method = "svmPoly",
                  trControl = control,
                  preProc = c("center", "scale"),
                  tuneLength = 2)

#-------------- Results -------------#

#SVM Linear
confusionMatrix(predict(svm_linear, newdata = testing), testing$Outcome)

#SVM Polynomial
confusionMatrix(predict(svm_poly, newdata = testing), testing$Outcome)

#SVM Radial
confusionMatrix(predict(svm_radial, newdata = testing), testing$Outcome)
