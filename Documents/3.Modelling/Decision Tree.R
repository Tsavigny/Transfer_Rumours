if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, rpart, caret, rpart.plot, pdp, randomForest, readr, corrplot, pROC, ISLR, dplyr, stringr)

##--------------Decision Tree-------------#
set.seed(30)
dec_tree_surr <- rpart(
  formula = Outcome ~ .,
  data    = training,
  method  = "class",
  control = list(cp = 0))

#Pruning
printcp(dec_tree_surr)
plotcp(dec_tree_surr)

#Pruned
dec_tree_surr_pruned <- rpart(
  formula = Outcome ~ .,
  data    = training,
  method  = "class",
  control = list(cp = 0.063))

predictions_dec <- predict(dec_tree_surr_pruned , newdata = testing[,-9], type = 'class')

confusionMatrix(predictions_dec, as.factor(testing$Outcome))


#plot the pruned tree
prp(dec_tree_surr_pruned)
