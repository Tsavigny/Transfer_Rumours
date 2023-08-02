if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, caret, readr, corrplot, dplyr, tidymodels, mgcv, ROCR)

#-------------- Hyper tuning -------------#
control <- trainControl(method = "repeatedcv",  # Cross-validation method
                        number = 10,  # Number of folds
                        repeats = 2,  # Number of repetitions
)  

grid_values_penalised <- expand.grid(alpha = 1, lambda = seq(0, 0.5, length = 20))

#-------------- Logistic Regression -------------#

model <- glm(Outcome ~ . ,family=binomial(link='logit'),data=training) #trControl = control

summary(model)

fitted.results <- predict(model,newdata=testing,type='response')

fitted.results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results), as.factor(testing$Outcome))

x <- model.matrix(Outcome ~ ., data = training)[,-4]  # Exclude intercept column

y <- training$Outcome

coefs <- data.frame(coef(model))

colnames(coefs)[1] ="Score"

Variables <- rownames(coefs)
rownames(coefs) <- NULL
coefs <- cbind(Variables, coefs)

outlets <- coefs %>%
  filter(str_detect(Variables, 'Outlet')) %>%
  mutate(across('Variables', str_replace, 'Outlet', ''))

temp <- coefs %>%
 filter(abs(Score) > 1.5)

My_Theme = theme(
  panel.background = element_rect(fill = 'white', colour = 'black'),
  plot.title = element_text(hjust = 0.5, size = 16),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 16))

ggplot(data= temp, aes(x=reorder(Variables, Score),y= Score)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Coefficient")+ xlab("Variables")+ 
  ggtitle("Top 15 Coefficient Scores Logistic Regression ") + 
  My_Theme

ggplot(data= outlets, aes(x=reorder(Variables, Score),y= Score)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + ylab("Coefficient")+ xlab("Variables")+ 
  ggtitle("Outlet Coefficient Scores Logistic Regression ") + 
  My_Theme

#--------------Penalized Regression-------------#
#Alternative Method
set.seed(69)
library(glmnet)
#Alpha = 1 --> Lasso Regression (Coefs become 0), Alpha = 0 --> Ridge Regression (Diminishing coefs)
cv.lasso <- cv.glmnet(x, y, alpha = 1, trControl = control, tuneGrid = grid_values_penalised, family = "binomial")

# Fit the final model on the training data
pen_model <- glmnet(x, y, 
               alpha = 1, 
               family = "binomial",
               lambda = cv.lasso$lambda.min)

coef(pen_model)

newx = model.matrix(Outcome~.,data= testing)[,-4]

probabilities <- predict(pen_model , newx = newx) # type = "response"

predictions <- ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(as.factor(predictions), as.factor(testing$Outcome)) 



