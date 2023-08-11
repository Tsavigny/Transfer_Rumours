if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, caret, readr, corrplot, dplyr, tidymodels, mgcv, ROCR)

#-------------- Hyper tuning -------------#
control <- trainControl(method = "cv",  # Cross-validation method
                        number = 5,  # Number of folds
)  

grid_values_penalised <- expand.grid(alpha = 1, lambda = seq(0, 0.5, length = 20))

#-------------- Logistic Regression -------------#
#weights = ifelse(reg_training$Outcome == "0", 1/(prop.table(table(reg_training$Outcome))["0"]),
#                 1/(prop.table(table(reg_training$Outcome))["1"])) 

set.seed(123)
model <- glm(Outcome ~ . ,family=binomial(link='logit'),data=reg_training) #trControl = control, weights = weights

summary(model)

fitted.results <- predict(model,newdata=reg_training, type='response')

fitted.results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

confusionMatrix(as.factor(fitted.results), as.factor(reg_training$Outcome))

x <- model.matrix(Outcome ~ ., data = reg_training)[,-4]  # Exclude intercept column

y <- reg_training$Outcome

coefs <- data.frame(coef(model))

summary(model)

colnames(coefs)[1] ="Score"

Variables <- rownames(coefs)
rownames(coefs) <- NULL
coefs <- cbind(Variables, coefs)

outlets <- coefs %>%
  filter(str_detect(Variables, 'Outlet')) %>%
  mutate(across('Variables', str_replace, 'Outlet', ''))


coefs %>%
  arrange(abs(Score))

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

# Fit the final model on the reg_training data
pen_model <- glmnet(x, y, 
               alpha = 1, 
               family = "binomial",
               lambda = cv.lasso$lambda.min)

coef(pen_model)

newx = model.matrix(Outcome~.,data= reg_training)[,-4]

probabilities <- predict(pen_model , newx = newx) # type = "response"

predictions <- ifelse(probabilities > 0.5, 1, 0)

confusionMatrix(as.factor(predictions), as.factor(reg_training$Outcome)) 



