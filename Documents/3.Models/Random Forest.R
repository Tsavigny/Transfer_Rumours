if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra, rpart, caret, rpart.plot, pdp, randomForest, readr, corrplot, pROC, ISLR, stringr)

#--------------Random Forest -------------#
set.seed(123)
RF <- train(Outcome ~ . , 
            data = training, 
            method = 'rf',
            tuneGrid = data.frame(mtry = c(1:10)),
            ntree = 500,
            trControl = trainControl(method = 'cv', # Use cross-validation
                                     number = 5)) # Use 5 folds for cross-validation

#Checking how many predictor variables to use
plot(RF)    

#15 works best even though its bigger than max mtry technically, seed(123)
set.seed(2)
RF_Final <- train(Outcome ~ . , 
                  data = training, 
                  method = 'rf',
                  tuneGrid = data.frame(mtry = 8),
                  ntree = 500,
                  trControl = trainControl(method = 'cv', # Use cross-validation
                                           number = 5)) # Use 5 folds for cross-validation

#plot(RF_Final$finalModel)

RF_Final$finalModel

predictions_RF <- predict(RF_Final, newdata = testing[-9])

confusionMatrix(predictions_RF, testing$Outcome)


#Variable Importance Plots
varImp2 <- varImp(RF_Final, scale = FALSE)
Imp <- data.frame(varImp2[1])

#Storing in adapted format
Variable <- rownames(Imp)
rownames(Imp) <- NULL
Imp <- cbind(Variable,Imp)

#Caret variable importance separates by level, reaggregating to create variable importance plot
Outlet <- Imp %>%
  filter(str_detect(Variable, 'Outlet')) %>%
  summarize(sum(Overall)) %>% 
  as.numeric()

Agent <- Imp %>%
  filter(str_detect(Variable, 'Agent')) %>%
  summarize(sum(Overall)) %>% 
  as.numeric()

Country <- Imp %>%
  filter(str_detect(Variable, 'Country')) %>%
  summarize(sum(Overall)) %>% 
  as.numeric()

League <- Imp %>%
  filter(str_detect(Variable, 'League')) %>%
  summarize(sum(Overall)) %>% 
  as.numeric()

Position <- Imp %>%
  filter(str_detect(Variable, 'Position'))%>%
  summarize(sum(Overall))%>% 
  as.numeric()

Age <- Imp %>%
  filter(Variable == 'Age')%>%
  summarize(sum(Overall))%>% 
  as.numeric()

Join <- Imp %>%
  filter(Variable == 'Join')%>%
  summarize(sum(Overall))%>% 
  as.numeric()

MV <- Imp %>%
  filter(Variable == 'Market_Value')%>%
  summarize(sum(Overall))%>% 
  as.numeric()

temp <- data.frame (Variable = c("Outlet", "Agent", "Country", "League", "Position", "Age", "Join", "Market_Value"),
                  Overall = c(Outlet, Agent, Country, League, Position, Age, Join, MV ))

Graph <- temp %>%
  arrange(desc(Overall))

#Outlets importance score
Outlets <- Imp %>%
  filter(str_detect(Variable, 'Outlet')) %>%
  mutate(across('Variable', str_replace, 'Outlet', ''))%>%
  arrange(desc(Overall)) 

My_Theme = theme(
  panel.background = element_rect(fill = 'white', colour = 'black'),
  plot.title = element_text(hjust = 0.5, size = 16),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 15),
  axis.text.y = element_text(size = 15),
  axis.title.y = element_text(size = 16))


ggplot(data= Graph, aes(x=reorder(Variable, Overall),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + xlab("Variable")+ ylab("Score")+
  ggtitle("Random Forest Variable Importance Plot") + My_Theme


ggplot(data= Outlets, aes(x=reorder(Variable, Overall),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black") + 
  coord_flip() + geom_point(color='skyblue') + xlab("Outlet")+ ylab("Score")+
  ggtitle("Random Forest Variable Importance Plot for Outlets") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

#--------------Random Forest Class Weighted-------------#
set.seed(123)
#5 is best
RF_Class_Weighted <- randomForest(Outcome~., data = training, ntree = 500, mtry = 5) # ,classwt=c(0.001,1000), mtry = 5

# Predicting the Test set results
predictions_RF_Weighted <- predict(RF_Class_Weighted, newdata = testing[-9], type = 'class')

# Confusion Matrix
confusionMatrix(predictions_RF_Weighted , testing$Outcome)

# Plotting model
plot(RF_Class_Weighted)

# Variable importance plot
varImpPlot(RF_Class_Weighted)
#-------------- Partial Dependence Plots-------------#
partial(RF_Final, pred.var = "Outlet", plot= TRUE, prob = TRUE, which.class = 2)

library(dplyr)
tab <- partial(RF_Final, pred.var = "Outlet", prob = TRUE, which.class = 2)

tab %>%
  arrange(desc(yhat))

partial(RF_Final, pred.var = c("Agent"), which.class = 2, plot= TRUE, prob = TRUE)

MV <- partial(RF_Final, pred.var = c("Market_Value"),plot= TRUE, prob = TRUE, return.grid=TRUE, which.class = 2)
Stay <- partial(RF_Final, pred.var = c("Join"), plot= TRUE, prob = TRUE, return.grid=TRUE, which.class = 2)
Age <- partial(RF_Final, pred.var = c("Age"), plot= TRUE, prob = TRUE, return.grid=TRUE, which.class = 2)

grid.arrange(MV, Stay, Age)
