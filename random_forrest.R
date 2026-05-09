library(randomForest)
library(ggplot2)

#Fit the model
set.seed(1)
rf_injury <- randomForest(PEDESTRIAN_Injury.Status~., 
                          data = train,
                          importance = TRUE)
#Predictions using the test data
test$random_forest_estimate <- predict(rf_injury, newdata = test)
table(test$random_forest_estimate, test$PEDESTRIAN_Injury.Status)

#Report the importance of the variables
importance(rf_injury)
#variable importance plot
varImpPlot(rf_injury)
