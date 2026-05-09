#Fit a logistic regression model with only the the two predictors identified 
logit1 <- glm(PEDESTRIAN_Injury.Status ~ PEDESTRIAN_Location.At.Time.Of.Crash + PEDESTRIAN_Actions.Prior.to.Crash, data = injury_data, family = binomial)
#Fit a logistic regression model that includes some of the predictors identified by the random forrest
logit2 <- glm(PEDESTRIAN_Injury.Status ~ PEDESTRIAN_Location.At.Time.Of.Crash + PEDESTRIAN_Actions.Prior.to.Crash + CRASH_Month + CRASH_Year, data = injury_data, family = binomial)
anova(logit1, logit2, test="LRT")

#Fit a tree to explore this relationship
tree_month <- rpart(PEDESTRIAN_Injury.Status ~ CRASH_Year, injury_data)
rpart.plot(tree_month, extra = 104)
