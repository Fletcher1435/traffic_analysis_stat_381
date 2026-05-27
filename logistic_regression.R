#Fit a logistic regression model with only the the two predictors identified 
logit1 <- glm(PEDESTRIAN_Injury.Status ~ CRASH_Month + CRASH_Year, data = train, family = binomial)

logit2 <- glm(PEDESTRIAN_Injury.Status ~ 1, data = train, family = binomial)
anova(logit1, logit2, test="LRT")

logit3 <- glm(PEDESTRIAN_Injury.Status ~ CRASH_Month, family = binomial, data = train)

logit4 <- glm(PEDESTRIAN_Injury.Status ~ CRASH_Year, family = binomial, data = train)

logit5 <- glm(data = train, PEDESTRIAN_Injury.Status ~ CRASH_Year + PEDESTRIAN_Actions.At.Time.Of.Crash, family = binomial)

logit6 <- glm(data = train, PEDESTRIAN_Injury.Status ~ CRASH_Year + PEDESTRIAN_Actions.At.Time.Of.Crash + PEDESTRIAN_Actions.At.Time.Of.Crash*CRASH_Year, family = binomial)

logit7 <- glm(data = train, PEDESTRIAN_Injury.Status ~ PERIOD, family = binomial)

#LRT tests
anova(logit1, logit2, test="LRT") #Are crash month or crash year associated with pedestrian injury status
anova(logit3, logit2, test="LRT") #Is crash month associated with pedestrian injury status
anova(logit4, logit2, test ="LRT") #Is crash year associated with pedestrian injury status
anova(logit5, logit6, test = "LRT") #Is there an interaction between pedestrian action at time of crash and crash year
anova(logit7, logit2, test = "LRT") #Is there an associated between PERIOD and pedestrian injury status 
tree_injury <- rpart(PEDESTRIAN_Injury.Status ~ CRASH_Year, train)

plot(tree_injury$cptable[, "nsplit"],
     tree_injury$cptable[, "xerror"],
     type = "b")

cp_table <- tree_injury$cptable
best_cp <- cp_table[cp_table[, "nsplit"] == 2, "CP"][1]
prune_injury <- prune(tree_injury, cp = best_cp)
rpart.plot(prune_injury, type = 2, extra = 104)


