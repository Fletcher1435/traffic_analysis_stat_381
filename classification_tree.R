library(tree)
library(dplyr)
library(rpart.plot)

###Fit classification decision tree

#Split the data into test and training sets
train <- injury_data %>%
  sample_n(floor(nrow(injury_data)*0.7))

test <- injury_data %>%
  setdiff(train)

#Fit the Classification Decision Tree
tree_injury <- rpart(PEDESTRIAN_Injury.Status ~ ., train)
summary(tree_injury)

#Plot the tree
rpart.plot(tree_injury)
#text(tree_injury, pretty = 0)

#Evaluating the performance of the tree
test$tree_pred <- predict(tree_injury, test, type = "class")
table(test$tree_pred, test$PEDESTRIAN_Injury.Status)

#Prune the tree
#Plot the error rate as a function of size
plot(tree_injury$cptable[, "nsplit"],
     tree_injury$cptable[, "xerror"],
     type = "b")

#pruning 
cp_table <- tree_injury$cptable
best_cp <- cp_table[cp_table[, "nsplit"] == 2, "CP"][1]
prune_injury <- prune(tree_injury, cp = best_cp)
rpart.plot(prune_injury, type = 2, extra = 104)

#prune_injury <- prune.misclass(tree_injury, best = 3)
#plot(prune_injury)
#text(prune_injury, pretty = 0)

#Evaluate the Tree after pruning
test$tree_pred_pruned <- predict(prune_injury, test, type = "class")
table(test$tree_pred_pruned, test$PEDESTRIAN_Injury.Status)
