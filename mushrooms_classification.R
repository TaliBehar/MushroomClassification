# Choose Your Own Project - Mushroom classification

## Data Preperation

## 1. Installing required packeges

if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(plot.matrix)) install.packages("plot.matrix", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(readr) # for reading csv file 
library(tidyverse)
library(caret)
library(gridExtra)
library(knitr)
library(plot.matrix) # for matrix plotting 
library(rpart) # for fitting decision trees
library(rpart.plot) # for decision trees plotting
library(ggplot2)
library(randomForest) # for random forest

## 2. Loading Mushroom classification Dataset

# Importing the data - read csv file 
# TODO: change to local file
mushrooms <- read.csv("D:/OneDrive/Documents/Tali/Tali data science studies/MushroomClassification/mushrooms.csv")

## 3. Get A Glimpse Of Mushroom classification Dataset

# The datadet contains 8k rows and 23 columns
tibble("class"=class(mushrooms), "nrow"=nrow(mushrooms), "ncol"=ncol(mushrooms)) %>%
  knitr::kable()

# The first row represent the class of the mushroom, while rows 2-23 represents
# he mushrooms chcharacteristics. Moving forward, we'll learn about the mushrooms chcharacteristics
# and their influence on the mashroom's claclassification
glimpse(mushrooms)

# The next table summarize  the variables class and levels
cbind.data.frame("mushrooms characteristics"= names(mushrooms),
                 "class"= sapply(mushrooms,(function(x){class(x)})), 
                 "levels"= sapply(mushrooms,function(x){nlevels(x)})) %>% 
  mutate(char = rownames(.)) %>% 
  select(-char)%>%
  knitr::kable()
# <TODO> look for NA's 

# veil.type has only 1 level, which means not contributing info as a feature; we'll remove it
mushrooms$veil.type<-NULL

# Mushrooms type - we'll distinguish between edible to poisonous 
mushrooms %>% 
  group_by(class) %>% 
  summarize (count=n())%>%
  mutate(prop=100*count/sum(count)) %>%
  setNames(c("Mushroom type","Count", "Proportion (%)"))

# Summary of the variables and thier count
mushrooms %>%
  summary()

#----------------------------------
## 4. Create training and test sets

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# Test set will be 20% of mushrooms data
test_index <- createDataPartition(y = mushrooms$class, times = 1, p = 0.2, list = FALSE)

train_set <- mushrooms %>% slice(-test_index)
test_set <- mushrooms %>% slice(test_index)
# Remove test_index
rm(test_index)

# Define the outcome (class of the mushrooms) = y  
y <- train_set$class

# Restore the true y from test set
y_test <- test_set$class

# Remove the outcome from training and test set
test_set <- test_set %>% select(-class)
train_set <- train_set %>% select(-class)
#---------------------------------------------
## 5. Mushroom classification Dataset Overview 
# (by train_set)

# cap characteristics
# figure 1 # 
cap_char <- 
  lapply(names(train_set[,1:4]),function(x){
    train_set %>%
      ggplot(aes_string(y,x,fill=y,color=y))+
      geom_jitter()+
      theme(legend.position = "none")
})
# plot all 4 cap figures in 2 columns and 2 rows 
grid.arrange(cap_char[[1]],cap_char[[2]],cap_char[[3]],cap_char[[4]],
             ncol=2, nrow=2)

# gill characteristics
# figure 2 # 
gill_char <- 
  lapply(names(train_set[,6:9]),function(x){
    train_set %>%
      ggplot(aes_string(y,x,fill=y,color=y))+
      geom_jitter()+
      theme(legend.position = "none")
})
# plot all 4 gill figures in 2 columns and 2 rows 
grid.arrange(gill_char[[1]],gill_char[[2]],gill_char[[3]],gill_char[[4]],
             ncol=2, nrow=2)

# stalk characteristics
# figure 3 #
stalk_char <- 
  lapply(names(train_set[,10:15]),function(x){
    train_set %>%
      ggplot(aes_string(y,x,fill=y,color=y))+
      geom_jitter()+
      theme(legend.position = "none")
})
# plot all 5 stalk figures in 2 columns and 3 rows 
grid.arrange(stalk_char[[1]],stalk_char[[2]],stalk_char[[3]],
             stalk_char[[4]],stalk_char[[5]],
             ncol=2, nrow=3)

# veil characteristics
# figure 4 #
veil_ring_char <- 
  lapply(names(train_set[,16:18]),function(x){
    train_set %>%
      ggplot(aes_string(y,x,fill=y,color=y))+
      geom_jitter()+
      theme(legend.position = "none")
  })
# plot all 3 veil figures in 2 columns and 2 rows 
grid.arrange(veil_ring_char[[1]],veil_ring_char[[2]],
             veil_ring_char[[3]], ncol=2, nrow=2)

# miscellaneous
# figure 5 #
other <- 
  lapply(names(train_set[,c(5,19:21)]),function(x){
    train_set %>%
      ggplot(aes_string(y,x,fill=y,color=y))+
      geom_jitter()+
      theme(legend.position = "none")
  })
# plot 4 assorted figures in 2 columns and 2 rows 
grid.arrange(other[[1]],other[[2]],other[[3]],other[[4]], ncol=2, nrow=2)

#----------------------------------------------
## 6. Relationships and correlation in the data 

# Check the relationships between the characteristics by performing chi^2 test
index <- expand.grid(1:21,1:21) 
chi2_test_for_char <- 
  sapply(1:nrow(index), function(i){
  n <- index[i,1]
  m <- index[i,2]
  chisq.test(train_set[,n],train_set[,m],
             simulate.p.value = TRUE, B = 1000)$p.value # B= number of monte-carlo simulations 
}) %>% 
  matrix(nrow=21, byrow = FALSE)

# The characteristics correlated with each other in case p.value < 0.05
# plot the p.value matrix 
# figure 6 #
plot(chi2_test_for_char, 
     col=c("white", "lightblue"), # "white" - p.value < 0.05 (the char are correlated)
     breaks=c(0, 0.05, 1),
     key=list(side=3, cex.axis=0.6), 
     digits=3, text.cell=list(cex=0.6))

# Check the relationships between the characteristics to class by performing chi^2 test
chi2_test_for_class <- 
  sapply(1:21, function(i){
  chisq.test(train_set[,i],y,
  simulate.p.value = TRUE, B = 1000)$p.value # B= number of monte-carlo simulations 
}) %>% 
  matrix(ncol=21, byrow = FALSE)

# The characteristics correlated with the mushroom class in case p.value < 0.05
# plot the p.value matrix 
# figure 7 #  
plot(chi2_test_for_class, 
     col=c("white", "lightblue"), # "white" - p.value < 0.05 (the char are correlated)
     breaks=c(0, 0.05, 1),
     key=list(side=3, cex.axis=0.6), 
     digits=3, text.cell=list(cex=0.6))

#-------------------------
## 7. Methods and analysis

# Model 1 - Random Forest

# Train rf model and find best parameters

train_control <- trainControl(method="cv", number = 10, p = 0.8) # use 10-folds cross-validation 
tune_grid <- data.frame(mtry =  c(1:5)) # find the best value for tuning mtry
# Train using "caret"
train_rf <- train(train_set, y,
                  method = "rf",
                  ntree = 200, 
                  trControl = train_control,
                  tuneGrid = tune_grid)
# Find best mtry parameter
train_rf$bestTune
# Plot the mtry selection by its accuracy
# figure 8#
plot(train_rf)

# Fit the model with best parameter 
# Fit using "randomForest"
fit_rf <- randomForest(train_set, y,
                       ntree = 200, 
                       nodesize = train_rf$bestTune$mtry)
# Plot ntree 
# figure 9#
plot(fit_rf)

# Predict the outcome  
y_hat_rf <- predict(fit_rf, test_set, type = "class")

# Report accuracy 
cm_rf <- confusionMatrix(y_hat_rf, y_test)
cm_rf
model_1_accuracy <- cm_rf$overall["Accuracy"]

# variable importhance list - rf
varImp(fit_rf) %>%
  mutate(char = rownames(.)) %>% 
  arrange(desc(varImp(fit_rf)$"Overall"))

# plot variable importhance - rf
varImpPlot(fit_rf)  

# Graph variable importhance - rf
# figure 10#
varImp(fit_rf)%>%
  mutate(char = rownames(.))%>%
  ggplot(aes(x = reorder(char,Overall),y = Overall))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Variable importance for random forest model", 
       x = "characteristics", y = "variable importance")

#------------------------------------------
# Model 2 - Classification (decision) trees

# (https://www.edureka.co/blog/implementation-of-decision-tree/) 
# http://www.milbo.org/rpart-plot/prp.pdf # rpart plots examples
# https://csantill.github.io/RTuningModelParameters/
# add explenation on cp from 31.10.3 (p.589)

# Train ct model and find best parameters

train_control <- trainControl(method="cv", number = 10, p = 0.8) # use 10-folds cross-validation 
tune_grid <- data.frame(cp = seq(0.0, 0.2, len=30)) # find the best value for cp
# Train using "caret"
train_ct <- train(train_set, y, 
                  method = "rpart", 
                  tuneLength = 6,
                  trControl = train_control,
                  tuneGrid = tune_grid)
# Find best cp value
train_ct$bestTune

# Plot the cp selection by itr accuracy
# figure 11#
plot(train_ct)

# Fit the model with best parameter 
# Fit using "rpart"
fit_ct <- rpart(y ~ ., 
                data = train_set, 
                method = "class", 
                control = rpart.control(cp = train_ct$bestTune$cp , minsplit = 20))
# Plot size of the tree (nodes) by cp
# figure 12#
plotcp(fit_ct)

# Plot the tree 
# figure 13#
rpart.plot(x = fit_ct, type =5, extra = 100, 
           box.palette = c("lightblue","orangered"), 
           fallen.leaves=TRUE, tweak = 2)
# Predict the outcome
y_hat_ct <- predict(fit_ct, test_set, type = "class")

# Report accuracy 
cm_ct <- confusionMatrix(y_hat_ct, y_test)
cm_ct
model_2_accuracy <- cm_ct$overall["Accuracy"]

# variable importhance list - ct
varImp(fit_ct) %>%
  mutate(char = rownames(.)) %>% 
  arrange(desc(varImp(fit_ct)$"Overall"))

# Graph variable importhance - ct
# figure 14#
varImp(fit_ct)%>%
  mutate(char = rownames(.))%>%
  ggplot(aes(x = reorder(char,Overall),y = Overall))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Variable importance for classification trees model", 
       x = "characteristics", y = "variable importance")

#----------------------------
# Model 2.1 - Classification (decision) trees - tuned by rf var importance

# create new dataset, eliminate the less important char by rf
new_mashrooms <- 
  mushrooms %>% 
  select(-gill.attachment,-veil.color,
         -cap.shape,-cap.surface,-stalk.shape,
         -ring.number,-stalk.color.below.ring,
         -stalk.color.above.ring,-gill.spacing)

# split the new data into training and test sets 
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

# Test_new will be 20% of new_mashrooms data
new_test_index <- createDataPartition(y = new_mashrooms$class, times = 1, p = 0.2, list = FALSE)

train_new <- new_mashrooms %>% slice(-new_test_index)
test_new <- new_mashrooms %>% slice(new_test_index)
# Remove test_index
rm(new_test_index)

# Define the outcome (class of the mushrooms) = y_new  
y_new <- train_new$class

# Restore the true y from test_new
y_test_new <- test_new$class

# Remove the outcome from training and test_new
test_new <- test_new %>% select(-class)
train_new <- train_new %>% select(-class)

# Train tuned ct model and find best parameters

# Train using "caret"
train_control <- trainControl(method="cv", number = 10, p = 0.8)
tune_grid <- data.frame(cp = seq(0.0, 0.2, len=30))
train_ct_tuned <- train(train_new, y_new, 
                  method = "rpart", 
                  tuneLength = 6,
                  trControl = train_control,
                  tuneGrid = tune_grid)
# Find best cp value
train_ct_tuned$bestTune

# Fit the model with best parameter 
# Fit using "rpart"
fit_ct_tuned <- rpart(y_new ~ ., 
                data = train_new, 
                method = "class", 
                control = rpart.control(cp = train_ct_tuned$bestTune$cp , minsplit = 20))

# Plot size of the tree (nodes) by cp
# figure 15#
plotcp(fit_ct_tuned)

# Plot the tree 
# figure 16#
rpart.plot(x = fit_ct_tuned, type =5, extra = 100, 
           box.palette = c("lightblue","orangered"), 
           fallen.leaves=TRUE, tweak = 2)
# Predict the outcome
y_hat_ct_tuned <- predict(fit_ct_tuned, test_new, type = "class")

# Report accuracy 
cm_ct_tuned <- confusionMatrix(y_hat_ct_tuned, y_test_new)
cm_ct_tuned
model_2.1_accuracy <- cm_ct_tuned$overall["Accuracy"]

# variable importhance list - ct tuned
varImp(fit_ct_tuned) %>%
  mutate(char = rownames(.)) %>% 
  arrange(desc(varImp(fit_ct_tuned)$"Overall"))

# Graph variable importhance - ct tuned
# figure 17#
varImp(fit_ct_tuned)%>%
  mutate(char = rownames(.))%>%
  ggplot(aes(x = reorder(char,Overall),y = Overall))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Variable importance for classification trees - tuned model", 
       x = "characteristics", y = "variable importance")

# no change
# remove the next objects
rm(test_new, train_new, y_new, y_test_new)

#---------------------------
# Model 3 - k-nearest neighbors 

# creating a new data frame with just the first column of mushroom class (poison vs. edible)
mushrooms_break_down <- mushrooms[1:1]

# removing it from the original data frame as we don't want to iterate over it by mistake
mushrooms$class <- NULL

# iterating the column names in the original data frame (without the class column)
for (i in colnames(mushrooms)) {
  print(paste('going over column:', i))
  # Extracting factor levels from mushroom. Using [[]] in order to extract it as factor and not subset it as a dataframe.
  # more here:  https://rpubs.com/tomhopper/brackets and https://www.datamentor.io/r-programming/data-frame/
  # Note that we can't use '$' e.g. mushrooms$i as this format expects a real static name
  current_column_levels <- levels(mushrooms[[i]])
  print(paste('found levels:', list(current_column_levels)))
  for (j in levels(mushrooms[[i]])) {
    print(paste('adding column for level:', j))
    next_column_name = paste(i, '.', j, sep = "")
    next_column <- ifelse(mushrooms[i] == j,1,0)
    mushrooms_break_down[next_column_name] <- next_column
  }
}

head(mushrooms_break_down[,1:8],15)

# split the new data into training and test sets 

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = mushrooms_break_down$class, times = 1, p = 0.2, list = FALSE)

train_set <- mushrooms_break_down %>% slice(-test_index)
test_set <- mushrooms_break_down %>% slice(test_index)

rm(test_index)

# define the outcome (class of the mushrooms) = y  
y <- train_set$class

# restore the true y from test set
y_test <- test_set$class

# remove the outcome from training and test set
test_set <- test_set %>% select(-class)
train_set <- train_set %>% select(-class)

# Train knn model and find best parameters 
set.seed(201, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(201)`
train_control <- trainControl(method="cv", number = 5, p = 0.8)
tune_grid <- data.frame(k = seq(1, 15, by=1))

set.seed(74, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(74)`
# Train knn model using "caret"
train_knn <- train(train_set, y, 
                   method = "knn", 
                   trControl = train_control,
                   tuneGrid = tune_grid)
# plot the selection of k by its accuracy
# figure 18 # 
plot(train_knn)

# find optimal k
train_knn$bestTune

# fit the model with best parameter using "caret"
fit_knn <- knn3(train_set, y, 
                k = train_knn$bestTune$k)

# predict the outcome
y_hat_knn <- predict(fit_knn, test_set, type = "class")

# Report accuracy 
cm_knn <- confusionMatrix(y_hat_knn, y_test)
cm_knn
model_3_accuracy <- cm_knn$overall["Accuracy"]

# increase the k to avoid overfitting 
set.seed(74, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(74)`
cm_knn_bigger_k <- knn3(train_set, y, k = 6)

# predict the outcome
cm_knn_bigger_k <- predict(cm_knn_bigger_k, test_set, type = "class")

# Report accuracy 
cm_knn_bigger_k <- confusionMatrix(cm_knn_bigger_k, y_test)
cm_knn_bigger_k
model_3.1_accuracy <- cm_knn_bigger_k$overall["Accuracy"]

#---------------------------
# summary of decision and rf 
cbind.data.frame("rf"= varImp(fit_rf) %>%
                   mutate(char = rownames(.)) %>% 
                   arrange(desc(varImp(fit_rf)$"Overall")) %>%
                   slice(1:10), 
                 "ct"= varImp(fit_ct) %>%
                   mutate(char = rownames(.)) %>% 
                   arrange(desc(varImp(fit_ct)$"Overall")) %>%
                   slice(1:10),
                 "ct_tuned"= varImp(fit_ct_tuned) %>%
                   mutate(char = rownames(.)) %>% 
                   arrange(desc(varImp(fit_ct_tuned)$"Overall")) %>%
                   slice(1:10)) %>%
  knitr::kable()

# summary of the models accuracy
rbind("Random Forest" = model_1_accuracy,
      "Classification Trees"= model_2_accuracy,
      "Classification Trees - tuned" = model_2.1_accuracy, 
      "Knn" = model_3_accuracy, 
      "Knn - bigger k" = model_3.1_accuracy) %>% 
knitr::kable()

