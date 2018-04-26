
library(dplyr)
library(caret)
library(skimr)
setwd('C:/Users/sprot/Documents/GitHub/dont_overfit')
rawData = read.csv('overfitting.csv')
train = rawData[1:250,]

#check for correlation with target--------------------
skim(train)

corTable = cor(train)
corDf = data.frame(corTable)
corDf2 = corDf %>%
  mutate(varName = row.names(.),
         absTarget = abs(Target_Practice)) %>%
           arrange(desc(absTarget))

absThreshold = 0.07
corDf3 = corDf2 %>%
  filter(absTarget > absThreshold) #down to 85 vars

varsToKeep = corDf3$varName

dataReduced = train %>%
  select(varsToKeep)

varsOnly = dataReduced %>%
  select(starts_with('var'))

#check for covariance between vars----------------
corVarsOnly = cor(varsOnly)
corVarsDf = data.frame(corVarsOnly)


#k-fold cross validation-------------------------StackOver ex
# define training control
# train_control <- trainControl(method="cv", number=10)
# # fix the parameters of the algorithm
# grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# # train the model
# model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# # summarize results
# print(model)

myTrainControl = trainControl(method="cv", number=10)
grid = expand.grid(.fL=c(0), .usekernel=c(FALSE))

train = train %>%
  mutate(Target_Practice = as.factor(Target_Practice)) #factorize 0 and 1 for functions to work properly

?train
model = train(Target_Practice~., data=train, trControl=myTrainControl, method='logicBag', tuneGrid)

#random forest------------------