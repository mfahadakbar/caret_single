library(MASS)
library(caret)
set.seed(1234)
# this codes demonstrates how we can use caret package to apply multiple models , with cross validations
## we will use Boston data set from MASS library

##1 ) Creat Partition : test & train

intrain = createDataPartition(y=Boston$crim,times = 1,p = .70,list = F)
training = Boston[intrain,]
test= Boston[-intrain,]


##2) now we want to set our training parameters 
# we will select repeatedcv (cross validation ), number as 5 , cross validation will happen 5 times and  we will set returnResamp to true
# this will save the cross validation results for each cross validation
# now we can use cv or repeatedcv
# cv is simple, it will do cross validation, number arguments will let the also know how many folds to preform
# so if you have set cv , and number = 10, reg$resample will give you 10 RMSE results
# now some time, you need to 'repeat' the cv process multiple times , in that case you use 'repeatedcv'. you specify number and repeat
# say you have set repeatedcv, number= 10 and repeat = 3, then reg$resample will give you 30 results (10 folds , repeated 3 times)
# Random means you want your computer to randomly select hyper parameters for tuning

trainctr = trainControl(method = 'repeatedcv', number = 10, repeats = 1 , returnResamp = 'all', search = 'grid')


# lets use LESSO regression for this (glmnet)
# when we do things through caret , are there are models with tunable parameters, we need to do a grid search
# although it does the grid search for us, but we can specify the parameters our self
### alpha = 1 means lesso , alpha = 0 means ridge



### grid search to find out the best possible tuning parameters
## we can also use grid to pass on specific values if we know the parameters

# grid search example
#grid = expand.grid(ets=c(.01,.05,.1,.3), max_depth =c(2,3,4,5))

# specific parameter example
#grid = expand.grid(eta =.3 , max_depth = 3, gamma =0 ,colsample_bytree =.8, min_child_weight = 1 , subsample = 1 ,nrounds =150  )


reg = train(x = training[,-14],y = training$medv  ,method = 'lm', trControl = trainctr , metric = 'RMSE' , tuneLength = 3)
### metric : pay attention, this is the criteria for choosing the specific model across all the cross validation , and grid matrix
### so if you specify MAE, it will go select the model with lowest MAE and will send the results to final model along with its 
### hyper parameters
## tuenlength , here you decide that how many values of each hyper parameter you want to test, this increases your chance for 
## getting accurate tuingin, but it also slows down your computer considerably. Default tune length is three
### finding, even though you set tuenlenght to three , formula wont check all the HPs , eg. there may be just one value of HP1 , instead of
## 3 or 5 (whatever you set). Cant find a reason for that


# check summary
summary(reg)
varImp(reg)
# check Results, all these results are averages for you cross validation result, if you want to see individual cross validation result
# then see reg$resample
reg$results

#check the results of different cross validations for the model that you trained
reg$resample


# check the result with the minimum criteria
reg$results[which.min(reg$results$RMSE),]
reg.original = reg$finalModel
# Check whihc lamba was chosen 
reg.original$lambdaOpt
# both these line should give us the same result
reg$bestTune

plot(reg)
plot(reg.original)
write.csv(reg$resample,'regresample.csv')
