## Loan prediction part iii ##
library(glmnet)
library(randomForest)
library(xgboost)
train1 <-read.csv("/Users/Santhosh/Downloads/train_loan_prediction.csv",header = TRUE, na.strings = c('',' '),stringsAsFactors = FALSE)
test <- read.csv("/Users/Santhosh/Downloads/test_loan_prediction.csv",header = TRUE, na.strings = c('',' '),stringsAsFactors = FALSE)

# View the explore the dataset
str(train)
View(train)
# look at the class for each variable
sapply(train,class)
# look at unique values
sapply(train,unique)
nas <-is.na(train$Gender)
complete <- complete.cases(train)

# the below code gives you the sum of missing values for each column.
# this is really helpful
sapply(train,function(x) sum(is.na(x))) # or colSums(is.na(train))

#train <-test
# variable transformation--Dependents
train[train$Dependents == '3+' & is.na(train$Dependents)==FALSE,4] <-'3'
train$Dependents <- as.integer(train$Dependents)

# convert 2 class education variable into factors
if(length(unique(train$Education)) ==2) train$Education <-as.factor(train$Education)

# missing value computation

sapply(train[c(2,3,4)],function(x) table(x,useNA = 'ifany'))

# impute Gender missing values at random
train$Gender[is.na(train$Gender)] <- sample((train$Gender[!is.na(train$Gender)]),length(train$Gender[is.na(train$Gender)]))

# impute Married missing values at random
train$Married[is.na(train$Married)] <- sample((train$Married[!is.na(train$Married)]),length(train$Married[is.na(train$Married)]))

# impute Dependents missing value at random
train$Dependents[is.na(train$Dependents)] <- sample((train$Dependents[!is.na(train$Dependents)]),length(train$Dependents[is.na(train$Dependents)]))

# impute Self_Employed missing value at random
train$Self_Employed[is.na(train$Self_Employed)] <- sample((train$Self_Employed[!is.na(train$Self_Employed)]),length(train$Self_Employed[is.na(train$Self_Employed)]))

# impute median for missing values in LoanAmount
train$LoanAmount[is.na(train$LoanAmount)] <-median(train$LoanAmount,na.rm = TRUE)

# impute mode for missing values in Loan_Amount_Term
train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)] <- 360

# imputing value for Credit_History is very important
#train[train$Loan_Status == 'N' & is.na(train$Credit_History),11] <- 0
#train[train$Loan_Status == 'Y' & is.na(train$Credit_History),11] <- 1
train$Credit_History[is.na(train$Credit_History)] <- 0.5

# finally convert other variables into factors
train$Gender <-as.factor(train$Gender)
train$Married <- as.factor(train$Married)
train$Self_Employed <- as.factor(train$Self_Employed)
train$Property_Area <- as.factor(train$Property_Area)
train$Loan_Status <- as.factor(train$Loan_Status)
# train logistic model
model.logit <- glm(Loan_Status ~., data = train[-1], family = 'binomial')
predictions1 <-predict(model.logit,newdata = test[-1],type = 'response')

predictions1 <-ifelse(predictions1 > 0.49,'Y','N')
predictions1 <- as.factor(predictions1)
result1 <- cbind(test[1],data.frame(Loan_Status = predictions1))

# train randomForest model
model.forest <-randomForest(Loan_Status~., data = train[-1],ntree = 300)
predictions.forest <-predict(model.forest, newdata =test[-1])
result.forest <-cbind(test[1],data.frame(Loan_Status = predictions.forest))

# train xgboost model
label_change <- ifelse(train$Loan_Status == 'Y',1,0)
model.xg <- xgboost(data = data.matrix(train[-c(1,13)]),label = label_change, nrounds = 2, objective = "binary:logistic")
y_pred <- predict(model.xg, data.matrix(test[-1]))
predictions.xgb <-ifelse(y_pred > 0.60,'Y','N')
result.xgb <-cbind(test[1],data.frame(Loan_Status = predictions.xgb))

# rpart
model.rpart <-rpart(Loan_Status~., data = train[-1],parms = c('information'),control = rpart.control(cp=0.05))
predictions.rpart <-predict(model.rpart,newdata=test[-1])
predict_rpart <-ifelse(predi[1]<predi[2],'Y','N')
result.rpart <-cbind(test[1],data.frame(Loan_Status = (predict_rpart[1])))
# write the result to a file
write.table(result.rpart,file = 'submission_loan_rpart.csv',sep = ',')

# spread
library(tidyr)
head(spread(train1,Property_Area,Property_Area,fill = '0'))
train1$total_income <-train1$ApplicantIncome + train1$CoapplicantIncome
train1['percent_loan'] <- (train1['total_income']/(train1['LoanAmount']*1000))*100
