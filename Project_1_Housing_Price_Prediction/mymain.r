library(glmnet) # LASSO
library(Matrix) # LASSO
library(xgboost)# Xgboost

train = read.csv('train.csv')
test = read.csv('test.csv')

# ============== data clean train ==============

train <- train[train$GrLivArea<=4000,] # remove 5 unusual observations


train$MasVnrType[is.na(train$MasVnrType)]<-"None"

nalevel<-c("Alley", "BsmtQual","BsmtCond" ,"BsmtExposure","BsmtFinType1",
           "BsmtFinType2","FireplaceQu", "GarageType","GarageFinish" ,"GarageQual", 
           "GarageCond" ,"PoolQC" ,"Fence" ,"MiscFeature")
nalevelsf<-c("PoolArea" ,
             "GarageArea", 
             "TotalBsmtSF", 
             "BsmtFinSF1"
             ,"MasVnrArea" )
for(x in nalevel)
{
  levels(train[[x]])<-c(levels(train[[x]]),"None")
  train[[x]][is.na(train[[x]])]<-"None"
}
for(x in nalevelsf)
{
  train[[x]][is.na(train[[x]])]<-0
}
for(i in 1:ncol(train))
{
  if(is.factor(train[1,i])==TRUE && sum(is.na(train[,i]))!=0)
  {
    train[,i][is.na(train[,i])]<-names(which.max(table(train[i])))
  }
  if(is.factor(train[1,i])!=TRUE && sum(is.na(train[,i]))!=0)
  {
    train[,i][is.na(train[,i])]<-mean(train[!is.na(train[,i]),i])
  }
}


# ============== train log transform ============== 


train$SalePrice<-log(train$SalePrice+1)


# ================ data clean test ================


test$MasVnrType[is.na(test$MasVnrType)]<-"None"

for(x in nalevel)
{
  levels(test[[x]])<-c(levels(test[[x]]),"None")
  test[[x]][is.na(test[[x]])]<-"None"
}
for(x in nalevelsf)
{
  test[[x]][is.na(test[[x]])]<-0
}
for(i in 1:ncol(test))
{
  if(is.factor(test[1,i])==TRUE && sum(is.na(test[,i]))!=0)
  {
    test[,i][is.na(test[,i])]<-names(which.max(table(test[i])))
  }
  if(is.factor(test[1,i])!=TRUE && sum(is.na(test[,i]))!=0)
  {
    test[,i][is.na(test[,i])]<-mean(test[!is.na(test[,i]),i])
  }
}



#================= selected lm ======================= 

usefulvar = c("MSZoning", "LotArea","OverallQual", "OverallCond", "YearBuilt","BsmtFinSF1",
              "TotalBsmtSF","GrLivArea","BsmtFullBath","KitchenAbvGr","KitchenQual",
              "YearRemodAdd","Fireplaces","GarageCars","ScreenPorch","SaleCondition")
# BIC
lmmodel<-lm(SalePrice~.,data=train[c(usefulvar,"SalePrice")])
y_lm<-predict(lmmodel,newdata = test)


# ================== Bagging LASSO ==================== 

n<- floor(3*nrow(train)/5)
r<- 200
y_round_LASSO<-rep(0,nrow(test))

for (i in 1:r)
{
  round_train<-train[sample(nrow(train))[1:n],]
  lassoX = data.matrix(round_train[,-c(1,81)])
  lassoY = data.matrix(round_train[,81])
  # select lambda
  lam.seq =  exp(seq(-4, 2, length=200))
  lasso_cv_out = cv.glmnet(lassoX, lassoY, alpha = 1, lambda = lam.seq)
  
  mylasso = glmnet(lassoX,lassoY, alpha = 1)
  best_lambda = lasso_cv_out$lambda.1se
  #refit
  mylasso.coef = predict(mylasso, s = best_lambda, type = "coefficients")
  varLASSO = rownames(mylasso.coef)[which(mylasso.coef != 0)]
  newlassoX = lassoX[, colnames(lassoX) %in% varLASSO]
  lm_lasso = lm(lassoY ~ newlassoX)
  
  yLASSOre = coef(lm_lasso)[1] + data.matrix(test[,colnames(test) %in% varLASSO]) %*% coef(lm_lasso)[-1]
  
  y_round_LASSO<-y_round_LASSO+yLASSOre
  
}

y_BaLasso<-y_round_LASSO/r


# ================== Xgboost ==================== 
temp_train<-train
temp_test<-test

y_train=train$SalePrice
train$Id=NULL
train$SalePrice=NULL
test$Id=NULL

#Row binding train & test set for feature engineering
train_test = rbind(train, test)
ntrain=nrow(train)

features=names(train)

#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(train_test[[f]]))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

train_x=train_test[1:ntrain,]
test_x=train_test[(ntrain+1):nrow(train_test),]

train_x[] <- lapply(train_x, as.numeric)
test_x[]<-lapply(test_x, as.numeric)

dtrain=xgb.DMatrix(as.matrix(train_x),label= y_train)
dtest=xgb.DMatrix(as.matrix(test_x))

#xgboost parameters
xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=1500

#train data
gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
y_xg<-predict(gb_dt,dtest)



# ================== Write ==================== 

train<-temp_train
test<-temp_test

temp = read.csv('test.csv')

result_xg = exp(y_xg)-1
result_BaLasso = exp(y_BaLasso)-1
result_lm = exp(y_lm)-1

filexg = data.frame( Id = temp$Id, SalePrice = result_xg )
fileBaLasso = data.frame( Id = temp$Id, SalePrice = result_BaLasso)
filelm = data.frame( Id = temp$Id, SalePrice = result_lm)

# write.csv(filexg, "result_xgboost.csv", row.names = FALSE)
# write.csv(fileBaLasso, "karesult_BaLasso.csv", row.names = FALSE)
# write.csv(filelm, "karesult_lm.csv", row.names = FALSE)

write.table(filexg,"mysubmission3.txt",row.names = FALSE, sep = ',')
write.table(fileBaLasso,"mysubmission2.txt",row.names = FALSE,sep = ',')
write.table(filelm,"mysubmission1.txt",row.names = FALSE,sep = ',')




















