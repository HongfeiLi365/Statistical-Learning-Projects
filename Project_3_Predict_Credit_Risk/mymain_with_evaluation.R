library(zoo) # for preproccessing (yearmon)
library(randomForest) # for randomForest
library(xgboost)  # for xgboost

# ======================================
#           Common preprocessing 
# ======================================

getid = function(x){
  # get test id
  id = x$id
  return(id)
}

getindex = function(x){
  # get index of recoveries
  recind = which(x$recoveries!=0)
  return(recind)
}

keepvar<-function(x){
  #function to keep useful variables
  keep<-c(   "loan_amnt"
             ,"funded_amnt"
             ,"funded_amnt_inv"
             ,"term"
             ,"int_rate"
             ,"installment"
             ,"grade"
             ,"emp_length"
             ,"home_ownership"
             ,"annual_inc"
             ,"verification_status"
             ,"issue_d"
             ,"label"
             
             ,"purpose"
             ,"addr_state"
             ,"dti"
             ,"delinq_2yrs"
             ,"earliest_cr_line"
             ,"inq_last_6mths"
             ,"mths_since_last_delinq"
             ,"mths_since_last_record"
             ,"open_acc"
             ,"pub_rec"
             ,"revol_bal"
             ,"revol_util"
             ,"total_acc"
             ,"initial_list_status"
             ,"out_prncp"
             ,"out_prncp_inv"
             ,"total_pymnt"
             ,"total_pymnt_inv"
             ,"total_rec_prncp"
             ,"total_rec_int"
             ,"total_rec_late_fee"
             ,"recoveries"
             ,"collection_recovery_fee"
             ,"last_pymnt_d"
             ,"last_pymnt_amnt"
             ,"next_pymnt_d"
             ,"last_credit_pull_d"
             ,"collections_12_mths_ex_med"
             ,"application_type"
             ,"acc_now_delinq"
             ,"tot_coll_amt"
             ,"tot_cur_bal"
             ,"total_rev_hi_lim"
  )
  traink<-x[,keep]
  return(traink)
}

keepvartest<-function(x){
  #function to keep useful variables
  keep<-c(   "loan_amnt"
             ,"funded_amnt"
             ,"funded_amnt_inv"
             ,"term"
             ,"int_rate"
             ,"installment"
             ,"grade"
             ,"emp_length"
             ,"home_ownership"
             ,"annual_inc"
             ,"verification_status"
             ,"issue_d"
             ,"purpose"
             ,"addr_state"
             ,"dti"
             ,"delinq_2yrs"
             ,"earliest_cr_line"
             ,"inq_last_6mths"
             ,"mths_since_last_delinq"
             ,"mths_since_last_record"
             ,"open_acc"
             ,"pub_rec"
             ,"revol_bal"
             ,"revol_util"
             ,"total_acc"
             ,"initial_list_status"
             ,"out_prncp"
             ,"out_prncp_inv"
             ,"total_pymnt"
             ,"total_pymnt_inv"
             ,"total_rec_prncp"
             ,"total_rec_int"
             ,"total_rec_late_fee"
             ,"recoveries"
             ,"collection_recovery_fee"
             ,"last_pymnt_d"
             ,"last_pymnt_amnt"
             ,"next_pymnt_d"
             ,"last_credit_pull_d"
             ,"collections_12_mths_ex_med"
             ,"application_type"
             ,"acc_now_delinq"
             ,"tot_coll_amt"
             ,"tot_cur_bal"
             ,"total_rev_hi_lim"
  )
  traink<-x[,keep]
  return(traink)
}


addlev<-function(x){
  # function to add level for last_pymnt_d,next_pymnt_d,last_credit_pull_d
  levels(x$last_pymnt_d)[levels(x$last_pymnt_d)==""]<-"NoneDate"
  levels(x$next_pymnt_d)[levels(x$next_pymnt_d)==""]<-"NoneDate"
  levels(x$last_credit_pull_d)[levels(x$last_credit_pull_d)==""]<-"NoneDate"
  return(x)
}

addmiss<-function(x){
  # replace numeric missing value by median
  miss<-c(
    "annual_inc"
    ,"delinq_2yrs"
    ,"inq_last_6mths"
    ,"open_acc"
    ,"pub_rec"
    ,"revol_util"
    ,"total_acc"
    ,"collections_12_mths_ex_med"
    ,"acc_now_delinq"
    ,"tot_coll_amt"
    ,"tot_cur_bal"
    ,"total_rev_hi_lim")
  for(i in miss)
  {
    me<-median(x[,i],na.rm = TRUE)
    x[,i][is.na(x[,i])]<-me
  }
  return(x)
}

addearly = function(x){
  # turn earliest_cr_line to Date, replace missing by median 
  x$earliest_cr_line<-as.character(x$earliest_cr_line)
  x$earliest_cr_line<-as.yearmon(x$earliest_cr_line,"%b-%Y")
  x$earliest_cr_line<-as.Date(x$earliest_cr_line)
  me<-median(x$earliest_cr_line,na.rm = TRUE)
  x$earliest_cr_line[is.na(x$earliest_cr_line)]<-me
  return(x)
}

addmth = function(mydata){
  # process mths_since_last_delinq and mths_since_last_record, recode NA into newlevel: NoneDate
  mydata$mths_since_last_delinq<-as.factor(ceiling(mydata$mths_since_last_delinq/12))
  leveldelinq<-levels(mydata$mths_since_last_delinq)
  leveldelinq[length(leveldelinq)]<-"NoneDate"
  levels(mydata$mths_since_last_delinq)<-leveldelinq
  mydata[is.na(mydata$mths_since_last_delinq),"mths_since_last_delinq"]<-"NoneDate"
  
  mydata$mths_since_last_record<-as.factor(ceiling(mydata$mths_since_last_record/12))
  levelrecord<-levels(mydata$mths_since_last_record)
  levelrecord[length(levelrecord)]<-"NoneDate"
  levels(mydata$mths_since_last_record)<-levelrecord
  mydata[is.na(mydata$mths_since_last_record),"mths_since_last_record"]<-"NoneDate"
  return(mydata)
}

addlabel = function(loan){
  # add new column label to dataframe
  label<-sapply(loan$loan_status,function(x){ifelse(x%in%c("Default","Charged Off","Late (16-30 days)","Late (31-120 days)","Does not meet the credit policy. Status:Charged Off"),'1','0')})
  names(label)<-NULL
  label<-as.factor(label)
  loan$label = label
  return(loan)
}


# ======================================
#     Random forest preprocessing 
# ======================================

drop4Date =function(loan){
  # drop 4 Date columns before randomforest
  keep<-c(   "loan_amnt"
             ,"funded_amnt"
             ,"funded_amnt_inv"
             ,"term"
             ,"int_rate"
             ,"installment"
             ,"grade"
             ,"emp_length"
             ,"home_ownership"
             ,"annual_inc"
             ,"verification_status"
             ,"label"
             ,"purpose"
             ,"addr_state"
             ,"dti"
             ,"delinq_2yrs"
             ,"earliest_cr_line"
             ,"inq_last_6mths"
             ,"mths_since_last_delinq"
             ,"mths_since_last_record"
             ,"open_acc"
             ,"pub_rec"
             ,"revol_bal"
             ,"revol_util"
             ,"total_acc"
             ,"initial_list_status"
             ,"out_prncp"
             ,"out_prncp_inv"
             ,"total_pymnt"
             ,"total_pymnt_inv"
             ,"total_rec_prncp"
             ,"total_rec_int"
             ,"total_rec_late_fee"
             ,"recoveries"
             ,"collection_recovery_fee"
             ,"last_pymnt_amnt"
             ,"collections_12_mths_ex_med"
             ,"application_type"
             ,"acc_now_delinq"
             ,"tot_coll_amt"
             ,"tot_cur_bal"
             ,"total_rev_hi_lim"
  )
  loan = loan[,keep]
  return(loan)
}

# ======================================
# ======================================
# =                Main                =
# ======================================
# ======================================
train = read.csv("train.csv")
test = read.csv("test.csv")


testid = getid(test)
recind = getindex(test)

newtrain = addlev(train)
newtrain = addmiss(newtrain)
newtrain = addearly(newtrain)
newtrain = addmth(newtrain)
newtrain = addlabel(newtrain)
newtrain = keepvar(newtrain)

newtest = addlev(test)
newtest = addmiss(newtest)
newtest = addearly(newtest)
newtest = addmth(newtest)
newtest = keepvartest(newtest)

test = addlabel(test)

# ======================================
#     [Model I]  Random forest 
# ======================================

start.time = Sys.time()

rftrain = drop4Date(newtrain)

rfModel = randomForest(label~.,data=rftrain, mtry = 7, ntree=400 )
rfPred = predict(rfModel, newtest ,type = "prob")
rfProb = rfPred[,"1"]
rfProb[recind] = 1

end.time = Sys.time()
run.time1 = as.numeric(difftime(end.time, start.time, units = 'sec'))


# write file
rfdataframe = data.frame(id = testid, prob = rfProb)
write.table(rfdataframe,"mysubmission1.txt", row.names = FALSE, sep = ',')


# ======================================
#        [Model II] xgboost 
# ======================================

start.time = Sys.time()

xgmodel<-function(){
  #train_nore<-newtrain[loan$recoveries==0,]
  train_nore<-subset(newtrain,select=-c(recoveries,collection_recovery_fee))
  test_nore<-subset(newtest,select=-c(recoveries,collection_recovery_fee))
  #id<-getid(test_nore)
  
  numvar<-NULL
  for (i in names(train_nore))
  {
    if(class(train_nore[1,i])=="numeric")
    {
      numvar<-c(numvar,i)
    }
  }
  numvar<-c("label",numvar)
  train_num<-train_nore[,numvar]
  test_num<-test_num[,numvar]
  
  gettrain<-function(x){
    return(subset(x,select=-c(label)))
  } 
  
  dtrain=xgb.DMatrix(as.matrix(gettrain(train_num)),label= (as.numeric(train_nore$label)-1))
  dtest<-xgb.DMatrix(as.matrix(test_num))
  
  xgb_params = list(
    booster = "gbtree", 
    objective = "binary:logistic", 
    max.depth = 12, 
    eta = 0.02, 
    nthread = 2, 
    nround = 2, 
    alpha = 1,
    gamma = 2,
    min_child_weight = 1, 
    subsample = 0.5, 
    colsample_bytree = 0.5 
    #num_parallel_tree = 1
  )
  
  best_n_rounds=1000# try more rounds
  
  #train data
  gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))
  result<-predict(gb_dt,dtest)
  result<-1-result
  result[recind]<-1
  
  end.time = Sys.time()
  run.time2 = as.numeric(difftime(end.time, start.time, units = 'sec'))
  
  
  out<-data.frame(id<-testid,prob<-result)
  write.table(out,file = "mysubmission1.txt",row.names = FALSE,sep = ",")
  
}




# ======================================
#        evaluation   
# ======================================

evaluate<-function(result,test)
{
  if(nrow(result)!=nrow(test))
  {print("Error")}
  else{
    label<-test$label
    a<-result[label=="1",]$prob
    b<-(1-result[label=="0",]$prob)
    e<-sum(log(a))+sum(log(b))
    e<-(-e)/nrow(test)
    return(e)
  }
}

errorxg = evaluate(out,test)
errorrf = evaluate(rfdataframe)

