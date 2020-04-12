library(forecast)
library(lubridate)

#==================== functions ========================

# --------------implement missing value-----------------
addmiss=function(train){ 
  exi_record<-table(train$Dept,train$Store)
  exi<-which(exi_record!=0, arr.ind=TRUE, useNames = FALSE)
  exi_s<-unique(train$Store)
  for (i in unique(train$Date)){
    holi<-unique(train[train$Date==i,]$IsHoliday)
    t_d<-unique(train[train$Date==i,]$Date)
    tmp_train=train[train$Date==i,]
    records=table(tmp_train$Dept,tmp_train$Store)
    t_missing_records=which(records==0, arr.ind=TRUE, useNames = FALSE)
    missing_records<-merge(exi,t_missing_records,all=FALSE)
    
    missing_dept<-as.numeric(row.names(exi_record)[missing_records[,1]])
    missing_store<-as.numeric(colnames(exi_record)[missing_records[,2]])
    if( length(missing_records)>0 | length(missing_dept)>0 ){
      store_average=rep(0,length(exi_s))
      for( j in 1:length(exi_s)){
        store_average[j]=mean(tmp_train[tmp_train$Store==exi_s[j],"Weekly_Sales"])
      }
      store_average=data.frame(Store=exi_s,Weekly_Sales=store_average)
      
      missing_add<-data.frame(Store=missing_store,Dept=missing_dept,Date=rep(t_d,length(missing_dept)),IsHoliday=rep(holi,length(missing_dept)))
      missing_add<-merge(missing_add,store_average, by="Store", all.x = TRUE)
      train=rbind(train,missing_add)
    }
  }
  return(train)
}

#--------------add column Wk and Column Yr---------------
addWeek = function(train){
  train.wk = train$Date
  train.wk = train.wk - as.Date("2010-02-05")  # date is now 0, 7, 14, ...
  train.wk = train.wk/7 + 5  # make 2010-2-5 as '5', and date becomes continuous integers, i.e., 5, 6, 7, ...
  train.wk = as.numeric(train.wk) %% 52  ## 52 weeks in a year
  train$Wk = train.wk
  train$Yr = year(train$Date)
  return(train)
}

# ------------ subset corresponding test ----------------
subtest = function(test, t){
  if (t<=9){
    beginday = as.Date(paste("2011-", t+2, "-01",sep = ''),format = "%Y-%m-%d")
    endday = as.Date(paste("2011-", t+3, "-01",sep = ''),format = "%Y-%m-%d")
  } else if (t==10){
    beginday = as.Date("2011-12-01",format = "%Y-%m-%d")
    endday = as.Date("2012-01-01",format = "%Y-%m-%d")
    
  } else {
    beginday = as.Date(paste("2012-", t-10, "-01",sep = ''),format = "%Y-%m-%d")
    endday = as.Date(paste("2012-", t-9, "-01",sep = ''),format = "%Y-%m-%d")
  }
  currenttest = test[(beginday <= test$Date) & (test$Date < endday),]
  
  return(currenttest)
}

#================== simple model 1 ==================== 

simple = function(train, test){
  store = sort(unique(test$Store))
  n.store = length(store)
  dept = sort(unique(test$Dept))
  n.dept = length(dept)
  
  for (s in 1:n.store){
    for (d in 1:n.dept){
      
      # cat("[Model 1] Store: ", store[s], "\t Dept ", dept[d], "\n")
      
      # find the data for (store, dept) = (s, d)
      test.id = which(test$Store == store[s] & test$Dept == dept[d])
      test.temp = test[test.id, ]
      train.id = which(train$Store == store[s] & train$Dept == dept[d])
      train.temp = train[train.id, ]
      
      for (i in 1:length(test.id)){
        id = which(train.temp$Wk == test.temp[i,]$Wk & train.temp$Yr == test.temp[i,]$Yr - 1)

        threeWeeksId = c(id - 1, id, id + 1)  ## three weeks in the last year
        tempSales = train.temp[threeWeeksId, 'Weekly_Sales']
        if (length(tempSales) == 0){
          test$Weekly_Pred1[test.id[i]] = mean(train.temp$Weekly_Sales)
        } else {
          test$Weekly_Pred1[test.id[i]] = mean(tempSales)
        }
        
      }
    }
  }
  
  # deal with NA and NaN
  s_d_w<-test[is.na(test$Weekly_Pred1) | is.nan(test$Weekly_Pred1),c("Store","Dept","Wk")]
  for(k in 1:nrow(s_d_w))
  {
    s <- s_d_w$Store[k]
    d <- s_d_w$Dept[k]
    w <- s_d_w$Wk[k]
    mean_s<-mean(train[train$Store==s,]$Weekly_Sales)
    test$Weekly_Pred1[test$Store==s & test$Dept==d & test$Wk == w]<- mean_s
  }
  
  
 # set holiday sales as the same as the week last year
  s_d_w<-test[test$Wk == 0 | test$Wk == 6 | test$Wk == 47 ,c("Store","Dept","Wk")]
  for(k in 1:nrow(s_d_w))
  {
    s <- s_d_w$Store[k]
    d <- s_d_w$Dept[k]
    w <- s_d_w$Wk[k]
    salesholiday<-mean(train$Weekly_Sales[train$Store==s & train$Dept == d & train$Wk == w])
    test$Weekly_Pred1[test$Store==s & test$Dept==d & test$Wk == w]<- salesholiday
  }
  
  test$Weekly_Pred1[is.nan(test$Weekly_Pred1)] = 0

  return(test)
}

#================== ts model 2 ==================== 
model2<-function(X,Y)
{
  train_s_d<-unique(X[,c("Store","Dept")])
  n_train<-nrow(train_s_d)
  
  test_s_d<-unique(Y[,c("Store","Dept")])
  n_test<-nrow(test_s_d)
  
  #all in#
  
  all_s_d<-merge(train_s_d,test_s_d,by=c("Store","Dept"))
  n_all<-nrow(all_s_d)
  
  #predict week#
  n_pred_w<-nrow(Y[Y$Store==1&Y$Dept==1,])
  #pred_w<-test[test$Store==1&test$Dept==1,]$Date
  
  for(k in 1: n_all)
  {
    s<-all_s_d$Store[k]
    d<-all_s_d$Dept[k]
    if (Y$Yr[1]<2012|(Y$Yr[1]==2012&Y$Wk[1]<=5)){
      Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred2 <- Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred1 
    }
    else{
      sale<-X[(X$Store==s)&(X$Dept==d),]$Weekly_Sales
      #print(d)
      #print(length(sale))
      #aa=X[(X$Store==s)&(X$Dept==d),]
      #if (nrow(aa)!=108){
      #  print(tail(aa))
      #}
      ts_sale<-ts(sale,frequency = 52)
      n_pred<- nrow(Y[Y$Store==s&Y$Dept==d,])
      #pred<-stlf(ts_sale, h=n_pred, s.window=3, method='arima',ic='bic')
      #pred<-forecast.HoltWinters(ts_sale, h=n_pred, seasonal="additive")
      #pred<- holt(ts_sale, h=n_pred, seasonal="additive")
      pred<- ses(ts_sale, h=n_pred, initial='simple',alpha=0.2)
      #pred<- rwf(ts_sale, h=n_pred, drift=T)
      pred<-as.numeric(pred$mean)
      Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred2<-pred[1:n_pred]
    }
  }
  
  #only in test#
  if(n_all!=n_test){
    otest_s_d<-Y[is.na(Y$Weekly_Pred2),c("Store","Dept")]
    n_otest<-nrow(otest_s_d)
    
    for(k in n_otest)
    {
      s<-otest_s_d$Store[k]
      d<-otest_s_d$Dept[k]
      mean_s<-mean(X[X$Store==s,]$Weekly_Sales)
      n_pred<- nrow(Y[Y$Store==s&Y$Dept==d,])
      
      Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred2<-rep(mean_s,n_pred)
    }
  }
  na_ix<-which(is.na(Y$Weekly_Pred2))
  for(l in na_ix)
  {
    Y[l,]$Weekly_Pred2<-mean(X[X$Store==Y[l,"Store"],]$Weekly_Sales)
  }
  return(Y)
}

#================== ts model 3 ===========forecast========= 
model3<-function(X,Y)
{
  train_s_d<-unique(X[,c("Store","Dept")])
  n_train<-nrow(train_s_d)
  
  test_s_d<-unique(Y[,c("Store","Dept")])
  n_test<-nrow(test_s_d)
  
  #all in#
  
  all_s_d<-merge(train_s_d,test_s_d,by=c("Store","Dept"))
  n_all<-nrow(all_s_d)
  
  #predict week#
  n_pred_w<-nrow(Y[Y$Store==1&Y$Dept==1,])
  #pred_w<-test[test$Store==1&test$Dept==1,]$Date
  
  for(k in 1: n_all)
  {
    s<-all_s_d$Store[k]
    d<-all_s_d$Dept[k]
    sale<-X[(X$Store==s)&(X$Dept==d),]$Weekly_Sales
    ts_sale<-ts(sale,frequency = 52)
    n_pred<- nrow(Y[Y$Store==s&Y$Dept==d,])
    pred<-forecast(ts_sale,h=n_pred)
    pred<-as.numeric(pred$mean)
    
    Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred3<-pred[1:n_pred]
  }
  
  #only in test#
  if(n_all!=n_test){
    otest_s_d<-Y[is.na(Y$Weekly_Pred3),c("Store","Dept")]
    n_otest<-nrow(otest_s_d)
    
    for(k in n_otest)
    {
      s<-otest_s_d$Storep[k]
      d<-otest_s_d$Dept[k]
      mean_s<-mean(X[X$Store==s,]$Weekly_Sales)
      n_pred<- nrow(Y[Y$Store==s&Y$Dept==d,])
      
      Y[Y$Store==s&Y$Dept==d,]$Weekly_Pred3<-rep(mean_s,n_pred)
    }
  }
  na_ix<-which(is.na(Y$Weekly_Pred3))
  for(l in na_ix)
  {
    Y[l,]$Weekly_Pred3<-mean(X[X$Store==Y[l,"Store"],]$Weekly_Sales)
  }
  return(Y$Weekly_Pred3)
}




#====================== main ============================

# this part will run when 'source(mymain.r)'
# the goal of this part should be setting up some varibales that will be used in functions
# these variables, such as dept.names and store.names, won't change during loop

train$Date = as.Date(train$Date, '%Y-%m-%d')
test$Date = as.Date(test$Date, '%Y-%m-%d')

dept.names = sort(unique(train$Dept))
store.names =sort(unique(train$Store))

n.dept=length(dept.names)
n.store=length(store.names)

#===================== frame of predict() ==============

predict = function(){
  
  if(t!=1){
    # if not first iteration: update train
    # t=1, Date=2011-03-04, newtest=NULL
    train <<- rbind(train,newtest)
  }
  
  # ------------ preprocessing ------------------
  
  train$Date = as.Date(train$Date, '%Y-%m-%d')
  test$Date = as.Date(test$Date, '%Y-%m-%d')
  
  # subset current test
  currenttest = subtest(test,t)
  # print(currenttest[is.na(currenttest$Store)])
  
  # add miss
  currenttrain = addmiss(train)
  #print(currenttrain[is.na(currenttrain$Store),])
  
  # add Wk and Yr 
  currenttrain = addWeek(currenttrain)
  currenttest = addWeek(currenttest)
  table(currenttrain$Store,currenttrain$Dept)
  # ------------ predict -----------------------
  # Call model 1
  
  currentpred = simple(currenttrain, currenttest)
  
  # Call model 2
  currentpred = model2(currenttrain, currentpred)
 
  # Call model 3
  currentpred3 = model3(currenttrain, currenttest)
  
  
  #rewite currenttest
  
  currentpred$Weekly_Pred3<-currentpred3

  #---------------------- merge test ----------------------------
   if (t<=9){
     beginday = as.Date(paste("2011-", t+2, "-01",sep = ''),format = "%Y-%m-%d")
     endday = as.Date(paste("2011-", t+3, "-01",sep = ''),format = "%Y-%m-%d")
   } else if (t==10){
     beginday = as.Date("2011-12-01",format = "%Y-%m-%d")
     endday = as.Date("2012-01-01",format = "%Y-%m-%d")

   } else {
     beginday = as.Date(paste("2012-", t-10, "-01",sep = ''),format = "%Y-%m-%d")
     endday = as.Date(paste("2012-", t-9, "-01",sep = ''),format = "%Y-%m-%d")
   }

  testbind = test[(test$Date < beginday) | (test$Date >= endday),]

  currentpred = subset(currentpred,select = -c(Wk, Yr))
  
  test1 = rbind(testbind, currentpred)
  test1$Date = as.factor(test1$Date)
  test <<- test1
}






