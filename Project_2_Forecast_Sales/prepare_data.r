##########################
# Project 2 Prepare Data #
##########################

train.raw = read.csv(unz('train.csv.zip', 'train.csv'))
# str(train.raw)
train.date = as.Date(train.raw$Date, '%Y-%m-%d')

# training data from 2010-02 to 2011-02.
train.start = as.Date('2010-02-01', '%Y-%m-%d')
train.end = as.Date('2011-02-28', '%Y-%m-%d')
train.id = which(train.date >= train.start & train.date <= train.end)
train = train.raw[train.id, ]
test = train.raw[-train.id, ]
test$Weekly_Sales = NULL
test$Weekly_Pred1 = 0
test$Weekly_Pred2 = 0
test$Weekly_Pred3 = 0

write.table(train, file = 'train.csv', quote = FALSE, sep = ',', 
    row.names = FALSE)
write.table(test, file = 'test.csv', quote = FALSE, sep = ',', 
    row.names = FALSE)


newtest.raw = train.raw[-train.id, ]
newtest.date = train.date[-train.id]
# month 1 --> 2011-03, and month 20 --> 2012-10.
newtest.name = 'xxx'
Month = 3
Year = 2011
i = 1
while (i <= 20){
    newtest.id = which(as.numeric(format(newtest.date, '%Y')) == Year 
        & as.numeric(format(newtest.date, '%m')) == Month)
    newtest = newtest.raw[newtest.id, ]
    write.table(newtest, file = paste(newtest.name, i, '.csv', sep = '')
                , quote = FALSE, sep = ',', row.names = FALSE)
    Month = Month + 1
    if (Month > 12){
        Year = Year + 1
        Month = 1
    }
    i = i + 1
}

