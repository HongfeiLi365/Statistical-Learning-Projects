#######################
# Project 2 Test Code #
#######################

# Note: 
# This is not the final version of test code. 
# I will change the name of the tthly newtest data, so that you CANNOT 
# get access to it when you are predicting.
# But it shouldn't affect your code. 

#######################################################################
# prepare data
# This chunk of code helps to create train.csv, test.csv,
# and the newtest files in the working directory. 
# You need to put the original train.csv.zip file in the working dir.
# Or if you have the newtest files you can comment it out, and change
# the newtest file names. 
newtest.name = 'xxx'
source('prepare_data.r')
#######################################################################

#######################################################################
# Test code begins
train = read.csv("train.csv")
test = read.csv("test.csv")

source("mymain.R")

wae = matrix(0, 20, 4)  # weighted mean absolute error (WMAE) and weight

for(t in 1:20){

    # predict the weekly sales for tth t, 
    # e.g., tth 1 --> 2011-03, and tth 20 --> 2012-10. 
    # Your prediction function should modify three columns of 'test', 
    # and update 'train'
    predict()
    
    # newtest: sales data for this tth; taking the
    # same format as "train". 
    tmp.filename = paste(newtest.name, t, '.csv', sep='');
    newtest = read.csv(tmp.filename)

    # Evaluate your prediction accuracy for this tth
    temp.mat = merge(newtest, test, 
                     by.x = c('Date', 'Store', 'Dept'),
                     by.y = c('Date', 'Store', 'Dept'),
                     all.x = TRUE
                    )
    for (met in 1:3){
        wae[t, met] = sum(abs(temp.mat[, 'Weekly_Sales']
            - temp.mat[, paste('Weekly_Pred', met, sep = '')])
            * ifelse(temp.mat[, 'IsHoliday.x'], 5, 1)
            )
    }
    wae[t, 4] = sum(ifelse(temp.mat[, 'IsHoliday.x'], 5, 1))
    
}

write.table(wae, 'Error.csv', sep = ',', row.names = FALSE, 
    col.names = FALSE)

