pkg = c('recommenderlab', 'reshape2')  # all packages you will use in your code
# Do not include unnecessary packages.
for (i in pkg){
  if (!require(i, character.only = TRUE)){
    install.packages(i)
  }
}

library(recommenderlab)  # realRatingMatrix
library(reshape2)  # acast

# ====================== read train ==========================
train = read.table('train.dat', sep = ':', 
                     colClasses = c('integer', 'NULL'), header = FALSE
)
colnames(train) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# ====================== read test ==========================

test = read.csv('test.csv', header = TRUE)

# ===================== read movie ==========================
movies = readLines('movies.dat')
movies = strsplit(movies, split = '::', fixed = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# ============ User-based Model ====================

ratingmat <- acast(train, UserID~MovieID, value.var = 'Rating')
ratingmat = as(ratingmat, 'realRatingMatrix')

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
UBmodel = Recommender(ratingmat, method = 'UBCF', parameter = list(normalize = 'Z-score', method = 'cosine', nn = 25))

UBrecom = predict(UBmodel, ratingmat, type = 'ratings')  # predict ratings. This may be slow.
UB_list = as(UBrecom, 'list')  # each element are ratings of that user

# For all lines in test file, one by one
for (u in 1:nrow(test)){

  userid = test$user[u]
  movieid = test$movie[u]
  
  rating = UB_list[[userid]][movieid]
  test$rating[u] = rating
}

test$rating[is.na(test$rating)] = mean(train$Rating)

# write submission file
write.table(test, file = 'mysubmission1.csv', row.names = FALSE,
            col.names = TRUE, sep = ',')


# ============ Item-based Model ====================

test$rating = NULL

IBmodel <- Recommender(data = ratingmat, 
                          method = "IBCF",
                          parameter = list(k = 5))

IBrecom = predict(IBmodel, ratingmat, type = 'ratings')
IB_list = as(IBrecom, 'list')  # each element are ratings of that user

# For all lines in test file, one by one
for (u in 1:nrow(test)){
  
  userid = test$user[u]
  movieid = test$movie[u]
  
  rating = IB_list[[userid]][movieid]
  test$rating[u] = rating
  
}

test$rating[is.na(test$rating)] = mean(train$Rating)

# write submission file
write.table(test, file = 'mysubmission2.csv', row.names = FALSE,
            col.names = TRUE, sep = ',')

