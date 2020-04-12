library(tm)
#library(FeatureHashing)
library(Matrix)
library(SnowballC)
#library(xgboost)
#library(slam)
#library(bigmemory)
library(glmnet)
library(wordcloud)
library(ggplot2)
setwd('C:/Users/mengd/Desktop/Stat/STAT542/IBDM')

train <- read.delim("labeledTrainData.tsv", quote = "", as.is = T,encoding="UTF-8")
test<-read.delim("TestData.tsv", quote = "", as.is = T,encoding="UTF-8")
test<-test[sample(nrow(test))[1:50],]
ini_tfidf<-function(x){
  temp<-x$review
  #temp<-tolower(gsub("[^[:alnum:] ]", " ", temp))
  corpus <- Corpus(VectorSource(temp))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  #corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, stemDocument, language="english")
  dtm<- DocumentTermMatrix(corpus, 
                           control = list(stopwords = TRUE,  
                                          removeNumbers = TRUE, removePunctuation = TRUE ,weighting = weightTfIdf
                           ))
  #dtm =  dtm[, term_tfidf >= 0.1]
  #dtm =  dtm[row_sums(dtm) > 0,]
  dtm <- removeSparseTerms(dtm, 0.999)
  
  return(dtm)
}

get_dict<-function(x){
  return(colnames(x))
}

trans_tfidf<-function(x,y){
  temp <- x$review
  #temp<-tolower(gsub("[^[:alnum:] ]", " ", temp))
  corpus <- Corpus(VectorSource(temp))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  #corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  #corpus <-Corpus(VectorSource(corpus))
  dtm<- DocumentTermMatrix(corpus, 
                           control = list(dictionary=y,stopwords = TRUE,  
                                          removeNumbers = TRUE, removePunctuation = TRUE
                           ))
  
  
  #corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stripWhitespace)
  #corpus <- tm_map(corpus, stemDocument, language="english")
  #dtm<- DocumentTermMatrix(corpus, 
  #                        control = list(dictionary=y,stemming = TRUE, stopwords = TRUE, minWordLength = 3, 
  #                           weighting = weightTfIdf   removeNumbers = TRUE, removePunctuation = TRUE))
  return(dtm)
}



traintm<-ini_tfidf(train)
mydictionary<-get_dict(traintm)
testtm<-trans_tfidf(test,mydictionary)


dtrain<-as.data.frame(as.matrix(traintm))
dtest<-as.data.frame(as.matrix(testtm))


label=as.factor(train$sentiment)
logis.fit<-glmnet(as.matrix(dtrain),train$sentiment,family = "binomial",alpha = 0,lambda = 1)
coe<- predict(logis.fit, type="coefficients")
words<-as.numeric(coe)
names(words)<-rownames(coe)
words<-words[-1]


#########################################################################

for(i in 1:50){
  temp<-dtest[i,]
  fre<-rbind(words,temp)
  fre<-rbind(fre,rep(0,ncol(temp)))
  fre<-fre[,fre[2,]!=0]
  for(j in 1:ncol(fre)){
    fre[3,j]=fre[1,j]*fre[2,j]
  }
  pf<-fre[3,fre[3,]>0]
  pname<-names(pf)
  a<-as.matrix(pf[1,])
  dp = data.frame(word=pname, freq=t(a))
  #
  nf<-fre[3,fre[3,]<0]
  nnames<-names(nf)
  a<-as.matrix(nf[1,])
  dn<-data.frame(word=nnames, freq=t(-a))
  
  picname_p<-paste0("p_wordcloud_",i,".png")
  picname_n<-paste0("n_wordcloud_",i,".png")
  
  png( picname_p, width=1280,height=800)
  wordcloud(dp$word, dp$X3, max.words=50, scale=c(4,.2), random.order=FALSE, rot.per=.25, 
            colors=brewer.pal(8,"Dark2"))
  dev.off()
  
  png( picname_n, width=1280,height=800)
  wordcloud(dn$word, dn$X3, max.words=50, scale=c(4,.2), random.order=FALSE, rot.per=.25, 
            colors=brewer.pal(8,"Dark2"))
  dev.off()
}

####################################################
prob<-predict(logis.fit,as.matrix(dtest),type="response")

prob<-prob-0.5
prob<-prob*2
d<-data.frame(Review=c(1:50),Prob=prob)
names(d)[2]<-"Degree"
ggplot(d, aes(x = Review, y = Degree,fill = Degree)) + geom_bar(stat = "identity")+ylim(-1,1)








