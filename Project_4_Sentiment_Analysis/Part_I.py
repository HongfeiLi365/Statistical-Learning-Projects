
# coding: utf-8

# In[1]:

get_ipython().magic('matplotlib inline')
import numpy as np
import pandas as pd
import scipy as sp
import re


# In[2]:

from bs4 import BeautifulSoup
from sklearn.metrics import accuracy_score
from sklearn.linear_model import LogisticRegression
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.utils import check_random_state
from sklearn.model_selection import train_test_split


# In[3]:

train=pd.read_csv('E:/542/Pro4/labeledTrainData.tsv', 
                  header=0,delimiter='\t',quoting=3)
test = pd.read_csv("E:/542/Pro4/testData.tsv", header=0, delimiter="\t", quoting=3)


# In[4]:

num_reviews = train["review"].size
clean_train=[]
for i in range( 0, num_reviews ):
    review_text = BeautifulSoup(train["review"][i],'html.parser').get_text()
    pattern = re.compile(r'[^\w\s]')
    letnum_only = re.sub(pattern, " ", review_text) 
    clean_train.append(letnum_only)
clean_train_df=pd.DataFrame({'text':clean_train})


# In[5]:

def model1(X_train, y_train, X_test, random_state):
    tfidf=TfidfVectorizer(stop_words = 'english',min_df=5,max_df=0.15,ngram_range=(1, 2))
    train_X = tfidf.fit_transform(X_train)
    test_X=tfidf.transform(X_test)
    
    model=LogisticRegression(penalty='l2', dual=True,random_state=random_state)
    model.fit(train_X,y_train)
    y_pred=model.predict(test_X)
    y_score=model.predict_proba(test_X)    
    return model,y_pred,y_score


# In[6]:

clean_test=[]
num_test = test["review"].size
for i in range( 0, num_test ):
    review_text = BeautifulSoup(test["review"][i],'html.parser').get_text()
    pattern = re.compile(r'[^\w\s]')
    letnum_only = re.sub(pattern, " ", review_text)
    clean_test.append(letnum_only)
clean_test_df=pd.DataFrame({'text':clean_test})


# In[7]:

model,y_pred,y_score= model1(clean_train_df['text'], train['sentiment'], clean_test_df['text'],random_state=check_random_state(800))


# In[8]:

y_proba=[]
for i in range(0,25000):
        y_proba.append(y_score[i][1])


# In[9]:

output=pd.DataFrame({'id':test['id'],'sentiment':y_proba})
output.to_csv("Prediction.csv", index=False, quoting=3)


# In[10]:

output=pd.DataFrame({'id':test['id'],'sentiment':y_pred})
output.to_csv("Prediction0or1.csv", index=False, quoting=3)


# In[ ]:



