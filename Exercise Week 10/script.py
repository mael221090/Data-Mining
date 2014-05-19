#!/usr/bin/env python -W ignore::DeprecationWarning
 
import sgmllib
import cgi, sys
import json
import string
import sys

from sklearn.datasets import fetch_20newsgroups
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import HashingVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.naive_bayes import MultinomialNB
from sklearn.feature_selection import SelectKBest, chi2
from sklearn.linear_model import RidgeClassifier
from sklearn.svm import LinearSVC
from sklearn.linear_model import SGDClassifier
from sklearn.linear_model import Perceptron
from sklearn.linear_model import PassiveAggressiveClassifier
from sklearn.naive_bayes import BernoulliNB, MultinomialNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors import NearestCentroid
from sklearn.feature_extraction import DictVectorizer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import Normalizer
from sklearn import metrics, svm
from sklearn.cluster import DBSCAN, Ward
from sklearn.cluster import KMeans, MiniBatchKMeans
from sklearn import cross_validation
from sklearn.cross_validation import KFold

import logging
from optparse import OptionParser
import sys
from time import time
import os

import numpy as np

from gensim import corpora, models, similarities
from operator import itemgetter
from collections import OrderedDict
import pylab as pl

import pandas

import nltk
from nltk.corpus import stopwords

from itertools import cycle

import warnings
warnings.simplefilter("ignore",DeprecationWarning)
    
class ExtractTags(sgmllib.SGMLParser):

    def __init__(self, verbose=0):
        sgmllib.SGMLParser.__init__(self, verbose)
        self.data = None
        self.dict = {};
        self.res = [];
        self.tags = ['title', 'body', 'topics', 'dateline', 'reuters'];
        
    def handle_data(self, data):
        if self.data is not None:
            self.data.append(data)

    def unknown_starttag(self, tag, attrs):
        self.data = [];
        if tag == 'reuters':
            text = ""
            for attr, value in attrs:
                text = text + " %s='%s'" % (attr, cgi.escape(value))
            self.dict[tag] = text;
            
        
    def unknown_endtag(self, tag):
        if tag in self.tags:
            if tag == 'body':
                self.dict[tag] = self.data[0];
            elif tag != 'reuters':
                self.dict[tag] = self.data;
            else:
                self.res.append(self.dict);
                self.dict = {};
            self.data = [];
        

def parseData(s):
    file = open(s)
    p = ExtractTags()
    p.feed(file.read())
    p.close()
    return p.res


class documents():
    def __init__(self):
        self.data = []
        self.option = 0
        self.training = []
        self.testing = []
        #list of the 10 most popular topics
        self.topics = ['earn', 'acq', 'money-fx', 'grain', 'crude', 'trade', 'interest', 'ship', 'wheat', 'corn']
        self.top10topics = []
     
    #collection of the data contained the 21 SGML files   
    def collection(self):
        print("Collection in process...")
        #loop through the 21 SGML files and store the data into data
        for i in range(0,22):
            if i < 10:
                self.data = self.data + parseData("./reuters21578/reut2-00" + str(i) + ".sgm");
            else:
                self.data = self.data + parseData("./reuters21578/reut2-0" + str(i) + ".sgm"); 
        self.storeInCSV(self.data, "collection.csv")
        print("Collection of data successffully completed!\n")
        print("The data is composed of " + str(len(self.data)) + " documents. \n")           
        return self.data
    

    #extract data from CSV
    def getDataFromCSV(self):
        print("Extraction in process...")
        df = pandas.read_csv("collection.csv")
        #delete the first columun which replicates the index column
        df = df.drop(df.columns[0], axis=1)  
        self.data = [];
        for i, row in enumerate(df.values):
            dict = {};
            dict['body'] = row[0]
            dict['dateline'] = row[1]
            dict['reuters'] = row[2]
            dict['title'] = row[3]
            dict['topics'] = row[4]
            self.data.append(dict) 
        print("Extraction of data successffully completed!\n")
        print("The data is composed of " + str(len(self.data)) + " documents. \n")  
        return self.data
    
    #store data into file for computational time purposes
    def storeInCSV(self, data, file):
        df = pandas.DataFrame(data)
        df.to_csv(file)
    #we split the data into two sets, training and testing set    
    def splitTrainTestData(self, data):
        print("Splitting in process...")
        #initialisation of two lists
        self.training = []
        self.testing = []    
        #loop through the entire list of documents
        for item in data:
            #if the tag reuters contains train
            if item['reuters'].split(' ')[2].split('=')[1] == "'TRAIN'":
                self.training.append(item)
            else:
                self.testing.append(item)
        print("Cardinality of the training set: " + str(len(self.training)) + " = " + str(len(self.training)/float(len(self.data)) * 100) + "%" )
        print("Cardinality of the testing set: " + str(len(self.testing)) + " = " + str(len(self.testing)/float(len(self.data)) * 100) + "%" )
        print("Splitting completed\n")
        return self.training, self.testing
    
    #function that extracts the documents which topics are within the 10 most frequent (given in the exercise sheet)
    def extract10Topics(self):   
        print("Extraction of the 10 most popular topics in process...")
        for item in self.data:
            if 'body' in item and type(item['body']) != float:
                if len(item['topics']) > 0:
                    if item['topics'][2:-2] in self.topics:
                        self.top10topics.append(item)
        print("The data is composed of " + str(len(self.top10topics)) + " documents. \n")
        print("Extraction of the 10 most popular topics in process...")
        return self.top10topics        
  
    #function that tokenizes the data, in order to apply the tf idf method for representing the terms of the documents
    def tokenize(self, data):
        tokens = nltk.word_tokenize(data)        
        return tokens
    
    #function that trains the tf idf function
    def trainTfIDF(self, data, tfidf):     
        #as part of pre-processing, we remove the punctuation
        l = [item['body'].lower().translate(None, string.punctuation) for item in data if type(item['body']) != float]       
        tfs_train = tfidf.fit_transform(l)
        return tfs_train, list(item['topics'] for item in data if type(item['body']) != float)
    
    #funtion that applies the tf idf method previously trained
    def applyTfIDF(self, data, tfidf):
        #as part of pre-processing, we remove the punctuation
        l = [item['body'].lower().translate(None, string.punctuation) for item in data if type(item['body']) != float]       
        tfs_test = tfidf.transform(l)     
        return tfs_test, list(item['topics'] for item in data if type(item['body']) != float)   
    
    #function that applies topic modelling using LDA (Latent Dirichlet Allocation)
    def applyLDA(self, n_topics, data):
        print("Topic modelling with LDA in process..")
        #as part of pre-processing, we remove the stopwords
        stoplist = stopwords.words('english')
        #as part of pre-processing, we remove the punctuation and tokenize the corpus
        texts = [nltk.word_tokenize(item['body'].lower().translate(None, string.punctuation+'\xfc')) for item in data if type(item['body']) != float]        
        dictionary = corpora.Dictionary(texts)
        corpus = [dictionary.doc2bow(text) for text in texts]   
        #tf-idf method applied on the corpus
        tfidf = models.TfidfModel(corpus) 
        corpus_tfidf = tfidf[corpus]        
        #LDA performed
        lda = models.ldamodel.LdaModel(corpus_tfidf, id2word=dictionary, num_topics=n_topics)
        for i in range(0, n_topics):
            temp = lda.show_topic(i, 10)
            terms = []
            for term in temp:
                terms.append(term[1])
            print "Top 10 terms for topic #" + str(i) + ": "+ ", ".join(terms)
         
        print 'Which LDA topic maximally describes a document?\n'
        print 'Topic probability mixture: ' + str(lda[corpus[1]])
        print 'Maximally probable topic: topic #' + str(max(lda[corpus[1]],key=itemgetter(1))[0])    
        print("Topic modelling with LDA successfully completed\n")
        return lda
    
    #Extraction of the words given by the results of the LDA 
    def extractTopicModeling(self, lda):
        l = {} 
        count = 0
        #loop through the topics and store them in a dictionary to be used with tfidfVectorizer
        for item in lda.show_topics(topics=100, topn=10, formatted=False):
            for word in item:
                if not l.has_key(str(word[1])):
                    l[str(word[1])] = count
                    count+=1
        return l
    
    def runMenu(self):                 
        print("********************* Start of the program *********************")
        print("* 1 - Collect the data from the 21 SGML files                  *\n")
        print("* 2 - Feature selection with LDA (Topic modelling)             *\n")
        print("* 3 - Feature selection with LDA and tfIdf combined            *\n")
        print("* 4 - Classification using best features                       *\n")
        print("* 5 - Clustering using the entire dataset                      *\n")
        print("****************************************************************")
        self.option = input("Please, select the option you desire:\n")
        print("You selected the option: " + str(self.option))
        
        if self.option == 1:
            self.data = self.collection() 
            self.runMenu()
            
        elif self.option == 2:
            if os.path.isfile("./collection.csv"):
                self.data = self.getDataFromCSV()
            else:
                self.data = self.collection()
            #get the Lda Model
            lda = self.applyLDA(100, self.data)
            #extract the top features from the lda model
            features = self.extractTopicModeling(lda)
            self.training, self.testing = self.splitTrainTestData(self.data) 
            #as part of pre-processing, we remove the stopwords and tokenize the corpus, and fix the vocabulary to the top featues from LDA
            tfidf = TfidfVectorizer(tokenizer=self.tokenize, max_df=0.8, lowercase=False, decode_error='ignore', vocabulary=features)            
            x_train, y_train = self.trainTfIDF(self.training, tfidf) 
            x_test, y_test = self.applyTfIDF(self.testing, tfidf)
            print("Apply classification")
            c = classifiers()
            results = []
            
            for clf, name in (
                (MultinomialNB(), "MultinomialNB"),
                (Perceptron(n_iter=500), "Perceptron"),
                (LinearSVC(), "SVM")):    
                results.append(c.applyClassifier(clf, name, x_train, x_test, y_train, y_test))        
                
            c.plotScoreClassifier(results, "plot_1000_LDA.png")
            self.runMenu()
            
        
        elif self.option == 3:
            if os.path.isfile("./collection.csv"):
                self.data = self.getDataFromCSV()
            else:
                self.data = self.collection()            
            self.top10topics = self.extract10Topics()
            #get the Lda Model
            lda = self.applyLDA(100, self.top10topics)
            #extract the top features from the lda model
            features = self.extractTopicModeling(lda)
            self.training, self.testing = self.splitTrainTestData(self.top10topics) 
            print("TF IDF in process ...")         
            #as part of pre-processing, we remove the stopwords and tokenize the corpus
            tfidf = TfidfVectorizer(tokenizer=self.tokenize, sublinear_tf=True, stop_words='english', max_features=1000, lowercase=False, decode_error='ignore')            
            x_train, y_train = self.trainTfIDF(self.training, tfidf) 
            #we select the top 1000 features from the previous tfidfvectorizer
            vocabularyCombined = tfidf.vocabulary_
            n = len(vocabularyCombined) + 1
            #loop through the dictionnary of features extracted from LDA, and add to these features to the vocabulary
            for feature, index in features.items():
                if not vocabularyCombined.has_key(str(feature)):
                    vocabularyCombined[str(feature)] = n
                    n += 1
            #this time, we get the tf idf sparse matrix with the vocabulary from LDA and the top 1000 features from the last TFIDF training           
            tfidf2 = TfidfVectorizer(tokenizer=self.tokenize, max_df=0.8, lowercase=False, decode_error='ignore', vocabulary=vocabularyCombined)            
            x_train, y_train = self.trainTfIDF(self.training, tfidf2)             
            x_test, y_test = self.applyTfIDF(self.testing, tfidf2)
            print("Apply classification")
            c = classifiers()
            results = []
            
            for clf, name in (
                (MultinomialNB(), "MultinomialNB"),
                (Perceptron(n_iter=500), "Perceptron"),
                (LinearSVC(), "SVM")):    
                results.append(c.applyClassifier(clf, name, x_train, x_test, y_train, y_test))
            
            c.plotScoreClassifier(results, "plot_2000features_combined.png") 
            self.runMenu()
            
        elif self.option == 4:
            if os.path.isfile("./collection.csv"):
                self.data = self.getDataFromCSV()
            else:
                self.data = self.collection()            
            self.top10topics = self.extract10Topics()
            self.training, self.testing = self.splitTrainTestData(self.top10topics) 
            print("TF IDF in process ...")         
            #as part of pre-processing, we remove the stopwords and tokenize the corpus
            tfidf = TfidfVectorizer(tokenizer=self.tokenize, sublinear_tf=True, stop_words='english', max_features=1000, lowercase=False, decode_error='ignore')            
            x_train, y_train = self.trainTfIDF(self.training, tfidf) 
            x_test, y_test = self.applyTfIDF(self.testing, tfidf)
            print("Apply classification")
            c = classifiers()
            results = []
            
            for clf, name in (
                (MultinomialNB(), "MultinomialNB"),
                (Perceptron(n_iter=500), "Perceptron"),
                (LinearSVC(), "SVM")):    
                results.append(c.applyClassifier(clf, name, x_train, x_test, y_train, y_test))
            
            c.plotScoreClassifier(results, "plot_1000features_classifiers.png") 
            self.runMenu()
            
        elif self.option == 5:
            if os.path.isfile("./collection.csv"):
                self.data = self.getDataFromCSV()
            else:
                self.data = self.collection()  
            print("TF IDF in process ...")   
            self.top10topics = self.extract10Topics()          
            #as part of pre-processing, we remove the stopwords and tokenize the corpus
            tfidf = TfidfVectorizer(tokenizer=self.tokenize, sublinear_tf=True, stop_words='english', max_features=1000, lowercase=False, decode_error='ignore')            
            x_train, y_train = self.trainTfIDF(self.top10topics, tfidf) 
            c = classifiers()
            results = []            
            print("Apply clustering")            
            for clf, name in (
                (KMeans(n_clusters=np.unique(y_train).shape[0], init='k-means++', max_iter=100, n_init=1), "KMeans"),
                (DBSCAN(eps=0.3, min_samples=10), "DBSCAN"),
                (Ward(n_clusters=10), "Ward")):
                    clustering = c.applyClustering(clf, name, x_train, np.unique(y_train).shape[0], y_train) 
                    
            self.runMenu()
        else:
            print("end")
            sys.exit()  


class classifiers():
    def applyClassifier(self, clf, name, training_set, testing_set, y_train, y_test):
        print("\nMODEL " + name)
        
        t0 = time()
        classifier = clf.fit(training_set, y_train)
        train_time = time() - t0
        print("train time: %0.3fs" % train_time)
        
        t0 = time()       
        y_nb_predicted = classifier.predict(testing_set)
        test_time = time() - t0
        print("test time:  %0.3fs" % test_time)
        
        precision = metrics.precision_score(y_test, y_nb_predicted)
        recall = metrics.recall_score(y_test, y_nb_predicted)
        f1_score = metrics.f1_score(y_test, y_nb_predicted)
        accuracy = metrics.accuracy_score(y_test, y_nb_predicted)
        micro_recall = metrics.recall_score(y_test, y_nb_predicted, average="micro")
        macro_recall = metrics.recall_score(y_test, y_nb_predicted, average="macro")
        micro_precision = metrics.precision_score(y_test, y_nb_predicted, average="micro")
        macro_precision = metrics.precision_score(y_test, y_nb_predicted, average="macro")        
        print 'The precision for this classifier is ' + str(precision)
        print 'The micro averaged precision for this classifier is ' + str(micro_precision)
        print 'The macro averaged precision for this classifier is ' + str(macro_precision)
        print 'The recall for this classifier is ' + str(recall)
        print 'The micro averaged recall for this classifier is ' + str(micro_recall)
        print 'The macro averaged recall for this classifier is ' + str(macro_recall)        
        print 'The f1 for this classifier is ' + str(f1_score)
        print 'The accuracy for this classifier is ' + str(accuracy) 
        
        return name, accuracy, precision, recall, micro_precision, micro_recall, macro_precision, macro_recall, train_time, test_time
    
    def plotScoreClassifier(self, results, filename):
        indices = np.arange(len(results))   
        results = [[x[i] for x in results] for i in range(10)]
        
        clf_names, accuracy, precision, recall, micro_precision, micro_recall, macro_precision, macro_recall, training_time, test_time = results
        training_time = np.array(training_time) / np.max(training_time)
        test_time = np.array(test_time) / np.max(test_time)
        
        pl.figure(figsize=(12,10))
        pl.title("Score")
        pl.barh(indices, accuracy, .1, label="accuracy", color='r')
        pl.barh(indices + .3, precision, .1, label="precision", color='g')
        pl.barh(indices + .6, recall, .1, label="recall", color='b')
        pl.yticks(())
        pl.legend(loc='best')
        pl.subplots_adjust(left=.25)
        pl.subplots_adjust(top=.95)
        pl.subplots_adjust(bottom=.05)
        
        for i, c in zip(indices, clf_names):
            pl.text(-.3, i, c)
         
        pl.savefig(filename)

    def applyClustering(self, clf, name, X, nb_clust, labels):
        print("\nClustering with %s" % name)
        t0 = time()
        if name == "Ward" or name == "DBSCAN":
            clf = clf.fit(X.toarray())
        else:
            clf = clf.fit(X)
        print("done in %0.3fs" % (time() - t0))        
        print("Homogeneity: %0.3f" % metrics.homogeneity_score(labels, clf.labels_))
        print("Completeness: %0.3f" % metrics.completeness_score(labels, clf.labels_))
        
        return clf

#We launch the menu when we launch the program
if __name__ == '__main__':    
    corpus = documents()
    corpus.runMenu() 