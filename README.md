# Quantifying the World - 7 Case Studies
This repository contains the assignments submitted for Quantifying the world class.


## Case Study 2 - Data Imputation in Python (MCAR, MAR and MNAR)
**Objective:** -Missing Data analysis, imputation and modelling

**Feedback:** - Well done.  Be careful those correlation plots were pretty small,. but mostly ancillary to the study.  Good Job with a table summary of results.


## Case Study 4 - ARIMA Stock Prices Time Series in Python 
**Objective:** - For case study 4, we are analyzing the the stock price of a major corporation, Western Union. Our goal is to analyze four-year stock data set for Western Union and come up with the appropriate guess of hyperparameters of ARIMA model to get the most optimal result. ARIMA model once trained can be used to forecast the stock price.

**Feedback:** - well done


## Case Study 6 - NLP (TFIDF matrix creation) along with clustering in R
**Objective:** - In this case study, we need to answer these questions:
Create a TF IDF matrix from the dataset. Some steps make take several minutes to process in R.
Do a K-means clustering for 5,10,20 clusters. What do you observe about the information R returns you? What do each of the parameters represent when you run the kmeans() function.
Run a DBScan on the data. How are the results different from your kmeans? What is the eps? ( hint: help(dbscan) ) What is the meaning of cluster 0?
These papers are for a conference submission. When would you use kmeans vs. DBScan in this instance. 
Using a cluster size of 10 and kmeans, create a word cloud of the different clusters most popular words. Are you able to determine a difference among the papers in different clusters? 

**Feedback:** - YOu guys did a ton of work.  What holds you back is your report is a little disorganized.  Remember, it is after all a report and not just your code.  Guide me through what you are trying to say--I can figure it out--but a key skill is communicating skillfully.  You almost gave me a 'Code blast', but you did attempt to make it a report AND its the first assignment.
Conceptually--dbscan vs kmeans was telling you there was basically one giant cluster *with outliers") . And while kmeans does perform better when the data is linearly seperable, it is not required--the idea is that you are trying to squeeze info out.  
Report wise, make sure graphs and figures have captions as titles.  Make sure your report is a narrative (using solid math concepts, but make it a story and less codes with comments).


## Case Study 8 - Modeling Age and Runner Time distribution in R
**Objective:** - The objective of this case study is to answer Question 7 in the chapter 2 of “Data Science in R: A Case Studies Approach to Computational Reasoning and Problem Solving” (Nolan and Lang). We are required to perform data cleaning process for the women’s race results data, working with any oddities that may be present in the raw data files, and build a data frame that enable us to perform data analysis for optimal results.

**Feedback:** - Very good.  Only issues were make sure graphs have titles and fixed-width font on code in appendix.


## Case Study 10 - Spam Classification Optimization in R
**Objective:** - In this case study, we aim to showcase how to use the R-package rpart (Recursive Partitioning and Regression Trees) to help with email classification as spam or ham. We tweaked with the rpart tunning parameters to fine-tuned and optimize the algorithm in order to improve prediction effectiveness.

**Feedback:** - Very good analysis of the models and the results.  I deducted 2 points for the metrics descriptions being vauge.   Define them explicitly or provide a reference to their definition.  i.e. precision = (TP/(TP+FP)) . or F1 = 2(PR/(P+R)).  especially for non standard metrics like kappa or hyperparms like cp (minimum information gain required for a plot)


## Case Study 12 - Neural Network parameter optimization with Neural Networks in Python's Keras and Tensorflow
**Objective:** - In this case study, we experimented with some Neural Network architectures to solve a classification problem targettted to distinguish between a signal process which produces Higgs bosons and a background process which does not. The dataset is obtained from UCI machine learning website.

**Feedback:** - Well done.  Would have like a little explantaion of your model being done, but you got the main point that loss/accuracy have stopped improving


## Case Study 14 - Complete Classification problem in Python
**Objective:** - This Case Study focuses on a classification problem with the dataset provided to us by the business for the purposes of reducing overall false predictions, which costs the business an expense. The objective of this project is to identifying the optimal model and the features that impact the model most.

**Feedback:** - Good use of correlated features to fill in missing values and eliminating correlated features.
It wasn't super clear what you did with your categorical (one hot encoded--found in your appendix).  Data Prep is one of the most significant impact to your results--be very clear.
Also make sure you understand what the Classification report is giving you.  While you averaged the 'Recall' what you did was look at the recall of both class 0 and class 1--what you wanted was the recall of class 1.  Now in the end you made the right decision as recall is influenced by false negatives, just remember the false negatives of class 0 (aka recall of class 0) are not the false negatives of class 1!  
I would not doing feature elimination--I would let the algorithm just learn with all the features in cases like this where you are getting a good score.  Not a 'wrong' answer, but when squeezing for those last few points, it can make a difference.
