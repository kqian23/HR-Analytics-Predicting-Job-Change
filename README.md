HR Analytics - Predicting Job Change of Data Scientists
================
Kun Qian
3/1/2021

*The following is an analysis of this [Kaggle
Dataset](https://www.kaggle.com/arashnic/hr-analytics-job-change-of-data-scientists).
Here’s contest and content extracted from the original site:* <br>

A company which is active in Big Data and Data Science wants to hire
data scientists among people who successfully pass some courses which
conduct by the company. Many people signup for their training. Company
wants to know which of these candidates are really wants to work for the
company after training or looking for a new employment because **it
helps to reduce the cost and time as well as the quality of training or
planning the courses and categorization of candidates**. Information
related to demographics, education, experience are in hands from
candidates signup and enrollment. <br>

This dataset designed to understand the factors that lead a person to
leave current job for HR researches too. By model(s) that uses the
current credentials,demographics,experience data you will **predict the
probability of a candidate to look for a new job or will work for the
company, as well as interpreting affected factors on employee
decision**. <br>

**Features**

  - enrollee\_id : Unique ID for candidate

  - city: City code

  - city\_ development \_index : Developement index of the city (scaled)

  - gender: Gender of candidate

  - relevent\_experience: Relevant experience of candidate

  - enrolled\_university: Type of University course enrolled if any

  - education\_level: Education level of candidate

  - major\_discipline :Education major discipline of candidate

  - experience: Candidate total experience in years

  - company\_size: No of employees in current employer’s company

  - company\_type : Type of current employer

  - lastnewjob: Difference in years between previous job and current job

  - training\_hours: training hours completed

  - target: 0 – Not looking for job change, 1 – Looking for a job change

# 0\. Read Data

``` r
set.seed(42)
# import data
hr <- read.csv("data/aug_train.csv")

# Take a look at the data
str(hr)
```

    ## 'data.frame':    19158 obs. of  14 variables:
    ##  $ enrollee_id           : int  8949 29725 11561 33241 666 21651 28806 402 27107 699 ...
    ##  $ city                  : chr  "city_103" "city_40" "city_21" "city_115" ...
    ##  $ city_development_index: num  0.92 0.776 0.624 0.789 0.767 0.764 0.92 0.762 0.92 0.92 ...
    ##  $ gender                : chr  "Male" "Male" "" "" ...
    ##  $ relevent_experience   : chr  "Has relevent experience" "No relevent experience" "No relevent experience" "No relevent experience" ...
    ##  $ enrolled_university   : chr  "no_enrollment" "no_enrollment" "Full time course" "" ...
    ##  $ education_level       : chr  "Graduate" "Graduate" "Graduate" "Graduate" ...
    ##  $ major_discipline      : chr  "STEM" "STEM" "STEM" "Business Degree" ...
    ##  $ experience            : chr  ">20" "15" "5" "<1" ...
    ##  $ company_size          : chr  "" "50-99" "" "" ...
    ##  $ company_type          : chr  "" "Pvt Ltd" "" "Pvt Ltd" ...
    ##  $ last_new_job          : chr  "1" ">4" "never" "never" ...
    ##  $ training_hours        : int  36 47 83 52 8 24 24 18 46 123 ...
    ##  $ target                : num  1 0 0 1 0 1 0 1 1 0 ...

<br>

# 1\. Exploratory Data Analysis

## 1.1 Explore Categorical Variables

**Write a function to generate two visualizations that allow us to
observe the relationship**  

``` r
# visualization
library(ggplot2)
library(dplyr)
library(gridExtra)
library(colorspace)

# Write a function to generate two visualizations that allow us to observe the relationship
# between each categorical variables and the target variable
two_plots <- function(category) {
  # total number of people in each category
  g1 <- ggplot(hr, aes(x={{category}}, fill={{category}})) + 
    geom_histogram(stat="count") + 
    theme(legend.position = 'none') + 
    scale_fill_discrete_qualitative() +
    theme(axis.text.x=element_text(angle=-10),
          axis.title.x = element_blank()) + 
    labs(title = deparse(substitute(category)))
  # Percentage of job switching group by category
  ratio_by_cat <- hr %>% group_by({{category}}) %>% summarise(percent_looking_for_job = sum(target)/n())
  g2 <- ggplot(ratio_by_cat, aes(x={{category}}, y =percent_looking_for_job, fill={{category}})) + 
    geom_col() +
    ylim(0,1) + 
    theme(legend.position = 'none') + 
    ylab("% Switching Job") + 
    scale_fill_discrete_qualitative() +
    theme(axis.text.x=element_text(angle=-10),
          axis.title.x = element_blank()) +
    labs(title = deparse(substitute(category)))
  # group the first two in a same plot
  g3 <- grid.arrange(g1,g2,nrow=1)
}
```

**Now let’s visualize each categorical variable and the target variable,
to identify interesting patterns of job switching decisions.**  

``` r
two_plots(gender)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

In this sample there’s more male than female. It seems like the gender
balance in Data Science field is still a problem. The pink bin stands
for missing gender.

``` r
two_plots(relevent_experience)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Most people in this sample has relevant experience in Data Science.
However, those who doesn’t tend to seeking for job change more often.
Maybe those are the people who wants to take the opportunity of this
training and make a career switch.

``` r
two_plots(enrolled_university)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Most people are not enrolled in a university right now. People who
enrolled in full time course are likely to look for job change. It makes
sense as most people enroll in universities to either get their first
job or seek for career switch

``` r
two_plots(education_level)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Most people have a master degree. That explains the high
‘no\_enrollment’ in the previous chart. This group of people might
be industry professionals who hold a master degree seeking for career
advancement or career switch. They have the highest intention for job
change.

``` r
two_plots(major_discipline)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

It’s not surprising that most candidates are from a STEM major as we are
analyzing a Data Science training program. Job switching intential is
even across majors.

``` r
two_plots(experience)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

More than 3000 people have over 20 years of experience; Only a tiny
amount of people have less than 1 year of experience. This is consistent
with our previous finding that the majority of people are industry
professional looking for career switch or career advancement.

``` r
two_plots(company_size)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Lots of missing data for company\_size. This could be missing with a
pattern. For example, people might leave it blank if they don’t
currently have a job. Further investigation into how the data is
collected and the missing pattern need to be done before drawing
conclusion. Other than this unknown category, job switch percentage
seems to be relatively even across all company sizes.

``` r
two_plots(company_type)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Near 100,000 people are from private limited company. The 2nd most
category is early stage companies, which have the highest percentage of
job switch intention.

``` r
two_plots(last_new_job)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

Most people only have 1 year between their previous job and current job.

<br>

## 1.2 Explore Continuous Variables

**correlation plot**  

``` r
library(corrplot)
# Correlation between continuous variables
num <- hr[,c(3,13,14)]
cor <- cor(num)
corplot <- corrplot(cor, type = 'lower', method = 'number')
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

City development index has a moderate negative correlation with the
target varibale. That means the more developed the city is, the less
people in that city are likely to look for job change. Also it seems
like training hour is not linearly correlated with job change.

**Look into the relationship between city development index and job
switch rate**  

``` r
# Look into the relationship between city development index and job switch rate
cities <- 
  hr %>% group_by(city) %>% 
  summarise(switch_rate = mean(target),
            avg_development_index = mean(city_development_index)) %>% 
  arrange(-switch_rate, -avg_development_index)

ggplot(cities, aes(x=avg_development_index, y=switch_rate, color=city)) + 
  geom_point() +
  theme(legend.position = 'none') +
  ggtitle("Average city development index and job switch rate")
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

There’s an interesting pattern between city development level and switch
rate. It seems like the job switch rate is high for cities that have a
development index less than 0.6. However, once a city reaches above 0.6,
the job switch rate shows a downward trend for developed cities.

**The distribution of city development index by different target group**
 

``` r
# The distribution of city development index by different target group
ggplot(hr, aes(x = city_development_index, group=as.factor(target), color=as.factor(target))) + geom_density(stat = 'density')+ ggtitle("Distribution of city development index by target group") + scale_colour_discrete("Switching Job?")
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

More people at a low city development index are looking for job change,
and less at high development index are looking for job change.

**Distribution of training hours by target group**  

``` r
# Distribution of training hours by target group
ggplot(hr, aes(x = training_hours, group=as.factor(target), color=as.factor(target))) + geom_density(stat = 'density')+ ggtitle("Distribution of training hours by target group") + scale_colour_discrete("Switching Job?")
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Training hours seem to not affect job switch a lot.

<br>

# 2\. Data Cleaning

## 2.1 Missing Value

``` r
# Check the percentage of missing value in each column, excluding the dummy
sort(colMeans(is.na(hr)), decreasing = TRUE)
```

    ##            enrollee_id                   city city_development_index 
    ##                      0                      0                      0 
    ##                 gender    relevent_experience    enrolled_university 
    ##                      0                      0                      0 
    ##        education_level       major_discipline             experience 
    ##                      0                      0                      0 
    ##           company_size           company_type           last_new_job 
    ##                      0                      0                      0 
    ##         training_hours                 target 
    ##                      0                      0

At the first look there’s no missing value. But from EDA we saw a couple
categories with empty names. Let’s check if there’s any other type of
missing values.

``` r
# number of columns
num_col <- dim(hr)[2]
# empty vector for later use
num_empty <- integer(num_col)

# loop around all columns to check if there's any empty string
for(i in 1:num_col) {
  cnt <- sum(hr[,i]=='')
  num_empty[i] <- cnt
}

# get the percentage of empty cells in each col
pct_empty <- num_empty/nrow(hr)
# link the empty value to each column names
pct_empty <- setNames(pct_empty, colnames(hr))
# see result
sort(pct_empty, decreasing=TRUE)
```

    ##           company_type           company_size                 gender 
    ##            0.320492745            0.309948846            0.235306399 
    ##       major_discipline        education_level           last_new_job 
    ##            0.146831611            0.024010857            0.022079549 
    ##    enrolled_university             experience            enrollee_id 
    ##            0.020148241            0.003392839            0.000000000 
    ##                   city city_development_index    relevent_experience 
    ##            0.000000000            0.000000000            0.000000000 
    ##         training_hours                 target 
    ##            0.000000000            0.000000000

‘company\_type’, ‘company\_size’, ‘gender’ have have the highest
percentage of missing value. But since none of them has extremely high
percentage (\>60%), we decide to retain all columns. And also since we
plan to mainly use tree based models, imputation might not be absolute
necessary at this point.

## 2.2 Categorical Variables Encoding: Ordinal

Use label encoding for ordinal variables: convert the data type of
ordinal categorical variables into integer

  - education\_level

  - company\_size

  - experience

  - last\_new\_job

  - relevant\_experience

  - enrolled\_university

<!-- end list -->

``` r
# replicate the original data frame
hr.cleaned <- hr
# convert education_level
hr.cleaned$education_level[hr.cleaned$education_level=='Primary School'] <- 0
hr.cleaned$education_level[hr.cleaned$education_level=='High School'] <- 1
hr.cleaned$education_level[hr.cleaned$education_level=='Graduate'] <- 2
hr.cleaned$education_level[hr.cleaned$education_level=='Masters'] <- 3
hr.cleaned$education_level[hr.cleaned$education_level=='Phd'] <- 4
hr.cleaned$education_level <- as.integer(hr.cleaned$education_level)

# convert company_size
# table(hr.cleaned$company_size)
hr.cleaned$company_size[hr.cleaned$company_size=='<10'] <- 0
hr.cleaned$company_size[hr.cleaned$company_size=='10/49'] <- 1
hr.cleaned$company_size[hr.cleaned$company_size=='50-99'] <- 2
hr.cleaned$company_size[hr.cleaned$company_size=='100-500'] <- 3
hr.cleaned$company_size[hr.cleaned$company_size=='500-999'] <- 4
hr.cleaned$company_size[hr.cleaned$company_size=='1000-4999'] <- 5
hr.cleaned$company_size[hr.cleaned$company_size=='5000-9999'] <- 6
hr.cleaned$company_size[hr.cleaned$company_size=='10000+'] <- 7
hr.cleaned$company_size <- as.integer(hr.cleaned$company_size)

# convert experience
# table(hr.cleaned$experience)
hr.cleaned$experience[hr.cleaned$experience=='<1'] <- '0'
hr.cleaned$experience[hr.cleaned$experience=='>20'] <- '21'
hr.cleaned$experience <- as.integer(hr.cleaned$experience)

# convert last_new_job
table(hr.cleaned$last_new_job)
```

    ## 
    ##          >4     1     2     3     4 never 
    ##   423  3290  8040  2900  1024  1029  2452

``` r
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='1'] <- 0
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='2'] <- 1
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='3'] <- 2
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='4'] <- 3
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='>4'] <- 4
hr.cleaned$last_new_job[hr.cleaned$last_new_job=='never'] <- 5
hr.cleaned$last_new_job <- as.integer(hr.cleaned$last_new_job)

# convert relevent experience
#table(hr.cleaned$relevent_experience)
hr.cleaned$relevent_experience[hr.cleaned$relevent_experience=='No relevent experience'] <- 0
hr.cleaned$relevent_experience[hr.cleaned$relevent_experience=='Has relevent experience'] <- 1
hr.cleaned$relevent_experience <- as.integer(hr.cleaned$relevent_experience)

# convert enrolled_university
table(hr.cleaned$enrolled_university)
```

    ## 
    ##                  Full time course    no_enrollment Part time course 
    ##              386             3757            13817             1198

``` r
hr.cleaned$enrolled_university[hr.cleaned$enrolled_university=='no_enrollment'] <- 0
hr.cleaned$enrolled_university[hr.cleaned$enrolled_university=='Part time course'] <- 1
hr.cleaned$enrolled_university[hr.cleaned$enrolled_university=='Full time course'] <- 2
hr.cleaned$enrolled_university <- as.integer(hr.cleaned$enrolled_university)
```

## 2.3 Categorical Variables Encoding: Nominal

Use dummy encoding for nominal variables.

  - city

  - gender

  - enrolled\_university

  - major discipline

  - company\_type

<!-- end list -->

``` r
library(caret)
# move all the non-nominal variables to the front
hr.cleaned <- hr.cleaned[,c(14,1,3,5,6,7,9,10,12,13,4,8,11,2)]
# str(hr)

# dummy encoding all the nominal variables
dmy <- dummyVars("~.", data = hr.cleaned)
hr.cleaned <- data.frame(predict(dmy, newdata = hr.cleaned))
#str(hr.cleaned)
```

<br>

# 3.Modeling

## 3.1Preparation For Modeling

``` r
library(rpart)
library(rpart.plot)
library(xgboost)
library(caTools)
# split the data set into 80% training, 20% testing, while maintaining the distribution of target variable
split <- sample.split(hr.cleaned$target, 0.8)
train <- hr.cleaned[split==TRUE,]
test <- hr.cleaned[split==FALSE,]

# Check if the distributions are still the same
sum(train$target)/nrow(train)
```

    ## [1] 0.2493639

``` r
sum(test$target)/nrow(test)
```

    ## [1] 0.2492822

**Create a function to calculate auc**  

``` r
library(pROC)
# function to get auc on both train and test set
result <- c()
get_auc <- function(model) {
  # train
  predict.train <- predict(model, train)
  auc.train <- roc(train$target, predict.train)
  # test
  predict.test <- predict(model, test)
  auc.test <- roc(test$target, predict.test)
  result <- c(auc.train$auc, auc.test$auc)
  names(result) <- c("train auc", "test auc")
  return(result)
}
```

## 3.2 Logistic Regression

``` r
# logistic regression
model.lr <- glm(target~., data=train, family = gaussian)
# logistic regression
get_auc(model.lr)
```

    ## train auc  test auc 
    ## 0.7981384 0.7688142

Logistic regression outputs a test AUC of 0.77. Not bad for a simple
model.

## 3.2 CART

## 3.2.1 Untuned CART

``` r
# first, define a basic tree
model.cart <- rpart(target~., train)
# basic auc
get_auc(model.cart)
```

    ## train auc  test auc 
    ## 0.7706857 0.7595151

The CART model performs a bit weaker than logistic regression. Let’s see
if we could improve this by tuning.

## 3.2.2 CART Tuning

``` r
train_CART <- train
train_CART$target[train_CART$target==1] <- 'yes'
train_CART$target[train_CART$target==0] <- 'no'
train_CART <- na.omit(train_CART)

# use 10 folds for cross-validation
numFolds <- trainControl(method='cv', 
                         number=10,
                         classProbs = TRUE)
# hand pick a sequence of possible cp values
cpGrid = expand.grid(cp=seq(0.000, 0.02, 0.002))   
# Use the train function to try each of the cp value on cross-validation sets
t <- caret::train(as.factor(target)~., 
                  train_CART,
                  method='rpart',
                  metric="Accuracy",
                  trControl=numFolds, 
                  tuneGrid=cpGrid)
# plot the relationship between cp and accuracy
plot(t$results$cp, t$results$Accuracy, type='b', ylab="Accuracy", xlab="cp")
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
t
```

    ## CART 
    ## 
    ## 10168 samples
    ##   150 predictor
    ##     2 classes: 'no', 'yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 9152, 9151, 9151, 9151, 9151, 9152, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp     Accuracy   Kappa    
    ##   0.000  0.8306457  0.3267903
    ##   0.002  0.8528721  0.4476017
    ##   0.004  0.8552327  0.4774990
    ##   0.006  0.8553316  0.4781697
    ##   0.008  0.8536600  0.4728989
    ##   0.010  0.8537584  0.4733527
    ##   0.012  0.8533649  0.4789810
    ##   0.014  0.8536599  0.4821890
    ##   0.016  0.8536599  0.4821890
    ##   0.018  0.8536599  0.4821890
    ##   0.020  0.8536599  0.4821890
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.006.

The best complexity parameter is 0.006

### 3.2.3 Tuned CART Performance

``` r
# fit with the tuned tree
model.cart <- rpart(target~., train, control = rpart.control(cp = t$bestTune$cp))
get_auc(model.cart)
```

    ## train auc  test auc 
    ## 0.7732642 0.7623894

``` r
# visualize the CART model
rpart.plot(model.cart)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

The tuned CART model has a slightly improved AUC of 0.76, still not as
good as logistic regression. However, the plot provides some useful
insights. According to the plot, ‘city\_development\_index’ is the most
significant predictor. For candidates living in a city with a
development index less than 0.62, the likelihood of job search is 59%.
Company\_type comes second. If company\_type is not missing, the chance
that the candidate looking for job switch is around 10%.

## 3.3 XGBoost

### 3.3.1 Untuned XGBoost

``` r
col.xgb <- c(2:151)
model.xgboost <- xgboost(data = data.matrix(train[,col.xgb]),
                  label = train[,1],
                  nround=100,
                  eval_metric = "auc",
                  objective = "binary:logistic",
                  verbose = 0
                  )
```

### 3.3.2 Untuned XGBoost Performance

``` r
# xgboost on train
pred.xgb.train <- predict(model.xgboost, as.matrix(train[,col.xgb]))
auc.xgb.train <- roc(train$target, pred.xgb.train)
auc.xgb.train
```

    ## 
    ## Call:
    ## roc.default(response = train$target, predictor = pred.xgb.train)
    ## 
    ## Data: pred.xgb.train in 11505 controls (train$target 0) < 3822 cases (train$target 1).
    ## Area under the curve: 0.9351

``` r
# xgboost on test
pred.xgb <- predict(model.xgboost, as.matrix(test[,col.xgb]))
test$pred.xgb <- pred.xgb
auc.xgb <- roc(test$target, test$pred.xgb)
auc.xgb
```

    ## 
    ## Call:
    ## roc.default(response = test$target, predictor = test$pred.xgb)
    ## 
    ## Data: test$pred.xgb in 2876 controls (test$target 0) < 955 cases (test$target 1).
    ## Area under the curve: 0.7879

The raw XGBoost model scored an AUC of 0.79 on the test set. It’s the
best performing model so far. Let’s see if we could further improve it.

### 3.3.3 XGBoost Tuning

**XGBoost Tuning: nround**  

What is nround?: It controls the maximum number of iterations. For
classification, it is similar to the number of trees to grow.

``` r
# convert dtaframe to DMatrix
dtrain <- xgb.DMatrix(data.matrix(train[,col.xgb]), label=train$target)
dtest <- xgb.DMatrix(data.matrix(test[,col.xgb]), label=test$target)

# setup the default parameter
params <- list(booster="gbtree", objective="binary:logistic", eval_metric='auc' , eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

# Tuning the parameter 'nrounds' using cross validation
xgbcv <- xgb.cv(params = params, data=dtrain, nrounds=100, nfold=5, showsd=T, stratified=T, print.every.n=10, early_stopping_rounds = 20, maximize = T)
```

    ## [1]  train-auc:0.812272+0.002582 test-auc:0.787053+0.013003 
    ## Multiple eval metrics are present. Will use test_auc for early stopping.
    ## Will train until test_auc hasn't improved in 20 rounds.
    ## 
    ## [11] train-auc:0.861105+0.002973 test-auc:0.800435+0.013501 
    ## [21] train-auc:0.879350+0.004559 test-auc:0.800792+0.013766 
    ## Stopping. Best iteration:
    ## [8]  train-auc:0.849104+0.003489 test-auc:0.801379+0.012724

``` r
xgbcv$best_iteration
```

    ## [1] 8

``` r
# The best nrounds = 8
max(xgbcv$evaluation_log$test_auc_mean)
```

    ## [1] 0.8013788

``` r
# The highest auc on CV set is 0.8014
```

**XGBoost Tuning: grid search**  

Here we tune the following other parameters:

  - eta: It controls the learning rate, i.e., the rate at which our
    model learns patterns in data. After every round, it shrinks the
    feature weights to reach the best optimum.

  - max\_depth: It controls the depth of the tree. Larger the depth,
    more complex the model; higher chances of over-fitting.

  - gamma: It controls regularization (or prevents over-fitting). The
    optimal value of gamma depends on the data set and other parameter
    values.

  - colsample\_bytree: It control the number of features (variables)
    supplied to a tree

<!-- end list -->

``` r
# set up the cross-validated hyper-parameter search
xgb_grid = expand.grid(
  nrounds=xgbcv$best_iteration,
  eta = c(0.1, 0.2, 0.3),
  max_depth = c(3,6,8,10),
  gamma = c(0,1,5),
  colsample_bytree=c(0.5,0.75,1),
  min_child_weight=1,
  subsample=1
)

# pack the training control parameters
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",               # save losses across all models
  classProbs = TRUE,                  # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
)

# train the model for each parameter combination in the grid,
# using CV to evaluate
train_target = ifelse(train[,1]==1, 'yes','no')
xgb_train_1 = caret::train(x=data.matrix(train[,col.xgb]), y=train_target, trControl = xgb_trcontrol, tuneGrid = xgb_grid, method = "xgbTree")
```

    ## Selecting tuning parameters
    ## Fitting nrounds = 8, max_depth = 6, eta = 0.3, gamma = 0, colsample_bytree = 0.75, min_child_weight = 1, subsample = 1 on full training set

### 3.3.4 Tuned XGBoost Model Fitting

``` r
model.xgboost.super <- xgboost(data = data.matrix(train[,col.xgb]),
                  label = train[,1],
                  nrounds = 8, 
                  max_depth = 6,
                  eta = 0.3, 
                  gamma = 0, 
                  colsample_bytree = 0.75, 
                  min_child_weight = 1, 
                  subsample = 1,
                  eval_metric = "logloss",
                  objective = "binary:logistic",
                  verbose = 0
                  )
```

### 3.3.5 Tuned XGBoost Model Performance - AUC

``` r
# xgboost on train
pred.xgb.s.train <- predict(model.xgboost.super, as.matrix(train[,col.xgb]))
auc.xgb.s.train <- roc(train$target, pred.xgb.s.train)
auc.xgb.s.train
```

    ## 
    ## Call:
    ## roc.default(response = train$target, predictor = pred.xgb.s.train)
    ## 
    ## Data: pred.xgb.s.train in 11505 controls (train$target 0) < 3822 cases (train$target 1).
    ## Area under the curve: 0.8434

``` r
# xgboost on test
pred.xgb.s <- predict(model.xgboost.super, as.matrix(test[,col.xgb]))
test$pred.xgb.s <- pred.xgb.s
auc.xgb.s <- roc(test$target, test$pred.xgb.s)
auc.xgb.s
```

    ## 
    ## Call:
    ## roc.default(response = test$target, predictor = test$pred.xgb.s)
    ## 
    ## Data: test$pred.xgb.s in 2876 controls (test$target 0) < 955 cases (test$target 1).
    ## Area under the curve: 0.8029

``` r
#plot the ROC on the test set
library(ROCR)
ROCRPred <- prediction(test$pred.xgb.s, test$target)
ROCRperf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0.1,1,by=0.1), text.adj=c(-0.2,1.7))
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

Best AUC on the test set: 0.80. Slightly improves from the untuned
XGBoost.

### 3.3.6 Tuned XGBoost Model Performance - Confusion Matrix

``` r
# confusion matrix using the 0.5 cut-off:
xgb.pred <- ifelse(pred.xgb.s>0.5,1,0)
cm <- confusionMatrix(as.factor(xgb.pred), as.factor(test$target), positive='1')
cm
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 2523  441
    ##          1  353  514
    ##                                           
    ##                Accuracy : 0.7927          
    ##                  95% CI : (0.7796, 0.8055)
    ##     No Information Rate : 0.7507          
    ##     P-Value [Acc > NIR] : 4.859e-10       
    ##                                           
    ##                   Kappa : 0.4287          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.002018        
    ##                                           
    ##             Sensitivity : 0.5382          
    ##             Specificity : 0.8773          
    ##          Pos Pred Value : 0.5928          
    ##          Neg Pred Value : 0.8512          
    ##              Prevalence : 0.2493          
    ##          Detection Rate : 0.1342          
    ##    Detection Prevalence : 0.2263          
    ##       Balanced Accuracy : 0.7077          
    ##                                           
    ##        'Positive' Class : 1               
    ## 

Interpretation:

  - Sensitivity indicates that among all the positive cases, our model
    will be able to identify 54% of them.

  - Specificity indicates that among all the cases the model predict
    positive, 88% of them will be actual positive.

This result is good for the purpose of our client. Since training and
planning for a candidate is a costly matter, we don’t want to waste a
lot of resources on those who are not looking for a job change. On the
other hand, it’s okay if we miss some candidates who are indeed looking
for job changes, as the loss is not as big as wasting resources on
unpromising candidates.

### 3.3.7 Tuned XGBoost Model Performance - Feature Importance

``` r
#view variable importance plot
mat <- xgb.importance (feature_names = colnames(test[,-1]),model = model.xgboost.super)
xgb.plot.importance (importance_matrix = mat[1:20]) 
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

According to our best model, city\_development\_index is the best
predictor to whether a candidate is looking for job change or not. From
our previous exploratory analysis, it seems like candidates in a less
developed city are more likely to look for job change. The second
highest predictor is company size. For this one we don’t have much
insight as missing value contributes to the highest company size
category. Again, further investigation on the data collection process
and the missing patter need to be done to get insights.

<br>

# 4\. Business Implication

Now let’s explore the potential business implication of our best model.

## 4.1 Baseline approach

First, let’s set a baseline for business value by taking a naive
approach. This approach assumes that the company adopts mass targeting
to all the candidates. This means to invest a certain amount of
resources, effort, and training for each of the candidates in hope that
some of them are looking for a job switch. Let’s assume it costs the
compnay -$50 for mass targeting, and will earn a profit of $200 if a
candidate is looking for job switch:

``` r
# naive approach
(sum(hr$target)*200)-(nrow(hr)*50)
```

    ## [1] -2500

The naive approach generates a loss of -$2,500 for the company. Now
let’s see if the model does better.

## 4.2 Model and Monetary Matrix

To evaluate the monetary impact of the model, we assign a monetary value
to each of the possible cases generates by the output of the tuned
XGBoost model:

  - True Positive
      - A true positive means the model successfully identify a person
        who’s looking for job change. This has the same benefits as the
        successful cases baseline model. We assign this category a
        monetary value of $200
  - False Positive
      - A false positive means the model falsely identify someone who’s
        not looking for a job as a prospect. Having the model indicates
        a candidate to be a prospect, the company is likely to spend
        more resources than mass targeting, assuming the sucess rate is
        higher. Thus we assign this category a monetary value of -$100.
  - True Negative
      - A true negative means the model correctly identify someone who’s
        not looking for job as s/he’s not looking for job. This does not
        bring extra value nor cost the company anything. Thus we assign
        this category a monetary value of $0.
  - False Negative
      - A False negative means the model falsely identify someone who’s
        actually looking for a job as someone who’s not. Company loses
        potential talent by the false categorization. But since this
        would be relatively smaller loss comparing to the false
        positive, we assign this category with a monetary value of -$50.

Hence we have a monetary matrix that looks like this:

<table style="width:69%;">
<colgroup>
<col style="width: 25%" />
<col style="width: 22%" />
<col style="width: 22%" />
</colgroup>
<thead>
<tr class="header">
<th></th>
<th>Actual False</th>
<th>Actual True</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Predicted False</td>
<td><pre><code>\$0</code></pre></td>
<td>-$50</td>
</tr>
<tr class="even">
<td>Predicted True</td>
<td>-$100</td>
<td>$200</td>
</tr>
</tbody>
</table>

Now let’s construct it in R

``` r
library(rlist)
# construct the monetary matrix
M <- matrix(c(0,-50,-100,200), nrow = 2, byrow = TRUE)

# multiply the confusion matrix with the monetary matrix
exp_profit <- M * cm$table

# set up a vector to store the threshold and totol profit
max_profit <- c()
# loop through all possible threshold and find the maximum profit and best threshold
for(i in seq(0.01,1,0.01)) {
  xgb.pred <- ifelse(pred.xgb.s>i,1,0)
  cm <- confusionMatrix(as.factor(xgb.pred), as.factor(test$target), positive='1')
  exp_profit <- M * cm$table
  max_profit <- list.append(max_profit,sum(exp_profit))
}

max_profit <- setNames(max_profit,seq(0.01,1,0.01))
# plot the threshold and profit
plot(seq(0.01,1,0.01), max_profit)
```

![](Kun_HR_Analytics_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
# return the best threshold and profit
max_profit[max_profit==max(max_profit)]
```

    ##  0.33 
    ## 70800

The max revenue by implementing the model will be $69,350 at a threshold
of 0.3. This is a massive improvement from the baseline model. Thus we
suggest the company to adopt the model to help them identify potential
job seekers in their training program. One thing that they might need to
keep in mind after deployment is that the trend and pattern captured in
our training data could change with time. Therefore, it’s necessary to
also implement an insurance mechanism to periodically review the
performance of the model into the model pipeline.
