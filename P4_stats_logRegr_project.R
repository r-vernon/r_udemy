
# load necessary packages
library(tidyverse)
library(patchwork) # allows subplots
library(corrplot)
library(caTools)
library(countrycode) # look up countries

# ------------------------------------------------------------------------------
# read in the data
# X - index (i)
# age - age (d)
# type_employer - State/Pricate etc (f)
# fnlwgt - final weight, num people entry represents (i)
# education - Bachelors/HS-grad etc (f)
# education_num - education in numerical form (i)
# marital - marriage status (f)
# occupation - (f)
# relationship - wife, husband etc, likely not useful (f)
# race - white, black etc (f)
# sex - male/female (f)
# capital_gain - (d)
# capital_loss - (d)
# hr_per_week - hours worked per week (d)
# country - (f)
# income - makes more than 50k or not '<=50k', '>50k' (f)
adult <- read_csv('dataFiles/adult_sal.csv', name_repair=make.names, 
                  col_types='idfififffffdddff')

# NOTE
# - a lot of the options taken where following a guide
# - I suspect in reality you'd be more cautious collapsing continuous 
#   predictors into categorical ones, due to loss of data!

# ------------------------------------------------------------------------------
# initial data massaging

# replace ? with NA
adult[adult=='?' | adult==' ?'] <- NA

# find colnames with NAs and drop any empty levels (as '?' was prev. a factor)
naCols <- colnames(adult[,colSums(is.na(adult)) > 0])
adult[,naCols] <- droplevels(adult[,naCols])

# drop na values in this example
adult <- na.omit(adult)

# drop index
adult <- select(adult, -X)

# check out employer type
table(adult$type_employer)
# collapse some groups
adult$type_employer <- fct_collapse(adult$type_employer, 
    Unemployed=c('Never-worked', 'Without-pay'), 
    SL_gov=c('State-gov', 'Local-gov'), 
    Self_emp=c('Self-emp-not-inc', 'Self-emp-inc'))

# collapse education (first making sure levels are ordered)
eduLevels <- distinct(adult[,c('education','education_num')]) %>% 
    arrange_at('education_num')
adult$education <- ordered(adult$education, 
    levels=as.vector(eduLevels$education))
# remap to roughly equal levels
adult <- mutate(adult, edu=ordered(case_when(
    education_num < 9 ~ '<HS', 
    education_num == 9 ~ 'HS', 
    education_num == 10 ~ 'Some College', 
    education_num < 13 ~ 'Assoc', 
    education_num == 13 ~ 'Grad', 
    education_num == 14 ~ 'Postgrad',
    education_num > 14 ~ 'Postdoc'), 
    levels=c('<HS', 'HS', 'Some College', 'Assoc', 
             'Grad', 'Postgrad', 'Postdoc')), 
    .after='education_num')

# collapse groups in marital status 
adult$marital <- fct_collapse(adult$marital, 
    Married=c('Married-AF-spouse', 'Married-civ-spouse', 'Married-spouse-absent'), 
    Not_married=c('Divorced', 'Separated', 'Widowed'), 
    Never_married=c('Never-married'))

# now for countries, first some ambiguous ones:
# - Hong likely Hong Cong as race mostly Asian, similarly South = South Korea
# - England, Scotland should map to United Kingdom
# - Columbia should be Colombia
adult <- mutate(adult, country=fct_recode(country, 
    'Hong Kong'='Hong', 'South Korea'='South',  'United Kingdom'='England', 
    'United Kingdom'='Scotland', 'Colombia'='Columbia'))
# then lets remap (using region: 7 regions defined by World Bank Dev. Index)
adult <- mutate(adult, region=as.factor(countrycode(country, 
    origin='country.name', destination='region')), .after=country)

# add binary function for income
adult$gtr50 <- adult$income == '>50K'

# ------------------------------------------------------------------------------
# now to visualise

# quick function to est. bin width using various rules
# not necessarily using it, just as a guide
nb <- function (x) {
    x.n <- length(x)
    x.r <- diff(range(x))
    nb_St <- ceiling(log2(x.n)) + 1 # Sturges
    nb_Ri <- ceiling(2*(x.n^(1/3))) # Rice
    nb_Sc <- ceiling(x.r / (3.49*sd(x)*(x.n^(-1/3)))) # Scotts
    nb_FD <- ceiling(x.r / (2*IQR(x)*(x.n^(-1/3)))) # Freedman-Diaconis
    return(c(nb_St, nb_Ri, nb_Sc, nb_FD))
    }

# check age distribution
pl <- ggplot(adult, aes(age)) + 
    geom_histogram(bins=30, color='black', aes(fill=income))
print(pl)

# check hours worked distribution
pl <- ggplot(adult, aes(hr_per_week)) + 
    geom_histogram(bins=40, color='black', aes(fill=income))
print(pl)
# huge spike at certain value - could make categorical!

# lets work out how you might make 'hr_per_week' categorical
hrVals <- adult %>% group_by(hr_per_week) %>%  summarise(n=n()) 
hrVals$cumsum_prop <- round(100 * (cumsum(hrVals$n)/sum(hrVals$n)),2)
# looks like natural groups might be 
# - <35 (part time), 15.55% data
# - 35-40 (full time), 53.96% data
# - 40+ (long week!), 30.49% data
# not going to do it though, as why throw away data!
# (just interesting...)

# check by region
# - group by region, income and summarise counts (n) for each
# - then drop last group (income) so now only grouped by region
# - then calc. frequency by dividing each n by the sum of n per region
regionFreq <- adult %>% group_by(region, income) %>%
    summarise(n=n(), .groups='drop_last') %>%
    mutate(freq=n/sum(n))
pl <- ggplot(regionFreq, aes(x=region, y=freq, fill=income)) + 
    geom_col(position=position_dodge()) + 
    theme(axis.text.x = element_text(angle=90))
print(pl)

# check by education
eduFreq <- adult %>% group_by(edu, income) %>%
    summarise(n=n(), .groups='drop_last') %>%
    mutate(freq=n/sum(n))
pl <- ggplot(eduFreq, aes(x=edu, y=freq, fill=income)) + 
    geom_col(position=position_dodge()) + 
    theme(axis.text.x = element_text(angle=90))
print(pl)
# very clear relationship here!

# ------------------------------------------------------------------------------
# build the model

# split into training and test data
adult_sample <- sample.split(adult$gtr50, SplitRatio=0.7) # randomly segment
adult_train <- subset(adult, adult_sample == T) # select training data
adult_test <- subset(adult, adult_sample == F) # select test data

# create the model
# - ignoring fnlwt, relationships, capitalGains/Losses as too vague or poor data
# - ignoring race as don't have balanced sample!
m1 <- glm(gtr50 ~ age + type_employer + edu + marital + occupation + sex + 
    hr_per_week + region, family=binomial(link='logit'), data=adult_train)
summary(m1)

# use stepwise search (def. forwards + backwards) to refine model
m2 <- step(m1)
summary(m2)

# calculate misclassification error
m1_prob <- predict(m1, adult_test, type='response')
m1_results <- ifelse(m1_prob>0.5, 1, 0)
misClassErr <- mean(m1_results != adult_test$gtr50)
print(1-misClassErr)

# create confusion matrix
cm <- table(adult_test$gtr50, m1_prob>0.5, dnn=c('testData','modelPred'))
print(round(cm/nrow(adult_test),2)) 

# for context, lets see how well the most naive of models would do!
train_TP <- sum(adult_train$gtr50) / nrow(adult_train)
test_TP <- sum(adult_test$gtr50) / nrow(adult_test)
naive_cm <- matrix(c(
    (1-test_TP)*(1-train_TP), (1-test_TP)*train_TP, 
    test_TP*(1-train_TP), test_TP*train_TP), ncol=2, byrow=T)
colnames(naive_cm) <- c('FALSE', 'TRUE')
rownames(naive_cm) <- c('FALSE', 'TRUE')
naive_cm <- as.table(naive_cm)
print(round(naive_cm,2)) 
