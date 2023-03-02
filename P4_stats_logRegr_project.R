
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
eduLevels <- arrange_at(distinct(adult[,c('education','education_num')]),
    'education_num')
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
adult <- mutate(adult, region=countrycode(country, 
    origin='country.name', destination='region'), .after=country)
adult$region <- as.factor(adult$region)

# ------------------------------------------------------------------------------
# now to visualise