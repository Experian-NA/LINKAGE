library(dplyr)
library(ggplot2)
library(caret)
#plot out age/sex/class comparision
## results
### female in first class has significant survivial
### female in second class has significant survival
### female in third class has significant survivial for younger and diminishing returns for higher age

#load titanic training data
titanic <- read.csv("train.csv", stringsAsFactors = FALSE)


ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Count",
       x = "Age",
       title = "Titanic Age/Class/Sex") +
  theme(plot.title = element_text(hjust=0.5))


### how to deal with missing age values?
# dummyVar results are poor for populating missing age values
# Kirk advises this is traditionally done when blank values are 5% or less

# tag instances where age was reported for historic reporting
titanic$Age.Reported <- ifelse(is.na(titanic$Age),"N", "Y")

#fill in blank values in Embarked
# S is the more popular port so it is used to fill in blanks
titanic$Embarked[titanic$Embarked == ""] <- "S"

# identify fields that should be factor
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Age.Reported <- as.factor(titanic$Age.Reported)


# create dataset with missing age to review
titanic.age.no <- titanic %>%
  filter (is.na(Age))

# create dataset with no missing age to review
titanic.age.yes <- titanic %>%
  filter (!is.na(Age))

####################REVIEW NAME TITLES IN FULL TRAINING FILE######################
# 
# name titles appear to be standardized accross the board and most match up with 
# SibSP and ParCh values
# Identify groups of passangers with the same title/class 
# and populate a calculated age for the NA's.
# !!!!still need to address blank Mr. fields.!!!!!!!!!!!11
###################################################################################

# 'Mrs' (title used for married females)
View(titanic %>% filter(grepl('Mrs.', titanic$Name) == TRUE))
# results show promise in using Mrs. as married female field

# 'Miss' (title used for unmarried females)
View(titanic %>% filter(grepl('Miss.', titanic$Name) == TRUE))
# results are mixed, need to use SipSp/ParCh fields to create rules to populate missing Age field

# 'Ms.' (title for all females no unqiue grouping)
View(titanic %>% filter(grepl('Ms.', titanic$Name) == TRUE ))
# results show only one passanger found and age is already populated

# 'Mme.' (title used used for french title of madam)
View(titanic %>% filter(grepl('Mme', titanic$Name) == TRUE ))
# results show only one passanger found and age is already populated

# 'Mlle' (title used in France aka Mademoiselle)
View(titanic %>% filter(grepl('Mlle', titanic$Name) == TRUE))
# results show only two passangers were found and age is already populated

#inspect male names sorted by age
View(titanic %>% filter(Sex == 'male'))

#'Master' (title used for male children)
View(titanic %>% filter(grepl('Master', titanic$Name) == TRUE))
#results show Master is used for males in the age range of 0 to 12 and MR. is for 13 and more

#'Mr.' (title used for male adults)
View(titanic_backup %>% filter(grepl('Mr. ', titanic_backup$Name) == TRUE))

#############  Populate missing AGE values using title rules ###################
# calculate the mean of all passangers were the age is populated with 'Master' in name

# 'Master' (title used for male children)
x <-titanic.age.yes %>% 
  filter(grepl('Master', Name) == TRUE)
mean.master <- mean(x$Age)

# 'Mlle' (title used in France aka Mademoiselle)
x <-titanic.age.yes %>% 
  filter(grepl('Mlle.', Name) == TRUE)
mean.mlle <- mean(x$Age)

# 'Mme.' (title used used for french title of madam)
x <-titanic.age.yes %>% 
  filter(grepl('Mme.', Name) == TRUE)
mean.mme <- mean(x$Age)

# 'Ms.' (title for all females no unqiue grouping)
x <-titanic.age.yes %>% 
  filter(grepl('Ms.', Name) == TRUE)
mean.ms <- mean(x$Age)

# 'Miss' (title used for unmarried females)
x <-titanic.age.yes %>% 
  filter(grepl('Miss.', Name) == TRUE)
mean.miss <- mean(x$Age)

# 'Mrs' (title used for married females)
x <-titanic.age.yes %>% 
  filter(grepl('Mrs.', Name) == TRUE)
mean.mrs <- mean(x$Age)

# 'Mr. ' (title used for adult males)
x <- titanic.age.yes %>%
  filter(grepl('Mr. ', Name) == TRUE)
mean.mr <- mean(x$Age)



for(i in 1:nrow(titanic)) {
  if((is.na(titanic[i,6])) & (grepl('Mrs.',titanic[i,4]) == TRUE))
    { titanic[i,6] <- mean.mrs
 } else if ((is.na(titanic[i,6])) & (grepl('Ms.',titanic[i,4]) == TRUE))
    {titanic[i,6] <- mean.ms
 } else if ((is.na(titanic[i,6])) & (grepl('Miss.',titanic[i,4]) == TRUE))
    {titanic[i,6] <- mean.miss
 } else if ((is.na(titanic[i,6])) & (grepl('Mme.',titanic[i,4]) == TRUE))
    {titanic[i,6] <- mean.mme
 } else if ((is.na(titanic[i,6])) & (grepl('Mlle.',titanic[i,4]) == TRUE))
    {titanic[i,6] <- mean.mlle
 } else if ((is.na(titanic[i,6])) & (grepl('Master',titanic[i,4]) == TRUE))
    {titanic[i,6] <- mean.master
 } else if ((is.na(titanic[i,6])) & (grepl('Mr. ',titanic[i,4]) == TRUE))
 {titanic[i,6] <- mean.mr
 }
}

#populates any Age value still as NA to 35
titanic$Age[is.na(titanic$Age)] <- 35
##  View(titanic)
#############  CREATE NEW FEATURES IN TRAINING FILE #######################

# NEW FEATURE family size on boat

# NEW FEATURE single adult male (over 12)

# NEW FEATURE single adult female

# NEW FEATURE married couple with children

# NEW FEATURE married couple with no children

## divide up 

##############################split up data to train and test on ########## 
## pulled from knn model example from David Lagner
## changes were made to fit enviornment
# David Lagner -- Use caret to create a 70/30% split of the training data,
# David Lagner -- keeping the proportions of the Survived class label the
# David Lagner -- same across splits.
titanic_backup <- titanic
## titanic <- titanic_backup
features <- c("Survived", "Pclass", "Sex", "Age", "SibSp",
              "Parch", "Fare", "Embarked", "Age.Reported")

titanic <- titanic[, features]

set.seed(10)
indexes <- createDataPartition(titanic$Survived,
                               times = 1,
                               p = 0.7,
                               list = FALSE)
titanic.train <- titanic[indexes,]
titanic.test <- titanic[-indexes,]


# Examine the proportions of the Survived class lable across
# the datasets.
prop.table(table(titanic$Survived))
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))



##################  RUN MODELS   #########################
## This is from Kirk Mettler (lm and glm)
## Run these first with new training file then move on to new models

model.lm <- lm(as.numeric(Survived)~.,data = titanic.train)
summary(model.lm)
pred.lm <- predict(model.lm,titanic.test)

confusionMatrix(pred.lm, titanic.test$Survived)
 #pred.lm is not factor w/2 levels, unknown????


##titanic.model.glm <- glm(Survived~.,family = "binomial",data = titanic.train)
##summary(titanic.model.glm)
##preds.glm <- predict(titanic.model.glm, titanic.train, type="response")
##confusionMatrix(preds.glm, titanic.train$Survived)


##SVM
##KNN
##Dtree




