# Titanic Survival Prediction
# Author : Vana Panagiotou
# Date : 03/11/2017

# Set your working directory
setwd("~/online teaching/Projects from Internet/Prediction of the survival of passengers in Titanic")


# Load training and testing data
train_set <- read.csv("train.csv")
test_set  <- read.csv("test.csv")

# Display variables and understand their data types
str(train_set)
str(test_set)


# The training set has 891 observations and 12 variables and the testing set has 
# 418 observations and 11 variables, which means that the traning set has 1 extra variable. 
# Check which variable is missing from the test set. 

colnames_check <- colnames(train_set) %in% colnames(test_set) 
# colnames_check has a False value for the missing variable
colnames(train_set[colnames_check==FALSE])

# As we can see we are missing the "Survived" variable in the test set, 
# which is something that was expected, since we must predict this by creating a model



#### EXPLORATORY ANALYSIS


# Examine what number and what percentage of passengers survived
table(train_set$Survived)
# 0   1   (0=Perished,1=Survived)
# 549 342 
prop.table(table(train_set$Survived))
# 0         1          (0=Perished,1=Survived)
# 0.6161616 0.3838384

# Summarize the number and the percentage of passengers survived
cbind(Amount = table(train_set$Survived), Percentage = prop.table(table(train_set$Survived)))
#    Amount Percentage  (0=Perished,1=Survived)
# 0    549  0.6161616
# 1    342  0.3838384

# Find the distribution of people across classes
table(train_set$Pclass)
#  1   2   3  (Pclass)
# 216 184 491 

# Check if the passenger class has an impact on survival
prop.table(table(train_set$Pclass,train_set$Survived), margin = 1)
#           0         1 (0=Perished,1=Survived)
# 1 0.3703704 0.6296296
# 2 0.5271739 0.4728261
# 3 0.7576375 0.2423625

# In Titanic the captain gave order for women and children to be saved first. 
# So, we look into the training set's "Sex" and "Age" variables for any patterns
prop.table(table(train_set$Sex,train_set$Survived), margin = 1)
#                0         1   (0=Perished,1=Survived)
# female 0.2579618 0.7420382
# male   0.8110919 0.1889081


# Examine passengers' age
summary(train_set$Age)
# There are 177 NA's in Age. But since Age is a very important variable for prediction
# we have to continue the analysis in order to get a better insight from other variables 
# that will help us to tackle the missing values from Age

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.42   20.12   28.00   29.70   38.00   80.00     177 


# Check for missing values (empty or NA) in the training set
library(reshape)
train_set.missing <- melt(apply(train_set[, -2], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(train_set.missing)[train_set.missing$value>0], train_set.missing[train_set.missing$value>0,])

#      [,1]       [,2] 
# [1,] "Age"      "177"
# [2,] "Cabin"    "687"
# [3,] "Embarked" "2" 


# Check for missing values (empty or NA) in the testing set
test_set.missing <- melt(apply(test_set[, -2], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(test_set.missing)[test_set.missing$value>0], test_set.missing[test_set.missing$value>0,])


#      [,1]    [,2] 
# [1,] "Age"   "86" 
# [2,] "Fare"  "1"  
# [3,] "Cabin" "327"

# We see that we have missing values in Age, Cabin and Embarked in the training set and 
# Age, Fare and Cabin in the testing set.
# To tackle this problem, we are going to predict the missing values with the full data set, 
# which means that we need to combine the training and testing sets together.


test_set2 <- test_set
test_set2$Survived <- NA
# Combine training and testing sets 
full_set <- rbind(train_set, test_set2)

# Check for missing values (empty or NA) in the full set (training + testing)
full_set.missing <- melt(apply(full_set[, -2], 2, function(x) sum(is.na(x) | x=="")))
cbind(row.names(full_set.missing)[full_set.missing$value>0], full_set.missing[full_set.missing$value>0,])
#      [,1]       [,2]  
# [1,] "Age"      "263" 
# [2,] "Fare"     "1"   
# [3,] "Cabin"    "1014"
# [4,] "Embarked" "2"



# Explore existing variables and create new variables that will help in the prediction

####  Variable "Cabin"
# "Cabin" is missing a lot of its values

# Create a Deck variable (A - F) by separating and pulling off the deck letter contained in 
# the Cabin
full_set$Deck <- sapply(as.character(full_set$Cabin), function(x) strsplit(x, NULL)[[1]][1])

table(full_set$Deck)
# A  B  C  D  E  F  G  T 
# 22 65 94 46 41 21  5  1 

# We see that there is a Deck value "T", which is invalid. We replace it with NA.
wrong_deck_row <- which(full_set$Deck == 'T')
full_set$Deck[wrong_deck_row] = NA

# Then replace all NA values with U (for Unknown)
full_set$Deck[which(is.na(full_set$Deck))] <- "U"

# Since this column has so many missing values, we will not further use it


####  Variable "Embarked"

# Find which passengers have missing Embarked variables
embarked.missing_rows <- which(is.na(full_set$Embarked) | full_set$Embarked=="")
full_set[embarked.missing_rows, ]

#      PassengerId Survived Pclass                                     Name    Sex Age SibSp Parch
# 62           62        1      1                       Icard, Miss. Amelie female  38     0     0
# 830         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0
#     Ticket Fare Cabin Embarked Deck
# 62  113572   80   B28             B
# 830 113572   80   B28             B

# We will infer their values for embarkment based on present data that seem relevant: 
# passenger class (Pclass) and fare (Fare)
# We see that passengers who have missing Embarked variables paid $80 and their class is 1

# Get rid of these two passengers with the missing Embarked variables
full_set.without_embarked <- full_set[-embarked.missing_rows,]

# Select only the values of 'Fare' and 'Embarked' for passengers from 1st Class
new_data1 <- subset(full_set.without_embarked, Pclass==1, select=c(Fare,Embarked))


embark_fare <- aggregate(Fare~Embarked,new_data1,median)
# Embarked    Fare
# 1        C 76.7292
# 2        Q 90.0000
# 3        S 52.0000

# The median fare for a 1st Class passenger departing from Cherbourg ('C') is 76.7292
# The median fare for a 1st Class passenger departing from Queenstown ('Q') is 90.0000
# The median fare for a 1st Class passenger departing from Southampton ('S') is 52.0000

# Since, passengers who have missing Embarked variables paid $80, we can replace their NA
# values with 'C'
full_set$Embarked[embarked.missing_rows] <- 'C'


####  Variable "Fare"
summary(full_set$Fare)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   7.896  14.454  33.295  31.275 512.329       1

# We see that there is zero fare
subset(full_set, Fare == 0, select = c(Age, Fare))


# There might be some error, since we see that 0 fares are not corresponding to infants,
# that possibly were allowed to travel free of cost.
# Replace 0 fares with the median values of the fares corresponding to each passenger class
# (Pclass) and embarkment (Embarked).

# class_embark_fare
cl_em_fa <- aggregate(Fare ~ Pclass + Embarked, full_set, median)

#  Pclass Embarked    Fare
#  1      1        C 78.2667   cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='C']
#  2      2        C 15.3146   cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='C']
#  3      3        C  7.8958   cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='C']
#  4      1        Q 90.0000   cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='Q']
#  5      2        Q 12.3500   cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='Q']
#  6      3        Q  7.7500   cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='Q']
#  7      1        S 52.0000   cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='S']
#  8      2        S 15.3750   cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='S']
#  9      3        S  8.0500   cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='S']



# Replace 0 fare for those departured from Cherbourg ('C')
full_set$Fare <- 
       ifelse( (round(full_set$Fare==0) & full_set$Pclass==1 & full_set$Embarked =='C'),cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='C'],
               ifelse( (round(full_set$Fare==0) & full_set$Pclass==2 & full_set$Embarked =='C'),cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='C'],
                       ifelse( (round(full_set$Fare==0) & full_set$Pclass==3 & full_set$Embarked =='C'),cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='C'], full_set$Fare)))

# Replace 0 fare for those departured from Queenstown ('Q')      
full_set$Fare <- 
       ifelse( (round(full_set$Fare==0) & full_set$Pclass==1 & full_set$Embarked =='Q'),cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='Q'],
               ifelse( (round(full_set$Fare==0) & full_set$Pclass==2 & full_set$Embarked =='Q'),cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='Q'],
                       ifelse( (round(full_set$Fare==0) & full_set$Pclass==3 & full_set$Embarked =='Q'),cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='Q'], full_set$Fare)))


# Replace 0 fare for those departured from Southampton ('S')      
full_set$Fare <- 
       ifelse( (round(full_set$Fare==0) & full_set$Pclass==1 & full_set$Embarked =='S'),cl_em_fa$Fare[cl_em_fa$Pclass==1 & cl_em_fa$Embarked=='S'],
               ifelse( (round(full_set$Fare==0) & full_set$Pclass==2 & full_set$Embarked =='S'),cl_em_fa$Fare[cl_em_fa$Pclass==2 & cl_em_fa$Embarked=='S'],
                       ifelse( (round(full_set$Fare==0) & full_set$Pclass==3 & full_set$Embarked =='S'),cl_em_fa$Fare[cl_em_fa$Pclass==3 & cl_em_fa$Embarked=='S'], full_set$Fare)))



# Find in which row Fare is missing
fare.missing_row <- which(is.na(full_set$Fare))
# We see that the passenger with the missing fare is a 3rd Class passenger, who embarked from
# Southampton ('S').

# Get rid of this passenger with the missing Fare variable
full_set.without_fare <- full_set[-fare.missing_row,]
# Select only the passengers from 3rd Class who departed from Southampton ('S')
new_data2 <- subset(full_set.without_fare, Pclass==3 & Embarked=='S', select=Fare)
# and see their median fare value
median(new_data2$Fare)
# [1] 8.05
# Replace the NA Fare value with the median value for their class and embarkment which is $8.05
full_set$Fare[fare.missing_row] <- median(new_data2$Fare)



####  Variable "Ticket"


table(full_set$Ticket)
# we see that some passengers have the same ticket number


# Create a new variable "TicketCount" which is the number of passengers that have the same 
# ticket number
ticket.count <- aggregate(full_set$Ticket, by=list(full_set$Ticket), function(x) sum( !is.na(x) ))
full_set$TicketCount <- apply(full_set, 1, function(x) ticket.count[which(ticket.count[, 1] == x["Ticket"]), 2])



# Now, we can create some new variables


####  Create the new variable "Title"

# As seen, the name is a combination of first name, last name and title (eg. Mr, Mrs etc)
# Convert all names from factors to strings
full_set$Name <- as.character(full_set$Name)
# Extract title from passenger names
full_set$Title <- strsplit(full_set$Name,split = "[,.]")
full_set$Title <- sapply(full_set$Title, function(x) x[2])
# Create the trim function, which returns a string without leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
full_set$Title <- trim(full_set$Title)

# summary
table(full_set$Title)

# Capt       Col          Don         Dona           Dr     Jonkheer         Lady 
# 1            4            1            1            8            1            1 
# Major   Master         Miss         Mlle          Mme           Mr          Mrs 
# 2           61          260            2            1          757          197 
# Ms         Rev          Sir the Countess 
# 2            8            1            1 

# we see that there are many different title groups. We will merge them to the most common 
# 4 groups: 
# Mr and Master for male, 
# Miss and Mrs for female

# Combine small title groups 
full_set$Title <- replace(full_set$Title, which(full_set$Title %in% 
                  c("Capt","Col","Don","Jonkheer","Major","Rev","Sir")), "Mr")

full_set$Title <- replace(full_set$Title, which(full_set$Title %in% 
                  c("Lady","Mlle","Mme","Ms","the Countess","Dr","Dona")),"Mrs")


# Show Title counts by sex 
table(full_set$Sex, full_set$Title)
#          Master Miss  Mr Mrs
# female        0  260   0 206
# male         61   0  775   7

# Extract surname from passenger name
full_set$Surname <- sapply(full_set$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Find how many unique surnames we have
nlevels(factor(full_set$Surname))


####  Create the new variable "FamilySize"
# based on number of siblings/spouse(SibSp) and number of children/parents(Parch).

# Create a family size variable including the passenger themselves
full_set$FamilySize <- full_set$SibSp + full_set$Parch + 1



# Check if FamilySize has an impact on survival
table(full_set$FamilySize,full_set$Survived)

#      0   1   (0=Perished,1=Survived)
# 1  374 163
# 2   72  89
# 3   43  59
# 4    8  21
# 5   12   3
# 6   19   3
# 7    8   4
# 8    6   0
# 11   7   0




# Probability table of FamilySize in relation to survival 
prop.table(table(full_set$FamilySize,full_set$Survived), margin = 1)

#            0         1   (0=Perished,1=Survived)
# 1  0.6964618 0.3035382
# 2  0.4472050 0.5527950
# 3  0.4215686 0.5784314
# 4  0.2758621 0.7241379
# 5  0.8000000 0.2000000
# 6  0.8636364 0.1363636
# 7  0.6666667 0.3333333
# 8  1.0000000 0.0000000
# 11 1.0000000 0.0000000


# We see that there's a survival penalty to singletons and those with family sizes above 4. 
# So, we will create a discretized family size variable with 3 levels, since there are 
# comparatively fewer large families.
 

####  Create a discretized family size variable "FamilySizeNote"
full_set$FamilySizeNote[full_set$FamilySize==1] <- 'Singleton'
full_set$FamilySizeNote[full_set$FamilySize>=2 & full_set$FamilySize<=4] <- 'Small_Family'
full_set$FamilySizeNote[full_set$FamilySize>=5] <- 'Large_Family'



####  Create a "FamilyID" variable 
full_set$FamilyID <- paste(full_set$Surname, full_set$FamilySize, sep='_')

# Find how many unique FamilyIDs we have
nlevels(factor(full_set$FamilyID))
# 928

# We see that some passengers have the same Surname but are not in the same family
# e.g. two people with surname Andersson that travelled alone and are not in the same family,
# have the same FamilyID, i.e. : Andersson_1

# Create a new variable family.count which is the number of passengers that have the same 
# Surname but are not in the same family
#install.packages("dplyr")
library(dplyr)
family.count <- full_set %>% group_by(FamilyID) %>% summarise(freq = n())
# or
# family.count <- aggregate(full_set$FamilyID, by=list(full_set$FamilyID), function(x) sum( !is.na(x) ))



# In addition, we know that large families might had trouble sticking together in the panic. 
# So let's change the FamilyIDs that have family size of two or less and call it a 
# "small" family. 

FamID <- data.frame(table(full_set$FamilyID))

# Subset the dataframe to show only the small FamilyID groups
FamID <- FamID[FamID$Freq <= 2,]

# Change FamilyIDs for those with small family sizes (<=2) and finally convert it to a factor:
full_set$FamilyID[full_set$FamilyID %in% FamID$Var1] <- 'Small'
full_set$FamilyID <- factor(full_set$FamilyID)

# Find how many unique FamilyIDs we have
nlevels(factor(full_set$FamilyID))
# 78


####  Variable "Age"

# Replacing the missing values from "Age" with the median age might not be the best idea,
# since the age may differ by groups and categories of the passengers. 
# To see an example, we will group the dataset by Sex, Title and Class, and for each
# subset we will compute the median age.


age_sex_title_class <- aggregate(Age ~ Sex + Title + Pclass, full_set, median )

age_sex_title_class[order(age_sex_title_class$Sex,age_sex_title_class$Title),]

#    Sex      Title Pclass Age
# 2  female   Miss      1 30.0
# 7  female   Miss      2 20.0
# 12 female   Miss      3 18.0
# 4  female    Mrs      1 45.0
# 9  female    Mrs      2 30.0
# 14 female    Mrs      3 31.0
# 1    male Master      1  6.0
# 6    male Master      2  2.0
# 11   male Master      3  6.0
# 3    male     Mr      1 42.0
# 8    male     Mr      2 30.0
# 13   male     Mr      3 26.0
# 5    male    Mrs      1 47.0
# 10   male    Mrs      2 38.5

# We see that the median age depends a lot on the Sex, Title and Pclass values

# We will use rpart (recursive partitioning for regression) to predict the missing Age values
#install.packages("rpart")
library("rpart")
# Set a random seed
set.seed(5)
Agefit <- rpart(Age ~ Pclass + Sex + Title + Fare + FamilySize + SibSp + Parch + Embarked,
                data=full_set[!is.na(full_set$Age),], method="anova")

tmp<-full_set
tmp$Age[is.na(full_set$Age)] <- predict(Agefit, full_set[is.na(full_set$Age),])

# Compare the original distribution of passenger ages with the predicted to ensure 
# that our prediction was correct

# Plot age distributions
par(mfrow=c(1,2))
hist(full_set$Age, freq=F, main='Original Age', col='bisque4')
hist(tmp$Age, freq=F, main='Predicted Age',col='bisque3')

# Replace missing Age values with the predicted
full_set$Age <- tmp$Age




# Now, that we have Age values for all passengers, we can create a few more age-dependent 
# variables

####  Create "Child" variable and indicate whether the passenger is child or adult
# Anyone who is less than 18 years is considered to be child
full_set$Child[full_set$Age < 18] <- 'Child'
full_set$Child[full_set$Age >= 18] <- 'Adult'


# Children are mostly likely to be rescued first. Check if this actually happened.
table(full_set$Child, full_set$Survived)

#         0   1 # (0=Perished,1=Survived)
# Adult 491 278
# Child  58  64

# Compute the probability to survive
aggregate(Survived ~ Child, data=full_set, FUN=function(x) {sum(x)/length(x)})
#   Child  Survived 
# 1 Adult 0.3615085
# 2 Child 0.5245902

# There is a ~50% chance that you will survive if you are a child


# Check if female children had a higher chance to survive compared to male children

table(full_set$Child, full_set$Survived, by=full_set$Sex)

# and compute their probability to survive
aggregate(Survived ~ Child + Sex, data=full_set, FUN=function(x) {sum(x)/length(x)})
#   Child    Sex  Survived
# 1 Adult female 0.7667984
# 2 Child female 0.6393443
# 3 Adult   male 0.1627907
# 4 Child   male 0.4098361

# It's obvious that female children are more likely to survive (0.6393443) compared to 
# male children (0.4098361)


####  Create a "Mother" variable to indicate whether the passenger is Mother or Not Mother
# Mother is a passenger who is female, over 18 years old, has 1 child or more and has the 
# Title "Mrs".


full_set$Mother <- 'Not Mother'
full_set$Mother[full_set$Sex == 'female' & full_set$Parch > 0 & full_set$Age > 18 & 
                       full_set$Title == 'Mrs'] <- 'Mother'


# Show counts
table(full_set$Mother, full_set$Survived)
#              0   1 # (0=Perished,1=Survived)
# Mother      16  39
# Not Mother 533 303

# Compute the probability to survive
aggregate(Survived ~ Mother, data=full_set, FUN=function(x) {sum(x)/length(x)})

#       Mother  Survived
# 1     Mother 0.7090909
# 2 Not Mother 0.3624402

# It's obvious that Mothers are more likely to survive (0.7090909) compared to 
# Not Mothers (0.3624402)
 


#### VISUALIZATIONS


#install.packages("ggplot2")
library(ggplot2)


# Passengers survived Vs. Passengers Class
ggplot(full_set[1:nrow(train_set),], aes(x = Pclass, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Passenger Class', y='Survived', title='Survived Passengers Vs. Class in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. Passengers Class in percentage
#install.packages("scales")
library(scales)
ggplot(full_set[1:nrow(train_set),],aes(x = Pclass, y = as.numeric(as.factor(Pclass)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Passenger Class', y='Survived', title='Percentage of Survived Passengers Vs. Class in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))



# Passengers survived Vs. Sex
ggplot(full_set[1:nrow(train_set),], aes(x = Sex, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Sex', y='Survived', title='Survived Passengers Vs. Sex in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. Sex in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = Sex, y = as.numeric(as.factor(Sex)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Sex', y='Survived', title='Percentage of Survived Passengers Vs. Sex in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))



# Passengers survived Vs. Age
ggplot(full_set[1:nrow(train_set),], aes(x = Age, fill = factor(Survived))) + 
       geom_histogram() +  
       labs(x = 'Age', y='Survived', title='Survived Passengers Vs. Age in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))



# Passengers survived Vs. Age + Sex 
# Include Sex in the plot, because it's a significant predictor
ggplot(full_set[1:nrow(train_set),], aes(x = Age, fill = factor(Survived))) + 
       geom_histogram() +  facet_grid(.~Sex) +
       labs(x = 'Age', y='Survived', title='Survived Passengers Vs. Age and Sex in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))


# Passengers survived Vs. Fare
ggplot(full_set[1:nrow(train_set),], aes(x = Fare, fill = factor(Survived))) + 
       geom_histogram() +  
       labs(x = 'Fare', y='Survived', title='Survived Passengers Vs. Fare in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))



# Passengers survived Vs. Embarked
ggplot(full_set[1:nrow(train_set),], aes(x = Embarked, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Embarked', y='Survived', title='Survived Passengers Vs. Embarked in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. Embarked in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = Embarked, y = as.numeric(as.factor(Embarked)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Embarked', y='Survived', title='Percentage of Survived Passengers Vs. Embarked in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))


# Passengers survived Vs. TicketCount
ggplot(full_set[1:nrow(train_set),], aes(x = TicketCount, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'TicketCount', y='Survived', title='Survived Passengers Vs. TicketCount in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. TicketCount in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = TicketCount, y = as.numeric(as.factor(TicketCount)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'TicketCount', y='Survived', title='Percentage of Survived Passengers Vs. TicketCount in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))


# Passengers survived Vs. Title
ggplot(full_set[1:nrow(train_set),], aes(x = Title, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Title', y='Survived', title='Survived Passengers Vs. Title in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. Title in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = Title, y = as.numeric(as.factor(Title)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Title', y='Survived', title='Percentage of Survived Passengers Vs. Title in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))



# Passengers survived Vs. FamilySize
ggplot(full_set[1:nrow(train_set),], aes(x = FamilySize, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Family Size', y='Survived', title='Survived Passengers Vs. Family Size in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. FamilySize in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = FamilySize, y = as.numeric(as.factor(FamilySize)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Family Size', y='Survived', title='Percentage of Survived Passengers Vs. FamilySize in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))




# Passengers survived Vs. FamilySizeNote
ggplot(full_set[1:nrow(train_set),], aes(x = FamilySizeNote, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Family Size Note', y='Survived', title='Survived Passengers Vs. Family Size Note in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. FamilySizeNote in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = FamilySizeNote, y = as.numeric(as.factor(FamilySizeNote)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Family Size Note', y='Survived', title='Percentage of Survived Passengers Vs. FamilySizeNote in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))




# Passengers survived Vs. Child/Adult
ggplot(full_set[1:nrow(train_set),], aes(x = Child, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Passenger', y='Survived', title='Survived Passengers Vs. Child/Adult in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))


# Passengers survived Vs. Child/Adult in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = Child, y = as.numeric(as.factor(Child)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Passenger', y='Survived', title='Percentage of Survived Passengers Vs. Child/Adult in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))




# Passengers survived Vs. Child/Adult + Sex 
# Include Sex in the plot, because it's a significant predictor
ggplot(full_set[1:nrow(train_set),], aes(x = Child, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  facet_grid(.~Sex) +
       labs(x = 'Passenger', y='Survived', title='Survived Passengers Vs. Child/Adult and Sex in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))


# Passengers survived Vs. Child/Adult + Sex in percentage
# Include Sex in the plot, because it's a significant predictor
ggplot(full_set[1:nrow(train_set),],aes(x = Child, y = as.numeric(as.factor(Child)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") + facet_grid(.~Sex) +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Passenger', y='Survived', title='Percentage of Survived Passengers Vs. Child/Adult and Sex in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))




# Passengers survived Vs. Mother
ggplot(full_set[1:nrow(train_set),], aes(x = Mother, fill = factor(Survived))) + 
       geom_bar(stat='count',position='dodge') +  
       labs(x = 'Woman', y='Survived', title='Survived Passengers Vs. Mother/Not Mother in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))

# Passengers survived Vs. Mother in percentage
ggplot(full_set[1:nrow(train_set),],aes(x = Mother, y = as.numeric(as.factor(Mother)),fill = factor(Survived))) + 
       geom_bar(position = "fill",stat = "identity") +
       scale_y_continuous(labels = percent_format())+
       labs(x = 'Woman', y='Survived', title='Percentage of Survived Passengers Vs. Mother/Not Mother in Training Set') + 
       scale_fill_discrete(labels=c("Perished","Survived"))






#### PREDICTION

# convert character variables to factor
full_set$Name <- as.factor(full_set$Name)
full_set$Cabin <- as.factor(full_set$Cabin)
full_set$Deck <- as.factor(full_set$Deck)
full_set$Title <- as.factor(full_set$Title)
full_set$Surname <- as.factor(full_set$Surname)
full_set$FamilySizeNote <- as.factor(full_set$FamilySizeNote)
full_set$FamilyID <- as.factor(full_set$FamilyID)
full_set$Child <- as.factor(full_set$Child)
full_set$Mother <- as.factor(full_set$Mother)



# Split the data back into the original training and testing sets
train_set_new <- full_set[1:nrow(train_set), ]
test_set_new <- full_set[(nrow(train_set)+1) : nrow(full_set), ]



#####      Decision Trees

#install.packages("rpart")
# library(rpart)    # it was loaded earlier
# Set the seed to ensure reproduceability
set.seed(50)
# Build a Decision Tree with rpart to predict Survived using the variables 
# Age and Sex
model.rpart <- rpart(Survived ~ Age + Sex, data=train_set_new, method="class")


# Load the packages to create a fancified visualized version of the tree
#install.packages("rattle")
library(rattle)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("RColorBrewer")
library(RColorBrewer)

# Plot decision tree
fancyRpartPlot(model.rpart)



# Performance on the training set
rpart_train_predict <- predict(model.rpart, newdata=train_set_new, type="class")
rpart_train_predict.t <- table(train_set_new$Survived, rpart_train_predict)

# Model Accuracy
rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)

# Print Accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", rpart_train_accuracy)
## Model 1 - Accuracy on Training Set:  0.8047138


# Performance on the testing set
rpart_test_predict <- predict(model.rpart, newdata=test_set_new, type="class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = rpart_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "dec_tree1.csv", row.names = FALSE)
# Kaggle score 0.76555




# Create a list of predictors

predictors <- list( c('Age','Sex','Pclass') , # 2
                    c('Age','Sex','Fare'),  # 3
                    c('Age','Sex','Pclass','Fare') , # 4
                    c('Age','Sex','Pclass','Fare','SibSp','Parch'), # 5
                    c('Age','Sex','Pclass','SibSp','Parch','Fare','Embarked'), # 6
                    c('Age','Sex','Pclass','FamilySize'), # 7
                    c('Age','Sex','FamilySize'), # 8
                    c('Age','Sex','FamilySizeNote'), # 9
                    c('Age','Sex','Pclass','FamilySizeNote'), # 10
                    c('Age','Sex','Pclass','FamilySize','Embarked'), # 11
                    c('Age','Sex','Pclass','Fare','Title'), # 12
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Title'), # 13
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother'), # 14
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 15
                     'Embarked','Title','FamilySizeNote'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Embarked', # 16
                     'Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 17
                      'Embarked','Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 18
                      'Embarked','Title','FamilySizeNote','TicketCount','Deck'),
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote','TicketCount'), # 19
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote', # 20
                      'TicketCount','FamilyID') )  


# Create a for loop to predict Survived using different variables each time

for (i in 1:length(predictors)) {
       
       my.formula<- as.formula(paste("Survived ~ ", paste(predictors[[i]], collapse= " + " )))
       # Set the seed to ensure reproduceability
       set.seed(50)
       # Build a Decision Tree with rpart to predict Survived
       model.rpart<- rpart(my.formula, data=train_set_new, method="class")
       # Plot decision tree
       # fancyRpartPlot(model.rpart)
       
       # Performance on the training set
       rpart_train_predict <- predict(model.rpart, newdata=train_set_new, type="class")
       rpart_train_predict.t <- table(train_set_new$Survived, rpart_train_predict)
       
       # Model Accuracy
       rpart_train_accuracy <- (rpart_train_predict.t[1, 1] + rpart_train_predict.t[2, 2]) / sum(rpart_train_predict.t)
       
       # Print Accuracy in Prediction
       # Model has the number i+1, since the first model was created outside of this loop
       cat("Model", i+1, "- Accuracy on Training Set: ", rpart_train_accuracy,'\n')
       
       # Performance on the testing set
       rpart_test_predict <- predict(model.rpart, newdata=test_set_new, type="class")
       # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
       my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = rpart_test_predict)
       # Write the solution to a csv file for submission in Kaggle
       write.csv(my_solution, file =  sprintf('dec_tree%d.csv',i+1), row.names = FALSE) 
       # the name of csv has the number i+1, since the first csv was created outside of this loop
}



# Summary for Decision Trees - Accuracy  


#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex                                      0.8047138     0.76555
# 2. Survived ~ Age + Sex + Pclass                             0.8282828     0.74641
# 3. Survived ~ Age + Sex + Fare                               0.7991021     0.77033
# 4. Survived ~ Age + Sex + Pclass + Fare                      0.8484848     0.79425
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch      0.8484848     0.79904 
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare +  
#               Embarked                                       0.8395062     0.78947
# 7. Survived ~ Age + Sex + Pclass + FamilySize                0.8395062     0.77990
# 8. Survived ~ Age + Sex + FamilySize                         0.8271605     0.78947
# 9. Survived ~ Age + Sex + FamilySizeNote                     0.8282828     0.78947
# 10. Survived ~ Age + Sex + Pclass + FamilySizeNote           0.8406285     0.77990
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked    0.8395062     0.77990
# 12. Survived ~ Age + Sex + Pclass + Fare + Title             0.8372615     0.77990
# 13. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Title                                         0.8372615     0.77990
# 14. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother                                0.8484848     0.79904 
# 15. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#          Child + Mother + Embarked + Title + FamilySizeNote  
#                                                              0.8327722     0.78947
# 16.  Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Embarked + Title + FamilySizeNote + TicketCount  
#                                                              0.8338945     0.79425
# 17. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch 
#               + Child + Mother +Embarked + Title + 
#               FamilySizeNote + TicketCount  
#                                                              0.8338945     0.79425
# 18. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother + Embarked + Title +
#                 FamilySizeNote + TicketCount + Deck  
#                                                              0.8338945     0.79425
# 19. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked + 
#                FamilySizeNote + TicketCount
#                                                              0.8338945     0.79425
# 20. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySizeNote + TicketCount + FamilyID       0.8496072     0.80382 (best)
       



#####      Random Forest



# Random Forests in R can only digest factors with up to 32 levels. 
# This means that our FamilyID variable, which has 78 levels, cannot be used as predictor. 
# There are two possible solutions to this problem: either change these levels to their 
# underlying integers (using the unclass() function) and having the tree treat them as 
# continuous variables, or manually reduce the number of levels to keep it under the threshold. 
# We will use the second approach.

FamID <- data.frame(table(full_set$FamilyID))

# Subset the dataframe to show only the small FamilyID groups
FamID <- FamID[FamID$Freq <= 3,]

full_set$FamilyID2 <- full_set$FamilyID

# Convert it from a factor back into a character string with as.character(). 
full_set$FamilyID2 <- as.character(full_set$FamilyID2)

# We can then increase our cut-off to be a "Small" family from 2 to 3 people. 

# Change FamilyIDs for those with small family sizes (<=3) and finally convert it to a factor:
full_set$FamilyID2[full_set$FamilyID2 %in% FamID$Var1] <- 'Small'
full_set$FamilyID2 <- factor(full_set$FamilyID2)

# Find how many unique FamilyIDs we have
nlevels(factor(full_set$FamilyID2))



# Create new training and testing sets that contain the FamilyID2 variable as well

# Split the data back into the training and testing sets
train_set_new2 <- full_set[1:nrow(train_set), ]
test_set_new2 <- full_set[(nrow(train_set)+1) : nrow(full_set), ]




#install.packages("randomForest")
library(randomForest)
# Set the seed to ensure reproduceability
set.seed(50)
# Build Random Forest with randomForest to predict Survived using the variables 
# Age and Sex

# Instead of specifying method="class" as with rpart, we force the model to predict our 
# classification by changing the target variable (Survived) to a factor with only two levels.  
# The importance=TRUE argument allows us to inspect variable importance.

model.rforest <- randomForest(factor(Survived) ~ Age + Sex,
                        data = train_set_new2, importance=TRUE)


# Look at relative variable importance by plotting the mean decrease in Gini calculated 
# across all trees
# Get importance
importance <- model.rforest$importance

#install.packages("dplyr")
# library('dplyr') # data manipulation   # it was loaded earlier
varImportance <- data.frame(Variables=row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
       mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
                 hjust=0, vjust=0.55, size = 4, colour="white") +
       labs(x = 'Variables' ,title='Variable Importance') +
       coord_flip() 



# Performance on the training set
rf_model_train_predict <- predict(model.rforest, newdata=train_set_new2, type="class")
rf_model_train_predict.t <- table(train_set_new2$Survived, rf_model_train_predict)
 
# Model Accuracy
rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)
 
# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", rf_model_train_accuracy)
# Model Accuracy on training data:  0.7979798

# Performance on the testing set
rf_model_test_predict <- predict(model.rforest, newdata=test_set_new2, type="class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new2$PassengerId, Survived = rf_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "r_forest1.csv", row.names = FALSE)
# Kaggle score 0.76555
 




# Create a list of predictors

predictors <- list( c('Age','Sex','Pclass'),  # 2
                    c('Age','Sex','Fare'),  # 3
                    c('Age','Sex','Pclass','Fare'),  # 4
                    c('Age','Sex','Pclass','Fare','SibSp','Parch'), # 5
                    c('Age','Sex','Pclass','SibSp','Parch','Fare','Embarked'), # 6
                    c('Age','Sex','Pclass','FamilySize'), # 7
                    c('Age','Sex','FamilySize'), # 8
                    c('Age','Sex','FamilySizeNote'), # 9
                    c('Age','Sex','Pclass','FamilySizeNote'), # 10
                    c('Age','Sex','Pclass','FamilySize','Embarked'), # 11
                    c('Age','Sex','Pclass','Fare','Title'), # 12
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Title'), # 13
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother'), # 14
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 15
                      'Embarked','Title','FamilySizeNote'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Embarked', # 16
                      'Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 17
                      'Embarked','Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 18
                      'Embarked','Title','FamilySizeNote','TicketCount','Deck'),
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote','TicketCount'), # 19 
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySize','FamilyID2',
                      'SibSp','Parch')) # 20



# Create a for loop to predict Survived using different variables each time

for (i in 1:length(predictors)) {
       
       my.formula<- as.formula(paste("factor(Survived) ~ ", paste(predictors[[i]], collapse= " + " )))
       # Set the seed to ensure reproduceability
       set.seed(50)
       # Build Random Forest with randomForest to predict Survived
       model.rforest <- randomForest(my.formula, data = train_set_new2, importance=TRUE)
       

       # Look at relative variable importance by plotting the mean decrease in Gini calculated 
       # across all trees
       # Get importance
       importance <- model.rforest$importance
       varImportance <- data.frame(Variables=row.names(importance), 
                                   Importance = round(importance[ ,'MeanDecreaseGini'],2))
       # Create a rank variable based on importance
       rankImportance <- varImportance %>%
              mutate(Rank = paste0('#',dense_rank(desc(Importance))))
       
       # Use ggplot2 to visualize the relative importance of variables
       print( ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                                  y = Importance, fill = Importance)) +
              geom_bar(stat='identity') + 
              geom_text(aes(x = Variables, y = 0.5, label = Rank),
                        hjust=0, vjust=0.55, size = 4, colour="white") +
              labs(x = 'Variables' ,title='Variable Importance') +
              coord_flip() )
       
       
       # Performance on the training set
       rf_model_train_predict <- predict(model.rforest, newdata=train_set_new2, type="class")
       rf_model_train_predict.t <- table(train_set_new2$Survived, rf_model_train_predict)
       
       
       # Model Accuracy
       rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)
       
       
       # Print accuracy in Prediction
       # Model has the number i+1, since the first model was created outside of this loop
       cat("Model", i+1, "- Accuracy on Training Set: ", rf_model_train_accuracy,'\n')
       
      
       # Performance on the testing set
       rf_model_test_predict <- predict(model.rforest, newdata=test_set_new2, type="class")
       # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
       my_solution <- data.frame(PassengerId = test_set_new2$PassengerId, Survived = rf_model_test_predict)
       # Write the solution to a csv file for submission in Kaggle
       write.csv(my_solution, file =  sprintf('r_forest%d.csv',i+1), row.names = FALSE) 
       # the name of csv has the number i+1, since the first csv was created outside of this loop
       
}


# From the variable importance plots, we see that the "Title" variable that we created 
# has the highest relative importance out of all of the predictor variables. 


# We could also tune the model to find the optimal parameter values

#install.packages("e1071")
library(e1071)

# Set the seed to ensure reproduceability
set.seed(50)
# Build Random Forest with randomForest to predict Survived using the variables 
# Age, Sex, Pclass, Fare, Title, FamilySizeNote, TicketCount

# Search for an optimal value of mtry (number of variables used at each split of the tree)
# and ntree (number of trees)
rforest.tune<-tune.randomForest(factor(Survived) ~ Age + Sex + Pclass + Fare + 
                                Title + FamilySizeNote + TicketCount,
                                data=train_set_new2, mtry=c(2,7),
                                ntree=c(500,1000,1500,2000), importance=TRUE)
# Get the best model
model.rforest <- rforest.tune$best.model



# Look at relative variable importance by plotting the mean decrease in Gini calculated 
# across all trees
# Get importance
importance <- model.rforest$importance

varImportance <- data.frame(Variables=row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
# Create a rank variable based on importance
rankImportance <- varImportance %>%
       mutate(Rank = paste0('#',dense_rank(desc(Importance))))


# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
                 hjust=0, vjust=0.55, size = 4, colour="white") +
       labs(x = 'Variables' ,title='Variable Importance') +
       coord_flip() 


# Performance on the training set
rf_model_train_predict <- predict(model.rforest, newdata=train_set_new2, type="class")
rf_model_train_predict.t <- table(train_set_new2$Survived, rf_model_train_predict)

# Model Accuracy
rf_model_train_accuracy <- (rf_model_train_predict.t[1, 1] + rf_model_train_predict.t[2, 2]) / sum(rf_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 20, "- Accuracy on Training Set: ", rf_model_train_accuracy)
# Model 20 - Accuracy on Training Set:  0.8911336

# Performance on the testing set
rf_model_test_predict <- predict(model.rforest, newdata=test_set_new2, type="class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new2$PassengerId, Survived = rf_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "r_forest20.csv", row.names = FALSE)
# Kaggle score 0.79904






# Summary for Random Forest - Accuracy       

#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex                                      0.7979798          0.76555
# 2. Survived ~ Age + Sex + Pclass                             0.8305275          0.75119
# 3. Survived ~ Age + Sex + Fare                               0.8002245          0.76076
# 4. Survived ~ Age + Sex + Pclass + Fare                      0.9046016          0.77033
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch      0.9102132          0.77990
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare +  
#               Embarked                                       0.9079686          0.77511
# 7. Survived ~ Age + Sex + Pclass + FamilySize                0.8529742          0.77033
# 8. Survived ~ Age + Sex + FamilySize                         0.8260382          0.77033
# 9. Survived ~ Age + Sex + FamilySizeNote                     0.8316498          0.78947 
# 10. Survived ~ Age + Sex + Pclass + FamilySizeNote           0.8439955          0.77990
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked    0.8754209          0.77990
# 12. Survived ~ Age + Sex + Pclass + Fare + Title             0.8832772          0.77511
# 13. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Title                                         0.8866442          0.78947
# 14. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother                                0.8720539          0.77990
# 15. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#        Child + Mother + Embarked + Title + FamilySizeNote
#                                                              0.9079686          0.79904 
# 16.  Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Embarked + Title + FamilySizeNote + TicketCount  
#                                                              0.9270483          0.79425  
# 17. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother + Embarked + Title +   
#                FamilySizeNote + TicketCount                  0.9124579          0.80382 (best)
# 18. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother + Embarked + Title +  
#                FamilySizeNote + TicketCount + Deck           0.9214366          0.78947
# 19. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +  
#                FamilySizeNote + TicketCount
#                                                              0.8967452          0.78947
# 20. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySize + FamilyID2 + SibSp + Parch        0.9281706          0.77511
# 21. Survived ~ Age + Sex + Pclass + Fare + Title + 
#                FamilySizeNote + TicketCount                  0.8911336          0.79904
 
         







#####      Forest of conditional inference trees



# Conditional inference trees make their decisions in slightly different ways, 
# using a statistical test rather than a purity measure, but the basic construction of 
# each tree is fairly similar.


# install.packages('party')
library(party)

# Set the seed to ensure reproduceability
set.seed(50)
# Build a Forest of conditional inference trees with cforest to predict Survived 
# using the variables Age and Sex


# Instead of specifying method="class" as with rpart, we force the model to predict our 
# classification by changing the target variable (Survived) to a factor with only two levels.  

# In cforest we have to specify the number of trees. 
# We also have to manually set the number of variables to sample at each node (mtry) as 
# the default value of 5 is pretty high for our dataset.

model.cforest <- cforest(factor(Survived) ~ Age + Sex,
                         data = train_set_new, 
                         controls=cforest_unbiased(ntree=2000, mtry=2))


# Performance on the training set
# put the Out-of-bag (OOB) error estimate = TRUE
cf_model_train_predict <- predict(model.cforest, newdata=train_set_new, OOB=TRUE, type = "response")
cf_model_train_predict.t <- table(train_set_new$Survived, cf_model_train_predict)


# Model Accuracy
cf_model_train_accuracy <- (cf_model_train_predict.t[1, 1] + cf_model_train_predict.t[2, 2]) / sum(cf_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", cf_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8069585


# Performance on the testing set
cf_model_test_predict <- predict(model.cforest, newdata=test_set_new, OOB=TRUE, type = "response")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = cf_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "c_forest1.csv", row.names = FALSE)
# Kaggle score 0.74641




# Create a list of predictors

predictors <- list( c('Age','Sex','Pclass'),  # 2
                    c('Age','Sex','Fare'),  # 3
                    c('Age','Sex','Pclass','Fare'),  # 4
                    c('Age','Sex','Pclass','Fare','SibSp','Parch'), # 5
                    c('Age','Sex','Pclass','SibSp','Parch','Fare','Embarked'), # 6
                    c('Age','Sex','Pclass','FamilySize'), # 7
                    c('Age','Sex','FamilySize'), # 8
                    c('Age','Sex','FamilySizeNote'), # 9
                    c('Age','Sex','Pclass','FamilySizeNote'), # 10
                    c('Age','Sex','Pclass','FamilySize','Embarked'), # 11
                    c('Age','Sex','Pclass','Fare','Title'), # 12
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Title'), # 13
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother'), # 14
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 15
                      'Embarked','Title','FamilySizeNote'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Embarked', # 16
                      'Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 17
                      'Embarked','Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 18
                      'Embarked','Title','FamilySizeNote','TicketCount','Deck'), # 19 
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','Title', 'Embarked', 'FamilySizeNote', # 20 
                     'TicketCount','FamilyID'),
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySize','FamilyID',
                      'SibSp','Parch')) # 21




# Create a for loop to predict Survived using different variables each time

for (i in 1:length(predictors)) {
       
       my.formula<- as.formula(paste("factor(Survived) ~ ", paste(predictors[[i]], collapse= " + " )))
       # Set the seed to ensure reproduceability
       set.seed(50)
       
       # Build a Forest of conditional inference trees with cforest to predict Survived 
       model.cforest <- cforest(my.formula, data = train_set_new, 
                                controls=cforest_unbiased(ntree=2000, mtry=2))
       

       # Performance on the training set
       cf_model_train_predict <- predict(model.cforest, newdata=train_set_new, OOB=TRUE, type = "response")
       cf_model_train_predict.t <- table(train_set_new$Survived, cf_model_train_predict)
       
       
       # Model Accuracy
       cf_model_train_accuracy <- (cf_model_train_predict.t[1, 1] + cf_model_train_predict.t[2, 2]) / sum(cf_model_train_predict.t)
       
       # Print accuracy in Prediction
       # Model has the number i+1, since the first model was created outside of this loop
       cat("Model", i+1, "- Accuracy on Training Set: ", cf_model_train_accuracy,'\n')
       
       
       # Performance on the testing set
       cf_model_test_predict <- predict(model.cforest, newdata=test_set_new, OOB=TRUE, type = "response")
       # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
       my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = cf_model_test_predict)
       # Write the solution to a csv file for submission in Kaggle
       write.csv(my_solution, file =  sprintf('c_forest%d.csv',i+1), row.names = FALSE) 
       # the name of csv has the number i+1, since the first csv was created outside of this loop
       
}




# Summary for Forest of conditional inference trees - Accuracy   



#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex                                         0.8069585         0.74641
# 2. Survived ~ Age + Sex + Pclass                                0.8260382         0.72248
# 3. Survived ~ Age + Sex + Fare                                  0.8451178         0.75598
# 4. Survived ~ Age + Sex + Pclass + Fare                         0.8327722         0.76076
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch         0.8473625         0.78468
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare +   
#               Embarked                                          0.8294052         0.78947
# 7. Survived ~ Age + Sex + Pclass + FamilySize                   0.8282828         0.75598
# 8. Survived ~ Age + Sex + FamilySize                            0.8260382         0.78947
# 9. Survived ~ Age + Sex + FamilySizeNote                        0.8237935         0.78947
# 10. Survived ~ Age + Sex + Pclass + FamilySizeNote              0.8237935         0.77990
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked       0.8237935         0.78468
# 12. Survived ~ Age + Sex + Pclass + Fare + Title                0.8327722         0.77990
# 13. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch +  
#                Title                                            0.8406285         0.79425
# 14. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother                                   0.8170595         0.78947
# 15. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#          Child + Mother + Embarked + Title + FamilySizeNote
#                                                                 0.8383838         0.80382 
# 16.  Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Embarked + Title + FamilySizeNote + TicketCount  
#                                                                 0.8406285         0.80382 
# 17. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#               Child + Mother + Embarked + Title +  
#               FamilySizeNote + TicketCount                      0.8395062         0.79904
# 18. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch +
#                Child + Mother + Embarked + Title + 
#                FamilySizeNote + TicketCount + Deck              0.8361392         0.79904
# 19. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +  
#                FamilySizeNote + TicketCount
#                                                                 0.8484848         0.80382 
# 20. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySizeNote + TicketCount + FamilyID          0.85409	       0.80382
# 21. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySize + FamilyID + SibSp + Parch            0.85409	       0.82296(best)
        	






#####      Logistic Regression
##    Generalized Linear Models (GLMs)


#install.packages("stats")
library(stats)
# Set the seed to ensure reproduceability
set.seed(50)

# Build a Generalized Linear Model with glm to predict Survived 
# using the variables Age and Sex

# In the glm function, the command "family = binomial" tells R to fit a 
# logistic regression model
# By specifying binomial(link = "logit"), we declare a binary outcome and that we are estimating
# a logit, rather than probit, model
model.glm <- glm(Survived ~ Age + Sex,
                 data = train_set_new,  family=binomial(link = "logit"))



# Performance on the training set

# By specifying type="response", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and by wrapping all of this in the
# as.numeric command, we count all probabilities above 0.5 as predicted values of 1 and all
# that are less than 0.5 as predicted values of 0
glm_model_train_predict <- predict(model.glm , newdata=train_set_new, type = "response")
glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
glm_model_train_predict.t <- table(train_set_new$Survived, glm_model_train_predict)


# Model Accuracy
glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", glm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.7867565


# Performance on the testing set
glm_model_test_predict <- predict(model.glm, newdata=test_set_new, type = "response")
glm_model_test_predict <- as.numeric(as.numeric(glm_model_test_predict)>0.5)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = glm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "glm1.csv", row.names = FALSE)
# Kaggle score 0.76555






# Create a list of predictors

predictors <- list( c('Age','Sex','Pclass'),  # 2
                    c('Age','Sex','Fare'),  # 3
                    c('Age','Sex','Pclass','Fare'),  # 4
                    c('Age','Sex','Pclass','Fare','SibSp','Parch'), # 5
                    c('Age','Sex','Pclass','SibSp','Parch','Fare','Embarked'), # 6
                    c('Age','Sex','Pclass','FamilySize'), # 7
                    c('Age','Sex','FamilySize'), # 8
                    c('Age','Sex','FamilySizeNote'), # 9
                    c('Age','Sex','Pclass','FamilySizeNote'), # 10
                    c('Age','Sex','Pclass','FamilySize','Embarked'), # 11
                    c('Age','Sex','Pclass','Fare','Title'), # 12
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Title'), # 13
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother'), # 14
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 15
                      'Embarked','Title','FamilySizeNote'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Embarked', # 16
                      'Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 17
                      'Embarked','Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote','TicketCount') ) # 18





# Create a for loop to predict Survived using different variables each time

for (i in 1:length(predictors)) {
       
       my.formula<- as.formula(paste("Survived ~ ", paste(predictors[[i]], collapse= " + " )))
       # Set the seed to ensure reproduceability
       set.seed(50)
       
       # Build a Generalized Linear Model with glm to predict Survived  
       model.glm <- glm(my.formula, data = train_set_new, 
                        family=binomial(link = "logit"))
       
       
       # Performance on the training set
       glm_model_train_predict <- predict(model.glm , newdata=train_set_new, type = "response")
       glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
       glm_model_train_predict.t <- table(train_set_new$Survived, glm_model_train_predict)
       
       
       # Model Accuracy
       glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)
       
       
       # Print accuracy in Prediction
       # Model has the number i+1, since the first model was created outside of this loop
       cat("Model", i+1, "- Accuracy on Training Set: ", glm_model_train_accuracy,'\n')
       
       
       # Performance on the testing set
       glm_model_test_predict <- predict(model.glm, newdata=test_set_new, type = "response")
       glm_model_test_predict <- as.numeric(as.numeric(glm_model_test_predict)>0.5)
       # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
       my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = glm_model_test_predict)
       # Write the solution to a csv file for submission in Kaggle
       write.csv(my_solution, file =  sprintf('glm%d.csv',i+1), row.names = FALSE) 
       # the name of csv has the number i+1, since the first csv was created outside of this loop
       
}




# If we train a model with FamilyID variable, we get an error, because in the testing set
# the (factor) variable FamilyID has new levels that are not present in the training set

# To avoid this, one would like to detect novel levels in new data and encode them in a way that
# the model can understand. This task is much easier when representing categorical variables
# as indicators.
# In vtreat, the procedure is as follows:

#install.packages("vtreat")
library("vtreat")

# The function designTreatmentsN() takes as input the data frame of training data, 
# the list of input columns, and the (numerical) outcome column.
treatments <- designTreatmentsN(train_set_new, varlist=
                                       c("Age","Sex","Pclass","Fare","Title",
                                         "Embarked","FamilySize","FamilyID","SibSp","Parch"), 
                                outcomename = "Survived")

# Once we have created the treatment plans using designTreatmentsN(), we can treat the 
# training and testing data frames using the function prepare(). 
# This creates new data frames that express the outcome in terms of the new transformed 
# variables. prepare() takes as input a list of treatment plans and a data set to be treated. 
train.treat <- prepare(treatments, train_set_new, pruneSig=NULL)
test.treat <- prepare(treatments, test_set_new, pruneSig=NULL)

# Now we can fit a model using the transformed variables:

# get the names of the variables
vars <- setdiff(colnames(train.treat), "Survived")
fmla <- paste("Survived ~ ", paste(vars, collapse=" + "))

model.glm <- glm(fmla, data=train.treat, family=binomial(link = "logit"))

glm_model_train_predict <- predict(model.glm , newdata=train.treat, type = "response")
glm_model_train_predict <- as.numeric(as.numeric(glm_model_train_predict)>0.5)
glm_model_train_predict.t <- table(train_set_new$Survived, glm_model_train_predict)


# Model Accuracy
glm_model_train_accuracy <- (glm_model_train_predict.t[1, 1] + glm_model_train_predict.t[2, 2]) / sum(glm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 19, "- Accuracy on Training Set: ", glm_model_train_accuracy)
# Model 19 - Accuracy on Training Set:  0.8540965


# Performance on the testing set
glm_model_test_predict <- predict(model.glm, newdata=test.treat, type = "response")
glm_model_test_predict <- as.numeric(as.numeric(glm_model_test_predict)>0.5)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = glm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "glm19.csv", row.names = FALSE)
# Kaggle score 0.80382






# Summary for Generalized Linear Models (GLMs) - Accuracy   

#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex                                        0.7867565   0.76555         
# 2. Survived ~ Age + Sex + Pclass                               0.7946128   0.75598
# 3. Survived ~ Age + Sex + Fare                                 0.7833895   0.76076
# 4. Survived ~ Age + Sex + Pclass + Fare                        0.7946128   0.75598
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch        0.8058361   0.75119
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare +  
#               Embarked                                         0.8058361   0.76555
# 7. Survived ~ Age + Sex + Pclass + FamilySize                  0.8058361   0.75598
# 8. Survived ~ Age + Sex + FamilySize                           0.7912458   0.77033
# 9. Survived ~ Age + Sex + FamilySizeNote                       0.8035915   0.77033
# 10. Survived ~ Age + Sex + Pclass + FamilySizeNote             0.8215488   0.75598
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked      0.8013468   0.76076
# 12. Survived ~ Age + Sex + Pclass + Fare + Title               0.8024691   0.77990
# 13. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch +  
#                Title                                           0.8327722   0.78947 
# 14. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother                                  0.8047138   0.75598
# 15. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Child + Mother +Embarked + Title + FamilySizeNote
#                                                                0.8338945   0.77990
# 16.  Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#              Embarked + Title + FamilySizeNote + TicketCount  
#                                                                0.8338945   0.78468
# 17. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#               Child + Mother + Embarked + Title +   
#               FamilySizeNote + TicketCount                     0.8350168   0.78468
# 18. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +  
#                FamilySizeNote + TicketCount
#                                                                0.8327722   0.77990
# 19. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySize + FamilyID + SibSp + Parch           0.8540965   0.80382 (best)









#####      Gradient Boosting Machine (GBM)


# install.packages('gbm')
library(gbm)

# Set the seed to ensure reproduceability
set.seed(50)


# Build a Gradient Boosting Machine with gbm to predict Survived 
# using the variables Age, Sex, Pclass, Fare, SibSp, Parch, Child, Mother, Embarked, Title,
# FamilySizeNote and TicketCount

model.gbm <- gbm(Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + Child + Mother +
                        Embarked + Title + FamilySizeNote + TicketCount,  
                 data = train_set_new, n.trees = 6000, distribution = "bernoulli")



# Performance on the training set
# By specifying type="response", we clarify that we want our predictions to be on the 
# probability scale.
# We then ask if each probability is greater than 0.5, and by wrapping all of this in the
# as.numeric command, we count all probabilities above 0.5 as predicted values of 1 and all
# that are less than 0.5 as predicted values of 0
gbm_model_train_predict <- predict(model.gbm, newdata=train_set_new, n.trees = 6000, type = "response")
gbm_model_train_predict <- as.numeric(as.numeric(gbm_model_train_predict)>0.5)
gbm_model_train_predict.t <- table(train_set_new$Survived, gbm_model_train_predict)


# Model Accuracy
gbm_model_train_accuracy <- (gbm_model_train_predict.t[1, 1] + gbm_model_train_predict.t[2, 2]) / sum(gbm_model_train_predict.t)


# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", gbm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8327722


# Performance on the testing set
gbm_model_test_predict <- predict(model.gbm, newdata=test_set_new, n.trees = 6000, type = "response")
gbm_model_test_predict <- as.numeric(as.numeric(gbm_model_test_predict)>0.5)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = gbm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "gbm1.csv", row.names = FALSE)
# Kaggle score 0.74641






# Set the seed to ensure reproduceability
set.seed(50)

# Build a Gradient Boosting Machine with gbm to predict Survived 
# using the variables Age, Sex, Pclass, Fare, Title, Embarked, FamilySizeNote and TicketCount

model.gbm <- gbm(Survived ~ Age + Sex + Pclass + Fare + Title + Embarked + FamilySizeNote + TicketCount,  
                 data = train_set_new, n.trees = 6000, distribution = "bernoulli")

# Performance on the training set
gbm_model_train_predict <- predict(model.gbm, newdata=train_set_new, n.trees = 6000, type = "response")
gbm_model_train_predict <- as.numeric(as.numeric(gbm_model_train_predict)>0.5)
gbm_model_train_predict.t <-table(train_set_new$Survived, gbm_model_train_predict)


# Model Accuracy
gbm_model_train_accuracy <- (gbm_model_train_predict.t[1, 1] + gbm_model_train_predict.t[2, 2]) / sum(gbm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", gbm_model_train_accuracy)
# Model 2 - Accuracy on Training Set:   0.8316498


# Performance on the testing set
gbm_model_test_predict <- predict(model.gbm, newdata=test_set_new, n.trees = 6000, type = "response")
gbm_model_test_predict <- as.numeric(as.numeric(gbm_model_test_predict)>0.5)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = gbm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "gbm2.csv", row.names = FALSE)
# Kaggle score 0.78947




# Set the seed to ensure reproduceability
set.seed(50)

# Build a Gradient Boosting Machine with gbm to predict Survived 
# using the variables Age, Sex, Pclass, Fare, Title, Embarked, FamilySize, FamilyID, SibSp and Parch

model.gbm <- gbm(Survived ~ Age + Sex + Pclass + Fare + Title + Embarked + FamilySize + FamilyID + SibSp + Parch,  
                 data = train_set_new, n.trees = 6000, distribution = "bernoulli")

# Performance on the training set
gbm_model_train_predict <- predict(model.gbm, newdata=train_set_new, n.trees = 6000, type = "response")
gbm_model_train_predict <- as.numeric(as.numeric(gbm_model_train_predict)>0.5)
gbm_model_train_predict.t <-table(train_set_new$Survived, gbm_model_train_predict)


# Model Accuracy
gbm_model_train_accuracy <- (gbm_model_train_predict.t[1, 1] + gbm_model_train_predict.t[2, 2]) / sum(gbm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 3, "- Accuracy on Training Set: ", gbm_model_train_accuracy)
# Model 3 - Accuracy on Training Set:   0.8540965


# Performance on the testing set
gbm_model_test_predict <- predict(model.gbm, newdata=test_set_new, n.trees = 6000, type = "response")
gbm_model_test_predict <- as.numeric(as.numeric(gbm_model_test_predict)>0.5)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = gbm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "gbm3.csv", row.names = FALSE)
# Kaggle score 0.79904




# Summary for Gradient Boosting Machine (GBM) - Accuracy   

#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#               Child + Mother + Embarked + Title +  
#               FamilySizeNote + TicketCount                    0.8327722    0.74641
# 2. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +  
#               FamilySizeNote + TicketCount
#                                                               0.8316498    0.78947 
# 3. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySize + FamilyID + SibSp + Parch          0.8540965    0.79904 (best)



#####      Neural Network (NN)

#install.packages("nnet")
library(nnet)


# With NN we have to normalize the (continuous) data so that all values are in the range 0 to 1.
full_set_norm <- full_set
full_set_norm$Age <- with(full_set_norm, (Age - min(Age)) / (max(Age) - min(Age)))
full_set_norm$Fare <- with(full_set_norm, (Fare - min(Fare)) / (max(Fare) - min(Fare)))


# Split the data back into the original training and testing sets
train_set_norm_new <- full_set_norm[1:nrow(train_set), ]
test_set_norm_new <- full_set_norm[(nrow(train_set)+1) : nrow(full_set), ]

# Set the seed to ensure reproduceability
set.seed(50)

# Build a Neural Network with nnet to predict Survived 
# using the variables Age, Sex, Pclass, Fare, SibSp, Parch, Child, Mother, Embarked, Title,
# FamilySizeNote and TicketCount

model.nnet <- nnet(factor(Survived) ~ Age + Sex + Pclass + Fare + SibSp + Parch + Child + Mother +
                        Embarked + Title + FamilySizeNote + TicketCount,  
                 data = train_set_norm_new, size=2)


# Performance on the training set
nnet_model_train_predict <- predict(model.nnet, newdata=train_set_norm_new, type = "class")
nnet_model_train_predict.t <- table(train_set_norm_new$Survived, nnet_model_train_predict)

# Model Accuracy
nnet_model_train_accuracy <- (nnet_model_train_predict.t[1, 1] + nnet_model_train_predict.t[2, 2]) / sum(nnet_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", nnet_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8552189

# Performance on the testing set
nnet_model_test_predict <- predict(model.nnet, newdata=test_set_norm_new, type = "class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_norm_new$PassengerId, Survived = nnet_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "nnet1.csv", row.names = FALSE)
# Kaggle score 0.74641


# Set the seed to ensure reproduceability
set.seed(50)

# Build a Neural Network with nnet to predict Survived 
# using the variables Age, Sex, Pclass, Fare, Title, Embarked, FamilySize, FamilyID , 
# SibSp and Parch

model.nnet <- nnet(factor(Survived) ~ Age + Sex + Pclass + Fare + Title +
                          Embarked + FamilySize + FamilyID + SibSp + Parch,  
                   data = train_set_norm_new, size=2)


# Performance on the training set
nnet_model_train_predict <- predict(model.nnet, newdata=train_set_norm_new, type = "class")
nnet_model_train_predict.t <- table(train_set_norm_new$Survived, nnet_model_train_predict)

# Model Accuracy
nnet_model_train_accuracy <- (nnet_model_train_predict.t[1, 1] + nnet_model_train_predict.t[2, 2]) / sum(nnet_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 2, "- Accuracy on Training Set: ", nnet_model_train_accuracy)
# Model 2 - Accuracy on Training Set:  0.8787879

# Performance on the testing set
nnet_model_test_predict <- predict(model.nnet, newdata=test_set_norm_new, type = "class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_norm_new$PassengerId, Survived = nnet_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "nnet2.csv", row.names = FALSE)
# Kaggle score 0.78947





# Summary for Neural Network (NN) - Accuracy   

#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch +
#               Child + Mother + Embarked + Title +
#               FamilySizeNote + TicketCount                    0.8552189    0.74641
# 2. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +
#                FamilySize + FamilyID + SibSp + Parch          0.8787879    0.78947 (best)






#####      Support Vector Machines (SVM) with Radial Basis Function (RBF) Kernel 


# We will tune the model to find the optimal parameter values

# Set the seed to ensure reproduceability
set.seed(50)


# Build a Support Vector Machine with svm to predict Survived 
# using the variables Age and Sex


# Search for an optimal value of gamma (defines how far the influence of a single 
# training example reaches, with low values meaning 'far' and high values meaning 'close')
# and cost (regularization term that controls the complexity of the model. 
# A high cost value will force the SVM to create a complex enough prediction function
# to missclassify as few training points as possible, while a lower cost parameter will lead
# to a simpler prediction function. )

fit.tune <- tune.svm(factor(Survived)~ Age + Sex,
                     data=train_set_new, kernel="radial",
                     gamma=10^(-2:2),cost=10^(-2:4))
# Get the best model                 
model.svm <- fit.tune$best.model                 

# Performance on the training set
svm_model_train_predict <- predict(model.svm, newdata=train_set_new, type="class")
svm_model_train_predict.t <- table(train_set_new$Survived, svm_model_train_predict)

# Model Accuracy
svm_model_train_accuracy <- (svm_model_train_predict.t[1, 1] + svm_model_train_predict.t[2, 2]) / sum(svm_model_train_predict.t)

# Print accuracy in Prediction
cat("Model", 1, "- Accuracy on Training Set: ", svm_model_train_accuracy)
# Model 1 - Accuracy on Training Set:  0.8069585


# Performance on the testing set
svm_model_test_predict <- predict(model.svm, newdata=test_set_new[,-2], type="class")
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = svm_model_test_predict)
# Write the solution to a csv file for submission in Kaggle
write.csv(my_solution, file =  "svm1.csv", row.names = FALSE)
# Kaggle score 0.74641






# Create a list of predictors

predictors <- list( c('Age','Sex','Pclass'),  # 2
                    c('Age','Sex','Fare'),  # 3
                    c('Age','Sex','Pclass','Fare'),  # 4
                    c('Age','Sex','Pclass','Fare','SibSp','Parch'), # 5
                    c('Age','Sex','Pclass','SibSp','Parch','Fare','Embarked'), # 6
                    c('Age','Sex','Pclass','FamilySize'), # 7
                    c('Age','Sex','FamilySize'), # 8
                    c('Age','Sex','FamilySizeNote'), # 9
                    c('Age','Sex','Pclass','FamilySizeNote'), # 10
                    c('Age','Sex','Pclass','FamilySize','Embarked'), # 11
                    c('Age','Sex','Pclass','Fare','Title'), # 12
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Title'), # 13
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother'), # 14
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 15
                      'Embarked','Title','FamilySizeNote'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Embarked', # 16
                      'Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','SibSp','Parch','Child','Mother', # 17
                      'Embarked','Title','FamilySizeNote','TicketCount'),
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySizeNote','TicketCount'), # 18
                    c('Age','Sex','Pclass','Fare','Title','Embarked','FamilySize','FamilyID',
                      'SibSp','Parch')) # 19



# Create a for loop to predict Survived using different variables each time

for (i in 1:length(predictors)) {
       
       my.formula<- as.formula(paste("factor(Survived) ~ ", paste(predictors[[i]], collapse= " + " )))
       # Set the seed to ensure reproduceability
       set.seed(50)
       
       # Build a Support Vector Machine with svm to predict Survived 
       fit.tune <- tune.svm(my.formula, data=train_set_new, kernel="radial",
                            gamma=10^(-2:2),cost=10^(-2:4))
       # Get the best model                 
       model.svm <- fit.tune$best.model 
       

       # Performance on the training set
       svm_model_train_predict <- predict(model.svm, newdata=train_set_new, type="class")
       svm_model_train_predict.t <- table(train_set_new$Survived, svm_model_train_predict)
       
       
       # Model Accuracy
       svm_model_train_accuracy <- (svm_model_train_predict.t[1, 1] + svm_model_train_predict.t[2, 2]) / sum(svm_model_train_predict.t)
       
       # Print accuracy in Prediction
       # Model has the number i+1, since the first model was created outside of this loop
       cat("Model", i+1, "- Accuracy on Training Set: ", svm_model_train_accuracy,'\n')
       
       
       # Performance on the testing set
       # exclude the second column which is the Survived column
       svm_model_test_predict <- predict(model.svm, newdata=test_set_new[,-2], type="class")
       # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
       my_solution <- data.frame(PassengerId = test_set_new$PassengerId, Survived = svm_model_test_predict)
       # Write the solution to a csv file for submission in Kaggle
       write.csv(my_solution, file =  sprintf('svm%d.csv',i+1), row.names = FALSE) 
       # the name of csv has the number i+1, since the first csv was created outside of this loop
       
}





# Summary for Support Vector Machines (SVM) - Accuracy  



#    Formula                                     Accuracy (Training Set)    Accuracy (Testing Set)
# 1. Survived ~ Age + Sex                                        0.8069585   0.74641
# 2. Survived ~ Age + Sex + Pclass                               0.8888889   0.69856
# 3. Survived ~ Age + Sex + Fare                                 0.8058361   0.77033
# 4. Survived ~ Age + Sex + Pclass + Fare                        0.8350168	0.77033
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch        0.8316498	0.77990
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare +    
#               Embarked                                         0.8350168	0.79904
# 7. Survived ~ Age + Sex + Pclass + FamilySize                  0.8417508	0.77990
# 8. Survived ~ Age + Sex + FamilySize                           0.8294052	0.79425
# 9. Survived ~ Age + Sex + FamilySizeNote                       0.8271605	0.78947
# 10. Survived ~ Age + Sex + Pclass + FamilySizeNote             0.8361392	0.79425
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked      0.8372615	0.78468
# 12. Survived ~ Age + Sex + Pclass + Fare + Title               0.8428732	0.76076
# 13. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch +  		
#                Title                                           0.8327722	0.78947
# 14. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#                Child + Mother                                  0.8428732	0.78468
# 15. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#         Child + Mother + Embarked + Title + FamilySizeNote
#                                                                0.8327722	0.78947
# 16.  Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Embarked + Title + FamilySizeNote + TicketCount  
#                                                                0.8350168	0.78947
# 17. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch + 
#             Child + Mother + Embarked + Title + 
#             FamilySizeNote + TicketCount                       0.8428732	0.78947
# 18. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked +  
#	       FamilySizeNote + TicketCount
#								         0.8585859	0.77990
# 19. Survived ~ Age + Sex + Pclass + Fare + Title + Embarked + 
#             FamilySize + FamilyID + SibSp + Parch              0.8597082   0.80382 (best)

      
