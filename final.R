#remove everything in the working environment.
rm(list =ls())
cat("\14") #clears console



#To install packages simply type install.packages("packageName")
if(!require(ggplot2)){
   install.packages("ggplot2")
   library(ggplot2)
}
#library loads installed packages 
library(ggplot2) 
library(randomForest) 


#set working directory
setwd("D:\\Projects\\Titanic") #specify path where you have your data


#Import train and test csv files 
# read.csv reads a file in table format and creates a data frame from it
# the default for stringsAsFactors is true, as we donot wish to convert character vectors to factors we make it false
train <- read.csv("train.csv", stringsAsFactors = FALSE) 
test <- read.csv("test.csv",stringsAsFactors = FALSE)

#view data
head(train)
head(test)

#adding survived variable to test set 
test$Survived <- NA
#combining both test and train dataframe by rows

fullset <- rbind(train,test)




####
#####
#--------------------Exploratory analysis ----------------------- 

str(fullset) #get the basic structure of fullset
# study the variables 
   
summary(fullset)



#####
#Understanding features
#
fullset$Pclass <- as.factor(fullset$Pclass)


ggplot(fullset[1:891,], aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar() +
  ggtitle("Impact of Class on Survival") 



####
fullset$Sex <- as.factor(fullset$Sex)
summary(fullset$Sex)
ggplot(fullset[1:891,], aes(x = Sex, fill = factor(Survived))) + 
  geom_bar() +
  ggtitle("Do females have higher survival rate?")

ggplot(fullset[1:891,], aes(Sex)) +
  facet_wrap(~Pclass) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill=factor(Survived)), stat= "count")+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  ggtitle("Class") + labs(y = "percent")




##### ---------------Feature Engineering---------------------
#creating new title variable
fullset$Title <- sapply(fullset$Name,FUN = function(x){strsplit(x,"[,.]")[[1]][2]})
fullset$Title <- sub(' ', '', fullset$Title)
fullset$Title <- as.factor(fullset$Title)
summary(fullset$Title)

table(fullset$Sex,fullset$Title)
fullset$Title <- as.character(fullset$Title)

fullset$Title[fullset$Title %in% c("Mlle","Ms")] <- "Miss"
fullset$Title[fullset$Title == "Mme"] <- "Mrs"
fullset$Title[fullset$Title %in% c( "Don", "Sir", "Jonkheer","Rev","Dr")] <- "Sir"
fullset$Title[fullset$Title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
fullset$Title[fullset$Title %in% c("Capt","Col", "Major")] <- "Officer"


fullset$Title <- as.factor(fullset$Title)
summary(fullset$Title)

ggplot(fullset[1:891,], aes(x = Age)) + 
  geom_histogram(aes(y = ..density.., color = Title, fill = Title),  alpha = 0.4, position = "identity") +
  geom_density(aes(color = Title), size =1)




###family size
fullset$Surname <- sapply(fullset$Name,FUN = function(x){strsplit(x,"[,.]")[[1]][1]})
fullset[fullset$Surname == "Palsson",]
fullset$FamSize <- fullset$SibSp + fullset$Parch + 1

ggplot(fullset[1:891,], aes(x = FamSize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')+ ggtitle("Family")

###family group
fullset$FamGroup[fullset$FamSize == 1] <- 'Individual'
fullset$FamGroup[fullset$FamSize < 5 & fullset$FamSize > 1] <- 'small'
fullset$FamGroup[fullset$FamSize > 4] <- 'large'
fullset$FamGroup <- as.factor(fullset$FamGroup)

#Missing value Imputation
#FARE
sum(is.na(fullset$Fare))
which(is.na(fullset$Fare))
fullset[1044,]

fullset$Fare[1044] <- median(fullset[fullset$Pclass == '3' & fullset$Embarked == 'S', ]$Fare, na.rm = TRUE)

sum(is.na(fullset$Ticket))

###
n <- data.frame(table(fullset$Ticket))
fullset <- merge(fullset,n, by.x="Ticket", by.y="Var1", x.all=T) # Assign the frequency of each ticket appearance
fullset$Fare2 <- fullset$Fare / fullset$Freq

fullset <- fullset[order(fullset$PassengerId),]

which(fullset$Cabin == '')


#EMBARKED
which(fullset$Embarked == '')
fullset[c(62,830),]


#who else is paying 40 and is from 1st class
fullset$Embarked <- as.factor(fullset$Embarked)
fullset[fullset$Fare2 >= 39 & fullset$Fare2 <= 41 & fullset$Pclass == 1,]
summary(fullset[fullset$Fare2 >= 39 & fullset$Fare2 <= 41 & fullset$Pclass == 1,"Embarked"])

fullset$Embarked <- as.character(fullset$Embarked)
fullset$Embarked[fullset$Embarked %in% c("","")] <-  "C"
fullset$Embarked <- as.factor(fullset$Embarked)

# #AGE 

sum(is.na(fullset$Age))
title.age <- aggregate(fullset$Age,by = list(fullset$Title), FUN = function(x) median(x, na.rm = T))
fullset[is.na(fullset$Age), "Age"] <- apply(fullset[is.na(fullset$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])
#Na value count
sum(is.na(fullset$Age))


ggplot(fullset[1:891,], aes(Age, fill = factor(Survived))) + 
  facet_grid(.~Sex) +
  geom_dotplot(binwidth = 2)


ggplot(fullset[1:891,], aes(factor(Title), fill = factor(Survived))) + 
  facet_grid(.~Sex) +
  geom_bar()

ggplot(fullset[1:891,], aes(factor(Title), fill = factor(Survived))) + 
  facet_grid(.~Sex+Pclass) +
  geom_bar()

#miss2
fullset$Title <-  as.character(fullset$Title)
fullset[fullset$Sex == "female" & fullset$Age < 18,"Title"] <- "Miss2"
fullset$Title <-  as.factor(fullset$Title)

summary(fullset$Title)



# Create the column isMinor, and indicate whether child or adult
fullset$isMinor[fullset$Age < 18] <- 'Minor'
fullset$isMinor[fullset$Age >= 18] <- 'Adult'
fullset$isMinor <- as.factor(fullset$isMinor)



fullset$Survived <- as.factor(fullset$Survived)
str(fullset)

#
train <- fullset[1:891,]
test <- fullset[892:1309,]

###########################################
#With tuneRF search for the optimal value (with respect to Out-of-Bag error estimate) of mtry for randomForest

set.seed(12345)
names(train)
bestmtry <- tuneRF(train[,c(4,10,13,12,16,6,19)],train[,3], stepFactor=1.5, improve=1e-5, ntree=1000)
print(bestmtry)
####

#----------------modelling----------------------
set.seed(786)

model <- randomForest(factor(Survived) ~ Pclass + Fare + Title + Embarked + FamGroup + Sex + isMinor,
                         data = train, importance = TRUE, ntree = 1000,mtry=2)


model
varImpPlot(model)
importance(model)



prediction <- predict(model,test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'report1.csv', row.names = F)
