#read data--------------------------------------------------------------
GCredit_Data  <- read.csv(file="GermanCredit_assgt_S18.csv",header=TRUE, stringsAsFactors = FALSE,sep=",")
str(GCredit_Data)

#install packages and libraries-------------------------------------------
install.packages("ggplot")
install.packages("ggplot2")

# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")
# Alternatively, install just dplyr:
install.packages("dplyr")
# Or the development version from GitHub:
install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
#libraries
library(rpart)
library(dplyr)
library(ggplot2)

#prepare data----------------------------------------------------------------
#missing values 
GCredit_Data$AGE = ifelse(is.na(GCredit_Data$AGE),
                          ave(GCredit_Data$AGE , FUN = function(x) mean(x, na.rm = TRUE)),
                          GCredit_Data$AGE)


#NA values
GCredit_Data$NEW_CAR[is.na(GCredit_Data$NEW_CAR)] <- 0
GCredit_Data$USED_CAR[is.na(GCredit_Data$USED_CAR)] <- 0
GCredit_Data$FURNITURE[is.na(GCredit_Data$FURNITURE)] <- 0
GCredit_Data$RADIO.TV[is.na(GCredit_Data$RADIO.TV)] <- 0
GCredit_Data$EDUCATION[is.na(GCredit_Data$EDUCATION)] <- 0
GCredit_Data$RETRAINING[is.na(GCredit_Data$RETRAINING)] <- 0

#Bring all the variables to their required format
GCredit_Data$CHK_ACCT <- as.factor(GCredit_Data$CHK_ACCT)
GCredit_Data$HISTORY <- as.factor(GCredit_Data$HISTORY)
GCredit_Data$NEW_CAR <- as.factor(GCredit_Data$NEW_CAR)
GCredit_Data$USED_CAR <- as.factor(GCredit_Data$USED_CAR)
GCredit_Data$FURNITURE <- as.factor(GCredit_Data$FURNITURE)
GCredit_Data$RADIO.TV <- as.factor(GCredit_Data$RADIO.TV)
GCredit_Data$EDUCATION <- as.factor(GCredit_Data$EDUCATION)
GCredit_Data$RETRAINING <- as.factor(GCredit_Data$RETRAINING)
GCredit_Data$SAV_ACCT <- as.factor(GCredit_Data$SAV_ACCT)
GCredit_Data$EMPLOYMENT <- as.factor(GCredit_Data$EMPLOYMENT)
GCredit_Data$MALE_DIV <- as.factor(GCredit_Data$MALE_DIV)
GCredit_Data$MALE_SINGLE <- as.factor(GCredit_Data$MALE_SINGLE)
GCredit_Data$MALE_MAR_or_WID <- as.factor(GCredit_Data$MALE_MAR_or_WID)
GCredit_Data$CO.APPLICANT <- as.factor(GCredit_Data$CO.APPLICANT)
GCredit_Data$GUARANTOR <- as.factor(GCredit_Data$GUARANTOR)
GCredit_Data$PRESENT_RESIDENT <- as.factor(GCredit_Data$PRESENT_RESIDENT)
GCredit_Data$REAL_ESTATE <- as.factor(GCredit_Data$REAL_ESTATE)
GCredit_Data$PROP_UNKN_NONE <- as.factor(GCredit_Data$PROP_UNKN_NONE)
GCredit_Data$OTHER_INSTALL <- as.factor(GCredit_Data$OTHER_INSTALL)
GCredit_Data$RENT <- as.factor(GCredit_Data$RENT)
GCredit_Data$OWN_RES <- as.factor(GCredit_Data$OWN_RES)
GCredit_Data$JOB <- as.factor(GCredit_Data$JOB)
GCredit_Data$TELEPHONE <- as.factor(GCredit_Data$TELEPHONE)
GCredit_Data$FOREIGN <- as.factor(GCredit_Data$FOREIGN)
GCredit_Data$RESPONSE <- as.factor(GCredit_Data$RESPONSE)

#delete the extra unwanted column
GCredit_Data$X <- NULL

#proportion of good to bad ------------------------------------------------------
counts <- table(GCredit_Data$RESPONSE)
barplot(counts,width = c(10, 10, 100),horiz = FALSE, main="proportion of Good to Bad cases", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$CO.APPLICANT)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="CO.APPLICANT", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$GUARANTOR)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="GUARANTOR", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$FOREIGN)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="FOREIGN", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$FOREIGN)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="FOREIGN", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$EMPLOYMENT)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="EMPLOYMENT", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")

counts <- table(GCredit_Data$EDUCATION)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="EMPLOYMENT", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")
counts <- table(GCredit_Data$MALE_MAR_or_WID)
barplot(counts,width = c(10, 10, 10),horiz = FALSE, main="MALE_MAR_or_WID", 
        xlab="Value", ylab="frequency",axes = TRUE, xlim = c(0, 70) , col = "yellow")
#scatter plot age vs amount
ggplot() +
  geom_point(aes(x = GCredit_Data$AGE,y= GCredit_Data$AMOUNT, color = GCredit_Data$RESPONSE) )+
  ggtitle("age vs amount") +
  xlab("age") +
  ylab("amount")

ggplot() +
  geom_point(aes(x = GCredit_Data$AGE,y= GCredit_Data$DURATION, color = GCredit_Data$RESPONSE) )+
  ggtitle("Age vs Duration") +
  xlab("age") +
  ylab("Duration")
#histogram of age 
hist(GCredit_Data$AGE,breaks= 10, main="Distribution of age",xlab="AGE",col="yellow")


#summary of data--------------------------------------------------------------
summary(GCredit_Data )
attributes(GCredit_Data )


#-----------------------------------------------------------
#Proportion of good to bad values
summary(GCredit_Data$RESPONSE)


#-----------------------------------------------------
library(ggplot2)


--------------------------------------------------------
#develope a rpart decision tree model
library(rpart)
rpModel1=rpart(GCredit_Data$RESPONSE ~ ., data=GCredit_Data, method="class")

#print the model -- text form
print(rpModel1)
#display plot tree
plot(rpModel1, uniform=TRUE,  main="Decision Tree for German Credit")
rpart.plot::prp(rpModel1, type=2, extra=1) #better visualisation

text(rpModel1, use.n=TRUE, all=TRUE, cex=.9)

printcp(rpModel1)
#plot of complexity parameter
plotcp(rpModel1, minline = TRUE, lty= 3,col = "red")
?plotcp


install.packages("rpart.plot")
library(rpart.plot)

#another way to display plot tree  #This one is better
rpart.plot::prp(rpModel1,digits=2, split.font=4,nn.font=4, digits=3, type=2, extra=2, box.col=3, split.box.col=6)


#A model with a binary response.
binary.model=rpart(GCredit_Data$RESPONSE ~ ., data=GCredit_Data, cp= .009)
rpart.plot(binary.model)
#_________________________________________________________________________________________________
#split the data into training and test(validation) sets - 70% for training, rest for validation
nr=nrow(GCredit_Data)
nr
#get a random 70%sample of row-indices
trnIndex = sample(1:nr, size = round(0.7*nr), replace=FALSE) 
#training data with the randomly selected row-indices
GCredit_DataTrn=GCredit_Data[trnIndex,]   
#test data with the other row-indices
GCredit_DataTst = GCredit_Data[-trnIndex,]  

#calculate dimention
dim(GCredit_DataTrn) 
# 700 33
dim(GCredit_DataTst)
#300 33


#develop a tree on the training data
rpModel2=rpart(RESPONSE ~ ., data=GCredit_DataTrn, method="class")
#Obtain the model's predictions on the training data
predTrn=predict(rpModel2, GCredit_DataTrn, type='class')


#Confusion table
table(pred = predTrn, true=GCredit_DataTrn$RESPONSE)
#    true
#pred   0   1
#0      119  39
#1      87 455

#Accuracy
mean(predTrn==GCredit_DataTrn$RESPONSE)
#[1] 0.82
#_________________CTHRESH=0.5________________________________________________________________________________

CTHRESH=0.5

predProbTrn=predict(rpModel1, GCredit_DataTrn, type='prob')
#Confusion table
predTrn = ifelse(predProbTrn[, '1'] >= CTHRESH, '1', '0')
ct = table( pred = predTrn, true=GCredit_DataTrn$RESPONSE)
#Accuracy
mean(predTrn==GCredit_DataTrn$RESPONS)
#________________________Lift curve________________________________________________________________


#get the 'scores' from applying the model to the data
predTrnProb=predict(rpModel1, GCredit_DataTrn, type='prob')
head(predTrnProb)
#So the firts column in predTrnProb give the predicted prob(default)
#-- assume 'default' is the class of interest. Next we sort the data 
#based on these values, group into say, 10 groups (deciles), and calculate
#cumulative response in each group

#we need the score and actual class (OUTCOME) values
trnSc <- subset(GCredit_DataTrn, select=c("RESPONSE")) # selects the OUTCOME column into trnSc
trnSc["score"]<-predTrnProb[, 1]  #add a column named 'Score' with prob(default) values in teh first column of predTrnProb

#sort by score
trnSc<-trnSc[order(trnSc$score, decreasing=TRUE),]

#we will next generate the cumulaive sum of the OUTCOME values -- for this, we want 'default'=1, and 'non-default'=0.   Notice that the OUTCOME is a factor (ie. categorical) variable
str(trnSc)

levels(trnSc$RESPONSE)

#So we should convert these to appropriate integer values 1 and 0

levels(trnSc$RESPONSE)[1]<-1
levels(trnSc$RESPONSE)[2]<-0
# this has not changed OUTCOME -- it will now have factor levels '1' and '0'
trnSc$RESPONSE<-as.numeric(as.character(trnSc$RESPONSE))
str(trnSc)

#obtain the cumulative sum of default cases captured
trnSc$cumDefault<-cumsum(trnSc$RESPONSE)
head(trnSc)
#Plot the cumDefault values (y-axis) by numCases (x-axis)
plot(seq(nrow(trnSc)), trnSc$cumDefault,type = "l", xlab='#cases', ylab='#GOOD')



#__________________________________________________________________________________
#ROCR curve -- you will need to install  the 'ROCR' package
install.packages("ROCR")
library(ROCR)

#obtain the scores from the model for the class of interest, here, the prob('good')
scoreTst=predict(rpModel1,GCredit_DataTrn, type="prob")[,'1']  
#same as predProbTst

#now apply the prediction function from ROCR to get a prediction object
rocPredTst = prediction(scoreTst, GCredit_DataTrn$RESPONSE, label.ordering = c('0', '1'))  

#obtain performance using the function from ROCR, then plot
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)

#optimal cutoff
cost.perf = performance(rocPredTst, "cost")
rocPredTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


#optimal cost with different costs for fp and fn
cost.perf = performance(rocPredTst, "cost", cost.fp = 2, cost.fn = 1)
rocPredTst@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

#other performance measures with the performance function
acc.perf = performance(rocPredTst, measure = "acc")
plot(acc.perf)

#AUC vaue
auc.perf = performance(rocPredTst, measure = "auc")
auc.perf@y.values
#-----------------------------------------------------
#Examine the ROCR prediction object

class(rocPredTst)
slotNames(rocPredTst)
sapply(slotNames(rocPredTst), function(x) class(slot(rocPredTst, x)))
sapply(slotNames(rocPredTst), function(x) length(slot(rocPredTst, x)))


#The prediction object has a set of slots with names as seen above.
#The slots hold values of predictions,  labels, cutoffs, fp values, ....etc.

#Similarly, examine the ROCR performance object

class(acc.perf)
slotNames(acc.perf)

acc.perf@x.name   #values in the x.name slot
acc.perf@y.name

#So the x.values give the cutoff values and the y.values goves the corresponding 
#accuracy. We can use these to find, for example, the cutoff corresponding to the 
#maximum accuracy
#The accuracy values are in y.values slot of the acc.perf object. 
#This is a list, as seen by: 
class(acc.perf@y.values)
#... and we can get the values through 
acc.perf@y.values[[1]]

#get the index of the max value of accuracy
ind=which.max(acc.perf@y.values[[1]])

#get the accuracy value coresponding to this index
acc = (acc.perf@y.values[[1]])[ind]

#get the cutoff corresponding to this index
cutoff = (acc.perf@x.values[[1]])[ind]

#show these results
print(c(accuracy= acc, cutoff = cutoff))

#_________________________________________________________________________________________________
#split the data into training and test(validation) sets - 80% for training, rest for validation
nr2=nrow(GCredit_Data)
#get a random 80%sample of row-indices
trnIndex2 = sample(1:nr2, size = round(0.8*nr2), replace=FALSE) 
#training data with the randomly selected row-indices
GCredit_DataTrn2=GCredit_Data[trnIndex2,]   
#test data with the other row-indices
GCredit_DataTst2 = GCredit_Data[-trnIndex2,]  

#calculate dimention
dim(GCredit_DataTrn2) 
# 800 33
dim(GCredit_DataTst2)
# 200 33


#develop a tree on the training data
rpModel3=rpart(RESPONSE ~ ., data=GCredit_DataTrn2, method="class")
#Obtain the model's predictions on the training data
predTrn2=predict(rpModel3, GCredit_DataTrn2, type='class')


#Confusion table
table(pred = predTrn2, true=GCredit_DataTrn2$RESPONSE)
#    true
#pred   0   1
#0      101  31
#1      139  529

#Accuracy
mean(predTrn2==GCredit_DataTrn2$RESPONSE)
#[1] 0.7875


#split the data into training and test(validation) sets - 50% for training, rest for validation
nr3=nrow(GCredit_Data)
#get a random 80%sample of row-indices
trnIndex3 = sample(1:nr3, size = round(0.5*nr3), replace=FALSE) 
#training data with the randomly selected row-indices
GCredit_DataTrn3=GCredit_Data[trnIndex3,]   
#test data with the other row-indices
GCredit_DataTst3 = GCredit_Data[-trnIndex3,]  

#calculate dimention
dim(GCredit_DataTrn3) 
# 500 33
dim(GCredit_DataTst3)
#500  33


#develop a tree on the training data
rpModel4=rpart(RESPONSE ~ ., data=GCredit_DataTrn3, method="class")
#Obtain the model's predictions on the training data
predTrn3=predict(rpModel4, GCredit_DataTrn3, type='class')


#Confusion table
table(pred = predTrn3, true=GCredit_DataTrn3$RESPONSE)
#    true
#pred   0   1
#0      84  23
#1      66  327

#Accuracy
mean(predTrn3==GCredit_DataTrn3$RESPONSE)
#[1] 0.822


#------------------------C50-------------------
install.packages("C50")
library(C50)
c50tree= C5.0(RESPONSE ~ ., data = GCredit_Data, rules=FALSE,size=0.2,
              control=C5.0Control(subset = TRUE, winnow = FALSE, CF= 0.5,
                                  minCases = 2, noGlobalPruning = FALSE, 
                                  sample = 0))
plot(c50tree)
print(c50tree)
?C5.0
#----------------------------------


