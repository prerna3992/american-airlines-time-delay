GCredit_Data <- read.csv(choose.files(), header = T, stringsAsFactors = FALSE)
str(GCredit_Data)

#replace NA with 0
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

str(GCredit_Data)

#delete the extra unwanted column
GCredit_Data$X <- NULL

#Proportion of good to bad values
summary(GCredit_Data$RESPONSE)
#  0   1 
#300 700 
response <- table(GCredit_Data$RESPONSE)
ptab <- prop.table(response)

#0   1 
#0.3 0.7

ggplot2::qplot(GCredit_Data$RESPONSE, ylim = c(0, 750))

#Yes there are missing vales
#handling the missing values

View(GCredit_Data)

#real - values attributes
psych::describe(GCredit_Data$DURATION)
#   vars    n mean    sd median trimmed mad min max range skew kurtosis   se
#X1    1 1000 20.9 12.06     18   19.47 8.9   4  72    68 1.09      0.9 0.38

psych::describe(GCredit_Data$AMOUNT)
#   vars    n    mean      sd median trimmed     mad min   max range skew kurtosis    se
#X1    1 1000 3271.16 2822.63 2319.5 2754.53 1627.15 250 18424 18174 1.94     4.25 89.26

psych::describe(GCredit_Data$INSTALL_RATE)
#   vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
#X1    1 1000 2.97 1.12      3    3.09 1.48   1   4     3 -0.53    -1.21 0.04

psych::describe(GCredit_Data$AGE)
#   vars   n  mean    sd median trimmed   mad min max range skew kurtosis   se
#X1    1 991 35.48 11.37     33   34.09 10.38  19  75    56 1.03     0.61 0.36

psych::describe(GCredit_Data$NUM_CREDITS)
#   vars    n mean   sd median trimmed mad min max range skew kurtosis   se
#X1    1 1000 1.41 0.58      1    1.33   0   1   4     3 1.27     1.58 0.02

psych::describe(GCredit_Data$NUM_DEPENDENTS)
#   vars    n mean   sd median trimmed mad min max range skew kurtosis   se
#X1    1 1000 1.16 0.36      1    1.07   0   1   2     1  1.9     1.63 0.01


#category value attributes
summary(GCredit_Data$CHK_ACCT)
#0   1   2   3 
#274 269  63 394

summary(GCredit_Data$HISTORY)
#0   1   2   3   4 
#40  49 530  88 293 

summary(GCredit_Data$NEW_CAR)
#0   1 
#766 234

summary(GCredit_Data$USED_CAR)
#0   1 
#897 103

summary(GCredit_Data$FURNITURE)
#0   1 
#819 181

summary(GCredit_Data$RADIO.TV)
#0   1 
#720 280

summary(GCredit_Data$EDUCATION)
#0   1 
#950  50 

summary(GCredit_Data$RETRAINING)
#0   1 
#903  97

summary(GCredit_Data$SAV_ACCT)
#0   1   2   3   4 
#603 103  63  48 183 

summary(GCredit_Data$EMPLOYMENT)
#0   1   2   3   4 
#62 172 339 174 253

summary(GCredit_Data$MALE_DIV)
#0   1 
#950  50 

summary(GCredit_Data$MALE_SINGLE)
#0   1 
#452 548 

summary(GCredit_Data$MALE_MAR_or_WID)
#0   1 
#908  92

summary(GCredit_Data$CO.APPLICANT)
#0   1 
#959  41 

summary(GCredit_Data$GUARANTOR)
#0   1 
#948  52 

summary(GCredit_Data$PRESENT_RESIDENT)
#1   2   3   4 
#130 308 149 413 

summary(GCredit_Data$REAL_ESTATE)
#0   1 
#718 282 

summary(GCredit_Data$PROP_UNKN_NONE)
#0   1 
#846 154

summary(GCredit_Data$OTHER_INSTALL)
#0   1 
#814 186

summary(GCredit_Data$RENT)
#0   1 
#821 179 

summary(GCredit_Data$OWN_RES)
#0   1 
#287 713

summary(GCredit_Data$JOB)
#0   1   2   3 
#22 200 630 148 

summary(GCredit_Data$TELEPHONE)
#0   1 
#596 404

summary(GCredit_Data$FOREIGN)
#0   1 
#963  37 

summary(GCredit_Data$RESPONSE)
#0   1 
#300 700


barplot(table(GCredit_Data$RESPONSE, GCredit_Data$CHK_ACCT ),  col = c("red","steel blue"), ylim = c(0, 500), ylab="Frequency", xlab="Checking Account Status")
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$DURATION), ylim =  c(0,200), ylab="Frequency", xlab = "Duration of Credit in Months", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$HISTORY), ylim =  c(0,600),ylab="Frequency", xlab="Credit History",  col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$AMOUNT), ylim =  c(0,4),ylab="Frequency", xlab="Credit Amount", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$SAV_ACCT), ylim =  c(0,900),ylab="Frequency", xlab="Avg. Balance in Saving Account", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$EMPLOYMENT), ylim =  c(0,400),ylab="Frequency", xlab="Present Employment since", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$INSTALL_RATE), ylim =  c(0,600),ylab="Frequency", xlab="Installment Rate", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$MALE_DIV), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant is Male and Divorced", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$MALE_SINGLE), ylim =  c(0,600),ylab="Frequency", xlab="Applicant is Male and Single", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$MALE_MAR_or_WID), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant is Male and married or widower", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$CO.APPLICANT), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant has a co-applicant", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$GUARANTOR), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant has a guarantor", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$PRESENT_RESIDENT), ylim =  c(0,500),ylab="Frequency", xlab="Present residence since-years", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$REAL_ESTATE), ylim =  c(0,800),ylab="Frequency", xlab="Applicant owns REal Estate", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$PROP_UNKN_NONE), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant owns no property", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$AGE), ylim =  c(0,60),ylab="Frequency", xlab="Age in years", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$OTHER_INSTALL), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant has other installment plan", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$RENT), ylim =  c(0,1000),ylab="Frequency", xlab="Applicant Rents", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$OWN_RES), ylim =  c(0,800),ylab="Frequency", xlab="Applicant owns Residence", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$NUM_CREDITS), ylim =  c(0,800),ylab="Frequency", xlab="Number of existing credits at this bank", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$JOB), ylim =  c(0,800),ylab="Frequency", xlab="Nature of Job", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$NUM_DEPENDENTS), ylim =  c(0,1000),ylab="Frequency", xlab="Number of Dependents", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$TELEPHONE), ylim =  c(0,600),ylab="Frequency", xlab="Applicant has a phone", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

barplot(table(GCredit_Data$RESPONSE, GCredit_Data$FOREIGN), ylim =  c(0,1000),ylab="Frequency", xlab="Foreign Worker", col = c("red","steel blue"))
legend("topright", 
       legend = c("Bad Credit", "Good Credit"), 
       fill = c("red", "steel blue"))

#Q2
#Install the 'rpart' package to develop deciction trees
install.packages("rpart")
library(rpart)
summary(GCredit_Data)
rtree=rpart(RESPONSE ~ .,data=GCredit_Data, method="class")
#decision tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot::prp(rtree, type=2, extra=1)

