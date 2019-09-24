# Importing Data by using "read.csv" command 
data3 <- read.csv("D:/projects/imarticus projects code/Case Study 1_ Optimizing Marketing Campaign/PVA97NK.csv")

# Structure of data 
str(data3)

# for View of data 
View(data3)

# Removing column from data which is not required 
data3<-data3[,-3]
data3<-data3[,-2]

# for knowing the level of Gender column 
levels(data3$DemGender)

# for checking the category's count
table(data3$DemGender)

# Impute unwanted values with NA 
data3$DemGender[data3$DemGender=='U']=NA
data3$DemMedHomeValue[data3$DemMedHomeValue=="$0 "]=NA
data3$DemMedIncome[data3$DemMedIncome=="$0 "]=NA

# convert data structure as per requirement from numeric to factor
data3$TargetB<- as.factor(data3$TargetB)
data3$StatusCatStarAll<- as.factor(data3$StatusCatStarAll)
data3$DemCluster<- as.factor(data3$DemCluster)
data3$DemGender<- as.factor(data3$DemGender)
data3$DemHomeOwner<- as.factor(data3$DemHomeOwner)

# convert data structure as per requirement from factor to numeric.
data3$GiftAvgLast<-as.numeric(data3$GiftAvgLast)
data3$GiftAvg36<-as.numeric(data3$GiftAvg36)
data3$GiftAvgAll<-as.numeric(data3$GiftAvgAll)
data3$GiftAvgCard36<-as.numeric(data3$GiftAvgCard36)
data3$DemMedHomeValue<-as.numeric(data3$DemMedHomeValue)
data3$DemMedIncome<-as.numeric(data3$DemMedIncome)

# for checking missing values 
colSums(is.na(data3))

# Imputing missing values 
hist(data3$DemAge)
data3$DemAge[is.na(data3$DemAge)]<-median(data3$DemAge,na.rm=T)
hist(data3$DemMedHomeValue)
data3$DemMedHomeValue[is.na(data3$DemMedHomeValue)]<-median(data3$DemMedHomeValue,na.rm=T)
hist(data3$DemMedIncome)
data3$DemMedIncome[is.na(data3$DemMedIncome)]<-median(data3$DemAge,na.rm=T)
table(data3$DemGender)
data3$DemGender[is.na(data3$DemGender)]<-"F"

# checking missing values 
colSums(is.na(data3))

#summary of data
summary(data3)

# boxplot for finding outliers and removing Outliers 
boxplot(data3$GiftCntAll)
summary(data3$GiftCntAll)
uf_colcnt <- 15 +1.5*IQR(data3$GiftCntAll) 
hist(data3$GiftCntAll)
data3$GiftCntAll[data3$GiftCntAll >uf_colcnt ] <-median(data3$GiftCntAll)
boxplot(data3$GiftCntAll)


boxplot(data3$GiftCntCardAll)
summary(data3$GiftCntCardAll)
uf_colcnt <- 8 +1.5*IQR(data3$GiftCntCardAll) 
hist(data3$GiftCntCardAll)
data3$GiftCntCardAll[data3$GiftCntCardAll >uf_colcnt ] <-median(data3$GiftCntCardAll)
boxplot(data3$GiftCntCardAll)

boxplot(data3$GiftAvgLast)
summary(data3$GiftAvgLast)
uf_colcnt <- 35 +1.5*IQR(data3$GiftAvgLast) 
hist(data3$GiftAvgLast)
data3$GiftAvgLast[data3$GiftAvgLast >uf_colcnt ] <-mean(data3$GiftAvgLast)
boxplot(data3$GiftAvgLast)

boxplot(data3$PromCnt12)
summary(data3$PromCnt12)
Lf_colcnt <- 11 -1.5*IQR(data3$PromCnt12)
uf_colcnt <- 13 +1.5*IQR(data3$PromCnt12) 
hist(data3$PromCnt12)
data3$PromCnt12[data3$PromCnt12 < Lf_colcnt ] <-median(data3$PromCnt12)
data3$PromCnt12[data3$PromCnt12 > uf_colcnt ] <-median(data3$PromCnt12)
boxplot(data3$PromCnt12)

boxplot(data3$PromCnt36)
summary(data3$PromCnt36)
Lf_colcnt <- 25 -1.5*IQR(data3$PromCnt36)
uf_colcnt <- 33 +1.5*IQR(data3$PromCnt36) 
hist(data3$PromCnt36)
data3$PromCnt36[data3$PromCnt36 <Lf_colcnt ] <-median(data3$PromCnt36)
data3$PromCnt36[data3$PromCnt36 >uf_colcnt ] <-median(data3$PromCnt36)
boxplot(data3$PromCnt36)

boxplot(data3$PromCntAll)
summary(data3$PromCntAll)
uf_colcnt <- 65 +1.5*IQR(data3$PromCntAll) 
hist(data3$PromCntAll)
data3$PromCntAll[data3$PromCntAll >uf_colcnt ] <-median(data3$PromCntAll)
boxplot(data3$PromCntAll)

boxplot(data3$DemPctVeterans)
summary(data3$DemPctVeterans)
Lf_colcnt <- 25 -1.5*IQR(data3$DemPctVeterans)
uf_colcnt <- 37 +1.5*IQR(data3$DemPctVeterans) 
hist(data3$DemPctVeterans)
data3$DemPctVeterans[data3$DemPctVeterans <Lf_colcnt ] <-mean(data3$DemPctVeterans)
data3$DemPctVeterans[data3$DemPctVeterans >uf_colcnt ] <-mean(data3$DemPctVeterans)
boxplot(data3$DemPctVeterans)
