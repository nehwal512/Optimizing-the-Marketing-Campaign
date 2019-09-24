# correlation 
library(dplyr)
# select only Numeric variables 
data3 %>% select_if(is.numeric) -> data_num1

# select only factors variable 
data3 %>% select_if(is.factor) -> data_fact1

# correlation matrix 
cor<-round(cor(data_num1, method = "pearson", use = "complete.obs"),2)
library(ggcorrplot)
ggcorrplot(cor,hc.order = TRUE, type = "upper",
           lab = TRUE)

# removing variables 
data3$GiftCntAll<- NULL
data3$PromCntCardAll<- NULL
data3$GiftTimeFirst<- NULL
data3$GiftCntCard36<- NULL

# for checking dimension of data 
length(data3)

# view data
View(data3)

# scalling data 
data_scal = scale(data_num1)

# make new data frame with catagorical variable and scaled numeric variables 
data= data.frame(data_fact1,data_scal)
str(data)
