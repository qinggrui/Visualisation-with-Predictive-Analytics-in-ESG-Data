library(data.table)
library(ggplot2)
library(nnet)
library(caTools)
library(class)
library(dplyr)
library(rpart)
library(rpart.plot)
library(pdp)
library(vip)
library(reshape)
library(tidyquant)
library(zoo)
library(Metrics)
library(e1071)
#string and text mining packages
library(stringr)
library(quanteda)
library(MLmetrics)

ESG1 = fread("C:/Users/Qing Rui/Desktop/BC2406 Analytics/AY21 Team Assignment and Project/Project data/ESGIndex2020.csv", stringsAsFactors = F)
summary(ESG1)
View(ESG1)
ESG1 = subset(ESG1,select= -c(V7,V8)) #drop last 2 NA columns of ESG1
summary(ESG1)

#Change header of ESG1 dt for easier coding
colnames(ESG1)=c('CountryName', 'CountryCode','Region','ESGScore','ESGRank','RiskEval')
ESG1 = ESG1[order(CountryName)] #Order by country name from A to Z 

ESG2 = fread("C:/Users/Qing Rui/Desktop/BC2406 Analytics/AY21 Team Assignment and Project/Project data/ESG_VariablesNEW.csv",
             na.strings='..', stringsAsFactors = F)

summary (ESG2)
View(ESG2)
ESG2 = ESG2[!(ESG2$Time=='2020'|ESG2$Time=='2050'),] #Drop year 2020 and 2050
ESG2 = ESG2[-c(4633:4637)] #drop last 5 empty rows
summary (ESG2)
colnames(ESG2)=c('Year', 'YearCode','CountryName','CountryCode','Voice','Political',
                 'GovtEff', 'RegQual','Law','Corruption','LifeExp', 'Carbon','Nitrous',
                 "FertRate")
View(ESG2)

ESG3 <- fread("C:/Users/Qing Rui/Desktop/BC2406 Analytics/AY21 Team Assignment and Project/Project data/GDPgrowth.csv", na.string = "..")
colnames(ESG3) = c('Year','YearCode','CountryName','CountryCode','GDP')
ESG3 = ESG3[order(CountryName)]
ESG3 = ESG3[-c(1:5)]
ESG3 = ESG3[!(ESG3$Year=='2020'|ESG3$Year=='2050'),]

#Merge both ESG1, ESG2 table
x = merge(x=ESG1,y=ESG2, by= 'CountryCode', all=TRUE) #Merge of ESG1 and ESG2 dataset
#Check how many countries in dataset (176 countries)

#Merge both y and ESG3 table

y1 <- merge(x, ESG3, by= c('CountryCode','Year','YearCode'), all=TRUE)
y1 = subset(y1, select=-c(CountryName,YearCode,CountryName.y)) #Remove duplicated Column name for countryname, yearcode and year
y2 = na.omit(y1,cols="ESGScore")
colnames(y2)[3]='CountryName' #Change column name of 2nd column
y2[y2 =='..']=NA #Ensure no '..' Values left and replace with NA
summary(y2)
View(y1)
length(unique(y2$CountryName))
write.csv(y2, 'Dataset for CART.csv') #write table y2 into csv file format with NA values for CART analysis
### For Prof: Substitute Dataset for CART into y2 variable

#Replace NA Values by mean of each country (create another duplicate table to work with first, prevent alteration to original merged table)
z = y2 #Creating new table based off table y

subna = aggregate(z[,8:18],list(z$CountryName), mean,na.rm=TRUE) #aggregate(z$Voice,list(z$CountryName), mean)
View(subna) #returns mean of each variable grouped by country for the 10 variables
summary(subna) #check that no data is NA in pseudo table

z = na.aggregate(z[,8:18],list(z$CountryName), mean, na.rm=TRUE)
ESGdata = cbind(y2,z)
View(ESGdata)
ESGdata = subset(ESGdata, select= -c(8:18)) #drop columns of ab with NA Values

summary(ESGdata) #no NA values in datatable
View(ESGdata)
write.csv(ESGdata, 'Dataset for NML and SVM.csv') #write table ESGdata into csv file format with NA values subbed using mean values NML, Linear Regression and SVM analysis 
### For Prof: Substitute Dataset for NML and SVM into ESGData variable

###Train and test set subset
set.seed(2004)
ESGdata2019 <- ESGdata[Year=="2019"]
ESGdata2019
train <- sample.split(Y = ESGdata2019$ESGScore, SplitRatio = 0.7)
trainset <- subset(ESGdata2019, train == T)
trainset
testset <- subset(ESGdata2019, train == F)
testset
class(ESGdata2019$Year)

library(corrplot)

Only_variables <- ESGdata2019[, c(5,8,9,10,11,12,13,14,15,16,17,18)] #Filter out columns such that only variables remain

Corrpurpose <- cor(Only_variables) #Using cor() to format the variables into correlation pairs

corrplot(Corrpurpose, method="color",
         diag=FALSE, # tl.pos="d", 
         type="upper", order="hclust", 
         title='Correlation of 10 ESG Variables versus GDP and ESGScore', 
         addCoef.col = "black", # Add coefficient of correlation
         # Combine with significance
         sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         mar=c(0,0,1,0)
)
############################################### NML by Mean median mode and Linear equation ########################################

###Investigating VOICE NML
summary(trainset[, Voice])
vs = round(summary(trainset[, Voice]),2)
boxplot(trainset[, Voice], main='Voice Box Plot', staplewex=1,labels=quantile(trainset$Voice)) +
  text(y = boxplot.stats(vs)$stats, labels = boxplot.stats(vs)$stats, x = 1.25, pos=4)

mean(trainset$Voice)

plot((trainset[, Voice]), trainset[, ESGScore], main='ESGScore vs 2019 Voice')
lm(trainset[, ESGScore]~trainset[, Voice]) #2020 ESGScore value= 49.04 - 14.00(Trainset$Voice)


###Investigating Political NML
summary(trainset[, Political])
ps = round(summary(trainset[, Political]),2)

out.ps <- boxplot.stats(ps)$out

boxplot(trainset[, Political], main = 'Political Box Plot', staplewex=1, labels=quantile(trainset$Political)) +
  text(y = boxplot.stats(ps)$stats, labels = boxplot.stats(ps)$stats, x = 1.25, pos = 4)
mtext(paste("Outliers: ", paste(out.ps, collapse = ",")))

mean(trainset$Political)

plot((trainset[, Political]), trainset[, ESGScore], main='ESGScore vs 2019 Political')
lm(trainset[, ESGScore]~trainset[, Political]) #2020 ESGScore value= 48.64 - 14.84(Trainset$Political)

###Investigating GovtEff NML
summary(trainset[, GovtEff])
ges = round(summary(trainset[, GovtEff]),2)
boxplot(trainset[, GovtEff], main='GovtEff Box Plot', staplewex=1,labels=quantile(trainset$GovtEff)) +
  text(y = boxplot.stats(ges)$stats, labels = boxplot.stats(ges)$stats, x = 1.25, pos=4)

mean(trainset$GovtEff)

plot((trainset[, GovtEff]), trainset[, ESGScore], main='ESGScore vs 2019 GovtEff')
lm(trainset[, ESGScore]~trainset[, GovtEff]) #2020 ESGScore value= 48.22 - 17.47 (Trainset$GovtEff)

###Investigating RegQual NML
summary(trainset[, RegQual])
rqs = round(summary(trainset[, RegQual]),2)
boxplot(trainset[, RegQual], main='RegQual Box Plot', staplewex=1,labels=quantile(trainset$RegQual)) +
  text(y = boxplot.stats(rqs)$stats, labels = boxplot.stats(rqs)$stats, x = 1.25, pos=4)

mean(trainset$RegQual)

plot((trainset[, RegQual]), trainset[, ESGScore], main='ESGScore vs 2019 RegQual')
lm(trainset[, ESGScore]~trainset[, RegQual]) #2020 ESGScore value= 47.89 -17.12 (Trainset$RegQual)

###Investigating Law NML
summary(trainset[, Law])
ls = round(summary(trainset[, Law]),2)
boxplot(trainset[, Law], main='Law Box Plot', staplewex=1,labels=quantile(trainset$Law)) +
  text(y = boxplot.stats(ls)$stats, labels = boxplot.stats(ls)$stats, x = 1.25, pos=4)
mean(trainset$Law)

plot((trainset[, Law]), trainset[, ESGScore], main='ESGScore vs 2019 Law')
lm(trainset[, ESGScore]~trainset[, Law]) #2020 ESGScore value= 47.9 - 16.4 (Trainset$Law)

###Investigating Corruption NML
summary(trainset[, Corruption])
cs = round(summary(trainset[, Corruption]),2)
boxplot(trainset[, Corruption], main='Corruption Box Plot', staplewex=1,labels=quantile(trainset$Corruption)) +
  text(y = boxplot.stats(cs)$stats, labels = boxplot.stats(cs)$stats, x = 1.25, pos=4)

mean(trainset$Corruption)

plot((trainset[, Corruption]), trainset[, ESGScore], main='ESGScore vs 2019 Corruption')
lm(trainset[, ESGScore]~trainset[, Corruption]) #2020 ESGScore value= 48.1 - 15.3 (Trainset$Corruption)

###Investigating LifeExp NML
summary(trainset[, LifeExp])
les = round(summary(trainset[, LifeExp]),2)
boxplot(trainset[, LifeExp], main='LifeExp Box Plot', staplewex=1,labels=quantile(trainset$LifeExp)) +
  text(y = boxplot.stats(les)$stats, labels = boxplot.stats(les)$stats, x = 1.25, pos=4)

mean(trainset$LifeExp)

plot((trainset[, LifeExp]), trainset[, ESGScore], main='ESGScore vs 2019 LifeExp')
lm(trainset[, ESGScore]~trainset[, LifeExp]) #2020 ESGScore value= 206.8 - 2.18 (Trainset$LifeExp)

###Investigating Carbon NML
summary(trainset[, Carbon])
cas = round(summary(trainset[, Carbon]),2)

out.cas <- sort(round(boxplot.stats(trainset$Carbon)$out, 2))

boxplot(trainset[, Carbon], main='Carbon Box Plot', staplewex=1,labels=quantile(trainset$Carbon)) +
  text(y = boxplot.stats(cas)$stats, labels = boxplot.stats(cas)$stats, x = 1.25, pos=4)
mtext(paste("Outliers: ", paste(out.cas, collapse = ",")))

mean(trainset$Carbon)

plot((trainset[, Carbon]), trainset[, ESGScore], main='ESGScore vs 2019 Carbon')
lm(trainset[, ESGScore]~trainset[, Carbon]) #2020 ESGScore value= 59.8 - 2.68 (Trainset$Carbon)

###Investigating Nitrous NML
summary(trainset[, Nitrous])
ns = round(summary(trainset[, Nitrous]),2)

out.ns <- sort(round(boxplot.stats(trainset$Nitrous)$out, 2))

boxplot(trainset[, Nitrous], main='Nitrous Box Plot', staplewex=1,labels=quantile(trainset$Nitrous)) +
  text(y = boxplot.stats(ns)$stats, labels = boxplot.stats(ns)$stats, x = 1.25, pos=4)
mtext(paste("Outliers: ", paste(out.ns, collapse = ",")))

mean(trainset$Nitrous)

plot((trainset[, Nitrous]), trainset[, ESGScore], main='ESGScore vs 2019 Nitrous')
lm(trainset[, ESGScore]~trainset[, Nitrous]) #2020 ESGScore value= 51.04 - 1.99 (Trainset$Nitrous)

###Investigating FertRate NML
summary(trainset[, FertRate])
frs = round(summary(trainset[, FertRate]),2)

out.frs <- sort(round(boxplot.stats(trainset$FertRate)$out, 2))

boxplot(trainset[, FertRate], main='FertRate Box Plot', staplewex=1,labels=quantile(trainset$FertRate)) +
  text(y = boxplot.stats(frs)$stats, labels = boxplot.stats(frs)$stats, x = 1.25, pos=4)
mtext(paste("Outliers: ", paste(out.frs, collapse = ",")))

mean(trainset$FertRate)

plot((trainset[, FertRate]), trainset[, ESGScore], main='ESGScore vs 2019 FertRate')
lm(trainset[, ESGScore]~trainset[, FertRate]) #2020 ESGScore value= 17.97 - 11.56 (Trainset$FertRate)

###Investigating GDP NML
summary(trainset[, GDP])
gdps = round(summary(trainset[, GDP]),2)

out.gdps <- sort(round(boxplot.stats(trainset$GDP)$out, 2))

boxplot(trainset[, GDP], main='GDP Box Plot', staplewex=1,labels=quantile(trainset$GDP)) +
  text(y = boxplot.stats(gdps)$stats, labels = boxplot.stats(gdps)$stats, x = 1.25, pos=4)
mtext(paste("Outliers: ", paste(out.gdps, collapse = ",")))

mean(trainset$GDP)

plot((trainset[, GDP]), trainset[, ESGScore], main='ESGScore vs 2019 GDP')
lm(trainset[, ESGScore]~trainset[, GDP]) #2020 ESGScore value= 45.49 + 1.468 (Trainset$GDP)


#######################################NML Prediction of region ESG Score with mean################################

#Predictive NML using mean of region
#Another method is to group the countries by region and
#Find the mean value to try to predict the region's ESGScore

ESGdata2019$Region
#Asia, Africa, Europe, South America, North America, Oceania
#Predictive NML using mean of region
#Another method is to group the countries by region and
#Find the mean value to try to predict the region's ESGScore

summary(ESGdata2019$Region)
#Asia, Africa, Europe, South America, North America, Oceania
summary(ESGdata2019$Region)
ESGdata2019$Region <- factor(ESGdata2019$Region)
ggplot(data= Asia, aes(y=CountryName, x = ESGScore)) +
  geom_point() + labs(title = "Asia Region ESGScore")

#50 Africa, 40 Asia, 43 Europe, 21 North America, 10 Oceania, 12 South America
#Split evenly for each region

#Asia (Boxplot for train and testset)
set.seed(2004)
Asiasplit <- ESGdata2019[Region=="Asia"]
train.Asia <- sample.split(Y = Asiasplit$ESGScore, SplitRatio = 0.7)
Asiatrain <- subset(Asiasplit, train.Asia == T)
Asiatest <- subset(Asiasplit, train.Asia==F)


summary(Asiatrain)
mean(Asiatrain$ESGScore)
boxplot(Asiatrain$ESGScore, main='Asia Trainset ESGScore Boxplot', staplewex=1,labels=quantile(Asiatrain$ESGScore)) +
  text(y = boxplot.stats(Asiatrain$ESGScore)$stats, labels = boxplot.stats(Asiatrain$ESGScore)$stats, x = 1.25, pos=4) #Asia TrainSet Boxplot
#Mean value of 53.61
#Using this mean value of 53.61 to test the Asia countries ESGScore in testset

summary(Asiatest)
mean(Asiatest$ESGScore)
boxplot(Asiatest$ESGScore, main='Asia Testset ESGScore Boxplot', staplewex=1,labels=quantile(Asiatest$ESGScore)) +
  text(y = boxplot.stats(Asiatest$ESGScore)$stats, labels = boxplot.stats(Asiatest$ESGScore)$stats, x = 1.25, pos=4) #Asia Testset Boxplot

## Above rmse is not working because cannot compare
predict.Asia.test <- rep((mean(Asiatrain$ESGScore)),12)

Asia.rmse <- rmse(Asiatest$ESGScore,predict.Asia.test)
Asia.rmse
#rmse score of 14.19281

Asia.mape <- mape(Asiatest$ESGScore,predict.Asia.test) #MAPE of 0.2935425


#Africa (Boxplot for train and testset)
set.seed(2004)
Africasplit <- ESGdata2019[Region=="Africa"]
train.Africa <- sample.split(Y = Africasplit$ESGScore, SplitRatio = 0.7)
Africatrain <- subset(Africasplit, train.Africa == T)
Africatest <- subset(Africasplit, train.Africa==F)

summary(Africatrain)
mean(Africatrain$ESGScore)
boxplot(Africatrain$ESGScore, main='Africa Trainset ESGScore Boxplot', staplewex=1,labels=quantile(Africatrain$ESGScore)) +
  text(y = boxplot.stats(Africatrain$ESGScore)$stats, labels = boxplot.stats(Africatrain$ESGScore)$stats, x = 1.25, pos=4) #Africa TrainSet Boxplot
#Mean value of 66.39
#Using this mean value of 66.39 to test the Africa countries ESGScore in testset
summary(Africatest)
mean(Africatest$ESGScore)
boxplot(Africatest$ESGScore, main='Africa Testset ESGScore Boxplot', staplewex=1,labels=quantile(Africatest$ESGScore)) +
  text(y = boxplot.stats(Africatest$ESGScore)$stats, labels = boxplot.stats(Africatest$ESGScore)$stats, x = 1.25, pos=4) #Africa Testset Boxplot

## Above rmse is not working because cannot compare
predict.Africa.test <- rep((mean(Africatrain$ESGScore)),15)

Africa.rmse <- rmse(Africatest$ESGScore,predict.Africa.test)
Africa.rmse
#rmse score of 7.666106

Africa.mape <- mape(Africatest$ESGScore,predict.Africa.test) #MAPE of 0.09333136

#Europe (Boxplot for train and testset)
set.seed(2004)
Europesplit <- ESGdata2019[Region=="Europe"]
train.Europe <- sample.split(Y = Europesplit$ESGScore, SplitRatio = 0.7)
Europetrain <- subset(Europesplit, train.Europe == T)
Europetest <- subset(Europesplit, train.Europe==F)


summary(Europe)
mean(Europetrain$ESGScore)
boxplot(Europetrain$ESGScore, main='Europe Trainset ESGScore Boxplot', staplewex=1,labels=quantile(Europetrain$ESGScore)) +
  text(y = boxplot.stats(Europetrain$ESGScore)$stats, labels = boxplot.stats(Europetrain$ESGScore)$stats, x = 1.25, pos=4) #Europe TrainSet Boxplot
#Mean value of 23.66
#Using this mean value of 23.66 to test the Europe countries ESGScore in testset
summary(Europetest)
mean(Europetest$ESGScore)
boxplot(Europetest$ESGScore, main='Europe Testset ESGScore Boxplot', staplewex=1,labels=quantile(Europetest$ESGScore)) +
  text(y = boxplot.stats(Europetest$ESGScore)$stats, labels = boxplot.stats(Europetest$ESGScore)$stats, x = 1.25, pos=4) #Europe Testset Boxplot

## Above rmse is not working because cannot compare
predict.Europe.test <- rep((mean(Europetrain$ESGScore)),13)

Europe.rmse <- rmse(Europetest$ESGScore,predict.Europe.test)
Europe.rmse
#rmse score of 12.80502

Europe.mape <- mape(Europetest$ESGScore,predict.Europe.test) #MAPE of 0.4215194

#North America (Boxplot for train and testset)
set.seed(2004)
North_Americasplit <- ESGdata2019[Region=="North America"]
train.North_America <- sample.split(Y = North_Americasplit$ESGScore, SplitRatio = 0.7)
North_Americatrain <- subset(North_Americasplit, train.North_America == T)
North_Americatest <- subset(North_Americasplit, train.North_America==F)


summary(North_Americatrain)
mean(North_Americatrain$ESGScore)
boxplot(North_Americatrain$ESGScore, main='North America Trainset ESGScore Boxplot', staplewex=1,labels=quantile(North_Americatrain$ESGScore)) +
  text(y = boxplot.stats(North_Americatrain$ESGScore)$stats, labels = boxplot.stats(North_Americatrain$ESGScore)$stats, x = 1.25, pos=4) #North America TrainSet Boxplot
#Mean value of 42.14
#Using this mean value of 42.14 to test the North America countries ESGScore in testset

summary(North_Americatest)
mean(North_Americatest$ESGScore)
boxplot(North_Americatest$ESGScore, main='North America Testset ESGScore Boxplot', staplewex=1,labels=quantile(North_Americatest$ESGScore)) +
  text(y = boxplot.stats(North_Americatest$ESGScore)$stats, labels = boxplot.stats(North_Americatest$ESGScore)$stats, x = 1.25, pos=4) #North America Testset Boxplot

## Above rmse is not working because cannot compare
predict.North_America.test <- rep((mean(North_Americatrain$ESGScore)),7)

North_America.rmse <- rmse(North_Americatest$ESGScore,predict.North_America.test)
North_America.rmse
#rmse score of 15.09285

North_America.mape <- mape(North_Americatest$ESGScore,predict.North_America.test) #MAPE of 0.1874863

#Oceania (Boxplot for train and testset

set.seed(2004)
Oceaniasplit <- ESGdata2019[Region=="Oceania"]
train.Oceania <- sample.split(Y = Oceaniasplit$ESGScore, SplitRatio = 0.7)
Oceaniatrain <- subset(Oceaniasplit, train.Oceania == T)
Oceaniatest <- subset(Oceaniasplit, train.Oceania==F)

summary(Oceaniatrain)
mean(Oceaniatrain$ESGScore)
boxplot(Oceaniatrain$ESGScore, main='Oceania Trainset ESGScore Boxplot', staplewex=1,labels=quantile(Oceaniatrain$ESGScore)) +
  text(y = boxplot.stats(Oceaniatrain$ESGScore)$stats, labels = boxplot.stats(Oceaniatrain$ESGScore)$stats, x = 1.25, pos=4) #Oceania TrainSet Boxplot
#Mean value of 50.64
#Using this mean value of 50.64 to test the Oceania countries ESGScore in testset

summary(Oceaniatest)
mean(Oceaniatest$ESGScore)
boxplot(Oceaniatest$ESGScore, main='Oceania Testset ESGScore Boxplot', staplewex=1,labels=quantile(Oceaniatest$ESGScore)) +
  text(y = boxplot.stats(Oceaniatest$ESGScore)$stats, labels = boxplot.stats(Oceaniatest$ESGScore)$stats, x = 1.25, pos=4) #Oceania Testset Boxplot


## Above rmse is not working because cannot compare
predict.Oceania.test <- rep((mean(Oceaniatrain$ESGScore)),3)

Oceania.rmse<- rmse(Oceaniatest$ESGScore,predict.Oceania.test)
Oceania.rmse
#rmse score of 23.51153

Oceania.mape <- mape(Oceaniatest$ESGScore,predict.Oceania.test) #MAPE of 0.8670959

#South America (Boxplot for train and testset)
set.seed(2004)
South_Americasplit <- ESGdata2019[Region=="South America"]
train.South_America <- sample.split(Y = South_Americasplit$ESGScore, SplitRatio = 0.7)
South_Americatrain <- subset(South_Americasplit, train.South_America == T)
South_Americatest <- subset(South_Americasplit, train.South_America==F)


summary(South_Americatrain)
mean(South_Americatrain$ESGScore)
boxplot(South_Americatrain$ESGScore, main='South_America Trainset ESGScore Boxplot', staplewex=1,labels=quantile(South_Americatrain$ESGScore)) +
  text(y = boxplot.stats(South_Americatrain$ESGScore)$stats, labels = boxplot.stats(South_Americatrain$ESGScore)$stats, x = 1.25, pos=4) #South America TrainSet Boxplot
#Mean value of 44.60
#Using this mean value of  44.60 to test the South America countries ESGScore in testset
summary(South_Americatest)
mean(South_Americatest$ESGScore)
boxplot(South_Americatest$ESGScore, main='South_America Testset ESGScore Boxplot', staplewex=1,labels=quantile(South_Americatest$ESGScore)) +
  text(y = boxplot.stats(South_Americatest$ESGScore)$stats, labels = boxplot.stats(South_Americatest$ESGScore)$stats, x = 1.25, pos=4) #South America Testset Boxplot


## Above rmse is not working because cannot compare
predict.South_America.test <- rep((mean(South_Americatrain$ESGScore)),4)

South_America.rmse <- rmse(South_Americatest$ESGScore,predict.South_America.test)
South_America.rmse
#rmse score of 8.552358

South_America.mape <- mape(South_Americatest$ESGScore,predict.South_America.test) #MAPE of 0.1760801

#Mean value of the rmse across all 6 region rmse
allcountryrmse <- c(Asia.rmse,Africa.rmse,North_America.rmse,Oceania.rmse,Europe.rmse,South_America.rmse)
NMLrmsemean<-mean(allcountryrmse)
#average rmse value of 14.19281

#Mean value of the MAPE score across all 6 region
allcountrymape <- c(Asia.mape, Africa.mape, North_America.mape, Oceania.mape, Europe.mape, South_America.mape)
NMLmapemean <- mean(allcountrymape) #average MAPE across all 6 region is 0.3398426

############################################### ML by GGPlot ########################################

#ML Voice
ggplot(data=trainset,aes(x= Voice,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Voice vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Voice sorted by 2019 Year


#ML Political

ggplot(data=trainset,aes(x= Political,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Political vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Political sorted by 2019 Year


#ML GovtEff
ggplot(data=trainset,aes(x= GovtEff,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs GovtEff vs 2019 Year Smoothing") #ggplot of overall ESGScore vs GovtEff sorted by 2019 Year


#ML RegQual
ggplot(data=trainset,aes(x= RegQual,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs RegQual vs 2019 Year Smoothing") #ggplot of overall ESGScore vs RegQual sorted by 2019 Year


#ML Law
ggplot(data=trainset,aes(x= Law,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Law vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Law sorted by 2019 Year


#ML Corruption
ggplot(data=trainset,aes(x= Corruption,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Corruption vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Corruption sorted by 2019 Year


#ML LifeExp
ggplot(data=trainset,aes(x= LifeExp,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs LifeExp vs 2019 Year Smoothing") #ggplot of overall ESGScore vs LifeExp sorted by 2019 Year


#ML Carbon
ggplot(data=trainset,aes(x= Carbon,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Carbon vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Carbon sorted by 2019 Year


#ML Nitrous
ggplot(data=trainset,aes(x= Nitrous,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs Nitrous vs 2019 Year Smoothing") #ggplot of overall ESGScore vs Nitrous sorted by 2019 Year


#ML FertRate
ggplot(data=trainset,aes(x= FertRate,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs FertRate vs 2019 Year Smoothing") #ggplot of overall ESGScore vs FertRate sorted by 2019 Year


#ML GDP
ggplot(data=trainset,aes(x= GDP,y= ESGScore)) + geom_jitter() + facet_wrap(Year~.)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "ESGScore vs GDP vs 2019 Year Smoothing") #ggplot of overall ESGScore vs GDP sorted by 2019 Year

### Linear regression for ESGScore for all variables for Year = 2019

##Voice
voice.esgscore1=lm(ESGScore ~ Voice , data=trainset)
voice.esgscore1 ## ESGScore in 2019, forecast Voice in 2019 value using trainset
summary(voice.esgscore1)

voice.gdp1=lm(GDP ~ Voice , data=trainset)
voice.gdp1 ## ESGScore in 2019, forecast Voice in 2019 value using trainset
summary(voice.gdp1)

##Political
Political.esgscore1=lm(ESGScore ~ Political , data=trainset)
Political.esgscore1 ## ESGScore in 2019, forecast Political in 2019 value using trainset
summary(Political.esgscore1)

Political.gdp1=lm(GDP ~ Political , data=trainset)
Political.gdp1 ## ESGScore in 2019, forecast Political in 2019 value using trainset
summary(Political.gdp1)

##GovtEff
GovtEff.esgscore1=lm(ESGScore ~ GovtEff , data=trainset)
GovtEff.esgscore1 ## ESGScore in 2019, forecast GovtEff in 2019 value using trainset
summary(GovtEff.esgscore1)

GovtEff.gdp1=lm(GDP ~ GovtEff , data=trainset)
GovtEff.gdp1 ## ESGScore in 2019, forecast GovtEff in 2019 value using trainset
summary(GovtEff.gdp1)

##RegQual
RegQual.esgscore1=lm(ESGScore ~ RegQual , data=trainset)
RegQual.esgscore1 ## ESGScore in 2019, forecast RegQual in 2019 value using trainset
summary(RegQual.esgscore1)

RegQual.gdp1=lm(GDP ~ RegQual , data=trainset)
RegQual.gdp1 ## ESGScore in 2019, forecast RegQual in 2019 value using trainset
summary(RegQual.gdp1)

##Law
Law.esgscore1=lm(ESGScore ~ Law , data=trainset)
Law.esgscore1 ## ESGScore in 2019, forecast Law in 2019 value using trainset
summary(Law.esgscore1)

Law.gdp1=lm(GDP ~ Law , data=trainset)
Law.gdp1 ## ESGScore in 2019, forecast Law in 2019 value using trainset
summary(Law.gdp1)

##Corruption
Corruption.esgscore1=lm(ESGScore ~ Corruption , data=trainset)
Corruption.esgscore1 ## ESGScore in 2019, forecast Corruption in 2019 value using trainset
summary(Corruption.esgscore1)

Corruption.gdp1=lm(GDP ~ Corruption , data=trainset)
Corruption.gdp1 ## ESGScore in 2019, forecast Corruption in 2019 value using trainset
summary(Corruption.gdp1)

##LifeExp
LifeExp.esgscore1=lm(ESGScore ~ LifeExp , data=trainset)
LifeExp.esgscore1 ## ESGScore in 2019, forecast LifeExp in 2019 value using trainset
summary(LifeExp.esgscore1)

LifeExp.gdp1=lm(GDP ~ LifeExp , data=trainset)
LifeExp.gdp1 ## ESGScore in 2019, forecast LifeExp in 2019 value using trainset
summary(LifeExp.gdp1)

##Carbon
Carbon.esgscore1=lm(ESGScore ~ Carbon , data=trainset)
Carbon.esgscore1 ## ESGScore in 2019, forecast Carbon in 2019 value using trainset
summary(Carbon.esgscore1)

Carbon.gdp1=lm(GDP ~ Carbon , data=trainset)
Carbon.gdp1 ## ESGScore in 2019, forecast Carbon in 2019 value using trainset
summary(Carbon.gdp1)

##Nitrous
Nitrous.esgscore1=lm(ESGScore ~ Nitrous , data=trainset)
Nitrous.esgscore1 ## ESGScore in 2019, forecast Nitrous in 2019 value using trainset
summary(Nitrous.esgscore1)

Nitrous.gdp1=lm(GDP ~ Nitrous , data=trainset)
Nitrous.gdp1 ## ESGScore in 2019, forecast Nitrous in 2019 value using trainset
summary(Nitrous.gdp1)

##FertRate
FertRate.esgscore1=lm(ESGScore ~ FertRate , data=trainset)
FertRate.esgscore1 ## ESGScore in 2019, forecast FertRate in 2019 value using trainset
summary(FertRate.esgscore1)

FertRate.gdp1=lm(GDP ~ FertRate , data=trainset)
FertRate.gdp1 ## ESGScore in 2019, forecast FertRate in 2019 value using trainset
summary(FertRate.gdp1)

##GDP
GDP.esgscore1=lm(ESGScore ~ GDP , data=trainset)
GDP.esgscore1 ## ESGScore in 2019, forecast GDP in 2019 value using trainset
summary(GDP.esgscore1)

################################# Linear regression for ESGscore and GDP with all variables for Year = 2019 ##################
set.seed(2004)

#model on trainset
m1 <- lm(ESGScore ~Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ Carbon+ Nitrous+ FertRate, data=trainset)
summary(m1) #all is significant except Law and Corruption
m1

#check RMSE
train.RMSE <- sqrt(mean(residuals(m1)^2))  # RMSE on trainset based on m1 model.
summary(abs(residuals(m1)))
#predict
testset$predict <- predict(m1,newdata=testset)
testset$error <- testset$ESGScore - testset$predict

# Testset Errors
test.RMSE <- sqrt(mean(testset$error^2))
summary(abs(testset$error))

linearrmse <- test.RMSE #6.901092
linearrmse
library(MLmetrics)
linearmape <- MAPE(testset$ESGScore, testset$predict) #MAPE 0.1381269
linearmape

############################################### ML by CART, Pruning using Mean ##############################################
##### Finding by mean value in ESGdata2019 ######
set.seed(2004)
summary(trainset)
esgcart1 <- rpart(ESGScore ~Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ Carbon+ Nitrous+ FertRate ,
                  data=trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
#plot maximal tree and results
rpart.plot(esgcart1, nn = T, main = "Maximal Tree in ESGCart1 with mean.csv")
#pruning sequence and 10-fold CV error
plotcp(esgcart1, main = 'Subtrees in ESGCart1')
#maximal tree and prune triggers
printcp(esgcart1)

print(esgcart1) #printing all nodes in esgcart1
## 0 error cases with both child/terminal trees being pruned

CVerror.cap <- esgcart1$cptable[which.min(esgcart1$cptable[,"xerror"]), "xerror"] +
  esgcart1$cptable[which.min(esgcart1$cptable[,"xerror"]), "xstd"] #Obtaining Cv error cap of ESGCart1

#short cut way to find optimal tree
i <- 1; j<- 4
while (esgcart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
i

cp.opt = ifelse(i > 1, sqrt(esgcart1$cptable[i,1] * esgcart1$cptable[i-1,1]), 1)
cp.opt ##Tolerance limit for node at 0.01304065

#set minimum value to prune at 0.01304065
cp1 <- cp.opt

CART2 <- prune(esgcart1, cp = cp1)
printcp(CART2, digits = 3)
plotcp(CART2) #Shows pruned tree and 10 fold CV error
print(CART2) #Shows error and pruning sequence
rpart.plot(CART2, nn=T, main = "Optimal Tree in ESGCart with Mean NA values") #Shows pruned decision trees

# To summarize nodes and decision rules
rpart.rules(CART2, nn = T, extra = 4, cover = T)
CART2$variable.importance


###Testdata fitting onto trainset
testdata= data.frame(testset)
cart.predict = predict(CART2, testdata, type = 'matrix')
mean.results = data.frame(testdata, cart.predict)
summary(mean.results)

# Make predictions on the test data
predicted.classes <- CART2 %>% predict(testdata)
# Compute model accuracy rate on test data
mean(predicted.classes == testdata$ESGScore)
predicted.classes

cart1rmse <- rmse(testset$ESGScore, mean.results$cart.predict) #RMSE 8.417404
cart1rmse

cart1mape <- MAPE(testset$ESGScore, mean.results$cart.predict) #MAPE 0.1550088
cart1mape


############################################### ML by CART, Pruning and Surrogates with Carbon and nitrous ########################################

##### Finding using surrogates in ESGdata (data with blanks) ######
set.seed(2004)

#Creating train and test set using data with NA values
ESGdata.cart <- y2[Year=="2019"]
summary(ESGdata.cart)
train2 <- sample.split(Y = ESGdata.cart$ESGScore, SplitRatio = 0.7)
trainset2 <- subset(ESGdata.cart, train2 == T)
trainset2
testset2 <- subset(ESGdata.cart, train2 == F)
testset2


# Continuous Y: Set method = 'anova'
esgcart2 <- rpart(ESGScore ~Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ Carbon+ Nitrous+ FertRate , data=trainset2, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

rpart.plot(esgcart2, nn = T, main = "Maximal Tree in ESGCart1 with Surrogates")
printcp(esgcart2)
#plot maximal tree and results

plotcp(esgcart2)

print(esgcart2)

# Compute min CVerror + 1SE in maximal tree esgcart2.
CVerror.cap2 <- esgcart2$cptable[which.min(esgcart2$cptable[,"xerror"]), "xerror"] + esgcart2$cptable[which.min(esgcart2$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree esgcart2.
h <- 1; j<- 4
while (esgcart2$cptable[h,j] > CVerror.cap2) {
  h <- h + 1
}
h

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt2 = ifelse(h > 1, sqrt(esgcart2$cptable[h,1] * esgcart2$cptable[h-1,1]), 1)
cp.opt2
# ------------------------------------------------------------------------------

# Prune the max tree using a particular CP value
esgcart3 <- prune(esgcart2, cp = cp.opt2)
printcp(esgcart3, digits = 3)

print(esgcart3)

rpart.plot(esgcart3, nn = T, main = "Optimal Tree in ESGCart with Surrogates")
## The number inside each node represent the mean value of Y.


# To summarize nodes and decision rules
rpart.rules(esgcart3, nn = T, extra = 4, cover = T)
esgcart3$variable.importance
## RegQual has the highest importance, LifeExp is second impt.


# Surrogates shown in summary() ----------------------------
summary(esgcart3)


###Testdata fitting onto trainset
testdata2= data.frame(testset2)
cart.predict2 = predict(esgcart3, testdata2, type = 'matrix')
mean.results2 = data.frame(testdata2, cart.predict2)
summary(mean.results2)

# Make predictions on the test data
predicted.classes2 <- esgcart3 %>% predict(testdata2)
# Compute model accuracy rate on test data
mean(predicted.classes2 == testdata2$ESGScore)
predicted.classes2

cart2rmse <- rmse(testset2$ESGScore, mean.results2$cart.predict2) #RMSE 9.016454
cart2rmse

cart2mape<- MAPE(testset2$ESGScore, mean.results2$cart.predict2) #MAPE 0.1640247
cart2mape

############################################### ML by CART, Pruning and Surrogates Without Carbon and nitrous (Column fully NA) ########################################

##### Finding using surrogates in ESGdata (Removal of column "Nitrous" and "Carbon" because both columns do not have any values

# Continuous Y: Set method = 'anova'
##### Finding using surrogates in ESGdata (Removal of column "Nitrous" and "Carbon" because both columns do not have any values
esgcart4 <- rpart(ESGScore ~Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ FertRate , data=trainset2, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
#plot maximal tree and results
printcp(esgcart4)

plotcp(esgcart4)

print(esgcart4)

# Compute min CVerror + 1SE in maximal tree esgcart4.
CVerror.cap3 <- esgcart4$cptable[which.min(esgcart4$cptable[,"xerror"]), "xerror"] + esgcart4$cptable[which.min(esgcart4$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree esgcart4.
h <- 1; j<- 4
while (esgcart4$cptable[h,j] > CVerror.cap3) {
  h <- h + 1
}
h

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt3 = ifelse(h > 1, sqrt(esgcart4$cptable[h,1] * esgcart4$cptable[h-1,1]), 1)
cp.opt3
# ------------------------------------------------------------------------------

# Prune the max tree using a particular CP value
esgcart5 <- prune(esgcart4, cp = cp.opt3)
printcp(esgcart5, digits = 3)

print(esgcart5)

rpart.plot(esgcart5, nn = T, main = "Optimal Tree in ESGCart with Surrogates")
## The number inside each node represent the mean value of Y.


# To summarize nodes and decision rules
rpart.rules(esgcart5, nn = T, extra = 4, cover = T)
esgcart5$variable.importance
## RegQual has the highest importance, LifeExp is second impt.


# Surrogates shown in summary() ----------------------------
summary(esgcart5)


###Testdata fitting onto trainset
testdata3= data.frame(testset2)
cart.predict3 = predict(esgcart5, testdata3, type = 'matrix')
mean.results3 = data.frame(testdata3, cart.predict3)
summary(mean.results3)

# Make predictions on the test data
predicted.classes3 <- esgcart5 %>% predict(testdata3)
# Compute model accuracy rate on test data
mean(predicted.classes3 == testdata3$ESGScore)
predicted.classes3

rmse(testdata3$ESGScore, mean.results3$cart.predict3) #RMSE 9.016454

MAPE(testdata3$ESGScore, mean.results3$cart.predict3) #MAPE 0.1640247     

############################################### ML by SVM ########################################
library("caret")

str(train)
set.seed(2004)
intrain <- createDataPartition(y = ESGdata2019$ESGScore, p= 0.7, list = FALSE)
training <- ESGdata2019[intrain,]
testing <- ESGdata2019[-intrain,]

summary(testing)
dim(training) #check dimensions of training dataset
dim(testing) #check dimensions of testing dataset

anyNA(trainset2) #check for NA values

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) #re-sampling and cross-validation of training dataset

#creating train dataset without scenario based analysis
svm_Linear <- train(ESGScore ~ Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ FertRate+ Carbon+Nitrous, data = training,
                    method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
print(svm_Linear)
varImp(svm_Linear, scale = FALSE) #Shows variable importance 


# RMSE    Rsquared  MAE    
# 7.5083  0.866227  6.20731


test_pred <- predict(svm_Linear, newdata = testing)
test_pred
summary(test_pred)
plot(test_pred, main='SVM without Error analysis', ylab='Predicted ESGScores', xlab = 'Data index')

testset3.error <- test_pred - testing$ESGScore
testset3.error

# Testset Errors
RMSE.svm.test <- sqrt(mean(testset3.error^2))
summary(abs(testset3.error))
rmsesvm1 <- RMSE.svm.test # 8.449825
rmsesvm1

# train the model without scenario based analysis
model <- train(ESGScore ~ Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ FertRate+ Carbon+Nitrous, data=training,
               trControl=trctrl,
               method="svmLinear")

# summarize results
print(model)

summary(table(test_pred, testing$ESGScore))
mapesvm1 <- MAPE(testing$ESGScore, test_pred) #MAPE for non-scenario-based analysis is 0.1429
mapesvm1


#scenario based analysis of SVM to check RMSE, C is the cost value for SVM (Higher cost means data should be classifed better)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5)) #setting limits for misclassification error

svm_Linear_Grid <- train(ESGScore ~ Law+ Voice+ Political+ GovtEff+ RegQual+ Corruption+ LifeExp+ FertRate, data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
plot(svm_Linear_Grid)
varImp(svm_Linear_Grid, scale = FALSE) #Shows variable importance 

svm_Linear_Grid #lower rmse than without scenario based analysis
# > svm_Linear_Grid
# Support Vector Machines with Linear Kernel 

# 125 samples
# 8 predictor

# Pre-processing: centered (8), scaled (8) 
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 113, 113, 113, 111, 113, 113, ... 
# Resampling results across tuning parameters:

#  C     RMSE      Rsquared   MAE     
# 0.00       NaN        NaN       NaN
# 0.01  7.292290  0.8713210  6.103159
# 0.05  7.047238  0.8762934  5.927052
# 0.10  7.144069  0.8747576  5.996394
# 0.25  7.270368  0.8715758  6.115490
# 0.50  7.341240  0.8697205  6.152134
# 0.75  7.372883  0.8684345  6.164531
# 1.00  7.386854  0.8683034  6.179836
# 1.25  7.394393  0.8680842  6.184816
# 1.50  7.401490  0.8678389  6.189288
# 1.75  7.404527  0.8677136  6.189626
# 2.00  7.408006  0.8676310  6.193277
# 5.00  7.407587  0.8674021  6.192889

# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was C = 0.05.
# plot(svm_Linear_Grid)


#testing testset on trainset model
test_pred2 <- predict(svm_Linear_Grid, newdata = testing)
test_pred2
summary(test_pred2)
plot(test_pred2)
print(svm_Linear_Grid)

# Testset Errors for scenario based analysis
testset4.error <- test_pred2 - testing$ESGScore
testset4.error


RMSE.svm.test2 <- sqrt(mean(testset4.error^2))
summary(abs(testset4.error))
rmsevm2<-RMSE.svm.test2 # 8.231792
mapesvm2<-MAPE(testing$ESGScore, test_pred2) #MAPE for scenario-based analysis is 0.1313

test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
RMSE.svm.test2 <- sqrt(mean(testset4.error^2))
RMSE.svm.test2 #8.231792
#RMSE by scenario/misclassification cost has lower RMSE

################################Bar chart for comparison analysis for RMSE and MAPE for all 6 Model #########################

#RMSE
RMSE<-c(NMLrmsemean,linearrmse,cart1rmse,cart2rmse,rmsesvm1,rmsevm2)
Methods<-c("NML","Linear Regression","CART(Mean)","CART(Surrogate)","SVM(no error based scenario","SVM(error based scenario")
RMSE.table <- data.frame(Methods,RMSE)

ggplot(data=RMSE.table,aes(x=Methods,y=RMSE,fill=Methods,))+geom_bar(stat = "identity")+ggtitle("RMSE for each method")+geom_label(size=5,aes(label=paste(round(RMSE,3),",Rank",rank(RMSE))))+theme(legend.position="none")

#MAPE
MAPE<-c(NMLmapemean, linearmape, cart1mape, cart2mape, mapesvm1,mapesvm2)
Methods<-c("NML","Linear Regression","CART(Mean)","CART(Surrogate)","SVM(no error based scenario","SVM(error based scenario")
MAPE.table <- data.frame(Methods,MAPE)

ggplot(data=MAPE.table,aes(x=Methods,y=MAPE,fill=Methods,))+geom_bar(stat = "identity")+ggtitle("RMSE for each method")+geom_label(size=5,aes(label=paste(round(MAPE,3),",Rank",rank(MAPE))))+theme(legend.position="none")

#GE Matrix
varname=c('NML', 'LinearReg', 'CARTMean', 'CARTSurrogates', 'SVMnoError', 'SVMErrorScenario')
ge.matrix<-data.frame(varname,RMSE,MAPE)
ggplot(data=ge.matrix,aes(x=RMSE,y=MAPE,fill=varname,color=varname))+geom_point(size=5)+
  ggtitle("GE-McKinsey Matrix")+
  theme(legend.position="top")+scale_y_reverse()

     