library(lubridate)
library(plyr)
library(tree)
library(sqldf)

#Import data
import = read.csv("/Users/kylehundman/Desktop/Walmart/train.csv", header=T)

#Create month variable
import$Date = as.Date(import$Date, "%m/%d/%y") #convert to date
import$month = month(import$Date) #grab month (using lubridate)
import$week = week(import$Date) #grab week (using lubridate)
import$year = year(import$Date) #grab week (using lubridate)

#Group Depts by average sales (35 bins)
deptAvgs = sqldf('SELECT avg(Weekly_Sales) as avg, Dept FROM import GROUP BY Dept')  #Look at average sales
deptAvgs = sqldf('SELECT * FROM deptAvgs ORDER BY avg')  
hist(deptAvgs$avg, breaks = 35)
deptAvgs$deptBin = cut(deptAvgs$avg, breaks = 35, labels = FALSE) #group depts into 35 "avg sales" bins
import = join(import, deptAvgs, by = "Dept") #vlookup equivalent (for dept bins) using "plyr" package
?join

#Group weeks by average sales (35 bins)
weekAvgs = sqldf('SELECT avg(Weekly_Sales) as avg, week FROM import GROUP BY week')  #Look at average sales
weekAvgs = sqldf('SELECT * FROM weekAvgs ORDER BY avg')  
hist(weekAvgs$avg, breaks = 35)
weekAvgs$weekBin = cut(weekAvgs$avg, breaks = 35, labels = FALSE) #group weeks into 35 "avg sales" bins
import = join(import, weekAvgs, by = "week") #vlookup equivalent (for week bins) using "plyr" package

# Drop unusable cols
train = with(import, data.frame(sales = Weekly_Sales , month = month, year = year, holiday = IsHoliday, 
                                      storeType = StoreType, storeSize = StoreSize, week = week,
                                      deptAvgSalesBin = deptBin, temp = Temperature, fuelPrice = Fuel_Price,
                                      cpi = CPI, unemployment = Unemployment, weekAvgSalesBin = weekBin))

#missing values
sum(is.na(train)) #128
train_complete = train[complete.cases(train),]

#Fit tree
control = tree.control(nobs=nrow(train), mincut = 1, minsize = 2, mindev = 0.0001)
#default is mindev = 0.01, which only gives a 10-node tree
train.tr <- tree(sales ~ .,train,control=control)
train.tr
summary(train.tr)
plot(train.tr,type="u"); text(train.tr,digits=2)  #type="p" plots proportional branch lengths
######now prune tree and plot deviance vs. complexity parameter
train.tr1<-prune.tree(train.tr)
plot(train.tr1)
######now plot CV deviance vs complexity parameter
plot(cv.tree(train.tr, , prune.tree))
######now find the final tree with the best value of complexity parameter
train.tr1<-prune.tree(train.tr, best = 10) #can replace replace argument “k=0.4” by “best=11”
train.tr1
plot(train.tr1,type="u");text(train.tr1,digits=3)

#Prediction error
yhat = predict(train.tr1)
plot(yhat, train_complete$sales)
r = 1-(var(train_complete$sales - yhat)/var(train_complete$sales))
r

#PREDICT TEST SET
#-----------------------------
#Import data
test = read.csv("/Users/kylehundman/Desktop/Walmart/test.csv", header=T)

#Create month variable
test$Date = as.Date(test$Date, "%m/%d/%y") #convert to date
test$month = month(test$Date) #grab month (using lubridate)
test$week = week(test$Date) #grab week (using lubridate)
test$year = year(test$Date) #grab week (using lubridate)

#Join weeks and depts with their assigned bins
test = join(test, deptAvgs, by = "Dept") #vlookup equivalent (for dept bins) using "plyr" package
test = join(test, weekAvgs, by = "week") #vlookup equivalent (for week bins) using "plyr" package

# Drop unusable cols
test = with(test, data.frame(month = month, year = year, holiday = IsHoliday, 
                                storeType = StoreType, storeSize = StoreSize, week = week,
                                deptAvgSalesBin = deptBin, temp = Temperature, fuelPrice = Fuel_Price,
                                cpi = CPI, unemployment = Unemployment, weekAvgSalesBin = weekBin))

test$month = as.character(test$month) 
test$deptAvgSalesBin = as.character(test$deptAvgSalesBin)

#NAs in test
sum(is.na(test$cpi)) #38,162 #38,162 - Only May, June, July missing
sum(is.na(test$unemployment)) #38,162 - Only May, June, July missing
sqldf('SELECT min(month) FROM test WHERE year == 2012') #earliest month for prediction - November 2012

#Impute avg
imputeCPI = sqldf('SELECT avg(cpi) FROM test WHERE year == 2013 and month == 4 OR month == 8') #avg April and August for imputing 
imputeUnemployment = sqldf('SELECT avg(unemployment) FROM test WHERE year == 2013 and month == 4 OR month == 8') #avg April and August for imputing
test$cpi[is.na(test$cpi)]= imputeCPI
test$unemployment[is.na(test$unemployment)]= imputeUnemployment
test$unemployment = as.numeric(test$unemployment)
test$cpi = as.numeric(test$cpi)

sum(is.na(test))

#Create Submission
yhatTest = predict(train.tr, test) 
sum(is.na(yhatTest))
write.table(yhatTest, col.names = "Weekly_Sales", file = "/Users/kylehundman/Desktop/Walmart/Submission_triplehugeTree.csv")

