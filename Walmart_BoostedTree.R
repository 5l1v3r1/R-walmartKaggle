library(lubridate)
library(plyr) 
library(tree)
library(sqldf)

#Import data
import = read.csv("/Users/kylehundman/Desktop/Walmart/train.csv", header=T)
sum(is.na(import))

#Create month variable
import$Date = as.Date(import$Date, "%m/%d/%y") #convert to date
import$month = month(import$Date) #grab month (using lubridate)
import$week = week(import$Date) #grab week (using lubridate)
import$year = year(import$Date) #grab week (using lubridate)

#In Excel File
#=IF(E11=TRUE,IF(MONTH(C11)=2,"SUPER BOWL",IF(MONTH(C11)=11,"THANKSGIVING",IF(MONTH(C11)=12,"CHRISTMAS",IF(MONTH(C11)=9,"LABOR DAY","NONE")))))

#Group Depts by average sales (35 bins)
deptAvgs = sqldf('SELECT avg(Weekly_Sales) as avg, Dept FROM import GROUP BY Dept')  #Look at average sales
deptAvgs = sqldf('SELECT * FROM deptAvgs ORDER BY avg')  
hist(deptAvgs$avg, breaks = 35)
deptAvgs$deptBin = cut(deptAvgs$avg, breaks = 35, labels = FALSE) #group depts into 35 "avg sales" bins
import = join(import, deptAvgs, by = "Dept") #vlookup equivalent (for dept bins) using "plyr" package

#Group weeks by average sales (35 bins)
weekAvgs = sqldf('SELECT avg(Weekly_Sales) as avg, week FROM import GROUP BY week')  #Look at average sales
weekAvgs = sqldf('SELECT * FROM weekAvgs ORDER BY avg')  
hist(weekAvgs$avg, breaks = 35)
weekAvgs$weekBin = cut(weekAvgs$avg, breaks = 35, labels = FALSE) #group weeks into 35 "avg sales" bins
import = join(import, weekAvgs, by = "week") #vlookup equivalent (for week bins) using "plyr" package

# Drop unusable cols
train = with(import, data.frame(sales = Weekly_Sales , month = month, year = year, holiday = Holiday, 
                                storeType = StoreType, storeSize = StoreSize, week = week,
                                deptAvgSalesBin = deptBin, temp = Temperature, fuelPrice = Fuel_Price,
                                cpi = CPI, unemployment = Unemployment, weekAvgSalesBin = weekBin))

train$month = factor(train$month) 
train$deptAvgSalesBin = factor(train$deptAvgSalesBin)
train$weekAvgSalesBin = factor(train$weekAvgSalesBin)
train$holiday = factor(train$holiday)
train$storeType = factor(train$storeType)

#missing values
sum(is.na(train)) #128
train_complete = train[complete.cases(train),]
head(train)

#Boosted Tree
library(gbm)
gbm1 = gbm(sales ~., data=train, distribution="gaussian", var.monotone=NULL, n.trees=1000, shrinkage=0.001, 
           interaction.depth=3, bag.fraction = .5, train.fraction = 1, n.minobsinnode = 10, cv.folds = 10,
           keep.data=TRUE, verbose=FALSE)
best.iter <- gbm.perf(gbm1,method="cv");best.iter
sqrt(gbm1$cv.error[best.iter])
##
summary(gbm1,n.trees=best.iter)  # based on the optimal number of trees
##
par(mfrow=c(1,3))
for (i in c(4,3,1)) plot(gbm1, i.var = i, n.trees = best.iter)
##
print(pretty.gbm.tree(gbm1,1))  #show the first tree
##
print(pretty.gbm.tree(gbm1,best.iter))  #show the last tree

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
test = with(test, data.frame(month = month, year = year, holiday = Holiday, 
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
yhatTest = predict(gbm1, test) 
sum(is.na(yhatTest))
write.table(yhatTest, col.names = "Weekly_Sales", file = "/Users/kylehundman/Desktop/Walmart/Submission_boostedTree.csv")
#
