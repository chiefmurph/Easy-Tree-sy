library(rpart)
library(rpart.plot)

origdata <- read.csv(file="RPMcase2.csv",header=TRUE)

# create depvar = loss ratio
origdata$depvar <- origdata$incloss / origdata$prem

# delete rows where premium is negative or zero amount
origdata <- origdata[origdata$prem > 0,]

tree1 <- rpart(depvar ~ 
                 modelyear +
                 countyname +
                 deductible,
               data=origdata,
               method="anova",
               control=rpart.control(minsplit=5000, 
                                     maxdepth = 5,
                                     cp=0.00001),
               weights=origdata$prem)
print(tree1)
#summary(tree1)
printcp(tree1)
rpart.plot(tree1, type=1, extra=1)  

