#Define file to be used for segmentation analysis. 
#incsv <- "H:\\Marketing\\Presentations\\External\\RPM\\RPM 2017\\Easy Treesy\\RPMcase2.csv"
# Assumed this script is stored in folder ..\code
incsv <- file.path("..", "data", "RPMcase2.csv")

# load data
infileRaw <- read.csv(incsv, header=T)
head(infileRaw)
nrow(infileRaw)
names(infileRaw)
str(infileRaw, list.len=999)

# define target variable and weight
infile <- infileRaw
infile$depvar <- infile$incloss / infile$prem

# split into training and holdout
ind <- sample(nrow(infile),nrow(infile)*.70)
train<-infile[ind,]
holdout<-infile[-ind,]

# grow big tree
library(rpart)
library(rattle)
weight <- train$prem
tree_full <- rpart(depvar~state+effyr+cancel+lapse+poltype+vehtype+deluxmulti+color+
                 V96+limit+modelyear+latepay+mileage_group+countyname+size_numeric+
                 deductible+power_numeric+policyage, data=train,weight=weight,
               method="anova",control=rpart.control(cp=0.00005, minbucket = 1000, maxdepth=3),xval=5)
summary(tree_full)
asRules(tree_full)
fancyRpartPlot(tree_full)
printcp(tree_full)

# prune it back
min_xerror <- which.min(tree_full$cptable[,"xerror"])

tree_pruned <- prune(tree_full,cp=min_xerror)
asRules(tree_pruned)
fancyRpartPlot(tree_pruned)

# lossratios by node on the holdout
score <- predict(tree_full,holdout)
validate<-cbind(holdout,score)
head(validate)
names(validate)

validate_node <- aggregate(cbind(prem,incloss) ~ score, data=validate, sum)
validate_node$lossratio <- validate_node$incloss / validate_node$prem

validate_node


