install.packages("data.table")
library(data.table)
library(rpart)
library(rpart.plot)
library(rpart.utils)

# set working directory and read original data 
setwd("C:/Users/Scott/Desktop/Grace/camico")
origdata <- read.csv(file="sample.csv",header=TRUE)

# create depvar: loss+expense ratio; new rates and normalized by year
# delete rows where premium is negative or zero amount
origdata["depvar"] <- NA
origdata$depvar <- origdata$NewNormL.E / origdata$EstNewEP
origdata <- origdata[origdata$EstNewEP > 0,]

#create new indep variables
origdata["staff2pros"] <- NA
origdata$staff2pros <- round(origdata$TOTALSTAFF / origdata$TOTALPROS, 2)

origdata["CA_indicator"] <- NA
origdata$CA_indicator <- ifelse(origdata$STATE == "CA", c("CA"), c("OTH"))

                   
#select columns and create smaller data frame "newdata"
newdata <- origdata[, c("policyid", "NUMCSE", "CLAIMEXPENSEOPTION", 
                        "SUITFORFEESEXCLUSION", 
                        "PEROCCURLIMIT", "AggLimitGroup",
                        "PERCLAIMDEDUCTIBLE", "AggDed.Group",
                        "PERCLAIMDEFENSE", "DOD", "TAX", "ATTEST",
                        "BOOKKEEPING", "CONSULTING", "SPECIALSERVICES",
                        "OTHER", "AuditNonPublic", "AuditPublic", "Auditold",
                        "Review", "Compilation", "AgreedUponRequest",
                        "TOTALPROS", "TOTALSTAFF", "staff2pros", "ProsGroup",
                        "IRC", "Uwdiscretion", "SOURCE", "BUSINESSTYPE",
                        "STATE", "CA_indicator", "FirmSizeCat",
                        "DisciplineFactorGroup", "MaturityNumb", 
                        "TerrGroup", "NewStateBaseGrp", "EstNewEP", "depvar")]
                    
tree1 <- rpart(depvar ~ 
                 NUMCSE +
                 CLAIMEXPENSEOPTION +
                 SUITFORFEESEXCLUSION +
                 PEROCCURLIMIT +
                 AggLimitGroup +
                 PERCLAIMDEDUCTIBLE + 
                 AggDed.Group +
                 PERCLAIMDEFENSE + 
                 DOD + 
                 TAX +
                 ATTEST +
                 BOOKKEEPING +
                 CONSULTING +
                 SPECIALSERVICES +
                 OTHER + 
                 AuditNonPublic +
                 AuditPublic + 
                 Auditold +
                 Review +
                 Compilation + 
                 AgreedUponRequest +
                 TOTALPROS + 
                 TOTALSTAFF + 
                 staff2pros + 
                 ProsGroup +
                 IRC +
                 Uwdiscretion + 
                 SOURCE + 
                 BUSINESSTYPE +
                 FirmSizeCat +
                 DisciplineFactorGroup + 
                 MaturityNumb +
                 TerrGroup + 
                 NewStateBaseGrp +
                 CA_indicator,
               data=newdata,
               method="anova",
               control=rpart.control(minsplit=50, minbucket=30, cp=0.001),
               weights=newdata$EstNewEP)

summary(tree1)
printcp(tree1)

# type 1 = label all nodes; extra 1 = display #obs
rpart.plot(tree1, type=1, extra=1)  

# prune by selected cp (note: smaller cp = larger tree)
# tree2 <- prune(tree1, cp=0.002)





############################################################################
# FOR UNIVARIATE ANALYSIS                                                  #
############################################################################

# summary by x=continuous-variable, into numgroup=number of groups
summary.cont <- function(x, numgroup) {
  DT<-data.table(a=x, b=newdata$depvar, c=newdata$EstNewEP)
  DT[, list(.N,wtavg=weighted.mean(b,c)),keyby=cut(x, unique(quantile(a,probs=seq(0,1,1/numgroup)),include=TRUE))]
}

summary.cont(newdata$BOOKKEEPING, 10)
summary.cont(newdata$TAX, 10)
summary.cont(newdata$Auditold,10)
summary.cont(newdata$staff2pros,10)
summary.cont(newdata$IRC, 10)

# summary by x=categorical-variable
summary.cat <- function(x) {
  DT<-data.table(a=x, b=newdata$depvar, c=newdata$EstNewEP)
  DT[,list(.N, wtavg=weighted.mean(b,c)), by=a]
}

summary.cat(newdata$CA_indicator)
summary.cat(newdata$FirmSize)

                       