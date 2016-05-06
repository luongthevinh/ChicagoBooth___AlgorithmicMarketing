# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Historical Data for Project

# load data
load(file = "Historical_Data.rdat")

# Loads a list calles histdat
# The list contains 318 elements
length(histdat)

# Each element is a matrix that gives you the results on an experiment that was run.
# For example the 11th element is a experiment with 48 messages
# The first set of variables (named V1,V2, etc.) correspond to message elements
# Unique_Clicks are responses and Unique_Sent are the number of emails sent.
levels(histdat[[11]]$V1)
levels(histdat[[11]]$V2)
levels(histdat[[11]]$V4)
levels(histdat[[11]]$V8)
colnames(histdat[[11]])

histdat[[200]]

# Different experiments may have different sets of message elements
# They will also have different sample sizes
# However, the variable names are consistent. So V1 in experiment 11
# is the same as V1 in experiment 35.
# In total there are 9 message elements and there will be the same 9 elements in the 
# upcoming experiments related to the project.

# By next Wednesday (May 04) please upload a csv file 
# with the first 9 columns labeled (V1,V2,...,V9) (all caps)
# there should be a 10th column called N 
# Each row in your csv file should correspond to a message
# That is the level of the message element V1 through V9.
# N for each row should be the same and should reflect the number of 
# emails you wish to send out for each message.
# I have placed a shell csv file with the appropriate 
# formatting in the project folder.


# Computing your profit score
# Each message campaign costs the client $200
# So if you have 32 messages it will cost your client $6400 to set up and mail these.
# The number of emails doesnt impact your cost (apart from the opportunity cost).

# Your total project profitability will be calculated as follows
N <- 5000000
profit = function(unique_clicks,ncampaigns,other)
{
	unique_clicks*.1 - 200*ncampaigns - other
} 

# so if you send out 5,000,000 emails and got a 10% response
# and you tested 64 messages in the first experiment
# and 32 messages in the second experiment
# and you purchased $5000 worth of other data
# you would have made the client
profit(5000000*.1,96,5000)
# $25,800

# If for example the control reponse rate was 3% and you decided to jsut 
# go with that, do no experimentation and bought no datathe client would make
profit(5000000*.03,0,0)
# $15,000



###################################################
#------------Exploratory            approach------#
###################################################
# Experiment one
histdat[[1]]
summary(histdat[[1]])

# Click rate
click.rate <- histdat[[1]]$Unique_Clicks/histdat[[1]]$Unique_Sent; click.rate

# Max click rate
click.rate.max.idx <- which.max(click.rate); click.rate.max.idx
click.rate.max <- click.rate[click.rate.max.idx]; click.rate.max
# Message combination
histdat[[1]][click.rate.max.idx,]

# For all experiments
# Initiate list
max.click.rates <- list()

# Calculate them all
for (i in 1:length(histdat)){
  # Click rate
  click.rate <- histdat[[i]]$Unique_Clicks/histdat[[i]]$Unique_Sent
  
  # Max click rate
  click.rate.max <- click.rate[which.max(click.rate)]
  
  # Assign value
  max.click.rates[i] <- click.rate.max
  
  # Index of max value
  click.rate.max.idx[i] <- which.max(click.rate)
}

# Check consistency
histdat[[83]][click.rate.max.idx[83],]
5533/32554

# Max click rate is 16.9% for experiment 83
max.click.rate.all <- max.click.rates[which.max(max.click.rates)]; max.click.rate.all
# Profits: maybe is not the highest
profit((N-sum(histdat[[83]]$Unique_Sent))*max.click.rate.all[[1]],dim(histdat[[83]])[1],0)


# Initiate list
profit.all <- list()
# profits for all experiments
for (i in 1:length(histdat)){
  # Profits are negative
  profit.all[i] <- profit((N-sum(histdat[[i]]$Unique_Sent))*max.click.rates[[i]],dim(histdat[[i]])[1],0)
}
best.profit <- which.max(profit.all); best.profit
profit.all[best.profit]
dim(histdat[[best.profit]])[1]
max.click.rates[[best.profit]]



profit.all.l <- unlist(profit.all)
sort(profit.all.l, decreasing = FALSE)

#----------------------------------------------------------#
# Break-even profits                                       #
#----------------------------------------------------------#

BE.click <- .1267147
ncampaigns <- 36
profit((N-ncampaigns*5000)*BE.click,ncampaigns,0)


#----------------------------------------------------------#
# Analysis of best campaings                               #
#----------------------------------------------------------#
for (i in 1:length(histdat)){
  if (profit.all[[i]]>35000){
    cat("Experiment",i,"N of messages","= ",dim(histdat[[i]])[1],"Profits:",profit.all[[i]],"\n")
  }
}
for (i in 1:length(histdat)){
  if (profit.all[[i]]>35000){
    cat("Experiment", i,"\n")
    print(histdat[[i]][click.rate.max.idx[i],])
    cat("\n")
  }
}


#----------------------------------------------------------#
# Analysis of worse campaings                              #
#----------------------------------------------------------#
for (i in 1:length(histdat)){
  if (profit.all[[i]]< (-30000)){
    cat("Experiment",i,"N of campaigns","= ",dim(histdat[[i]])[1],"Profits:",profit.all[[i]],"\n")
  }
}

#----------------------------------------------------------#
# Optimal design    campaings                              #
#----------------------------------------------------------#
# Initiate list
design.list <- list()
j=1
for (i in 1:length(histdat)){
  if (profit.all[[i]]>35000){
    cat("Experiment",i,"N of campaigns","= ",dim(histdat[[i]])[1],"Profits:",profit.all[[i]],"\n")
    design.list[j]<-i
    j <- j+1
  }
}

for (i in 1:length(design.list)){
  cat(colnames(histdat[[design.list[[i]]]]),mean(histdat[[design.list[[i]]]]$Unique_Sent),"\n")
}
# All messages sent
mean.sent = sapply(histdat,function(dt) { mean(dt$Unique_Sent) }) 
summary(mean.sent)

# Subset
hist.sub <- list()
for (i in 1:length(design.list)){
  hist.sub[i] <- histdat[design.list[[i]]]
}

# Design sent
mean.sent = sapply(hist.sub,function(dt) { mean(dt$Unique_Sent) }) 
summary(mean.sent)

colnames(histdat[[10]])
histdat[[10]]

###################################################
#-------Building a naive model with all data------#
###################################################

# Run library
library(AlgDesign)

max.level.V1 <- 0
max.level.V2 <- 0
max.level.V3 <- 0
max.level.V4 <- 0
max.level.V5 <- 0
max.level.V6 <- 0
max.level.V7 <- 0
max.level.V8 <- 0
max.level.V9 <- 0
# Get max levels per var
for (i in 1:length(histdat)){
  if (max.level.V1 < length(levels(histdat[[i]]$V1))){
    max.level.V1 <- length(levels(histdat[[i]]$V1))
  }  
  if (max.level.V2 < length(levels(histdat[[i]]$V2))){
    max.level.V2 <- length(levels(histdat[[i]]$V2))
  } 
  if (max.level.V3 < length(levels(histdat[[i]]$V3))){
    max.level.V3 <- length(levels(histdat[[i]]$V3))
  } 
  if (max.level.V4 < length(levels(histdat[[i]]$V4))){
    max.level.V4 <- length(levels(histdat[[i]]$V4))
  } 
  if (max.level.V5 < length(levels(histdat[[i]]$V5))){
    max.level.V5 <- length(levels(histdat[[i]]$V5))
  } 
  if (max.level.V6 < length(levels(histdat[[i]]$V6))){
    max.level.V6 <- length(levels(histdat[[i]]$V6))
  } 
  if (max.level.V7 < length(levels(histdat[[i]]$V7))){
    max.level.V7 <- length(levels(histdat[[i]]$V7))
  } 
  if (max.level.V8 < length(levels(histdat[[i]]$V8))){
    max.level.V8 <- length(levels(histdat[[i]]$V8))
  } 
  if (max.level.V9 < length(levels(histdat[[i]]$V9))){
    max.level.V9 <- length(levels(histdat[[i]]$V9))
  } 
}


# Factorial design
# Full Factorial Design
mat = gen.factorial(
  levels=c(6,6,3,3,5,4,2,5,6),
  varNames=paste("V",1:9,sep=""),
  factors="all"
)
dim(mat)


# Finding the messages to test
set.seed(61113)
ds = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = mat, nTrials = 36,criterion="I") # If you change the number of variables, you need more datapoints (n of trials)
ds
ds1 = ds$design

# Evaluating the model #determinant 12.6%
eval.design(~V1+V2+V3+V4+V5+V6+V7+V8+V9,ds1)


## ------------Professor Appendix-----------------------
# Professors Code
# Loop across the datasets
ctr = NULL
for(j in 1:length(histdat)) {
  dt = histdat[[j]]
  ctr = c(ctr,mean(dt$Unique_Clicks/dt$Unique_Sent)) 
}
summary(ctr)

# Alternatively, checkout lapply or sapply
ctr2 = sapply(histdat,function(dt) { mean(dt$Unique_Clicks/dt$Unique_Sent) }) 
summary(ctr2)

# These give you the same result
all.equal(ctr,ctr2)


## ------------Eric's attempt at reg model-----------------------
## Running regression on experiments that include all 9 variables
## Find the experiments that include all 9 variables
indices = numeric()
j = 1
for(i in 1:length(histdat)) {
  dat = histdat[[i]]
  if(!is.null(dat$V1) & !is.null(dat$V2) & !is.null(dat$V3) & !is.null(dat$V4) & !is.null(dat$V5)  & !is.null(dat$V6) & !is.null(dat$V7) & !is.null(dat$V8) & !is.null(dat$V9)){
    indices[j] = i
    j = j+1
  }
}
##Combine the data from those experiments into the combined_dat matrix
j=1
combined_dat <- histdat[[indices[1]]]
for(i in indices) {
  num_campaigns = length(histdat[[i]]$V1)-1
  combined_dat[j:(j+num_campaigns),] <- histdat[[i]]
  j = j+num_campaigns
}
#Run the regression on the combined data
combined_model<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=combined_dat,family='binomial')
summary(combined_model)

#Calculate the predicted CTR
combined_results <- 1/(1+exp(-predict.glm(combined_model, mat)))
#Show the top 10 (a little bit of a hack here, getting the top 10 by taking the CTR rates that are above .159)
cbind(mat[combined_results>.159,],combined_results[combined_results>.159])

#Find the best 36 values to test based on the OptFederov function.  Temporarily commented out as the function takes a long time to run
#ds1 = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = mat, nTrials = 36,criterion="I")$design
opt_federov_results <- cbind(ds1, 1/(1+exp(-predict.glm(combined_model, ds1))))
# Design evaluation XX% variation
eval.design(~V1+V2+V3+V4+V5+V6+V7+V8+V9,ds1)

#-------------Julio's analysis of Eric's results-----------#
Beta <- coef(combined_model)
Odds <- exp(Beta); Odds
# Variable/level that extremely reduce the odds and needs not to be in the model "TRUE"
# Be careful that V1 is used as based level so it doesnt matter that the odds are lower for that case
# We will include the biggest odds of it (V13 or V14)
print(Odds<0.7)
# Variable/level that needs to be included "TRUE"
print(Odds>1.2)
# V26, V55, V72, V92, V94

# Eric's messages match with the analysis of level 2 and 4 for variable 9
# level 2 for 7
# level 5 for 5
# level 6 for 2


###################################################
#-------Building a better model             ------#
###################################################
#-------------Julio regression for every model-----------#
#Run the regression on the model
model <- list()
Beta <- list()
Odds <- list()
for(i in 1:length(histdat)){
  model[[i]]<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=histdat[[i]],family='binomial')
  Beta[[i]]<- coef(model[[i]])
  Odds[[i]] <- exp(Beta[[i]])
}
# Example for experiment 1
summary(model[[1]])
Beta[[1]]
Odds[[1]]

# Checking that odds and probabilities are accurate
Beta[[1]]["V13"]
exp(Beta[[1]]["V13"])
p.hat2 <- exp(Beta[[1]]["V12"])/(1+exp(Beta[[1]]["V12"])); p.hat2
p.data2 <- histdat[[1]]$Unique_Clicks[1]/histdat[[1]]$"Unique_Sent"[1]; p.data2

# Inspecting all Odds to find visual patters (better to use professor's apporach at the end)
Odds[[174]]

# Julio's approch to create matrix
# Let's take the variable/levels that generate the most variability to later create a subset of the
# mat. On that 'mat' we use the Russian formula
# V1: levels 3,4,6
# V2: levels 2,3,4,5,6
# V3: levels 2,3
# V4: levels  1,2
# V5: levels 2,3,4,5
# V6: levels 2,3 (maybe 6 from exp 88)
# V7: levels 2 (I added level 1 for federov to work)
# V8: levels 2,4,5 
# V9: levels 2,4,5 (maybe 3 and 6 from exp 87 and 144)
3*5*2*2*4*2*1*3*3
3420/8
36/(4320/8)

# Factorial design
# Reduced Factorial Design
# This is an approximation of the optimal design because I needed to reduce the levels.
red.mat = gen.factorial(
  levels=c(3,5,2,2,4,2,2,3,3),
  varNames=paste("V",1:9,sep=""),
  factors="all"
)
dim(red.mat)
ds.red = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = red.mat, nTrials = 36,criterion="I")$design
# Design evaluation 19.13% variation: MUCH BETTER THAN BEFORE!
eval.design(~V1+V2+V3+V4+V5+V6+V7+V8+V9,ds.red)

# The result needs to be translated into the keys above
# For example V1 is from 1 to 3, but should be 3,4,6
levels(ds.red$V1) <- c(3,4,6)
levels(ds.red$V2) <- c(2,3,4,5,6)
levels(ds.red$V3) <- c(2,3)
levels(ds.red$V4) <- c(1,2)
levels(ds.red$V5) <- c(2,3,4,5)
levels(ds.red$V6) <- c(2,3)
levels(ds.red$V7) <- c(1,2)
levels(ds.red$V8) <- c(2,4,5)
levels(ds.red$V9) <- c(2,4,5)
ds.red

# Predictions
opt_federov_results <- cbind(ds.red, 1/(1+exp(-predict.glm(combined_model, ds.red))))
opt_federov_results
# Rename column 10, so name is shorter
colnames(opt_federov_results)[10]<- c("ctr")

###################################################
#------------Finding the best message----------#
###################################################
# Explanation
# The best message is the one with higher odds to 
# generate a high Click through rate (CTR) and
# the determinant of the  Federov design is
# high enough

#-----------Julio's model versus All model--------#

#---------------Julio's---------------------------#
# Preparing the matrix
ds1 <- opt_federov_results[,1:9]
ds1

# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)
dim(mm.ds1)
mm.ds1

# Find those messages in our data
y.ds1 = opt_federov_results[,10]
y.ds1

# # cbind(y.ds1,N-y.ds1) = Success, No Success
# sm.ds1 = summary(glm(cbind(y.ds1,1-y.ds1)~mm.ds1-1,family='binomial'))
# sm.ds1
# # Alternative way to do it
# sm.ds1.b = summary(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
# sm.ds1.b
Beta.ds <- coef(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["mm.ds1(Intercept)"]+Beta.ds["mm.ds1V14"]+Beta.ds["mm.ds1V26"]+Beta.ds["mm.ds1V33"]+Beta.ds["mm.ds1V55"]+Beta.ds["mm.ds1V63"]+Beta.ds["mm.ds1V72"]+Beta.ds["mm.ds1V84"]))); p.hat
# Best message get 17.65%

#--------------- Best way to do it--------------#
# NOW IS RUNNING!
# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds)
p.hat <- 1/(1+exp(-(Beta.ds["(Intercept)"]+Beta.ds["V14"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat


###################################################
#------------Exploratory professors approach------#
###################################################

cf1 = NULL
ctr = NULL
for (l in 1:318){
  dat = histdat[[l]]
  if("V1" %in% names(dat)){
    res = glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~V1,data=dat,family="binomial")
    cf1 = rbind(cf1, coef(res))
  }
  ctr = c(ctr,mean(dat$Unique_Clicks/dat$Unique_Sent))
}
summary(cf1)
dim(cf1)
colMeans(cf1)
hist(ctr, breaks=50)
# boxplot(cfmat[,c("V92","V93","V94","V95","V96")])
# If we sent 5,000 emails:
# Only 150 datapoints to get the estimates, that should be fine
0.03*5000


#Run the regression on the combined data
combined_model<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=combined_dat,family='binomial')
summary(combined_model)


# Function to spit out coefficients
get.cf = function(dat){
  gres = glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~.,data=dat,family="binomial")
  coef(gres)
}

# Apply Function to List (Same as "Beta" in Julios model)
cf = lapply(histdat,get.cf)


# Whats the full model?
cfmax = cf[[which.max(sapply(cf,length))]]

# Variable Names (same as getting levels in Julios: "Max.level.V9")
nms = names(cfmax)

# Storage
cfmat = matrix(0,318,length(nms))
# Loop across names
for(j in 1:length(nms))
{
  vname = nms[j]
  # Loop across experiments
  for(l in 1:318)
  {
    cfmat[l,j] = NA
    # Find experiments which have variable
    # and store estiamte
    if(vname %in% names(cf[[l]])) {
      cfmat[l,j] = cf[[l]][which(vname==names(cf[[l]]))]
    }
  }
}

# Rename Cols
colnames(cfmat) = nms
# Average ignoring missing
v.means = colMeans(cfmat,na.rm=TRUE)

# Peek
barplot(v.means[2:6])
barplot(v.means[c("V32","V33")])
boxplot(cfmat[,c("V92","V93","V94","V95","V96")])

# All variables distribution
boxplot(cfmat[,2:32])

dim(cfmat)
length(nms)

# Evaluating your design
dsr = sample(1:nrow(mat),36)
ds.rand1 = mat[dsr,]
eval.design(~V1+V2+V3+V4+V5+V6+V7+V8+V9,ds.rand1)
eval.design(~V1+V2+V3+V4+V5+V6+V7+V8+V9,ds.red)

####################################################
#----------TEAM MODELS-----------------------------#
####################################################

# Jorge, Vinh, Shao, and Eric
# Based on the data above you can create your models here an evaluate in term of:
# 1. $determinant = means the variability that the ortogonal design is capturing
# 2. ctr = of the best created model: you can use the odds approach (refer to p.hat)
# 3. Variation on sample: we need to create a way to test our models in the complete database (including
# experiments that doesn't have certain messages). Nonetheless, Eric's approach to test on the combined data
# is fine by the moment

######################################
# Experiment 1
######################################
# Based on variability of the barplot
# V1: levels 4,6
# V2: levels 3,6
# V3: levels 2,3
# V4: levels  2,3
# V5: levels 0
# V6: levels 2,3 (maybe 6 from exp 88)
# V7: levels 0
# V8: levels 2,4
# V9: levels 4,5,6 

# Re-run the above experiment but substracting level 4
# Test with 2 levels for var 3
red.mat2 = gen.factorial(
  levels=c(2,2,2,2,2,2,3),
  varNames=c("V1","V2","V3","V4","V6","V8","V9"),
  factors="all"
)
dim(red.mat2)
ds2.red = optFederov( ~ V1+V2+V3+V4+V6+V8+V9,data = red.mat2, nTrials = 36,criterion="I")$design

# Design evaluation 27.4% variation
eval.design(~V1+V2+V3+V4+V6+V8+V9,ds2.red)

# Rename levels before predict
levels(ds2.red$V1) <- c(4,6)
levels(ds2.red$V2) <- c(3,6)
levels(ds2.red$V3) <- c(2,3)
levels(ds2.red$V4) <- c(2,3)
levels(ds2.red$V6) <- c(2,3)
levels(ds2.red$V8) <- c(2,4)
levels(ds2.red$V9) <- c(4,5,6)
ds2.red

# Evaluating the model
# Add variable V5 and V2 for level 2
ds2.red['V5'] <- factor(2)
ds2.red['V7'] <- factor(2)
ds2.red

# Predicting
opt_federov_results <- cbind(ds2.red, 1/(1+exp(-predict.glm(combined_model, ds2.red))))
opt_federov_results
colnames(opt_federov_results)[10]<- c("ctr")


# Preparing the matrix
ds1 <- opt_federov_results[,1:9]
ds1
ds1['V5']<-NULL
ds1['V7']<-NULL

# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)
dim(mm.ds1)
mm.ds1

# Load the ctr
y.ds1 = opt_federov_results[,10]
y.ds1

# # cbind(y.ds1,N-y.ds1) = Success, No Success
# sm.ds1 = summary(glm(cbind(y.ds1,1-y.ds1)~mm.ds1-1,family='binomial'))
# sm.ds1
# # Alternative way to do it
# sm.ds1.b = summary(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
# sm.ds1.b
Beta.ds <- coef(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["mm.ds1(Intercept)"]+Beta.ds["mm.ds1V26"]+Beta.ds["mm.ds1V33"]+Beta.ds["mm.ds1V63"]+Beta.ds["mm.ds1V84"]))); p.hat
# Best message get 10.2% 

# Not need to be run: Only to see the best message without variables 5 and 7
# # Using predictive over models without V5 and V7
# ## Running regression on experiments that include all 9 variables
# ## Find the experiments that include all 9 variables
# indices = numeric()
# j = 1
# for(i in 1:length(histdat)) {
#   dat = histdat[[i]]
#   if(!is.null(dat$V1) & !is.null(dat$V2) & !is.null(dat$V3) & !is.null(dat$V4) & is.null(dat$V5)  & !is.null(dat$V6) & is.null(dat$V7) & !is.null(dat$V8) & !is.null(dat$V9)){
#     indices[j] = i
#     j = j+1
#   }
# }

# ##Combine the data from those experiments into the combined_dat matrix
# j=1
# combined_dat <- histdat[[indices[1]]]
# for(i in indices) {
#   num_campaigns = length(histdat[[i]]$V1)-1
#   combined_dat[j:(j+num_campaigns),] <- histdat[[i]]
#   j = j+num_campaigns
# }
# #Run the regression on the combined data
# combined_model<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=combined_dat,family='binomial')
# summary(combined_model)
# 
# #7.6% in the case with experiments with only 
# Beta.ds <- coef(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
# Odds.ds <- exp(Beta.ds); Odds.ds
# p.hat <- 1/(1+exp(-(Beta.ds["mm.ds1(Intercept)"]+Beta.ds["mm.ds1V33"]+Beta.ds["mm.ds1V84"]))); p.hat


######################################
# Experiment 2
######################################
# Based on variability of the barplot
# V1: levels 4,6
# V2: levels 3,6
# V3: levels 2,3
# V4: levels  2,3
# V5: levels 2,5
# V6: levels 2,3 (maybe 6 from exp 88)
# V7: levels 1,2
# V8: levels 2,4
# V9: levels 4,5,6 

# Re-run the above experiment but substracting level 4
# Test with 2 levels for var 3
red.mat3 = gen.factorial(
  levels=c(2,2,2,2,2,2,2,2,3),
  varNames=c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),
  factors="all"
)
dim(red.mat3)
ds.red = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = red.mat3, nTrials = 36,criterion="I")$design

# Design evaluation 27.4% variation
eval.design(~V1+V2+V3+V4+V6+V8+V9,ds.red)

# Rename levels before predict
levels(ds.red$V1) <- c(4,6)
levels(ds.red$V2) <- c(3,6)
levels(ds.red$V3) <- c(2,3)
levels(ds.red$V4) <- c(2,3)
levels(ds.red$V5) <- c(2,5)
levels(ds.red$V6) <- c(2,3)
levels(ds.red$V7) <- c(1,2)
levels(ds.red$V8) <- c(2,4)
levels(ds.red$V9) <- c(4,5,6)
ds.red

# Evaluating the model
# Predicting
opt_federov_results <- cbind(ds.red, 1/(1+exp(-predict.glm(combined_model, ds.red))))
opt_federov_results
colnames(opt_federov_results)[10]<- c("ctr")


# Preparing the matrix
ds1 <- opt_federov_results[,1:9]
ds1


# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)
dim(mm.ds1)
mm.ds1

# Load the ctr
y.ds1 = opt_federov_results[,10]
y.ds1

# # cbind(y.ds1,N-y.ds1) = Success, No Success
# sm.ds1 = summary(glm(cbind(y.ds1,1-y.ds1)~mm.ds1-1,family='binomial'))
# sm.ds1
# # Alternative way to do it
# sm.ds1.b = summary(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
# sm.ds1.b
Beta.ds <- coef(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["mm.ds1(Intercept)"]+Beta.ds["mm.ds1V26"]+Beta.ds["mm.ds1V33"]+Beta.ds["mm.ds1V55"]+Beta.ds["mm.ds1V63"]+Beta.ds["mm.ds1V72"]+Beta.ds["mm.ds1V84"]))); p.hat
# ctr = 15.7%
#--------------- Best way to do it--------------#
# NOW IS RUNNING!
# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds)
p.hat <- 1/(1+exp(-(Beta.ds["(Intercept)"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat
# ctr = 15.7% same as before

# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.-1,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["V14"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat
# ctr = 15.7% same as before

# Best message get 15.76% 


######################################
# Experiment 3
######################################
# Based on variability of the barplot
# V1: levels 4,6
# V2: levels 3,6
# V3: levels 2,3
# V4: levels  1,2,3
# V5: levels 2,5
# V6: levels 2,3
# V7: levels 1,2
# V8: levels 2,4
# V9: levels 4,5,6 

# I added level 1 only for variable 4 compared to experiment #3
red.mat4 = gen.factorial(
  levels=c(2,2,2,3,2,2,2,2,3),
  varNames=c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),
  factors="all"
)
dim(red.mat4)
ds4.red = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = red.mat4, nTrials = 36,criterion="I")$design

# Design evaluation 20.1% variation
eval.design(~V1+V2+V3+V4+V6+V8+V9,ds4.red)

# Rename levels before predict
levels(ds4.red$V1) <- c(4,6)
levels(ds4.red$V2) <- c(3,6)
levels(ds4.red$V3) <- c(2,3)
levels(ds4.red$V4) <- c(1,2,3)
levels(ds4.red$V5) <- c(2,5)
levels(ds4.red$V6) <- c(2,3)
levels(ds4.red$V7) <- c(1,2)
levels(ds4.red$V8) <- c(2,4)
levels(ds4.red$V9) <- c(4,5,6)
ds4.red

# Evaluating the model
# Predicting
opt_federov_results <- cbind(ds4.red, 1/(1+exp(-predict.glm(combined_model, ds4.red))))
opt_federov_results
colnames(opt_federov_results)[10]<- c("ctr")


# Preparing the matrix
ds1 <- opt_federov_results[,1:9]
ds1


# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)
dim(mm.ds1)
mm.ds1

# Load the ctr
y.ds1 = opt_federov_results[,10]
y.ds1

# # cbind(y.ds1,N-y.ds1) = Success, No Success
# sm.ds1 = summary(glm(cbind(y.ds1,1-y.ds1)~mm.ds1-1,family='binomial'))
# sm.ds1
# # Alternative way to do it
# sm.ds1.b = summary(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
# sm.ds1.b
Beta.ds <- coef(glm(log(y.ds1/(1-y.ds1))~mm.ds1-1))
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["mm.ds1(Intercept)"]+Beta.ds["mm.ds1V26"]+Beta.ds["mm.ds1V33"]+Beta.ds["mm.ds1V55"]+Beta.ds["mm.ds1V63"]+Beta.ds["mm.ds1V72"]+Beta.ds["mm.ds1V84"]))); p.hat
# ctr = 17.5%
#--------------- Best way to do it but need to match variables selected--------------#
# NOW IS RUNNING!
# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds)
p.hat <- 1/(1+exp(-(Beta.ds["(Intercept)"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat
# ctr = 17.5% same as before

# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.-1,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["V14"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat
# ctr = 17.5% same as before

# Best message get 17.5% 

######################################
# Experiment 5
######################################
# Based on variability of the barplot
# V1: levels 4,6
# V2: levels 3,6
# V3: levels 2,3
# V4: levels  1,2,3
# V5: levels 2,5
# V6: levels 2,3
# V7: levels 1,2
# V8: levels 2,4
# V9: levels 4,5,6 

# NEW THING TO TRY: Fater this discovery try interactions
red.mat5 = gen.factorial(
  levels=c(2,2,2,3,2,2,2,2,3),
  varNames=c("V1","V2","V3","V4","V5","V6","V7","V8","V9"),
  factors="all"
)
dim(red.mat5)
ds5.red = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = red.mat5, nTrials = 36,criterion="I")$design

# Design evaluation 20.1% variation
eval.design(~V1+V2+V3+V4+V6+V8+V9,ds5.red)

# Rename levels before predict
levels(ds5.red$V1) <- c(4,6)
levels(ds5.red$V2) <- c(3,6)
levels(ds5.red$V3) <- c(2,3)
levels(ds5.red$V4) <- c(1,2,3)
levels(ds5.red$V5) <- c(2,5)
levels(ds5.red$V6) <- c(2,3)
levels(ds5.red$V7) <- c(1,2)
levels(ds5.red$V8) <- c(2,4)
levels(ds5.red$V9) <- c(4,5,6)
ds5.red

# Evaluating the model
# Predicting
opt_federov_results <- cbind(ds5.red, 1/(1+exp(-predict.glm(combined_model, ds5.red))))
opt_federov_results
colnames(opt_federov_results)[10]<- c("ctr")

# Correct to run the regression get the odds and calculate the CTR for the best message to be sent
model.ds<-glm(cbind(ctr,(1-ctr))~.-1,data=opt_federov_results,family='binomial')
Beta.ds<- coef(model.ds)
Odds.ds <- exp(Beta.ds); Odds.ds
p.hat <- 1/(1+exp(-(Beta.ds["V14"]+Beta.ds["V26"]+Beta.ds["V33"]+Beta.ds["V55"]+Beta.ds["V63"]+Beta.ds["V72"]+Beta.ds["V84"]))); p.hat
# ctr = 17.5% same as before

# Best message get 17.5% 



#### Testing histdata #######
# 10: 23.16% 
# 27: 30.9% 
# 65: 72% INVESTIGATE FURTHER
# 87: 53% INVESTIGATE FURTHER
# 88: 38.7%
# 116: 12.3% (don't take variables out)
# 144: 13.7% (don't take variables out)
# 171: 14.8% (don't take variables out)
# 172: 22.8% Again, 7 is not in the model
# 174: 17.9% Again, 9 is not in the model
# 221: 82.3% with 4, 5 and 7 out of the model (interesting)
# 294: 31.14%

# Function to spit out coefficients
get.cf2 = function(dat){
  gres = glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~.*.,data=dat,family="binomial")
  coef(gres)
}

# Apply Function to List (Same as "Beta" in Julios model)
cf2 = lapply(histdat,get.cf2)

cf2[[65]]
i <- 65
Odds[[i]]
Beta.l <- cf2[[65]]
p.hat <- 1/(1+exp(-(Beta.l["(Intercept)"]+Beta.l["V15"]+Beta.l["V26"]+Beta.l["V32"]+Beta.l["V55"]+Beta.l["V63"]+Beta.l["V72"]+Beta.l["V84"]+Beta.l["V94"]+Beta.l["V13:V23"]))); p.hat
# Improves to 88% if I include interaction between V23 and V13

# V12 con V24
# V13 con V23
# V15 con V22
# Interact variable 1 and 2
# Eric if you can help me here would be great

i <- 221
Odds[[i]]
Beta.l <- Beta[[i]]
p.hat <- 1/(1+exp(-(Beta.l["V12"]+Beta.l["V26"]+Beta.l["V32"]+Beta.l["V64"]+Beta.l["V85"]))); p.hat
histdat[[i]]
model.ds<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=combined_dat,family='binomial')
summary(model.ds)
Beta.ds<- coef(model.ds)
p.hat <- 1/(1+exp(-(Beta.ds["V12"]+Beta.ds["V26"]+Beta.ds["V32"]+Beta.ds["V64"]+Beta.ds["V85"]))); p.hat
# Only 18%

# Only experiment 221 has that combination, I would rely much on it
indices = numeric()
j = 1
for(i in 1:length(histdat)) {
  dat = histdat[[i]]
  if(!is.null(dat$V1) & !is.null(dat$V2) & !is.null(dat$V3) & is.null(dat$V4) & is.null(dat$V5)  & !is.null(dat$V6) & is.null(dat$V7) & !is.null(dat$V8) & !is.null(dat$V9)){
    indices[j] = i
    j = j+1
  }
}

