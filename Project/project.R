# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Historical Data for Project

# load data
load(file = "Historical_Data.rdat")
df <- read.csv("results_design1.csv")

# Rename vars for later use
colnames(df)[colnames(df)=="N"] <- "Unique_Sent"
colnames(df)[colnames(df)=="Clicks"] <- "Unique_Clicks"
df2 <- sapply(df[,1:9],as.factor)
df[,1:9] <- df2[,1:9]
df <- as.data.frame(unclass(df))

# Levels in our design for the analysis
str(df)

# Your total project profitability will be calculated as follows
N <- 5000000
profit = function(unique_clicks,ncampaigns,other)
{
	unique_clicks*.1 - 200*ncampaigns - other
} 

## ------------Running a few useful functions-----------------------
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

# Function to spit out coefficients
get.cf = function(dat){
  gres = glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~.,data=dat,family="binomial")
  coef(gres)
}

# Apply Function to List (Same as "Beta" in Julios model)
cf = lapply(histdat,get.cf)

# Whats the full model?
cfmax = cf[[which.max(sapply(cf,length))]]

# Variable Names
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



###############################################
############# Our Design analysis #############
###############################################
# Run a regression on our results
reg1 <- glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~.,data=df,family="binomial")
cf2 = coef(glm(cbind(Unique_Clicks,Unique_Sent-Unique_Clicks)~.,data=df,family="binomial"))
cf2

# All variables distribution in a barplot to see differences
# If someone can automate this please
boxplot(cfmat[,2:32])
points(5,cf2[2],col="red", cex=1, pch=19) #V16
points(10,cf2[3],col="red", cex=1, pch=19) #V26
points(12,cf2[4],col="red", cex=1, pch=19) #V33
points(13,cf2[5],col="red", cex=1, pch=19) #V42
points(14,cf2[6],col="red", cex=1, pch=19) #V43
points(18,cf2[7],col="red", cex=1, pch=19) #V55
points(20,cf2[8],col="red", cex=1, pch=19) #V63
points(22,cf2[9],col="red", cex=1, pch=19) #V72
points(25,cf2[10],col="red", cex=1, pch=19) #V84
points(30,cf2[11],col="red", cex=1, pch=19) #V95
points(31,cf2[12],col="red", cex=1, pch=19) #V96

# Findind the best message
# just to make my point clear
Beta <- cf2
# odds from the betas
Odds <- exp(Beta); Odds
p.hat <- 1/(1+exp(-(Beta["(Intercept)"]+Beta["V26"]+Beta["V33"]+Beta["V55"]+Beta["V63"]+Beta["V72"]+Beta["V84"]))); p.hat
# Our best model has a expected click through rate of 18.8%  

# Variables in design, so: V14 in intercept, V41, V94 (maybe V92 better)
# V1: levels 4,6
# V2: levels 3,6
# V3: levels 2,3
# V4: levels  1,2,3
# V5: levels 2,5
# V6: levels 2,3
# V7: levels 1,2
# V8: levels 2,4
# V9: levels 4,5,6 


# expected profit: $83,984.1
profit((N-(36*5000))*p.hat+sum(df$Unique_Clicks),36,0)

# summary regression
# Almost all parameters are really significant! 
summary(reg1)


#------ Testing performance of message based on historical data ------#
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
# Run the regression on the combined data
combined_model<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=combined_dat,family='binomial')
summary(combined_model)
Beta.h <- coef(combined_model)
Odds.h <- exp(Beta.h); Odds.h
p.hat2 <- 1/(1+exp(-(Beta.h["V14"]+Beta.h["V26"]+Beta.h["V33"]+Beta.h["V55"]+Beta.h["V63"]+Beta.h["V72"]+Beta.h["V84"]+Beta.h["V94"]))); p.hat2
# We would have get a 17.5%

# Can we do it better?
Odds
# Maybe V92 instead of V94

###################################
########## Second Design ##########
###################################
# What would be needed to improve

# expected profit1: $83,984.1

# Expected profit design 2 of 36 no improvement in clicks: 73985.5
profit((N-(72*5000))*p.hat+2*sum(df$Unique_Clicks),72,0)

# CTR for design 2 of 36 to break even: 20.95%
profit((N-(72*5000))*0.2095+2*sum(df$Unique_Clicks),72,0)


# Expected profit design 2 of 20 no improvement in clicks: 78689.24
profit((N-(56*5000))*p.hat+2*sum(df$Unique_Clicks),56,0)

# CTR for design 2 of 36 to break even: 19.92%
profit((N-(56*5000))*0.1992+2*sum(df$Unique_Clicks),56,0)

# Gap to be improved?
0.1992-0.188


# Model to try:
# V42, V55, V61, V72, V84, V92

# V92 seems similar to V94, and maybe better
# V61 similar to V63, and maybe better
# V42: more incertainty V41 seems better, maybe an interaction?
# V33
# V26
# V14
