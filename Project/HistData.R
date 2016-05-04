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

N <- 5000000

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
  if (profit.all[[i]]>40000){
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
histdat[[1]]

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

# Can we find set of messages to test?
# set.seed(61113)
# ds = optFederov( ~ V1+V2+V3+V4+V5+V6+V7+V8+V9,data = mat, nTrials = 36,criterion="I") # If you change the number of variables, you need more datapoints (n of trials)
# ds
# ds1 = ds$design



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


#-------------Julio's analysis of Eric's results-----------#
Beta <- coef(combined_model)
Odds <- exp(Beta); Odds
# Variable/level that extremely reduce the odds and needs not to be in the model "TRUE"
print(Odds<0.7)
# Variable/level that needs to be included "TRUE"
print(Odds>1.2)
# V26, V55, V72, V92, V94

# Eric's messages match with the analysis of level 2 and 4 for variable 9
# level 2 for 7
# level 5 for 5
# level 6 for 2

#-------------Julio regression for every model-----------#
#Run the regression on the model
model <- list()
Beta <- list()
Odds <- list()
for(i in 1:length(histdat)){
  model[[i]]<-glm(cbind(Unique_Clicks,(Unique_Sent-Unique_Clicks))~.-1,data=histdat[[i]],family='binomial')
  Beta[[i]] <- coef(model[[i]])
  Odds[[i]] <- exp(Beta[[i]])
}
summary(model[[1]])
Beta[[1]]
Odds[[1]]
