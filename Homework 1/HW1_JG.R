# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Group Homework #1

# A new DMP claims that hey have a better set of variables
# to match your customers on. 
# In particular, they have an ecom_index which they claim 
# Offers an inrementally better match for you.
# Unfortunately the cost of matching customers via them is 
# significantly higher!

cost2 = 3.25

# They have given you a sample target dataset 
# and a matched version of your customer dataset
# These are in cust2.rdat and target2.rdat
# Hint: Careful the ecom_index is in the 12th column

# Use this new data to answer the following questions.
# 1. Would you go with the new DMP? Justify your answer. (Is the extra column worth it?)

# I would go for the new DMP, see final graph. The extra column allow to target aprox more than 75% of all 
# possible target that would spend money. This happens because in the regression ecom_index is much more significant
# than retail_index. Meaning that there is less noise in ecom_index compared to retail_index, so there is
# more value in that variable that is appreciated that the expected profit is higher.

# 2. Is there any circumstance when you would ignore the 
#    matching and go after the entire target file audience? Justify.

# Yes, if the cost of reaching them is very low compared to the proportion of them that spend money.

# Originally, we had the following profits:

# Load Data
load(file="cust.rdat") #Data comes from Comescore, that track everything they do on the web.
load(file="target.rdat") #Data comes from Comescore, that track everything they do on the web.

# Obviously we can blanket all customers
cost = .5
profit_all = sum(target[,"spend"])-cost*length(target[,"spend"])
profit_all

# Building the Lookalike Audience
# Let's start with k-Means

# First we need to create the "Seed"
# Valuable Customers
# What is our cutoff? $10
cs=cust[cust$spend>10,2:10]
CV = mean(cust[cust$spend>10,"spend"]); CV
EV = mean(cust[,"spend"]); EV

# In this case we have to  use model matrix 
# because variables are categorical
seed=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index-1, data=cs)

# Note -1 is because we dont need an intercept

# Transform the target dataset to match cust 

ts = target[,2:10]
targ=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index-1, data=ts)

# Now make sure all variables (names) match
# If not, retain only those that do
targ=targ[,intersect(colnames(seed),colnames(targ))]


# Let's use FNN

# Use package
library(FNN)

# Lets repeat this for different k
# Storage
profit=rep(0,20)

# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit[k] = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
}

plot(profit,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
# Best k
k.star = which.max(profit); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit.star = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])

profit.star

# Explore matches
# How many did we really match?
table(target[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target[matches,"spend"])/sum(target[,"spend"])
perc.captured

# Can we do better?
# Which variables really matters?
# This is a very naive approach
# Let's regress spend on variables
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index, data=cust)
summary(reg)


# Redo knn with the variables that aren't noise
seed = with(cs,cbind(retail_index,household_income==6,household_size==6))
targ = with(target,cbind(retail_index,household_income==6,household_size==6))
# Storage
profit2=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit2[k] = sum(target[matches,"spend"])-cost*length(target[matches,"spend"])
}

# Plot
plot(profit2,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star = which.max(profit2); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit2.star = sum(target[matches,"spend"])-cost*length(target[matches,"spend"]); profit2.star

# Explore matches
# How many did we really match?
table(target[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target[matches,"spend"])/sum(target[,"spend"])
perc.captured

# How do the two compare
plot(profit2,type='b',pch=19,ylim=c(-500,4000))
lines(profit,type='b')
abline(h=0,lty=2)

# Using the data from the new DMP at the new cost of 3.25
cost2

# Load Data
load(file="cust2.rdat") #Data comes from Comescore, that track everything they do on the web.
load(file="target2.rdat") #Data comes from Comescore, that track everything they do on the web.

# In this case, if we blanket all customer we get:
profit_all2 = sum(target2[,"spend"])-cost*length(target2[,"spend"])
profit_all2

# Building the Lookalike Audience
# Let's start with k-Means

# First we need to create the "Seed"
# Valuable Customers
# What is our cutoff? $10
cs2=cust2[cust2$spend>10,2:12]
cs2$spend <- NULL
CV = mean(cust2[cust2$spend>10,"spend"]); CV
EV = mean(cust2[,"spend"]); EV

# In this case we have to  use model matrix 
# because variables are categorical (I included ecom_index)
seed2=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index+ecom_index-1, data=cs2)

# Note -1 is because we dont need an intercept

# Transform the target dataset to match cust 

ts2 = target2[,2:12]
ts2$spend <- NULL

targ2=model.matrix(~census_region 
                  +household_size+hoh_oldest_age    
                  +household_income+children         
                  +racial_background+connection_speed  
                  +country_of_origin+retail_index+ecom_index-1, data=ts2)

# Now make sure all variables (names) match
# If not, retain only those that do
targ2=targ2[,intersect(colnames(seed2),colnames(targ2))]


# Let's use FNN

# Use package
# library(FNN) # Already loaded

# Lets do this for different k
# Storage
profit_dmp=rep(0,20)

# Loop
for(k in 1:20){
  mk = get.knnx(targ2,seed2,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit_dmp[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

plot(profit_dmp,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
# Best k
k.star = which.max(profit_dmp); k.star
mk.star = get.knnx(targ2,seed2,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit_dmp.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])

profit_dmp.star

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)

# What percentage did we capture?
perc.captured.dmp = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured.dmp

# Can we do better?
# Which variables really matters?
# This is a very naive approach
# Let's regress spend on variables
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index+ecom_index, data=cust2)
summary(reg)


# Redo knn with 
seed2 = with(cs2,cbind(ecom_index,household_income==6,household_size==6))
targ2 = with(target2,cbind(ecom_index,household_income==6,household_size==6))
# Storage
profit2_dmp=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ2,seed2,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit2_dmp[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

# Plot
plot(profit2_dmp,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star = which.max(profit2_dmp); k.star
mk.star = get.knnx(targ2,seed2,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit2_dmp.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"]); profit2_dmp.star

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured

# How do the two compare
plot(profit2_dmp,type='b',pch=19,ylim=c(-5000,5000))
lines(profit_dmp,type='b', col='blue')
lines(profit2,type='b', col='red')
lines(profit,type='b', col='green')
abline(h=0,lty=2)
legend("bottomleft", bty="n",
       fill=c("black","blue","red","green"),legend=c("New_Dmp_selec_var","New_Dmp_all_var","selec_var","all_var"))
