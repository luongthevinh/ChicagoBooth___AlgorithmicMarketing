# Digital and Algorithmic Marketing
# Session 2, April 06, 2016
# Instructor: Sanjog Misra
# Homework #1 Suggested Solutions
# By Mohammad Moravvej


# Q1. Would you go with the new DMP? Justify your answer.
# You can find the grading rules for this question below. 
# Also, a suggested R code for this question is provided. Note that there are
# various ways to answer this question.

# 1- (10) Using regression to find the most relevant parameters after inducing 
#         the “ecom index” variable. Some groups compared a few models to find 
#         the one that better predicts potential buyers, instead of performing 
#         a regression. They have received (5) partial points for this effort.
#         Those who have not described their variable selection process nor the 
#         predictive variables they chose for your model will lose 5 points.

# 2- (10) Finding the highest profits that is achievable with the new DMP and 
#         comparing it with the original DMP

# 3- (5)  Calculating the new percentage captured rates [With the new DPM the 
#         capture rate improves from 56% to 85%]

# 4- (5)  Realizing that the best choice of k in the K-nearest neighbors algorithm 
#         depends upon the data, and simulating the DMP over a range of k’s.

# 5- (5)  Professional presentation of the results and conclusions.


# # Set the working directory to where you have stored "target2.rdat" and "cust2.rdat"
# setwd(" ")

# Load New files 
load(file="target2.rdat")
load(file="cust2.rdat")

cost = 3.25

# What variables should we choose for the KNN model?
# Let's regress spend on variables
reg = lm(spend~census_region 
         +household_size+hoh_oldest_age    
         +household_income+children         
         +racial_background+connection_speed  
         +country_of_origin+retail_index+ecom_index, data=cust2)
summary(reg)
# The coefficients for ecom_index, household_income6, and household_size6 are significant at 5%
# The next two important factors seems to be census_region2 and retail_index
# Here, I construct four model that all include the first 3 variables and choose the model 
# that achieves the largest profits (model 4)

# Careful the ecom_index is in the 12th column
cs2  = cust2[cust2$spend>10,c(2:10,12)]


# # Model 1 (maximum profit =  4413.61)
# seed = with(cs2,cbind(ecom_index,household_income==6,household_size==6))
# targ = with(target2,cbind(ecom_index,household_income==6,household_size==6))

# # Model 2 (maximum profit =  4423.22)
# seed = with(cs2,cbind(ecom_index,household_income==6,household_size==6,census_region==2))
# targ = with(target2,cbind(ecom_index,household_income==6,household_size==6,census_region==2))

# # Model 3 (maximum profit =  5084.34)
# seed = with(cs2,cbind(retail_index,ecom_index,household_income==6,household_size==6,census_region==2))
# targ = with(target2,cbind(retail_index,ecom_index,household_income==6,household_size==6,census_region==2))

# Model 4( maximum profit =  5090.73)
seed = with(cs2,cbind(retail_index,ecom_index,household_income==6,household_size==6))
targ = with(target2,cbind(retail_index,ecom_index,household_income==6,household_size==6))

# Storage
profit.new=rep(0,20)
# Loop
for(k in 1:20){
  mk = get.knnx(targ,seed,k=k,algorithm='brute')
  matches=unique(as.vector(mk$nn.index))
  profit.new[k] = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])
}

# Plot
plot(profit.new,type='b',pch=19)
abline(h=2,lty=2)

# Max profits
k.star = which.max(profit.new); k.star
mk.star = get.knnx(targ,seed,k=k.star,algorithm='brute')
matches=unique(as.vector(mk.star$nn.index))
profit.new.star = sum(target2[matches,"spend"])-cost*length(target2[matches,"spend"])

# The maximum profit we can acheive using this model is 
profit.new.star

# Explore matches
# How many did we really match?
table(target2[matches,"spend"]>0)

# What percentage did we capture?
perc.captured = sum(target2[matches,"spend"])/sum(target2[,"spend"])
perc.captured

# How do the two compare
plot(profit.new,type='b',pch=19,ylim=c(-500,8000))
# The maximum profit we obtained without using the “ecom index” variable (in session 2 code)
profit2.star = 3714.99
abline( h= profit2.star,lty=2)

# Q2. Is there any circomstance when you would ignore the matching and go after 
#     the entire target file audience? Justify.

# This question has 30 point. Thorough answers that analyze a few situations in which matching 
# to the existing customer set would not yield desired results will obtain the full credit. 
# Those who mentioned that targeting everyone is never a profit maximizing strategy will lose 10 point.

# 1- Explaining that for sufficiently low broadcasting costs the firm would be profit maximizing 
#    by targeting everyone. Note that the firm should way the costs and benefits of specific targeting
#    using data management platforms against each other. 

# 2- Describing situations in which firms might ignore the matching with those similar to their 
#    current high spending costumers. For instance, when a firm is in the early stages of growth 
#    and needs capturing higher market share to stay competitive. Another in which firms should 
#    ignore the matching is when the long run benefits of targeting are significant and not captured 
#    by the expenditure variable in the data. If the product has been released recently, the initial 
#    wave of customers may not represent ideal target audience.  Another scenario where a firm would 
#    benefit by broadcasting is when the firm is repositioning a product or a brand to attract a 
#    different customer segment. 

