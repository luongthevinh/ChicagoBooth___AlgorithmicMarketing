# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Homework #4 (Group)

# The persado_experiment.xlsx file contains a real experiment run by Persado
# Use the data to answer the following questions.

# Load package to load xlsx files
library(xlsx)

# Load data
df <- read.xlsx("persado_experiment.xlsx",
                sheetIndex = 1)

# Q1: Assuming that each all relevant variables were tested how many possible 
#		message combinations are there? 

# Levels per variable
intro <- 4
headline <- 4
main_text <- 2
button <- 2
action <- 2
purpose <- 4
symbol <- 2

Possible.combinations <- intro*headline*main_text*button*action*purpose*symbol; Possible.combinations
# We have 1024 possible combinations

# Analog calculation 1
library(AlgDesign)

# Full Factorial Design
mat = gen.factorial(
  levels=c(4,4,2,2,2,4,2),
  varNames=paste("V",1:7,sep=""),
  factors="all"
)

# Number of levels multiplied equals the dimension
dim(mat)



# Q2: Estimate two logit models based on the data (assuming a simple linear 
#		specification) - one for opens and another for clicks. Discuss your results based
#		on the nature of the variables (see variable names and descriptions tab).

# Load the models used by persado
ds1 <- df[1:16,8:14]
#colnames(ds1)<-paste(rep("V",ncol(ds1)),c(1:ncol(ds1)),sep="")
ds1

# For opens
# Review the experiment data
# Find those messages in our data
y.ds1 = df$unique_opened[1:16]
y.ds1
# Now build a model with that subset
mm.ds1 = model.matrix(~.,ds1)
dim(mm.ds1)
mm.ds1

# Number of messages sent
N <- df$"unique_sent"[1:16]; N

# cbind(y.ds1,N-y.ds1) = Success, No Success
sm.ds1 = summary(glm(cbind(y.ds1,N-y.ds1)~mm.ds1-1,family='binomial'))
sm.ds1

# sm.ds1.a = summary(glm(cbind(y.ds1/N,(N-y.ds1)/N)~mm.ds1-1,family='binomial'))
# sm.ds1.a
# 
# sm.ds1.b = summary(glm(log(y.ds1/(N-y.ds1))~mm.ds1-1))
# sm.ds1.b


# Intro
# Intro seems significant for all levels. Intro level 1 and 3 are the most significants 
# that is the "Thank you! Enjoy MORE Everything" and "MORE Everything has been activated" with 
# positives coefficients

# Headline
# Besides level 1: "*YOU???LL LOVE IT", only level 2 "*CONGRATS!" seems sifnificant with 
# a positive coefficient.

# Main text
# Level 2: "*Check out our selection for you ??? add a new phone, tablet, or other device!"
# has a negative coefficient meaning that it is not good to use it

# Action 
# Level 2: "*Click Here to" is also negative and significant,
# maybe people thinks that is deceiving

# Purpose
# Level 3 is also negative: "View More" but not significant

# Symbol
# Level 2 is significant "*???" and positive


# For clicks
# Review the experiment data
# Find those messages in our data
y.ds2 = df$unique_clicks[1:16]
y.ds2
# Now build a model with that subset (same experiment)
mm.ds2 = mm.ds1


# Number of messages sent
N <- df$"unique_sent"[1:16]; N

# cbind(y.ds1,N-y.ds1) = Success, No Success
sm.ds2 = summary(glm(cbind(y.ds2,N-y.ds2)~mm.ds2-1,family='binomial'))
sm.ds2

# For clicks the intercept is negative and significant. 
# V3 Level 2 is also negative. People still think is deceiving
# V5 Level 2 is positive now. So is deceiving in in the decision to open but work well once 
# people trust
# V6 Level 3 still negative and significant

# Only V1L2, V1L3, V2L2, V2L3, V2L4,V5L2 are positives.



# Q3: Use the estimated models to compute predicted probabilities for all possible message
# 		combinations (separate predictions for opens and clicks). 
#		Which messages have the highes fitted response probabilities? 
#		Are the two messages similar or different? Discuss.

# Let's Generate data from this design
# Convert to a model matrix
mm1 = model.matrix(~.,data=mat)
dim(mm1)


# For opens
# Some coefficients 
cf.mm  = sm.ds1$coeff[,1]

# Simulating Consumer behavior
# Utility
u = mm1%*%cf.mm
# probability of clicking
prob.open = 1/(1+exp(-u)); prob.open
summary(prob.open)

# find the highest
which.max(prob.open)
# get the model
mm1[which.max(prob.open),]



# For clicks
# Some coefficients 
cf.mm2  = sm.ds2$coeff[,1]

# Simulating Consumer behavior
# Utility
u = mm1%*%cf.mm2
# probability of clicking
prob.click = 1/(1+exp(-u)); prob.click
summary(prob.click)

# find the highest
which.max(prob.click)
# get the model
mm1[which.max(prob.click),]



# Q4: Please use the historical data provided in the project section of chalk to provide 
#		me your experimental design. See HistData.R for details.	  
