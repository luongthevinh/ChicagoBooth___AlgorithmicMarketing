# # Digital and Algorithmic Marketing
# Session 7, May 11, 2016
# Instructor: Sanjog Misra
# Based on: ZipRecruiter Experiment Project 
# Confidential - Do not share data

# Set Working Directory
setwd("~/Class/")
# Loas R data object
load(file="ZipDat.Rd")
head(dz1)
table(dz1[,"company_declared_job_slots_needed"])
names(dz1)

# Library of some utility functions
# install.packages("Hmisc") if needed
library(Hmisc)

# Range of prices
table(dz1$prc)

# Barplot of Conversion Rate  = f(Price Point)
tabS = table(dz1$prc,dz1$SUB); tabS
tabS = tabS[,2]/rowSums(tabS); tabS
NS = rowSums(table(dz1$prc,dz1$SUB)); NS
se = sqrt(tabS*(1-tabS)/NS); se
bp = barplot(tabS,ylim=c(0,.4),col='grey',ylab="Conversion Rate",xlab="Price Point")
errbar(bp[,1],tabS,tabS+1.96*se,tabS-1.96*se,add=T,xlab="",lwd=2,cex=1.25,lty=1)

# Barplots for Revenues ~ Price Point
bp2 = barplot(as.numeric(names(tabS))*tabS,ylim=c(0,55),ylab="Revenues/Customer",xlab="Price Point")
hts = as.numeric(names(tabS))*tabS
se2 = as.numeric(names(se))*se # Delta Method Used here
errbar(bp2[,1],hts,hts+1.96*se2,hts-1.96*se2,add=T)

# Simple Regression
reg1 = lm(SUB~prc,data=dz1)
reg2 = lm(SUB~prc+I(prc^2),data=dz1)
summary(reg2)

# Elasticity
tabS = table(dz1$prc,dz1$SUB)
probs = tabS[,2]/rowSums(tabS)
prcs = as.numeric(names(probs))
elas = (reg2$coef[2]+2*reg2$coef[3]*prcs)*prcs/probs; elas
# Results are not close to 1, so the demand is inelastic

# If we have tried a simpler model
elas1 = (reg1$coef[2])*prcs/probs; elas1

# Suggests inflexion around $249-$299 
g1 = glm(SUB~(factor(job_state)+factor(job_category_classified_by_ai))*prc,data=dz1,family='binomial')
# Why are there warnings? This happens because the problem is overspecified (NAs in the summary)
summary(g1)
hist(predict(g1,type='response'))
summary(predict(g1))

Odds = exp(-1.249); Odds
0.5*Odds # This is the sign up ratio

# prob implicit
prob = 1/(1+exp(1.249)); prob
# Automatically to probs like:
summary(predict(g1, type='response'))

levels(factor(dz1$job_state))
levels(factor(dz1$job_category_classified_by_ai))
# Size
#the model only states: 
61+43+61+43


# Construct Prediction Function
predict.rev = function(price,jstate="CA",jcat="Legal")
{
  #Supress Warnings
  options(warn=-1)
  # "New Data"
  nd = data.frame(job_state=jstate,job_category_classified_by_ai=jcat,prc=price)
  # Predict
  phat = predict(g1,nd,type='response')  
  # Reset Warnings
  options(warn=0)
  # Expected Revenues
  erev = as.numeric(phat*price)
  return(erev)
}

# Job Categories
jcats = unique(dz1$job_category_classified_by_ai)
jtab = table(dz1$job_category_classified_by_ai)
jtab=sort(jtab)
barplot(tail(jtab,10))
# price Seq
pseq = seq(from=49,to=399,by=10)

# Revs at each price
# Health Care in NYC
revhat = predict.rev(pseq,jstate="NY",jcat=jcats[1])
plot(revhat~pseq)
# Optimal Price / Erev
pseq[which.max(revhat)]
revhat[which.max(revhat)]

# probability of purchase
revhat[which.max(revhat)]/pseq[which.max(revhat)]

# Optimize
optimize(f = predict.rev, interval = c(0,1000), jstate="TX", jcat=jcats[1], maximum = TRUE)


# HOMEWORK
# Use the Ziprecruiter case data and code to answer the following questions:
#   
# Q1: If the costs of servicing each customer are $10 (rather than zero) what is the optimal uniform price that Ziprecruiter should charge?
# 
# Q2: Ziprecruiter is deciding between segmenting their customer base either by state or by job category and then charging a flat fee for each group within the segment (i.e. for each state or category). In other words they would charge different uniform prices for each state or job category. Use the data and the code to justify which approach you think would get them higher expected revenues.
# 
# Q3: Does your answer to Q2 change if you assume marginal costs per customer are $10 and the decision was made based on profits rather than revenues?


