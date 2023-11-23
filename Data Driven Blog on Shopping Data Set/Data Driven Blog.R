library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)
shopping_data <- shopping_behavior_updated
summary(shopping_data)
##################################################################################
################################# Part A #########################################
############################# Null Hypothesis ####################################
#The average spending on clothing by individuals over the age of 60, whether they are men or women, 
#does not exhibit a significant difference.
############################ Alternative Hypothesis ##############################
#The average spending on clothing by females over the age of 60 is higher than that of males in 
#the same age group.
############################ Permutation Testing #################################
data23 <- shopping_data[shopping_data$Age>60,]
tapply(data23$`Purchase Amount (USD)`,data23$Gender,mean)
x <- permutation_test(data23,"Gender","Purchase Amount (USD)",1000,"Male","Female")
x ## p-value less then 0.05.
################################# Z-Test #########################################
y<-z_test_from_data(data23,"Gender","Purchase Amount (USD)","Male", "Female")
y # P-Value less then  0.05 
# Understand z-Value that u get from z-Test

##################################################################################
################################ Part B ##########################################
# Formulate at least one Bayesian Odds Task and use Odds version 
# of Bayesian Theorem (as in section 13) to calculate posterior odds.
# Compute the odds that Jeans are bought by Females..
# Observation is that jeans are bought. 
# Belief is that jeans are bought by females.
##################################################################################
p1 <- nrow(shopping_data[shopping_data$`Item Purchased`=="Jeans",])
p1 # Total number of times where jean is bought
p2 <- nrow(shopping_data)
p2 # Total number of Rows where something is bought
prior<- p1/p2
prior # All the Jeans are bought by Females.
priorodds <- round(prior/(1-prior),2)
priorodds ## Converting to Odds
TrueP <- nrow(shopping_data[shopping_data$Gender=="Female"& shopping_data$`Item Purchased`== "Jeans",])/nrow(shopping_data[shopping_data$`Item Purchased`=="Jeans",])
TrueP
## True Positive jeans are bought buy females. 
FalseP <- nrow(shopping_data[shopping_data$Gender=="Female"& shopping_data$`Item Purchased`!= "Jeans",])/nrow(shopping_data[shopping_data$`Item Purchased`!="Jeans",])
FalseP ## False Positive  all the others items except Jeans that are bought by females. 
LHR <- round(TrueP/FalseP,2)
LHR # LikelihoodRatio
posteriorOdds <- LHR*priorodds
posteriorOdds ## Posterior Odds 
Posterior <- posteriorOdds/(1+posteriorOdds) # Converting back to Probability
Posterior 
####################################################################################
################################# Plots ############################################
f1 <- shopping_data[shopping_data$Gender=="Female"& shopping_data$`Item Purchased`== "Jeans",]
f2 <- shopping_data[shopping_data$Gender=="Male"& shopping_data$`Item Purchased`== "Jeans",]
f3 <- shopping_data[shopping_data$`Item Purchased`=="Jeans",]$Gender
f4 <- table(f3)
colors2 = c("blue","black")
barplot(f4,col = colors2,main ="Unveiling the Gender Divide in Jeans Retail Trends",xlab ="Gender",ylab = "# of Jeans Bought")



###################################################################################
########################### Exploration ###########################################
tapply(shopping_data$`Purchase Amount (USD)`,shopping_data$Gender,mean)
p00 <- permutation_test(shopping_data,"Gender","Purchase Amount (USD)",100,"Male","Female")
p00
data23 <- shopping_data[shopping_data$Age>60,]
nrow(data23)
nrow(data23[data23$Gender=="Female",])
nrow(data23[data23$Gender=="Male",])

data24 <- shopping_data[shopping_data$Age>60 & shopping_data$Gender=="Female",]$`Purchase Amount (USD)`
mean(data24)
tapply(data23$`Purchase Amount (USD)`,data23$Gender,mean)
f1 <- sum(shopping_data[shopping_data$Gender=="Female"&shopping_data$Category== "Clothing",]$`Purchase Amount (USD)`)
f1
f2 <- sum(shopping_data$`Purchase Amount (USD)`)
f2
c2 <- sum(shopping_data[shopping_data$Gender=="Female"& shopping_data$Category== "Clothing",]$`Purchase Amount (USD)`)
c2
c3 <- sum(shopping_data[shopping_data$Gender=="Female"& shopping_data$Category== "Clothing",]$`Purchase Amount (USD)`)
c3
c4 <- sum(shopping_data[shopping_data$Gender=="Female",]$`Purchase Amount (USD)`)
c4
c5 <- sum(shopping_data[shopping_data$Gender=="Male",]$`Purchase Amount (USD)`)
c5
