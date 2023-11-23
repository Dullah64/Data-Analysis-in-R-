library(readr)
library(readxl)
data1 <- read.csv(file.choose())
View(data1)
summary(data1)
#######################################################################################
############################ 1st interesting find #####################################
total_div <- data1[data1$marital.status == 'Divorced',]
total_div #Total number of people divorced
d1 <-nrow(data1[data1$marital.status == 'Divorced'& data1$income..50K <=0, ])
d1 # Total number of people that make less than 50k and are divorced
d2 <-nrow(data1[data1$marital.status == 'Divorced'& data1$income..50K >0, ])
d2 # Total number of people that are divorced and make more than 50k
t2 <-table(data1$income..50K,data1$marital.status) # Table of income and martial status
t2
d4 <- d1/(d2+d1) # percentage of people that are divorced with low then 50k
d4               # are divorced according to this data..
d5 <- d2/(d1+d2) # This shows that approximately 10% of people with high income
d5               # are divorced
colors <- c("red","blue")
barplot(t2,xlab ="marital.status",ylab="income..50k",col = colors,
        main = "martial status  vs Income",border ="black") #Bar plot

#######################################################################################
################################ 2nd Interesting Find #################################
table(data1$sex)
gender <-factor(data1$sex) # Making Levels for gender
t3 <-tapply(data1$hours.per.week,gender,mean) # calculating the mean of the hours
colors1 <- c("purple","green") #Colors
barplot(t3,xlab ="sex",ylab="hours.per.week",col = colors1,
        main = "Sex Vs Hours per week",border ="black") #Bar Plot
male <- data1[data1$sex =="Male",]$hours.per.week
male
female <- data1[data1$sex =="Female",]$hours.per.week
female
boxplot(male, main="Male Hours Per Week", 
        ylab="Hours", col="blue")    #Box plot for Male
boxplot(female,main="Female Hours Per Week",
        ylab = "Hours",col = "red")  # Box plot for Female

###############################################################################
################################ 3rd Interesting Find #########################
unique(data1$workclass)
ave <- data1[data1$workclass == "Private",]$age
ave  # Ages on people that work at private jobs
ave2 <- data1[data1$workclass == "State-gov"|data1$workclass == "Local-gov"|
             data1$workclass == "Federal-gov",]$age #Ages of people that work
ave2                                              # Goverment jobs
boxplot(ave,main ="Average Ages of People having Private Jobs",
              ylab ="Ages",col = "red")
boxplot(ave,main ="Average Ages of People having Gov Jobs",
        ylab ="Ages",col = "purple")
summary(ave) # Summary of Private Jobs
summary(ave2) # Summary of Gov Jobs
