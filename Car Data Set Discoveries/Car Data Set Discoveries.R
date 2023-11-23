library(readr)
################################################################################
################################ Exploring Data ################################
summary(Cars2023c_1)
mean(Cars2023c_1$Buyer_Income)
t1 <- factor(Cars2023c_1$Car)
income <- tapply(Cars2023c_1$Buyer_Income,t1,mean)
income
t1 <-Cars2023c_1[Cars2023c_1$Buyer_Income < 50000, ]$Car
t1
table(t1)
t2 <- factor(Cars2023c_1$Dealership)
tapply(Cars2023c_1$Buyer_Income,t2,mean)
################################################################################
################################# Query 1 ######################################
q1 <-mean(Cars2023c_1[Cars2023c_1$Dealership == "LA" & Cars2023c_1$Car == 
                        "Hyundai",]$Buyer_Income)
q1   #,Mean of my Query
total_mean <- mean(Cars2023c_1$Buyer_Income)
Low_Difference <- total_mean - q1
Low_Difference #The Buyers income mean from my query 
    #is $9389 lower than overall data Income Mean

################################################################################
################################# Query 2 ######################################
q2 <-mean(Cars2023c_1[Cars2023c_1$Dealership == "San Diego" & Cars2023c_1$Car == 
                        "Toyota",]$Buyer_Income)
q2
High_Difference = q2 - total_mean #The Buyers income mean from my query 
High_Difference           #is $4827 lower than the overall data Income Mean

################################################################################
###################### CROSSVALIDATION PART Lower Income #######################
Car1 <- Cars2023c_1[1:25000,]
q1.1 <-mean(Car1[Car1$Dealership == "LA" & Car1$Car == "Hyundai",]
          $Buyer_Income)
q1.1   # 154469 is the average lower income 
q1-q1.1 ### (6334) Difference from the original query #       6334
Car2 <- Cars2023c_1[25001:50000,]
q1.2 <-mean(Car2[Car2$Dealership == "LA" & Car2$Car == "Hyundai",]
            $Buyer_Income)
q1.2  # 166520 is the average lower income
q1-q1.2  #### (-5716) Difference from the orignal query.#     -5716
q1 ## 160803
Car3 <- Cars2023c_1[1:20000,]
q1.3 <-mean(Car3[Car3$Dealership == "LA" & Car3$Car == "Hyundai",]
            $Buyer_Income)
q1.3 #155324 is the Average lower income
q1-q1.3 # (5479) Difference from the original Query... #       5479

Car4 <- Cars2023c_1[30001:50000,]
q1.4 <-mean(Car4[Car4$Dealership == "LA" & Car4$Car == "Hyundai",]
            $Buyer_Income)
q1.4 #170554 is the Average lower Income
q1-q1.3   #(-9750) Difference from the original Query. #       -9750
################################################################################
###################### CROSSVALIDATION PART Higher Income ######################
q2
q2.1 <-mean(Car1[Car1$Dealership == "San Diego" & Car1$Car == "Toyota",]
            $Buyer_Income)
q2.1 # 177340 is the Average Higher Income
q2-q2.1 #(-2319) Difference from the original Query...          -2319
q2.2 <-mean(Car2[Car2$Dealership == "San Diego" & Car2$Car == "Toyota",]
            $Buyer_Income)
q2.2 # 172600 is the Average Higher Income
q2-q2.2 #(2420) Difference from the original                     2420
q2.3 <-mean(Car3[Car3$Dealership == "San Diego" & Car3$Car == "Toyota",]
            $Buyer_Income)
q2.3 #179416 is the Average Higher Income
q2-q2.3 # (-4395) Difference from the original Query...          -4395
q2.4 <-mean(Car4[Car4$Dealership == "San Diego" & Car4$Car == "Toyota",]
            $Buyer_Income)
q2.4   #173498 is the Average Higher Income
q2-q2.4 # (1522)Difference from the original Query......          1522
colnames(Cars2023c_1)
