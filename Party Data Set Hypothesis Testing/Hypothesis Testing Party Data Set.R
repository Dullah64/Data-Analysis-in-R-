library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)
party <- read.csv("https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/party2023.csv")
View(party)
alldj <- party$DJ
alldj
tapply(party$Attendance,alldj,mean) ## Average mean of attendence...
###########################################################################
############################### Hypothesis 1 (C) ##########################
# Alternative Hypotheses
# HipHop parties mean of attendance is higher on Friday than on Thursday.
# Null Hypotheses
#There is no difference between the Hiphop parties attendance mean on Thursday
# and on Friday.
tapply(party[party$Music == "HipHop",]$Attendance,party[party$Music == "HipHop",]$Day,
       mean)
party0 <- party[party$Music == "HipHop" & (party$Day == "Thursday" | party$Day==
                                             "Friday"), ]
party0
                                ##### Permutation Test ###
p1 <- permutation_test(party0,"Day","Attendance",10000,"Thursday","Friday")
p1 # 0.102 meaning we can not reject the null Hypothesis.. 
# There is not a significant difference between the mean of the Hiphop 
                                        #Dance on Friday and Thrusday.

################################################################################
################################# Hypothesis 2 (B) #############################
# Alternative Hypothesis: The attendance mean at DJ Blue  on Friday is higher than 
# at DJ Alex Party...
# Null Hypothesis: There is no difference in the attendence mean of DJ Blue Party on
# Friday and DJ Alex party on Friday....
tapply(party$Attendance,party$DJ,mean)
tapply(party[party$DJ == "Blue",]$Attendance,party[party$DJ == "Blue",]$Day
       ,mean)
tapply(party[party$DJ == "Alex",]$Attendance,party[party$DJ == "Alex",]$Day
       ,mean)
                           ##Permutation Test ##

party1.1 <- party[(party$DJ == "Blue" | party$DJ == "Alex") & (party$Day == "Friday"), ]
p2 <- permutation_test(party1.1,"DJ","Attendance",10000,"Alex","Blue")
p2 # P value of 0.0154...< 0.05 Means we can reject the null hypothesis thesis.....
# So, The attendance mean at DJ Blue  on Friday is higher than 
# at DJ Alex Party...

################################################################################
#################################### Hypothesis 3 (A) ##########################
# Alternative Hypothesis 
# The mean of Attendance of HipHop played by DJ Carol on Saturday is lower than
# HipHop Played by DJ Ania on Saturday....
# Null Hypothesis: There is no difference between the mean of 
 #Attendance of HipHop played by DJ Carol on Saturday and 
# HipHop Played by DJ Ania on Saturday....

tapply(party[party$DJ == "Ania" & party$Music == "HipHop",]$Attendance,
       party[party$DJ == "Ania" & party$Music == "HipHop",]$Day,mean)

tapply(party[party$DJ == "Carol" & party$Music == "HipHop",]$Attendance,
       party[party$DJ == "Carol" & party$Music == "HipHop",]$Day,mean)

                        ### Permutation Test ###
party1.2 <- party[(party$DJ == "Ania" | party$DJ == "Carol") & party$Music == "HipHop"
                  & party$Day== "Saturday",]
party1.2
p1.2   #  Making a subset of the data frame to only get the  data we need for our Hypothesis Testing.
p2 <- permutation_test(party1.2,"DJ","Attendance",10000,"Carol","Ania")
p2    # 0.0092
