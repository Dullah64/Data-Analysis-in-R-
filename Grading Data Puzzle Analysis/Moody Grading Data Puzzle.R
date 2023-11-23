library(readr)
file.choose()
######################### Moody Data Puzzle ################################
###########################  Exploration ###################################
summary(moody2023F2)
unique(moody2023F2$Section)
p1 <-moody2023F2[moody2023F2$Grade == "P",]$Score
p1
summary(p1)
table(moody2023F1$Score,moody2023F2$Grade)
moody1 <- subset(moody2023F2, select = c(1,5))
moody1
moody2 <- moody1[moody1$Grade == "P",]  #All the Students that Passed
length(moody2$Grade) # Number of Students that passed
moody3 <- moody1[moody1$Grade == "F",]
moody3
summary(moody2)
summary (moody3)
# The Minimun of Passing is 50 but the maximum for failing is 59.
#So, this means that some other factors are effecting the Grades.
##########################################################################
############################### Grade Vs Rows ###########################
moody56 <- moody2023F2[moody2023F2$Score > "50" & moody2023F2$Score <= "59",
                      ]$Row
moody56
moody72 <- moody2023F2[moody2023F2$Score > "50" & moody2023F2$Score <= "59",
                      ]$Grade
t1 <- table(moody72,moody56)
colors2 = c("yellow","orange")
barplot(t1,col = colors2,main ="Grade Vs Section",xlab ="Sections",ylab =
          "Grade")
###########################################################################
########################## Effect of Sections on Grade ####################
moody6 <- moody2023F2[moody2023F2$Score >= "50" & moody2023F2$Score <= "59",
                      ]$Grade #Grades of the students
moody8 <- moody2023F2[moody2023F2$Score >= "50" & moody2023F2$Score <= "59",
]$Section
moody6
b1 <-table(moody6,moody8)
colors1 = c("blue","green")
barplot(b1,col = colors1,main ="Grade Vs Section",xlab ="Sections",ylab =
          "Grade")
#########################################################################
###################### Effect of Attendence on Grade ####################
moody10 <- moody2023F2[moody2023F2$Score >= "50" & moody2023F2$Score <= "59",
]$Grade
moody11 <- moody2023F2[moody2023F2$Score >= "50" & moody2023F2$Score <= "59",
]$Attendance
t4 <- table(moody10,moody11)
t4
colors3 <- c("red","blue","yellow")
plot(t4, main = "Attendence VS Grades",col = colors3,xlab = "Grades",
     ylab ="Attendence" )
