library(Metrics)
library(rpart)
library(rpart.plot)
rm(data)
data <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/IncomeTest2023_Kaggle.csv')
View(data)
############################################ Prediction Model ##########################################################
model1 <- lm(Salary~GPA,data=data[data$Major=="Professional",])
model2 <- lm(Salary~GPA,data=data[data$Major=="STEM",])
model3 <- lm(Salary~GPA,data=data[data$Major=="Humanities",])
model4 <- lm(Salary~GPA,data=data[data$Major=="Vocational",])
data$LinkedIN2 <- data$LinkedIN^2
model7 <- lm(Salary~LinkedIN2,data= data[data$Major=="Other",])
data$piraty <- ifelse(data$DOB %% 2 ==0,1,0)
model8 <- lm(Salary~GPA, data =data[data$Major == "Buisness" & data$piraty== 1,])
model9 <- lm(Salary~GPA, data = data[data$Major == "Buisness" & data$piraty==0,])

pred1 <- predict(model1, newdata=data[data$Major=="Professional",])
pred2 <- predict(model2,newdata =data[data$Major=="STEM",])
pred3 <- predict(model3,newdata=data[data$Major=="Humanities",])
pred4 <- predict(model4,newdata =data[data$Major=="Vocational",])
pred7 <-predict(model7,newdata = data[data$Major=="Other",] )
pred8 <- predict(model8,newdata =data[data$Major == "Buisness" & data$piraty== 1,] )
pred9 <- predict(model9, newdata =data[data$Major == "Buisness" & data$piraty== 0,] )

decision <- rep(0,nrow(data))
decision[data$Major=="Professional"] <-pred1
decision[data$Major=="STEM"] <-pred2
decision[data$Major=="Humanities"]<-pred3
decision[data$Major=="Vocational"] <-pred4
decision[data$Major=="Other"] <- pred7
decision[data$Major == "Buisness" & data$piraty== 1 ] <- pred8
decision[data$Major == "Buisness" & data$piraty== 0] <- pred9
decision
mean((decision-data$Salary)^2)


################################################################################################################################################
########################################################### Submission File ####################################################################
rm(submission)
ids<-c(1:nrow(incomeTest))
submission<-data.frame(ID=ids)
submission$Salary <- decision
write.csv(submission, 'submission.csv', row.names=FALSE)
View(submission)


#################################################################################################################################################
########################################################## Exploration ##########################################################################
v2 <- lm(Salary~LinkedIN+GPA,data=data[data$Major=="Professional",])
v2 <- lm(Salary~LinkedIN+GPA+College_location,data=data[data$Major=="STEM",])
v3 <- lm(Salary~GPA,data=data[data$Major=="Humanities",])
v4 <- lm(Salary~GPA+College_location,data=data[data$Major=="Vocational",])
data[data$Major=="Buisness",]

b3 <- data[data$Major=="Other",]
View(b3)
summary(b3)
tree7 <-rpart(Salary~.,data = b3, method= "anova",)
tree7
rpart.plot(tree7)
unique(data$Major)
nrow(incomeTest[incomeTest$LinkedIN<23,])

b4 <- data[data$Major=="Other",]
View(b4)

model7 <- lm(Salary ~ LinkedIN, data = b4[b4$Major == "Other" & b4$LinkedIN >= 38 & b4$LinkedIN <= 54,])
model8 <- lm(Salary ~ LinkedIN, data = b4[b4$Major == "Other" & b4$LinkedIN >= 54 & b4$LinkedIN <= 69,])
model9 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = b4[b4$Major == "Other" & b4$LinkedIN <= 23,])
model10 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = b4[b4$Major == "Other" & b4$LinkedIN > 69 & b4$LinkedIN <= 84,])
model11 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = b4[b4$Major == "Other" & b4$LinkedIN > 84 & b4$LinkedIN <= 101,])
model12 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = b4[b4$Major == "Other" & b4$LinkedIN > 101,])
model13 <- lm(Salary ~ LinkedIN, data = b4[b4$Major == "Other" & b4$LinkedIN >23 & b4$LinkedIN < 38,])
model14 <- lm(Salary ~ LinkedIN, data = b4[b4$Major == "Other" & b4$LinkedIN >=38 & b4$LinkedIN < 54,])
pred7 <- predict(model7, newdata = b4[b4$Major == "Other" & b4$LinkedIN >= 38 & b4$LinkedIN <= 54,])
pred8 <- predict(model8, newdata = b4[b4$Major == "Other" & b4$LinkedIN >= 54 & b4$LinkedIN <= 69,])
pred9 <- predict(model9, newdata = b4[b4$Major == "Other" & b4$LinkedIN <= 23,])
pred10 <- predict(model10, newdata = b4[b4$Major == "Other" & b4$LinkedIN > 69 & b4$LinkedIN <= 84,])
pred11 <- predict(model11, newdata = b4[b4$Major == "Other" & b4$LinkedIN > 84 & b4$LinkedIN <= 101,])
pred12 <- predict(model12, newdata = b4[b4$Major == "Other" & b4$LinkedIN > 101,])
pred13 <- predict(model13, newdata = b4[b4$Major == "Other" & b4$LinkedIN >23 & b4$LinkedIN < 38,])
pred14 <- predict(model14,newdata = b4[b4$Major == "Other" & b4$LinkedIN >=38 & b4$LinkedIN < 54,])
decision1 <- rep(0, nrow(b4))
decision1[b4$Major == "Other" & b4$LinkedIN >= 38 & b4$LinkedIN <= 54] <- pred7
decision1[b4$Major == "Other" & b4$LinkedIN >= 54 & b4$LinkedIN <= 69] <- pred8
decision1[b4$Major == "Other" & b4$LinkedIN <= 23] <- pred9
decision1[b4$Major == "Other" & b4$LinkedIN > 69 & b4$LinkedIN <= 84] <- pred10
decision1[b4$Major == "Other" & b4$LinkedIN > 84 & b4$LinkedIN <= 101] <- pred11
decision1[b4$Major == "Other" & b4$LinkedIN > 101] <- pred12
decision1[b4$Major == "Other" & b4$LinkedIN >23 & b4$LinkedIN < 38] <- pred13
decision1[b4$Major == "Other" & b4$LinkedIN >=38 & b4$LinkedIN < 54] <-pred14
decision1
mean((decision1 - b4$Salary)^2)

data <- read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/incomeTrain2023.csv')

model1 <- lm(Salary~LinkedIN+GPA,data=data[data$Major=="Professional",])
model2 <- lm(Salary~LinkedIN+GPA+College_location,data=data[data$Major=="STEM",])
model3 <- lm(Salary~GPA,data=data[data$Major=="Humanities",])
model4 <- lm(Salary~GPA+College_location,data=data[data$Major=="Vocational",])
model7 <- lm(Salary ~ LinkedIN, data = data[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN <= 54,])
model8 <- lm(Salary ~ LinkedIN, data = data[data$Major == "Other" & data$LinkedIN >= 54 & data$LinkedIN <= 69,])
model9 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = data[data$Major == "Other" & data$LinkedIN <= 23,])
model10 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = data[data$Major == "Other" & data$LinkedIN > 69 & data$LinkedIN <= 84,])
model11 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = data[data$Major == "Other" & data$LinkedIN > 84 & data$LinkedIN <= 101,])
model12 <- lm(Salary ~ LinkedIN + College_location + Tuition + DOB, data = data[data$Major == "Other" & data$LinkedIN > 101,])
model13 <- lm(Salary ~ LinkedIN, data = data[data$Major == "Other" & data$LinkedIN > 23 & data$LinkedIN < 38,])
model14 <- lm(Salary ~ LinkedIN, data = data[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN < 54,])

pred1 <- predict(model1, newdata=data[data$Major=="Professional",])
pred2 <- predict(model2,newdata =data[data$Major=="STEM",])
pred3 <- predict(model3,newdata=data[data$Major=="Humanities",])
pred4 <- predict(model4,newdata =data[data$Major=="Vocational",])
pred7 <- predict(model7, newdata = data[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN <= 54,])
pred8 <- predict(model8, newdata = data[data$Major == "Other" & data$LinkedIN >= 54 & data$LinkedIN <= 69,])
pred9 <- predict(model9, newdata = data[data$Major == "Other" & data$LinkedIN <= 23,])
pred10 <- predict(model10, newdata = data[data$Major == "Other" & data$LinkedIN > 69 & data$LinkedIN <= 84,])
pred11 <- predict(model11, newdata = data[data$Major == "Other" & data$LinkedIN > 84 & data$LinkedIN <= 101,])
pred12 <- predict(model12, newdata = data[data$Major == "Other" & data$LinkedIN > 101,])
pred13 <- predict(model13, newdata = data[data$Major == "Other" & data$LinkedIN > 23 & data$LinkedIN < 38,])
pred14 <- predict(model14, newdata = data[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN < 54,])

decision <- rep(10002.14,nrow(data))
decision[data$Major=="Professional"] <-pred1
decision[data$Major=="STEM"] <-pred2
decision[data$Major=="Humanities"]<-pred3
decision[data$Major=="Vocational"] <-pred4
decision[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN <= 54] <- pred7
decision[data$Major == "Other" & data$LinkedIN >= 54 & data$LinkedIN <= 69] <- pred8
decision[data$Major == "Other" & data$LinkedIN <= 23] <- pred9
decision[data$Major == "Other" & data$LinkedIN > 69 & data$LinkedIN <= 84] <- pred10
decision[data$Major == "Other" & data$LinkedIN > 84 & data$LinkedIN <= 101] <- pred11
decision[data$Major == "Other" & data$LinkedIN > 101] <- pred12
decision[data$Major == "Other" & data$LinkedIN > 23 & data$LinkedIN < 38] <- pred13
decision[data$Major == "Other" & data$LinkedIN >= 38 & data$LinkedIN < 54] <- pred14
decision
mean((decision- data$Salary)^2)

mean(data[data$Major=="Buisness",]$Salary)

#summary(data)
#mypre <- rep(10240,nrow(data))
#mypre
#mypre[data$Major=="Professional"]<- quantile(data$Salary,0.75)
#mypre[data$Major=="Business"& data$Major=="STEM" &data$Major=="Humantities"] <- 10000
#mse(data$Salary,mypre)
c4 <- lm(Salary~.,data=data)
summary(c4)
v1 <- lm(Salary~.,data=data[data$Major!="Other",])
summary(v1) # The model is working on the part of the data where the major!= other.(Humanities,STEM,Profesional)......

v2 <- lm(Salary ~ LinkedIN+GPA+College_location+Tuition+DOB, data = data[data$Major == "Vocational",])
#predict()
summary(v2) # This part is a little hard to figure out...
pred <- predict(v2,data = data[data$Major == "Vocational",])
#pred
mse(data[data$Major == "Vocational",]$Salary,pred)

#data[data$Major=="Buisness",]$Salary

tree9 <-rpart(Salary~.,data = data, method= "anova",control=rpart.control(minsplit = 50))
tree9
rpart.plot(tree9)
mean(data$Salary)
summary()
nrow(data[data$Major!="Other",])

subset <- data[data$Major=="Other" & data$LinkedIN>=20,]
nrow(subset)


unique(data$Major)











































