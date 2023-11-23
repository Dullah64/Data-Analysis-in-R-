library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)
Car_Data <- read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/Cars2022.csv")
View(Car_Data)
## , find the Car maker (brand) C which has the highest mean age of buyers.   
tapply(Car_Data$Buyer_Age,Car_Data$Car,mean)  ## Ram 

######################################### Hypothesis Testing ################################
p1 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Chevrolet","Ram")
p1 # 
p2 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Ford","Ram")
p2 # 
p3 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"GMC","Ram")
p3 # 
p4 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Honda","Ram")
p4 # 
p5 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Hyundai","Ram")
p5#
p6 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Jeep","Ram")
p6 # 
p7 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Kia","Ram")
p7 # 
p8 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Nissan","Ram")
p8 # 
p9 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Subaru","Ram")
p9 # 
p10 <- permutation_test(Car_Data,"Car","Buyer_Age",1000,"Toyota","Ram")
p10

