library(rpart)
library(rpart.plot)
movies<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')
View(movies)
summary(movies)
myRatings <- rep("Great",nrow(movies))

myRatings[movies$Income<=(median(movies$Income)-(median(movies$Income)*0.213)) & ((movies$Content=="R" | movies$Content=="PG13" | movies$Content=="PG") |
                                                                                    movies$Audience>=median(movies$Income)-(median(movies$Income)*0.213))]<- "Average"

myRatings[movies$Income>=median(movies$Income)-(median(movies$Income)*0.213) & movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.077)) & (movies$Genre!="Drama" | movies$Content!="R")]<- "Average"

myRatings[movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.344)) & (movies$Content!="R" & movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.077)))] <- "Average"

myRatings[movies$Audience<(median(movies$Audience)-(median(movies$Audience)*0.344)) & movies$Income>(median(movies$Income)-(median(movies$Income)*0.242))] <-"Great"

myRatings[movies$Audience<(median(movies$Audience)-(median(movies$Audience)*0.344)) & movies$Income<(median(movies$Income)-(median(movies$Income)*0.242)) & movies$Audience>(median(movies$Audience)-(median(movies$Audience)*0.286))]<- "Average"

myRatings[movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.344)) & movies$Content!="R" & movies$Audience<=(median(movies$Audience)-(median(movies$Audience)*0.077))& movies$Income<(median(movies$Income)+(median(movies$Income)*0.235))]<- "Average"

myRatings[movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.344)) & (movies$Content!="R" & movies$Audience>=(median(movies$Audience)-(median(movies$Audience)*0.077)))] <- "Average"

myRatings
##################################################################################################
########################################## Submission #######################################
submission<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/submissionMovies2023.csv')
View(submission)
summary(movies)
submission$RATING<-myRatings
submission
write.csv(submission, 'submission.csv', row.names=FALSE)

###############################################################################
############################## Exploration ####################################
table(movies$Content,movies$RATING) # the rating of R was also 50%50
table(movies$Genre,movies$RATING)  # The rating for drama is 50%50...
table(movies$Content,movies$RATING)
subset1 <- movies[movies$Income<=5134 & (movies$Content!="R" | movies$Audience>=4000) ,]$RATING
table(subset1) # 50 Percent of the data....
subset2 <- movies[movies$Income>= 5134 ,]$RATING
table(subset2)
table(movies[movies$Income>=5134 & movies$Audience>=4500 & (movies$Genre!="Drama" | movies$Content!="R"),]$RATING) 
table(movies[movies$Income>=5134 & (movies$Audience<=4000 | movies$Content!="R"),]$RATING)
table(movies[movies$Income<=4000 & (movies$Content=="R" | movies$Audience>=4000),]$RATING)
tree99 <-rpart(RATING~Income+Content+Audience,data=movies,method= "class",control=rpart.control(minsplit = 200))
tree99
rpart.plot(tree99)
table(movies[movies$Audience>=3197 & (movies$Content!="R" & movies$Audience>=4454),]$RATING)
table(movies[movies$Audience>=3197 & movies$Content!="R" & movies$Audience<=4454& movies$Income<6645,]$RATING)
table(movies[movies$Audience<3197 & movies$Income<3853 & movies$Audience>1395,]$RATING)
table(movie[movie$Content=="R",]$RATING)


movies88<-read.csv('https://raw.githubusercontent.com/paramshah4/data101_tutorial/main/files/dataset/movies2023Test_Student.csv')
summary(movies88)


