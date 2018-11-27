#Rpart is a line of code that gives the decision tree algorithm that will train our training data

library(rpart)
#THIS IS THE ALGORITHMIC (FORMULA) THAT CREATES A DECISION TREE FLOWCHART
predicted_age <-  rpart(Age ~ Pclass+Sex+SibSp+Parch+Fare+Embarked+Titles,
                        data=combined[!is.na(combined$Age),],method="anova")


library(partykit)
#THIS GIVES THE PLOT OF THE DECISION TREE
plot(as.party(predicted_age))

#THIS IS THE PREDICTION ON THE TARGET VARIABLE RETURNED AS A NUMERIC VALUE IN THE NA VALUES OF AGE COLUMN
combined$Age[is.na(combined$Age)]<- 
                   predict(predicted_age,newdata=combined[is.na(combined$Age),])



library(VIM)# gives a plot of aggregated NA values of each column
aggr(combined)


predict.model <-  rpart(Survived~ Pclass+Age+Sex+SibSp+Parch+Fare+
                          Embarked+family+family_cat+Titles,
                        data= newtrain , method="class")


plot(as.party(predict.model))

#THIS IS THE PREDICTION ON THE TARGET VARIABLE "SURVIVED" RETURNED AS A FACTOR WITH 2 LEVELS

model.build <-  predict(predict.model , newdata= newtest , type="class")

solution <-  data.frame ( passengerid= newtest$PassengerId , Survived = model.build)

write.csv(solution , "dectree2.csv" , row.names= F )

library(randomForest)

#this is the random forest algorithm training the newtrain data

model.rf <-  randomForest(as.factor(Survived)~ Pclass+Age+Sex+SibSp+Fare+
                            Embarked+family+Titles , data= newtrain , method="rf")



#prediction of the rf model on the newtest data and returned as a factor with 2 levels of 1 or 0


predict.rf <-  predict(model.rf , newdata=newtest , type="response")

solution <-  data.frame (passengerid= test$PassengerId , Survived =predict.rf)

write.csv (solution , "randomforesttt.csv" , row.names=F)

















































