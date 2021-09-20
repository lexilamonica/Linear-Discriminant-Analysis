# Lexi Lamonica 
######################
# All rights reserved#
######################

   #setwd("C:/Users/Lexi L/RfilesDSC324/homework4")
    heart_failure = read.csv("heartfailure.csv", header = TRUE, sep = ",")
    head(heart_failure)
  
    levels(heart_failure$DEATH_EVENT)
    heart_failure$DEATH_EVENT<-as.factor(heart_failure$DEATH_EVENT)
    dim(heart_failure)
  
    heart_failureLDA <- lda(DEATH_EVENT ~ ., data=heart_failure)
    heart_failureLDA
    #dim(heart_failureLDA)
    summary(heart_failureLDA)

# Plot
      heart_failureLDA <- lda(DEATH_EVENT ~ ., data=heart_failure)
      heart_failureLDA
      plot(heart_failureLDA)

######################################################
### predicting  from the original data      ##########
#### Cross-validation will be further below ##########
######################################################      
      p1 = predict(heart_failureLDA, newdata=heart_failure[,1:12])$class
      p1
      
# Compare the results of the prediction (Confusion Matrix)
      tabel1 <-table(p1, heart_failure$DEATH_EVENT)
      tabel1
      
      accuracy1 <-sum(diag(tabel1)/sum(tabel1))
      accuracy1
            # 0.8528428
      mean(p1 == heart_failure$DEATH_EVENT)
      
#######################################################
####         TESTING AND TRAINING            ##########
#######################################################
      install.packages("caTools")
      require(caTools)  # loading caTools library
      library(caTools)
      set.seed(123)   # ensuring it always will have same random numbers generated
      sample = sample.split(heart_failure, SplitRatio = 0.70) 
      train = subset(heart_failure, sample ==TRUE) 
      test = subset(heart_failure,  sample==FALSE)
  
  # Plots:
      heart_failureLDA = lda(DEATH_EVENT ~ ., data=train)
      heart_failureLDA
      plot(heart_failureLDA)
 
 # Predict: 
      p2 = predict(heart_failureLDA, newdata=train[,1:12])$class
      p2
      
      # Compare the results of the prediction
      tabel2 <- table(p2, train$DEATH_EVENT)
      
      accuracy2 <- (sum(diag(tabel2)/sum(tabel2)))
      accuracy2
      #0.99
      
      mean(p2 == train$DEATH_EVENT)
      #0.99
      
#######################################################
###########"Leave-one-out" cross-validation############
#######################################################
      heart_failureLDA2 = lda(DEATH_EVENT ~ ., data=train, CV=T)
      heart_failureLDA2
      
      table(heart_failureLDA2$class, train$DEATH_EVENT)
      
      coef(heart_failureLDA)
      
      library(devtools)
      install_github("fawda123/ggord")
      library(ggord)
      ggord(heart_failureLDA2, train$DEATH_EVENT, ylim=c(-5, 5))
###########################################################################
###########################################################################      
      install.packages("caret")
      library(caret)
      modelFit <- train(DEATH_EVENT ~ ., method='lda',preProcess=c('scale', 'center'), data=train)
      
      #Confusion Matrix
      confusionMatrix(train$DEATH_EVENT, predict(modelFit, train))
      
      
      
      
      

      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      