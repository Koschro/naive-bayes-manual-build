
d1	<- c(1,	0,	0,	1,	0,	0,	0,	1)
d2	<- c(0,	1,	0,	1,	1,	0,	0,	0)
d3	<- c(0,	0,	1,	0,	0,	0,	1,	0)
d4	<- c(0,	0,	0,	1,	0,	0,	0,	0)
d5	<- c(0,	0,	0,	0,	0,	0,	1,	0)
d6	<- c(0,	0,	0,	1,	0,	1,	0,	1)
d7	<- c(0,	0,	1,	0,	0,	1,	0,	1)
d8	<- c(1,	0,	0,	0,	0,	0,	0,	1)
d9	<- c(0,	0,	0,	0,	0,	1,	0,	1)
d10	<- c(1,	1,	0,	0,	0,	1,	0,	1)
nb_df	<- as.data.frame(rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
names(nb_df) <- c("BadCredit", "HasStableJob", "OwnsHouse", "BigLoan",	
                  "HasLargeBankAccount", "HasPriorLoans", "HasDependents", "Decision")

pr0 <- (sum(nb_df$Decision==0)/nrow(nb_df))
pr1 <- 1-pr0
priors <- c(pr0,pr1)
priors

rows <- aggregate(nb_df[,-8], by= list(nb_df$Decision), sum)
row1<- rows[1,2:8]/sum(nb_df$Decision==0)
row2<- rows[2,2:8]/sum(nb_df$Decision==1)
PF <- rbind.data.frame(row1,row2)
row.names(PF) <- c("P(Fi=1|Class=0)", "P(Fi=1|Class=1)")
PF

#At the first row we have computed the propability that the feature takes the value 1 
#for class 0.In order for the two rows to add up to 1s we would have to present in the second row the complementary possibility which is the propability that the feature takes the value 0 for class 0. In the second row we do not have that propability but the one that the feature takes the value 1 for class 1.

PFa <- 1-PF[,]
row.names(PFa) <- c("P(Fi=0|Class=0)", "P(Fi=0|Class=1)")
PFa

#According to our model,the propability that the bank will decide to give out a loan(Decision = 0) when the customer has a bad credit score(BadCredit = 1) is 0.This is going to be a problem during prediction due to the fact that as we do not have no observations in the matrix with BadCredit=1 for the class of 0 and thus the accurancy of the predictions will be reduced.In technical terms, the propability P(F1=0|C=0) = 0 will result in P(C=0|F1=1) = 0.It should be mmentioned that the question was answered based on the assumption that there is a typographic mistake in the question itself at the part where it states:"when the customer has a bad credit score(BadCredit = 0)".Propably what the question meant was (BadCredit = 1).

rows <- aggregate(nb_df[,-8], by= list(nb_df$Decision),sum)
row1<- (rows[1,2:8]+1)/(sum(nb_df$Decision==0)+2)
row2<- (rows[2,2:8]+1)/(sum(nb_df$Decision==1)+2)
PFb <- rbind.data.frame(row1,row2)
row.names(PFb) <- c("P(Fi=1|Class=0)", "P(Fi=1|Class=1)")
PFb

predict_nb	<- function(test_df, priors, prob_matrix)	{
  pr0<-c()
  pr1<-c()
  prediction <- c()
  
  for (i in 1: nrow(test_df)) {
    for (n in 1:length(test_df)) {
      if (test_df[i,n]=='1') {      
        pr0 <-c(pr0, prob_matrix[1,n])
        pr1 <-c(pr1, prob_matrix[2,n])
      }    
      else {
        pr0 <- c(pr0,(1-prob_matrix[1,n]))
        pr1 <-c(pr1,(1-prob_matrix[2,n]))
      }
    }
    x0 <- priors[1] * prod(pr0)
    x1 <- priors[2] * prod(pr1)
    
    if (x0[1]> x1[1]) {
      prediction <- c(prediction, 0) }
    else { 
      prediction <-  c(prediction, 1)
    }  
  }
  return(prediction)
}

preds<-predict_nb(nb_df[-8], priors, PFb)
preds

confusion_Matrix<- table(preds, nb_df$Decision )
accuracy <- (confusion_Matrix[1,1]+confusion_Matrix[2,2])/sum(confusion_Matrix)
accuracy

