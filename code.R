
#library(network)
#library(sna)
library(sand)
library(RSiena) 
library(faraway)

#I have data on a friendship network of high school students.  The students
#identified their friends.  They also answered a series of questions about
#their personal behavior (ie- how often do you drink alcohol, and how often do you smoke marijuana)
#I am curious, do friends behave similarly (homophily)?  That is, is there a relationship
#between similar habits and friendship?  

#Step one, look at the data

load("Glasgow-friendship.RData")
load("Glasgow-selections.RData")
load("Glasgow-substances.RData")

head(alcohol)
head(cannabis)
head(friendship.1)

#this code goes through all of the variables I will be using and picks out the 129 students 
#who were available during the entire testing period.  We are only going to 
#look at friendship and substance use at time 1.  This gets rid of all missing data.

friendship<-friendship.1[which(selection129=="TRUE"),which(selection129=="TRUE")]
alcohol<- alcohol[which(selection129=="TRUE"),]
pot = cannabis[which(selection129=="TRUE"),]

#Students could code 1 (best friends) 2 (friends) or 0 (not friends) 

friendship[friendship == 2] <- 1 #just friends
friendship[friendship == 10] <- 0 #missing data
alcohol[is.na(alcohol)] <- 0 #missing data work around


set.seed(10)
graph1=graph.adjacency(friendship, mode = "undirected", weighted=TRUE)
plot(graph1, vertex.label = NA, vertex.size = 5)
title("Friendship Network")

#looking at connected subgraph only.
confriends = friendship[which(clusters(graph1)$membership == 1), which(clusters(graph1)$membership ==1)]

#Now we need to force our matix to be symmetric
pmean <- function(x,y) (x+y)/2
confriends[] <- pmean(confriends, matrix(confriends, nrow(confriends), byrow=TRUE))

#We need to make the response be a vector of possible friendships
to.upper <- function(X) t(X)[lower.tri(X)]
Y <- to.upper(confriends)  #(check: should be a vector of size 121 choose 2)
Y[which(Y==.5)] = 1 #friends or not friends...ignore the rest for now

#the covariates must describe the friends TOGETHER.  If they drink the same amount they get a high
#score...if they drink different amounts they get low scores.  Same goes for smoking. 
X1 <- alcohol[which(clusters(graph1)$membership == 1),1]
X2 = pot[which(clusters(graph1)$membership == 1),1]

score = matrix(NA, nrow = dim(confriends)[1], ncol = dim(confriends)[1])

###For Alcohol###
for (i in 1:dim(confriends)[1]){
  score[,i] = abs(X1[i]-X1)
}
alcscore <- to.upper(score)
alcscore <- exp(-alcscore)
alcscore[which(alcscore == min(alcscore))] <- 0

###For Pot###
for (i in 1:dim(confriends)[1]){
  score[,i] = abs(X2[i]-X2)
}
potscore <- to.upper(score)
potscore <- exp(-potscore) #similar friends = small differences
potscore[which(potscore == min(potscore))] <- 0

plot(jitter(Y) ~ jitter(alcscore))
plot(jitter(Y) ~ jitter(potscore))

####we have a response and two predictor variables.  Lets fit a glm
mod1 <- glm(Y ~ alcscore + potscore, family = "binomial")
summary(mod1)
#is our model better than the null model?  Deviance measures how close the model comes to perfection.
#A smaller deviance is better! Null deviance is comparing the null model to the perfect model.
#Residual deviance is the deviance for the current model.  Has there been an improvement?  
#Provided that our data is truly binomial, the deviance has a chi-square distribution.  
anova(mod1, test = "Chi")
improve <- 1-pchisq(2599 - 2583.6, 2)


#interpreting coefficients: a unit increase in alcohol score increases the log odds of friendship by
#Beta 1 (holding everything else constant)

#recall the setup for binomial regression log(p_i/1-p_i) = eta_i = X_i^T B + e
pfZEROSCORES <- ilogit(mod1$coefficients[1])
pfONESCORES <- ilogit(mod1$coefficients[1]+mod1$coefficients[2]+mod1$coefficients[3])



