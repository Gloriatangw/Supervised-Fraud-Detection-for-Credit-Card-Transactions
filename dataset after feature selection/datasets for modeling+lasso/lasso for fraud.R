library(dplyr)
library(lars)
library(glmnet)
library(leaps)
library(caret)

train_input=read.csv('train after ks.csv')
test=read.csv('test after ks.csv')
oot=read.csv('out_of_date after ks.csv')
# lasso
library(glmnet)
'%ni%'= Negate('%in%')
train_lasso = na.omit(train_input)
fctr = lapply(train_lasso[sapply(train_lasso, is.factor)], droplevels)
sapply(fctr, nlevels)
x = model.matrix(Fraud~.,data=train_lasso)
x=x[,-1]
View(x)
glmnet1=cv.glmnet(x=x,y=train_lasso$Fraud,type.measure='mse',nfolds=5,alpha=.5)

c=coef(glmnet1,s='lambda.min',exact=TRUE)
inds=which(c!=0)
variables=row.names(c)[inds]
variables=variables[variables %ni% '(Intercept)']
variables=c(variables,"Fraud")
variables
train=train_input[,variables]
test=test[,variables]
oot=oot[,variables]
write.csv(train,"train after lasso.csv",row.names = FALSE)
write.csv(test,"test after lasso.csv",row.names = FALSE)
write.csv(oot,"oot after lasso.csv",row.names = FALSE)
View(train)
