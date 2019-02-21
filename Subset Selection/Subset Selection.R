library(dplyr)
library(leaps)


setwd("C:/Users/alok_/Google Drive/Fraud Analytics Projects/Project Three/Subset Selection/")

CCD_Exprt_DF =readRDS("expert variables")
Train_CCD_DF =readRDS("train.rds")
Test_CCD_DF =readRDS("test.rds")
OOT_CCD_DF =readRDS("oot.rds")

write.csv(CCD_Exprt_DF, file = "CCD_expert_variables.csv")
write.csv(Train_CCD_DF, file = "Train_CCD.csv")
write.csv(Test_CCD_DF, file = "Test_CCD.csv")
write.csv(OOT_CCD_DF, file = "OOT_CCD.csv")



Train_CCD_TXN_DF =read.csv("Train_CCD_2.csv")

Train_CCD_TXN_KS_DF =read.csv("Train_CCD_6.csv")

summary(Train_CCD_TXN_DF)
names(Train_CCD_TXN_DF)



leaps_lm.Train_CCD_TXN_frwd = regsubsets(Fraud ~ ., data = Train_CCD_TXN_KS_DF,nbest=1,nvmax=30,really.big=T,method="forward")
summary(leaps_lm.Train_CCD_TXN_frwd)
leaps_lm.Train_CCD_TXN_frwd_summary = summary(leaps_lm.Train_CCD_TXN_frwd)
plot(leaps_lm.Train_CCD_TXN_frwd_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq",main="FRWD Subset Selection", type="b")
plot(leaps_lm.Train_CCD_TXN_frwd_summary,scale="adjr2",main="FRWD Subset Selection")


leaps_lm.Train_CCD_TXN_bkwd = regsubsets(Fraud ~ ., data = Train_CCD_TXN_KS_DF,nbest=1,nvmax=30,really.big=T,method="backward")
summary(leaps_lm.Train_CCD_TXN_bkwd)
leaps_lm.Train_CCD_TXN_bkwd_summary = summary(leaps_lm.Train_CCD_TXN_bkwd)
plot(leaps_lm.Train_CCD_TXN_bkwd_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq",main="BKWRD Subset Selection", type="b")
plot(leaps_lm.Train_CCD_TXN_bkwd_summary,scale="adjr2",main="BKWRD Subset Selection")


leaps_lm.Train_CCD_TXN_exhst = regsubsets(Fraud ~ ., data = Train_CCD_TXN_KS_DF,nbest=1,nvmax=30,really.big=T)
summary(leaps_lm.Train_CCD_TXN_exhst)
leaps_lm.Train_CCD_TXN_exhst_summary = summary(leaps_lm.Train_CCD_TXN_exhst)
plot(leaps_lm.Train_CCD_TXN_exhst_summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq",main="Exhaustv Subset Selection", type="b")
plot(leaps_lm.Train_CCD_TXN_exhst_summary,scale="adjr2",main="Exhaustv Subset Selection")


which.max(leaps_lm.Train_CCD_TXN_exhst$adjr2)
points (21, leaps_lm.Train_CCD_TXN_exhst$adjr2[21], col ="red",cex =2, pch =20)

plot(leaps_lm.Train_CCD_TXN_exhst$bic, xlab="Number of Variables", ylab="bic",main="Exhaustv Subset Selection", type="b")
which.min(leaps_lm.Train_CCD_TXN_exhst$bic)
points (9, leaps_lm.Train_CCD_TXN_exhst$bic[9], col ="red",cex =2, pch =20)

plot(leaps_lm.Train_CCD_TXN_exhst$cp, xlab="Number of Variables", ylab="Cp",main="Exhaustv Subset Selection", type="b")
which.min(leaps_lm.Train_CCD_TXN_exhst$cp)
points (14, leaps_lm.Train_CCD_TXN_exhst$cp[14], col ="red",cex =2, pch =20)

plot(leaps_lm.Train_CCD_TXN_exhst$rsq, xlab="Number of Variables", ylab="RSq",main="Exhaustv Subset Selection", type="b")
which.max(leaps_lm.Train_CCD_TXN_exhst$rsq)
points (34, leaps_lm.Train_CCD_TXN_exhst$rsq[34], col ="red",cex =2, pch =20)

coef(leaps_lm.Train_CCD_TXN_exhst,7)


