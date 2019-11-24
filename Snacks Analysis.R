setwd("G:\\DATA SCIENCE\\Regression practice")
getwd()

library(gains)
library(caTools)
library(ROCR)


goodforu = read.table(file.choose(),header = T,sep = ",")

summary(goodforu$X23)

goodforu$X23 = ifelse(goodforu$X23 >= 5 ,1,0)


#splitting

split = sample.split(goodforu, SplitRatio = 0.7)

training = subset(goodforu,split == "TRUE")
testing = subset(goodforu,split == "FALSE")

#MOdel

Mod1 = glm(training$X23~.,training[,-1],family = "binomial")
summary(Mod1)

step(Mod1,direction = "both")

#iteration 2

Mod2 = glm(training$X23 ~ X1 + X2 + X3 + X4 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X15 + X16 + X17 + X18 + X19 + X21 + X22 + X24 +  X25 + X26 + X27 + X28 + X29 + X30 + X31 + X34 + X35 + X36 +  X38 + X40 + X41 + X43 + X44 + X47 + X49 + X50 + X51 + X54 +  X57 + X59 + X62,training,family = "binomial")

summary(Mod2)

training$X49dum = ifelse(training$X49 >=3,1,0)	
training$X54dum = ifelse(training$X54 >=3,1,0) 	
training$X57dum = ifelse(training$X57 >=3,1,0)	
training$X4dum = ifelse(training$X4 == "1",1,0) 	
training$X7dum = ifelse(training$X7 == "1",1,0)	
training$X11dum = ifelse(training$X11 == "1",1,0) 	
training$X17dum = ifelse(training$X17 =="1",1,0) 	
training$X35dum = ifelse(training$X35 >=6,1,0)  	
training$X36dum = ifelse(training$X36 >=6,1,0) 	
training$X38dum = ifelse(training$X38 >=6,1,0) 	
training$X41dum = ifelse(training$X41 >=3,1,0)	
training$X47dum = ifelse(training$X47 >=3,1,0)
###################################################
testing$X49dum = ifelse(testing$X49 >=3,1,0)	
testing$X54dum = ifelse(testing$X54 >=3,1,0) 	
testing$X57dum = ifelse(testing$X57 >=3,1,0)	
testing$X4dum = ifelse(testing$X4 == "1",1,0) 	
testing$X7dum = ifelse(testing$X7 == "1",1,0)	
testing$X11dum = ifelse(testing$X11 == "1",1,0) 	
testing$X17dum = ifelse(testing$X17 =="1",1,0) 	
testing$X35dum = ifelse(testing$X35 >=6,1,0)  	
testing$X36dum = ifelse(testing$X36 >=6,1,0) 	
testing$X38dum = ifelse(testing$X38 >=6,1,0) 	
testing$X41dum = ifelse(testing$X41 >=3,1,0)	
testing$X47dum = ifelse(testing$X47 >=3,1,0)

#iteration 3
training$X23 = as.factor(training$X23)

Mod3 = glm(training$X23 ~ X1 + X2 + X3 + X4dum + X6 + X7dum + X8 + X9 + X10 + X11dum + X12 + X15 + X16 + X17dum + X18 + X19 + X21 + X22 + X24 +  X25 + X26 + X27 + X28 + X29 + X30 + X31 + X34 + X35dum + X36dum + X38dum + X40 + X41dum + X43 + X44 + X47dum + X49dum + X50 + X51 + X54dum +  X57dum + X59 + X62,training,family = "binomial")

summary(Mod3)

#prediction

res = predict(Mod3,testing,type = "response")

res

#ROCR curve

ROCRpred = prediction(res,testing$X23)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize = TRUE,print.cutoffs.at = seq(0.1,by=0.1))

#confusion matrix
(table(actual_value =testing$X23,predicted_value = res>0.5))

(3176+3207)/(3176+3207+500+627) #85% accurate

#gain chart

gains(testing$X23,res,groups = 10)

#customer rating the snacks more than 80%

testing$probability = predict(Mod3,testing,type = "response")

Target = testing[testing$probability>= 0.804863466 & testing$probability<=1,"Panel.ID"]
Target

Target = as.factor(Target)
Target
summary(Target)