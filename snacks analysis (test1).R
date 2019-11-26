library(gains)
library(caTools)
library(ROCR)


goodforu = read.table(file.choose(),header = T,sep = ",")

attach(goodforu)
summary(goodforu$X23)

goodforu$X23 = ifelse(goodforu$X23 >= 5 ,1,0)

goodforu$X1 = NULL
goodforu$X3 = NULL
goodforu$X4 = NULL
goodforu$X5 = NULL
goodforu$X6 = NULL
goodforu$X7 = NULL
goodforu$X8 = NULL
goodforu$X10 = NULL
goodforu$X11 = NULL
goodforu$X12 = NULL
goodforu$X13 = NULL
goodforu$X14 = NULL
goodforu$X15 =NULL
goodforu$X17 = NULL
goodforu$X18 = NULL
goodforu$X19 = NULL
goodforu$X20 = NULL
goodforu$X21 = NULL
goodforu$X22 = NULL
goodforu$X24 = NULL
goodforu$X25 = NULL
goodforu$X26 = NULL
goodforu$X27 = NULL
goodforu$X28 =NULL
goodforu$X29 = NULL
goodforu$X31 = NULL
goodforu$X34 = NULL
goodforu$X35 = NULL
goodforu$X36 = NULL
goodforu$X37 = NULL


#splitting

split = sample.split(goodforu, SplitRatio = 0.7)

training = subset(goodforu,split == "TRUE")
testing = subset(goodforu,split == "FALSE")

#MOdel

Mod1 = glm(training$X23~.,training[,-1],family = "binomial")
summary(Mod1)

step(Mod1,direction = "both")

#iteration 2

Mod2 = glm(training$X23 ~ X2 + X9 + X16 + X30 + X38 + X39 + X41 + X44 +  X46 + X47 + X48 + X50 + X53 + X54 + X55 + X56 + X57 + X58 +  X59 + X61 + X62,training,family = "binomial")

summary(Mod2)

#creating Dummy variables

training$X46dum = ifelse(training$X46 >=3,1,0)	
training$X54dum = ifelse(training$X54 >=3,1,0) 	
testing$X46dum = ifelse(testing$X46 >=3,1,0)	
testing$X54dum = ifelse(testing$X54 >=3,1,0) 	


#iteration 3
training$X23 = as.factor(training$X23)

Mod3 = glm(training$X23 ~ X2 + X9 + X16 + X30 + X38 + X39 + X41 + X44 +  X46dum + X47 + X48 + X50 + X53 + X54dum + X55 + X56 + X57 + X58 +  X59 + X61 + X62,training,family = "binomial")

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

Target = testing[testing$probability>= 0.814863466 & testing$probability<=1,"Panel.ID"]
Target

Target = as.factor(Target)
Target
summary(Target)