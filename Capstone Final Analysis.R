library(base)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(lm.beta)
library(ranger)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

#Capstone Regression
setwd("C:/Users/lucyc/Dropbox/Columbia Master/Capstone - Leadership Personality/Capstone-Final")
data= read.csv('master_data_IBM_extract.csv',header = TRUE)
#Split Data
set.seed(1)
split = sample.split(data$CEO,SplitRatio = 0.7)
train = data[split,]
test = data[!split,]
summary(data)

#we do regression only on traits that are trainable - based on what we pre-defined

#Recommend.to.a.Friend
#Ranger
set.seed(1)
model_recommend_friend= ranger(Recommend.to.a.Friend ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability, train)
pred_recommend_friend = predict(model_recommend_friend, test)
sse_recommend_friend = sum((pred_recommend_friend$predictions - test$Recommend.to.a.Friend)^2)
sst_recommend_friend = sum((mean(test$Recommend.to.a.Friend)-test$Recommend.to.a.Friend)^2)
model_r2_recommend_friend = 1 - sse_recommend_friend/sst_recommend_friend; model_r2_recommend_friend
rmse_recommend_friend = sqrt(mean((pred_recommend_friend$predictions-test$Recommend.to.a.Friend)^2)); rmse_recommend_friend
#comment on ranger: data sample is too small, result is not valid as it change significantly with different seed, and R-square is invalid


#Stepwise test on good factors
#Recommend.to.a.Friend
start_recommend_friend = lm(Recommend.to.a.Friend ~ 1,data=data)
empty_recommend_friend = lm(Recommend.to.a.Friend~1,data=data)
full_recommend_friend = lm(Recommend.to.a.Friend ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data)
hybridStepwise_recommend_friend = step(start_recommend_friend, scope=list(upper=full_recommend_friend, lower=empty_recommend_friend), direction='both')
summary(hybridStepwise_recommend_friend)
#Result: Cannot conclude

#The result is aligned with glm model with family = gaussian
#start_recommend_friend = glm(Recommend.to.a.Friend ~ 1,family=gaussian,data=data)
#empty_recommend_friend = glm(Recommend.to.a.Friend~1,family=gaussian,data=data)
#full_recommend_friend = glm(Recommend.to.a.Friend ~ Adventurousness+Artistic.interests+Emotionality+Imagination+Intellect+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Cheerfulness+Excitement.seeking+Friendliness+Gregariousness+Altruism+Cooperation+Modesty+Morality+Sympathy+Trust+Anger+Anxiety+Depression+Immoderation+Self.consciousness+Vulnerability,family=gaussian,data=data)
#hybridStepwise_recommend_friend = step(start_recommend_friend, scope=list(upper=full_recommend_friend, lower=empty_recommend_friend), direction='both')
#summary(hybridStepwise_recommend_friend)

#CEO.Approval
start_CEO_Approval = lm(CEO.Approval ~ 1,data=data)
empty_CEO_Approval = lm(CEO.Approval~1,data=data)
full_CEO_Approval = lm(CEO.Approval ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data)
hybridStepwise_CEO_Approval = step(start_CEO_Approval, scope=list(upper=full_CEO_Approval, lower=empty_CEO_Approval), direction='both')
summary(hybridStepwise_CEO_Approval)
#Result: Sympathy and Vulnerability are most significant to CEO Approval
#Suggestion: we could also associate the item with items that highly correlates with it

#Positive_Bus_Outlook
start_Positive_Bus_Outlook = lm(Positive.Business.Outlook ~ 1,data=data)
empty_Positive_Bus_Outlook = lm(Positive.Business.Outlook~1,data=data)
full_Positive_Bus_Outlook = lm(Positive.Business.Outlook ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data)
hybridStepwise_Positive_Bus_Outlook = step(start_Positive_Bus_Outlook, scope=list(upper=full_Positive_Bus_Outlook, lower=empty_Positive_Bus_Outlook), direction='both')
summary(hybridStepwise_Positive_Bus_Outlook)
#Result: This is not depend on CEO

#Correlation plot - version 2
corMatrix = as.data.frame(cor(data[,3:32]))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:29)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

#Tree -- I used default ANOVA for it cp= 0.005
#Recommend_to_friend
tree_recommend_friend = rpart(Recommend.to.a.Friend ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data,control=rpart.control(minbucket=1),cp=0.005)
rpart.plot(tree_recommend_friend)
#CEO Approval
tree_CEO_Approval = rpart(CEO.Approval ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data,control=rpart.control(minbucket=1),cp=0.005)
rpart.plot(tree_CEO_Approval)
#Business Outlook
tree_Positive_Bus_Outlook = rpart(Positive.Business.Outlook ~ Adventurousness+Emotionality+Imagination+Liberalism+Achievement.striving+Cautiousness+Dutifulness+Orderliness+Self.discipline+Self.efficacy+Activity.level+Assertiveness+Altruism+Cooperation+Sympathy+Trust+Anger+Immoderation+Self.consciousness+Vulnerability,data=data,control=rpart.control(minbucket=1),cp=0.005)
rpart.plot(tree_Positive_Bus_Outlook)