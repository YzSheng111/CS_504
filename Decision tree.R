maindata<-read.csv("~/Desktop/CS 504/Project/Project2/project/Main.Data.csv")
library(party)
library(partykit)

at<-as.data.frame(table(maindata$attacktype1))
at.f<-at[1:2760,2]
at.f[is.na(at.f)] <- 0
View(at.f)

tg<-as.data.frame(table(maindata$targtype1))
tg.f<-tg[1:2760,2]
tg.f[is.na(tg.f)] <- 0
View(tg.f)

al<-as.data.frame(table(maindata$Attacks.Location))
al.f<-al[1:2760,2]
al.f[is.na(al.f)] <- 0
View(al.f)

wt<-as.data.frame(table(maindata$weaptype1))
wt.f<-wt[1:2760,2]
wt.f[is.na(wt.f)] <- 0
View(wt.f)

DI<-as.data.frame(table(maindata$Dominant.Ideology))
DI.f<-DI[1:2760,2]
DI.f[is.na(DI.f)] <- 0
View(DI.f)

success1<-as.data.frame(table(maindata$success))
sc.f<-success[1:22,2]
sc.f[is.na(sc.f)] <- 0
View(sc.f)

class(maindata$success)
maindata$success<-as.numeric(maindata$success)
son<-ifelse(maindata$success==0,"failed","success")
View(son)

sc1<-as.numeric(maindata$success)
attack_type<-as.numeric(maindata$attacktype1)
target_type<-as.numeric(maindata$targtype1)
weapon_type<-as.numeric(maindata$weaptype1)
attack_location<-as.numeric(maindata$Attacks.Location)
newdf<-data.frame(attack_type,target_type,weapon_type,attack_location)
View(newdf)
View(son)
table(son)
str(newdf)
# set up training data
trainData = data.frame(newdf,son)
rPartModel = rpart( son~attack_type+target_type+weapon_type+attack_location, data=trainData, method="anova", control=rpart.control(cp=0.01,maxdepth=6) ) 
rpartTree = as.party(rPartModel)
dev.new()
plot(rpartTree)

set.seed(2)
train<-sample(1:nrow(newdf),nrow(newdf)/2)
test=-train

trainingdata=newdf[train,]
testingdata=newdf[test,]
testingsc=son[test]

treemodel=tree(son~.,data=newdf)
plot(treemodel)
text(treemodel)
View(trainData)
rPartModel = rpart(son~., data=newdf)
rpartTree = as.party(rPartModel)
dev.new()
plot(rpartTree)

predict1<-predict(rPartModel)
rPart_yHat = predict(rPartModel,newdata=data.frame(al.f))
rtPR = postResample(pred=rPart_yHat, obs=tg.f)
rtPR

ctree()