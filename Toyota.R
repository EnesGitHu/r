dim(toyota)
library(tidyverse)
library(caret)

model_data = toyota%>%select(price,mpg,tax,mileage)
view(model_data)
names(toyota)
levels(as.factor(toyota$fuelType))
h = table(toyota$year)

barplot(h,
        axis.lty = 1,
        col = "red")

as.data.frame(h)
barplot(table(toyota$engineSize),
        col = "blue",
        axis.lty = 1)
table(toyota$engineSize)
class(toyota$model)

class(toyota$price)

table(toyota$transmission)

names(model_data)
num = c("mpg","tax","mileage")

scaled = preProcess(model_data[,num],
                    method = c("center","scale"))


model_scaled = predict(scaled,
                       model_data)

library(ggplot2)
ggplot(data = model_data,
       aes(x = "price",y= "mileage"))+
    geom_point(size = 2)

plot(model_scaled$price,
     
     model_scaled$mileage,
     bty = "L",
     pch =19)

abline(lm(model_scaled$mileage~model_scaled$price),
       col= "red")
cor(model_data)
cor(model_scaled)
pairs(model_scaled)

view(model_scaled)

set.seed(145)
index = sample(1:nrow(model_scaled),
                  size = 0.8*nrow(model_scaled))
index

trainSet = model_scaled[index,]
testSet = model_scaled[-index,]

nrow(trainSet)
nrow(testSet)

model_scaled_1 = lm(price~mpg+tax+mileage,
                    data = model_scaled) 

names(model_scaled)
summary(model_scaled_1)


plot(model_scaled$price,
     model_scaled$tax,
     pch = 19,
     bty = "L")



distance = cooks.distance(model_scaled_1)
ort = mean(distance)
ort
olcut1 = ort*3
olcut2 = 4/length(distance)

sampleİndex = which(distance> olcut1)

model_scaled_out = model_scaled[-sampleİndex,]
nrow(model_scaled[sampleİndex,])

model_scaled_out_2 = lm(price~mpg+tax+mileage,
                    data = model_scaled_out) 

summary(model_scaled_out_2)

cor(model_scaled_out)

model_scaled_pred = predict(model_scaled_out_2,
                            testSet)

model_scaled_pred_1 = predict(model_scaled_1,
                            testSet)
view(model_scaled_pred_1)
view(model_s)
library(caret)
R2(model_scaled_pred_1,
   testSet$price)

nrow(testSet)

model_scaled_pred



View(toyota)

h = table(toyota$transmission)
View(USvideos)

View(toyota)
library(tidyverse)
yea = c("2016","2017","2018","2019","2020")

model_new =toyota%>%filter(year == 2016|year == 2017|year == 2018|year == 2019
                |year == 2020)%>%filter(transmission == "Manual"|transmission =="Automatic")


class(model_new$transmission)
model_new$transmission= as.character(model_new$transmission)

view(model_new)
table(model_new$year)
table(model_new$transmission)
levels(as.factor(model_new$transmission))

nrow(toyota)
nrow(model_new)

set.seed(155)

num = c("mileage","tax","mpg")
scaled = preProcess(model_new[,num],
           method = c("center","scale"))

model_scaled = predict(scaled,
                       model_new)


nrow(model_scaled)

View(toyota)
length(table(toyota$mpg))
length(table(toyota$engineSize))


index
View(model_scaled)

model_scaled_1 = model_scaled%>%mutate(year = as.factor(year),
                      engineSize = as.factor(engineSize))

names(model_scaled)
modelDummmy = model.matrix(price~.,
                           data = model_scaled_1)

head(modelDummmy,10)                           
class(model_scaled_1$year)
view(model_scaled_1)
ncol(modelDummmy)


View(modelDummmy)

dim(modelDummmy)


set.seed(155)
index = sample(1:nrow(modelDummmy),
               size = 0.8*nrow(modelDummmy))

trainSetX = modelDummmy[index,]
testSetX = modelDummmy[-index,]

nrow(trainSetX)
nrow(testSetX)

trainSetY = model_scaled_1$price[index]
testSetY = model_scaled_1$price[-index]


library(glmnet)
library(tidyverse)
model_Gl = glmnet(trainSetX,
                  trainSetY,alpha = 0,lambda = 0.05)


model_Gl$beta
model_Gl$lambda

a = toyota%>%group_by(model)%>%
    summarise(mean(price))

write.csv(a,"fiyat")
View(toyota)
toyota$model


plot(a$model,
     a$`mean(price)`)


library(ggplot2)

fi = ggplot(data = a,
       aes(x = a$model,
       y = a$`mean(price)`,
       fill = a$model))+
    ylab("ortalama")+xlab("model")+geom_col(position = "dodge")
?geom_line
fi

lambdas = 10^seq(-3,2,by = 0.01)
lambdas
RidgeCv = cv.glmnet(trainSetX,
                    trainSetY,lambda = lambdas,nfolds = 10)


plot(RidgeCv)

RidgeCv$cvm
bestLambda = RidgeCv$lambda.min
RidgeCv$glmnet.fit

bestLambda

fitGl = glmnet(trainSetX,
               trainSetY,lambda = bestLambda,alpha = 0)

fitGl$dev.ratio
fitGl$beta
model_Gl$beta
model_Gl$dev.ratio
