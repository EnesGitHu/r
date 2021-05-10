df = CarPrice_Assignment%>%select(wheelbase,
                             carlength,carwidth,
                             carheight,price,curbweight
                             )

df_2 =  

set.seed(145)
index = sample(1:nrow(df),size = 0.8*nrow(df))
train = df[index,]
test = df[-index,]
index
nrow(test)
nrow(train)

names(CarPrice_Assignment)
view(df)
view(train)
library(mice)
md.pattern(train)
hist(df$carlength)
hist(df$wheelbase)
hist(df$carwidth)

pairs(train,
      pch = 19)


shapiro.test(df$carlength)
mod_1 = lm(price~wheelbase+carlength+carwidth+carheight+curbweight
          ,data = train)
mod_ = lm(train$price~.,
           data = train)
names(train)
cor(train)

summary(mod_1)
distance = cooks.distance(mod_1)

ort = mean(distance) 

olcut1 =ort*3
olcut2 =4/length(dist)

idx = which(distance> olcut1)
trainOut = train[-idx,]
mod_1_out = lm(price~wheelbase+carlength+carwidth+carheight+curbweight,
                 data = trainOut)
mod_2_out = lm(price~carlength+carwidth+curbweight,
               data = trainOut)

summary(mod_1_out)
names(trainOut)
ort
distance
train[idx,]
summary(mod_2_out)
cor(trainOut)


summary(mod_1_out)
summary(mod_2_out)


mod_1_predict = predict(mod_1_out,
                        test)
mod_2_predict = predict(mod_2_out,
                        test2)
mod_2_predict
mod_1_predict
a = data.frame("Gerçek" = test2$price,
           "Tahmin" = mod_2_predict)

b = data.frame("araba" = new$CarName,
              "Gerçek" = test$price,
               "Tahmin" = mod_1_predict)

View(a)
View(b)
length(mod_1_pred)
nrow(test)

View(test)

test2 = test[-c(1,4)]
View(test2)

df[-index,]
x = (CarPrice_Assignment[-index,])
new = x%>%select(CarName)

library(tidyverse)




