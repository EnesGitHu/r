dim(weatherAUS)
as.data.frame(tail(weatherAUS$Date))
library(tidyverse)
## Çoklu Doðrusal Regressyon 

## Australia Rain Dataset
## Nem Oraný tahmin modeli

length(which(is.na(weatherAUS)))



df_albury = weatherAUS%>%filter(Location == "Albury")
nrow(df_albury)
view(df_albury)

table(weatherAUS$Location)

df_albury = df_albury%>%select(Humidity9am,
                   MinTemp,
                   MaxTemp,
                   WindSpeed9am,
                   Pressure9am,
                   Temp9am,
                   Rainfall)


names(df_albury)

view(df_albury)
cor(na.omit(df_albury))


##pairs(na.omit(df_albury),pch = 19)_Çoklu_Plot

weatherAUS$Rainfall


View(df_albury)
imputed = mice(data = df_albury,
          m = 5,
          defaultMethod = c("pmm"))
md.pattern(df_albury)

library(mice)
length(which(is.na(df_albury)))
names(imputed)
imputed$imp
df_albury_imputed = complete(imputed,3)


View(df_albury_imputed)
md.pattern(df_albury_imputed)


##Çoklu Doðrusal Regresyon
set.seed(145)
sampleIndex = sample(1:nrow(df_albury_imputed),size = 0.8*nrow(df_albury_imputed))
sampleIndex
trainSet = df_albury_imputed[sampleIndex,]
testSet = df_albury_imputed[-sampleIndex,]

View(testSet)
View(testSet)


model_1 = lm(Humidity9am~Temp9am+MinTemp+MaxTemp+WindSpeed9am+Pressure9am+Rainfall,
               data = trainSet)

model_1 = lm(Humidity9am~.,
             data = trainSet)
names(trainSet)


model_1

summary(model_1)

model_2 = lm(Humidity9am~Temp9am+MinTemp+MaxTemp+WindSpeed9am+Rainfall,
             data = trainSet)


summary(model_2)

AIC(model_1,k = 8)
AIC(model_2,k = 7)

BIC(model_1)
BIC(model_2)

#Artýk Plot
plot(model_2)
library(caret)
testSet2 = testSet[-5]
view(testSet2)


predictions = predict(model_2,
              testSet2)

RMSE(predictions,
     testSet2$Humidity9am)
R2(predictions,
     testSet2$Humidity9am)
MAE(predictions,
     testSet2$Humidity9am)

summary(testSet2$Humidity9am)


a = data.frame("gercek" = testSet$Humidity9am,
           "tahmin" = predictions)
View(a)


##Cook_distance

dist = cooks.distance(model_2)
ort = mean(dist)

olcut1 =ort*3
olcut2 =4/length(dist)

olcut1Index = which(dist > olcut1)
olcut2Index = which(dist>olcut2)


dist[olcut1Index]
dist[olcut2Index]


length(olcut1Index)
length(olcut2Index)


plot(1:length(dist),
     dist,
     type = "p",
     ylim = range(dist)*c(1,0.07))

abline(h = olcut1,
       col = "red")


trainsetRemeoveOut = trainSet[-olcut1Index,]
View(trainsetRemeoveOut)

## Model Karþýlaþtýrmalarý

model_3 = lm(Humidity9am~Temp9am+MinTemp+MaxTemp+WindSpeed9am+Rainfall,
             data = trainsetRemeoveOut)
model_3
summary(model_2)
summary(model_3)


AIC(model_3,k = 7)
AIC(model_2,k = 7)

BIC(model_3)
BIC(model_2)

predictions_3 = predict(model_3,
                testSet2)


RMSE(predictions,
     testSet2$Humidity9am)
R2(predictions,
   testSet2$Humidity9am)
MAE(predictions,
    testSet2$Humidity9am)


RMSE(predictions_3,
     testSet2$Humidity9am)
R2(predictions_3,
   testSet2$Humidity9am)
MAE(predictions_3,
    testSet2$Humidity9am)


## VIF Varyans Þiþkinli Faktörü

library(car)


?vif

vif(mod = model_3)




