library(mice)
library(tidyverse)

md.pattern(`student_info(2)`)


view(train)
view(test)

nrow(train)
dim(test)

ipm = mice(`student_info(2)`,
           m = 5,
           defaultMethod = c("pmm"))


names(ipm)

ipm$imp

`student_info(2)` = complete(ipm,5)

view(`student_info(2)`)

md.pattern(`student_info(2)`)

set.seed(145)
index = sample(1:nrow(`student_info(2)`),size = 0.8*nrow(`student_info(2)`))
train = `student_info(2)`[index,]
test = `student_info(2)`[-index,]


view(test)

uzak = mahalanobis(train,
            center = colMeans(train),
            cov = cov(train))

cutoff = qchisq(p = 0.95,
                df = 2)

uzak
cutoff


mod_1 = lm(student_marks~study_hours,
           data = train)

mod_2 = lm(student_marks~study_hours,
           data = train_Out)

plot(mod_1)
plot(mod_2)

res = which(uzak>cutoff)
train_Out = train[-res,]
nrow(train)
nrow(train_Out)

plot(train$study_hours,
     train$student_marks,
     pch = 19,
     bty = "L")

plot(train_Out$student_marks,
     train_Out$study_hours,
     pch = 19,
     bty = "L")

### Burad R2 değerine bakılırsa Modelin doğruluğu %95 oranında gayet iyi değer
### Buradaki Mod_1 aykırı değer çıkarılmamış
### 1 saat fazladan çalışmak notlara 3.88 artmış etkisi olmuş

summary(mod_1)
summary(mod_2)
### Mod_2 Mod_1 den iyi R2 değeri vermiştir Mod_2 de verilerin doğruluğu biraz daha artmıştır.
library(caret)

AIC(mod_1)
AIC(mod_2)

BIC(mod_1)
BIC(mod_2)


mod_1_pred = predict(mod_1,
                     test)

mod_2_pred = predict(mod_2,
                     test)
## Gerçek Değerler ve Tahmin Edilen Değerler Karşılaştırılmıştır.
a = data.frame("Gercek" = test$student_marks,
           "Tahmin" = mod_1_pred)
b =  data.frame("Gercek" = test$student_marks,
                "Tahmin" = mod_2_pred)
view(a)
view(b)

R2(mod_1_pred,
   test$student_marks)

R2(mod_2_pred,
   test$student_marks)

RMSE(mod_1_pred,
     test$student_marks)

RMSE(mod_2_pred,
     test$student_marks)

MAE(mod_1_pred,
     test$student_marks)

MAE(mod_2_pred,
     test$student_marks)



### K-Fold Cross Validation ##################
train.control = trainControl(method = "cv",number = 10,
                             verboseIter = TRUE)

??traincontrol
mod_1_cv =train(student_marks~study_hours,
                 data =train ,method = "lm",
                 trControl = train.control)
mod_2_cv = train(student_marks~study_hours,
                  data =train_Out ,method = "lm",
                  trControl = trainControl())

summary(mod_1_cv)
summary(mod_2_cv)

?trainControl()
mod_1_cv
mod_2_cv

model_cv_pred_1 = predict(model1_cv,
                          testSet)
model_cv_pred_2 = predict(model2_cv,
                          testSet)



# Artılkların Çıkarılmış Olduğu Veri seti daha doğru sonuçlar verir



R2(model_cv_pred_1,
   testSet$price)

R2(model_cv_pred_2,
   testSet$price)
RMSE(model_cv_pred_1,testSet$price)
RMSE(model_cv_pred_2,testSet$price)

























