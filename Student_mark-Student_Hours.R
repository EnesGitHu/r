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


mod_1 = lm(train$student_marks~train$study_hours,
           data = train)


plot(mod_1)

res = which(uzak>cutoff)
train_Out = train[-res,]

par(mfrow = c(1,2))
plot(train$study_hours,
     train$student_marks,
     pch = 19,
     bty = "L")

plot(train_Out$student_marks,
     train_Out$study_hours,
     pch = 19,
     bty = "L")












