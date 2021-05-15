library(tidyverse)
library(mice)
library(glmnet)
df = HRDataset_v14
md.pattern(data)

data = df%>%mutate(SpecialProjectsCount = as.factor(SpecialProjectsCount))%>%
  
  select(Sex,Department,SpecialProjectsCount,Salary)

salary = (data$Salary/12)
data$Salary = salary
view(data)
summary(data)
view(df$SpecialProjectsCount)
table(df$SpecialProjectsCount)
as.data.frame(table(data$Department))
class(data$SpecialProjectsCount)
ids = which(data$SpecialProjectsCount == 0)

a = data[ids,]

view(a)
names(df)
as.data.frame(table(a$Department))
view(data)
view(data)
summary(data)
which(data$Salary == 3754)
view(df[311,])
modelDataDummy = model.matrix()



kruskal.test(data$Department~data$Salary)


library(dunn.test)
dunn.test(x = data$Salary,
          g = data$Department)
shapiro.test(a$Salary)
kruskal.test(data$Salary~data$Department)

plot(data$SpecialProjectsCount,
     data$Salary,
     bty = "L",
     pch = 19)



mean(data$Salary[data$Department == "Admin Offices"])
mean(data$Salary[data$Department == "Executive Office"])
mean(data$Salary[data$Department == "IT/IS"])
mean(data$Salary[data$Department == "Sales"])
mean(data$Salary[data$Department == "Software Engineering"])
mean(data$Salary[data$Department ==  "Production"])
table(data$Department)

any(is.na(data$Salary))

a = data%>%filter(Department == "Software Engineering")

view(a)

modelDummy = model.matrix(Salary~.,
             data = data)

view(modelDummy)

dim(data)
set.seed(145)
index = sample(1:nrow(data),
               size = 0.8*nrow(data))

trainX = modelDummy[index,]
testX = modelDummy[-index,]

trainY = data$Salary[index]
testY = data$Salary[-index]

view(trainX)
view(trainY)



