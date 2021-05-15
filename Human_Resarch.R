library(tidyverse)
library()
df = HRDataset_v14


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

shapiro.test(a$Salary)
kruskal.test(data$Salary~data$Department)

plot(data$SpecialProjectsCount,
     data$Salary,
     bty = "L",
     pch = 19)







