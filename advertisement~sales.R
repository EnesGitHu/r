class(maMarketing_Data$)
df = Marketing_Data
nrow(Marketing_Data)

class(Marketing_Data$ï..youtube)
index = sample(1:nrow(df),
               size = 0.8*nrow(df))

train = df[index,]
test = df[-index,]
View(train)
nrow(test)
nrow(train)
library(mice)
###Sales Normallik##
hist(df$sales)
hist(df$facebook)
index
cor(df)
cor(train)


plot(df$sales,
     df$facebook,
     pch = 19,
     bty = "L")

abline(lm(df$sales~df$facebook),
       col = "red",
         )
cor(df$facebook,
    df$sales)


plot(df$sales,
     df$ï..youtube,
     pch =  19,
     bty = "L")

pairs(df,
      pch = 19)
md.pattern(df)

cor(df$sales,
    df$ï..youtube)


mod_1= lm(sales~ï..youtube+facebook+newspaper,
          data = train)


names(df)
plot(mod_1)
##Gazeteye Verilen Reklam Bütçesinde artış ya da azalış satışlarda Herhangi Bir artışa Neden Olmamıştır.##
summary(mod_1)

distance

## Aykırı Değer Tespiti ##
distance = cooks.distance(mod_1)
ol1 = ort*3
ol2 = 4/length(distance)
ol1index = which(distance> ol1)
ol2index = which(distance>ol2)
ol1index
train[ol1index,]
train[ol2index,]
ort = mean(distance)

trainOut = train[-ol1index,]
nrow(train)

View(trainOut)
nrow(trainOut)
mod_1_out= lm(sales~ï..youtube+facebook+newspaper,
          data = trainOut)
summary(mod_1_out)
summary(mod_1)



mod_2_out =  lm(sales~ï..youtube+facebook,
             data = trainOut)
mod_2 =  lm(sales~ï..youtube+facebook,
            data = train)
summary(mod_2_out)
summary(mod_2)


library(caret)
mod_2_out_pred = 












