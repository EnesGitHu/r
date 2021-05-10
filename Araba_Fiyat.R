df = CarPrice_Assignment%>%select(wheelbase,
                             carlength,carwidth,
                             carheight,price,curbweight
                             )

set.seed(145)
index = sample(1:nrow(df),size = 0.8*nrow(df))
train = df[index,]
test = df[-index,]

names(CarPrice_Assignment)
view(df)
view(train)
library(mice)
md.pattern(train)
hist(df$carlength)
hist(df$wheelbase)
hist(df$carwidth)
shapiro.test(df$carlength)

mod_1 = lm(train$price~.,
           data = train)

summary(mod_1)
cooks.distance(mod_1)












