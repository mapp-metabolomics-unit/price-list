library(rgl)

samples <- c(1,5,10,20,50,75,100,250,500)
taro <- c(100,250,280,450,550,680, 750,900, 1000)
tara <- c(100,250,280,450,550,680, 750,900, 1000)
mat <- data.frame(samples,taro)

plot(samples,taro)
plot3d(samples,taro,tara)

windows();plot(mtcars)

model <- loess(taro~samples, data = mat, span = 0.9)



mat_pred <- NULL
mat_pred$samples <- c(1:500)

my_predict = predict(model,newdata = c(1:500))
plot(mat_pred$samples, my_predict)


#######

samples <- c(1:length(my_predict))
taro <- my_predict
mat <- data.frame(samples,taro)

plot(samples,taro)


model <- loess(taro~samples, data = mat, span = 0.95)



mat_pred <- NULL
mat_pred$samples <- c(1:500)

my_predict = predict(model,newdata = c(1:500))
plot(mat_pred$samples, my_predict)





plot(mat_pred$samples, my_predict)
plot(mat_pred$samples, predict(model,newdata = mat_pred,type="response"))


attach(mtcars)
fit <- lm(mpg ~ hp + I(hp^2))
plot(mpg~hp)
points(hp, fitted(fit), col='red', pch=20)
lines(sort(hp), fitted(fit)[order(hp)], col='red', type='b') 



plot(taro~samples)
lines(sort(samples), fitted(model)[order(samples)], col='red', type='b') 





plot(exp(seq(1,0.01,-0.01)))

plot(0.5^(0.75*seq(1,1000,1)))


plot(1+1/x, fonction(x))




y<-runif(50,2,8)
plot(y,exp(y))



x<-seq(1,500,1)

#plot(x,1*exp(x))

plot(x, 50 + (100*log(x+1, 2)))

plot(-100*0.5^(0.75*x))




price_per_sample = 100
price = 100

obj = seq(0.005,0.000001,-0.0000001)

for (sample in c(1:1000)){
  price = price * (1 - obj[sample])
  price_per_sample = c(price_per_sample,price)
  
}

plot(price_per_sample) 


#######################

library(aomisc)
library(dplyr)
library(tidyr)
library(tidyverse)


set.seed(1234)
X <- c(1, 3, 5, 7, 9, 11, 13, 20)
a <- 20; b <- 5; c <- 0.3
Ye <- asymReg.fun(X, a, b, c)
epsilon <- rnorm(8, 0, 0.5)
Y <- Ye + epsilon
model <- drm(Y ~ X, fct = DRC.asymReg())
plot(model, log="", main = "Asymptotic regression")


install.packages("devtools")
devtools::install_github("onofriAndreaPG/aomisc")


set.seed(1234)
X <- c(1,5,10,20,50,75,100,250,500)
a <- 20; b <- 5; c <- 0.3
Ye <- asymReg.fun(X, a, b, c)
epsilon <- rnorm(8, 0, 0.5)
Y <- Ye + epsilon
model <- drm(Y ~ X, fct = DRC.asymReg())
plot(model, log="", main = "Asymptotic regression")





samples <- c(0,1, 2, 5,10,20,50,75,100,250,500, 1000)
taro <- c(0,100,120,150,200,300,450,600, 700,1000, 1250, 1500)
mat <- data.frame(samples,taro)

model <- drm(taro ~ samples, fct = DRC.asymReg(), data = mat)

mat_pred <- data.frame(samples =  c(1:500), CURVE=1 )
mat_pred$samples <- c(1:500)
mat_pred$taro = NULL
pred = predict(model, newdata = mat_pred, curveid=CURVE)


#plot(model, log="", main = "Asymptotic regression")
plot(pred)
points(samples,taro, col = "red", pch = 16)


pred[1]

# on cherche a faire des paliers dans la putain de courbe asymptot

paliers = as.numeric(cut(pred, breaks = 10))


classes = unique(cut(mat_pred$samples, breaks = 10))


values = round(tapply(pred,paliers, min))

names(values) = classes


# on arrete la et il manque a faire la table propre 

pred_df = as.data.frame(pred)

pred_df <- tibble::rownames_to_column(pred_df, "Samples_number")

pred_df$Samples_number = as.numeric(pred_df$Samples_number)

hist(pred_df$Samples_number)
hist(pred_df$pred, breaks="Sturges")

plot(pred_df)


library(Hmisc) # cut2
split(pred_df, cut2(pred_df$pred))

pred_df$gp <- split(pred_df, cut2(pred_df$pred, 10))
