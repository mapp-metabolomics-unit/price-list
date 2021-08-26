
library(plotly)
library(aomisc)
library(dplyr)
library(tidyr)
library(tidyverse)




samples <- c(0,1, 2, 5,10,20,50,75,100,250,500, 1000)
tarifs <- c(0,100,120,150,200,300,450,600, 700,1000, 1250, 1500)
mat <- data.frame(samples,tarifs)

model <- drm(tarifs ~ samples, fct = DRC.asymReg(), data = mat)

mat_pred <- data.frame(samples =  c(1:500), CURVE=1 )
mat_pred$samples <- c(1:500)
mat_pred$tarifs = NULL
pred = predict(model, newdata = mat_pred, curveid=CURVE)

mat_pred$pred = pred


plot(model, log="", main = "Asymptotic regression")
plot(pred)
points(samples,tarifs, col = "red", pch = 16)

input_df = data.frame(samples, tarifs)

merged_df = merge(input_df, mat_pred, by = 'samples', all.y = TRUE)



fig <- plot_ly(x = samples, y = tarifs)
fig <- add_trace(data = mat_pred, x = mat_pred$samples, y = mat_pred$pred)
fig


fig <- plot_ly(data = merged_df, x = ~samples, y = ~tarifs, type = 'scatter') %>% 
        add_trace(x =~samples,  y = ~pred , type = 'scatter', mode = 'lines+markers')
fig

pred[1]


library(plotly)
data <- data.frame(
  X = c (0, 1, 2, 3, 4, 5),
  Line = c(1.5, 1, 1.3, 0.7, 0.8, 0.9),
  Bar = c(1, 0.5, 0.7, -1.2, 0.3, 0.4))

fig <- plot_ly(data , x = ~X, y = ~Bar, type = 'bar') %>%
  add_trace(data , x = ~X, y = ~Line, type = 'scatter',  mode = 'lines+markers')

fig

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
