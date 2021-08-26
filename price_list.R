
library(plotly)
library(aomisc)
library(dplyr)
library(tidyr)
library(tidyverse)


# Ressources
# https://www.statforbiology.com/2020/stat_nls_usefulfunctions/






samples <- c(0,1, 2, 5,10,20,50,75,100,250,500, 1000)
tarifs <- c(0,40,100,150,200,300,450,600, 700,1000, 1250, 1500)
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

merged_df$pred_rounded = round(merged_df$pred, digits = -1)

fig <- plot_ly(data = merged_df, x = ~samples, y = ~tarifs, type = 'scatter') %>% 
        add_trace(x =~samples,  y = ~pred_rounded , type = 'scatter', mode = 'lines+markers')
fig


# selection 

merged_df_sel = merged_df  %>% 
mutate(prix_CHF = pred_rounded)  %>% 
select(samples, prix_CHF)

# output


write_csv(merged_df_sel, 'data/price_bioinf.csv')