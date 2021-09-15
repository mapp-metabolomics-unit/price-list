
library(plotly)
library(aomisc)
library(dplyr)
library(tidyr)
library(tidyverse)


# Ressources
# https://www.statforbiology.com/2020/stat_nls_usefulfunctions/






# samples <- c(0,1, 2, 5,10,20,50,75,100,250,500, 1000)
# tarifs <- c(0,40,100,150,200,300,450,600, 700,1000, 1250, 1500)

# this is the range for the basic proc
samples <- c(1,100,500)
tarifs <- c(3,150,350)

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

merged_df$pred_rounded = round(merged_df$pred, digits = 0)

fig <- plot_ly(data = merged_df, x = ~samples, y = ~tarifs, type = 'scatter') %>% 
        add_trace(x =~samples,  y = ~pred_rounded , type = 'scatter', mode = 'lines+markers')
fig


# selection 

merged_df_sel = merged_df  %>%
mutate(price_chf_unifr = pred_rounded) %>%
mutate(price_chf_academics = pred_rounded * 1.5) %>%
mutate(price_chf_private = pred_rounded * 5 ) %>%
dplyr::select(samples, price_chf_unifr, price_chf_academics, price_chf_private)


# write_tsv(merged_df_sel, 'data/price_bioinf.tsv')
write_tsv(merged_df_sel, 'data/price_bioinf_basics_allactors.tsv')
write_csv(merged_df_sel, 'data/price_bioinf_basics_allactors.csv')