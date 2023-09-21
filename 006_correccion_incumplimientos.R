############### Ejercicio 1 ####################333333


url1= "https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/plantas.csv"


df_plantas=read.csv(url1)

plot(costo~n_plantas, data=df_plantas)
mod=lm(costo~n_plantas, data=df_plantas)

abline(mod, col="red", lwd="2") ### el modelo se evidencia que no tiene un buen ajuste

summary(mod) ### no parece  haber alertas


####
res=mod$residuals


###1 . normalidad 

shapiro.test(res) ## como valor p es muy pequeña (menor a significancia) se rechaza supuesto
library(tseries)
jarque.bera.test(res)

hist(res)


###2 varianza constante u homocedasticidad
library(lmtest)
bptest(mod) ### se rechaza supuesto porque valor p es menor a significancia



##### indpendencia
## como el valor p es muy pequeño se rechaza supuesto

bgtest(mod)
dwtest(mod)


##### corrección del incumplimiento de supuestos ####
q1=quantile(df_plantas$costo, 0.25)
q3=quantile(df_plantas$costo, 0.75)
riq=q3-q1

big_sup= q3 + 1.5*(riq)
big_inf= q1 - 1.5*(riq)


df_plantas2=subset(df_plantas, df_plantas$costo<= big_sup & df_plantas$costo >= big_inf)

plot(costo~n_plantas, data=df_plantas2)


mod2=lm(costo~n_plantas, data=df_plantas2)
abline(mod2, col="red")


res2=mod2$residuals

### validar supuestos despues de correcion ###

shapiro.test(res2)
jarque.bera.test(res2)

##valor p mayor a sign(0.05) entonces se cumple supuesto


bptest(mod2) ## también se cumple supuesto
####independencie

bgtest(mod2)
dwtest(mod2)

##### corrección con medidas de influencia #####
influence.measures(mod) ### visualizar
sim=summary(influence.measures(mod)) ### extrae las filas en las que identificó influyentes
lista_f_eliminar= as.numeric(row.names(sim)) ## extraer solo el numero de las filas y los convierte a numero

df_plantas3=df_plantas[-lista_f_eliminar,] ### filtramos las filas identificadas


#### de tare: ajustar modelo con df_plantas 3 


