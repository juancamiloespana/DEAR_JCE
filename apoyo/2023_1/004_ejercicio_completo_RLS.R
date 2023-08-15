##### cargar librerias 
library(lmtest)## para prueba de bptest homocedasticidad

ruta_pv='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/precios_viviendas.csv'
ruta_vsp='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/viviendas_sin_precios.csv'


df_pv=read.csv(ruta_pv)
df_pv=df_pv[,-1]  ### filas se deja vacio para que traiga todas, columnas se ponen en negativo las que se quieren eliminar
#### punto 1
plot(precio~m2, data=df_pv)

## punto 2 para estimar cuanto aumenta el precio de la vivienda cuado aumentan los M2 en una unidad.
mod=lm(precio~m2, data=df_pv)
confint(mod, level=0.95) 

### punto 3cuanto vale una vivienda cuando los M2 son 0
summary(mod)

### para saber las predicciones(valores ajustados) de cada fila
df_pv$predichos=mod$fitted.values

#### calcular error en todas las filas de entrenamiento

df_pv$error= df_pv$precio-df_pv$predichos
sd(df_pv$error) ### para calcular manualmente el residual standar error
mean(df_pv$precio) ## para evaluar si el residual estandar error es alto o bajito


#### para graficar el modelo
plot(precio~m2, data=df_pv)
abline(mod, col='red',lwd=2)


#### punto 7 validar supuesto de varianza constante 

res=mod$residuals

plot(res~df_pv$m2)
bptest(mod)

mean(res)
