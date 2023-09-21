


url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/precio_viviendas2.csv"

url2="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/viviendas_sin_precio2.csv"

df= read.csv(url)
df_nuevos=read.csv(url2)

plot(precio~m2, data=df)

modelo=lm(precio~m2, data=df) ## ajustar el modelo
summary(modelo)
###intervalo de coeficientes de regresión 
confint(modelo, level=0.90)


###intervalo de confianza para valores ajustados

pred_va=predict(modelo, interval = "confidence", level = 0.90)

)
####intervalo de confianza predicciones (datos nuevos)


pred_datos_nuevos=predict(modelo, newdata=df_nuevos, interval="prediction", level=0.98)

df_nuevo2=data.frame(m2=80)
pred_datos_nuevos=predict(modelo, newdata=df_nuevo2, interval="prediction", level=0.80)

####supuestos ###

### normalidad ####

library(car) ### qqPlot
library(tseries)### jarquebeta
library(lmtest)##dw test, bg test 

res=modelo$residuals
hist(res)
qqPlot(res)
shapiro.test(res)
jarque.bera.test(res)
mean(res)

#### varianza constante

plot(res~modelo$fitted.values)
plot(res~ df$m2)
bptest(modelo)


### 3 independencia
plot(res)
acf(res)
pacf(res)
bgtest(modelo)
dwtest(modelo)



