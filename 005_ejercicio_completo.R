


url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/precio_viviendas2.csv"

url2="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/viviendas_sin_precio2.csv"

df= read.csv(url)
df_nuevos=read.csv(url2)

plot(precio~m2, data=df)

modelo=lm(precio~m2, data=df) ## ajustar el modelo
summary(modelo)
###intervalo de coeficientes
confint(modelo, level=0.95)


###intervalo de confianza para valores ajustados

pred_va=predict(modelo, interval = "confidence")


####intervalo de confianza predicciones (datos nuevos)

pred_datos_nuevos=predict(modelo, newdata=df_nuevos, interval="prediction")



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



