
####cargar paquetes

library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)

#####

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/utilidad.csv'
datos=read.csv(url)

names(datos)=c('y','x') ### renombrar columnas
#######

plot(datos$x,datos$y)

mean(datos$y)

######

modelo=lm(formula=y~x,data=datos) ###Ajustar modelo de regresión
summary(modelo) ###analizar resultados del modelo


res=modelo$residuals
####VAlidar supuestos:

#####normalidad: procedimiento gráfico

hist(res,breaks = 20)
boxplot(res,horizontal = T)
qqPlot(res)

#####normalidad:pruebas estadisticas sig:0.05

shapiro.test(res)
jarque.bera.test(res)

stats::ks.test(res,pnorm,0,sd(res))

goftest::ad.test(res,pnorm,0,sd(res)) 

goftest::cvm.test(res,pnorm,0,sd(res))


#####VArianza constante procedimiento gráfico

predichos=modelo$fitted.values
plot(predichos,res)
abline(h=0)

plot(datos$x,res)
abline(h=0)

####prueba estadísticas VArianza constante: homocedasticidad

bptest(modelo) 


###Independencia gráficos

par(mfrow=c(1,2))
acf(res)
pacf(res)


plot(res)
abline(h=0)

######Independencia pruebas estadísticas

dwtest(modelo)
bgtest(modelo)







