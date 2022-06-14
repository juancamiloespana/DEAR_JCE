

##### 0 Cargar librerías, ya deben estar instaladas ######

library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)

##### Cargar datos ####

datos<-read.csv("precios_viviendas.csv")
datos_nuevos=read.csv("viviendas_sin_precios.csv")



###### 1 Descripción de los datos.

plot(datos$m2, datos$precio)
mod1<- lm(precio~m2,data=datos)
abline(mod1,col="red")
residuales=mod1$residuals



###### Respuesta 1: la variable respuesta es el precio porque es la variable que se quiere predecir
###### El modelo no es muy apropiado para elconjunto de datos porque hay puntos inusuales 
###### y  además parece que la relación no es líneal.

  summary(mod1)
  
  mean(datos$precio)

  ###Respuesta 2: 
  ### Modelo  y=9.09  + 0.008047x +e
  ###Valores predichos y_gorro= 9.09  + 0.008047x
  
  #Respuesta 3: Bo  -> Ho: Bo=0 y Ha: Bo <> 0
  # estadístico t para Bo: 160.5
  # valor p <2e-16, como es menora 0.05 se rechaza Ho y se dice que el intercepto es significativo
  
  
  #Respuesta 3: B1  -> Ho: B1=0 y Ha: B1 <> 0
  # estadístico t para B1: 147.7
  # valor p <2e-16, como es menora 0.05 se rechaza Ho y se dice que el coeficiente B1 es significativo
  
  
  # Respuesta 4:  Bo: 0.057, B1: 5.447e-05
  
  # Respuesta 5: 1.13
  
  


confint(mod1,level=0.95)



####Respuesta 6. Bo: (8.987779065, 9.21025384)
################ B1: (0.007939784, 0.00815356)


pred=predict(mod1,  interval= "confidence")

datos_con_pred=data.frame(datos,pred)

## respuesta 7: para fila 1: (18.001172, 18.156124) ## un intervalo por fila

pred_test=predict(mod1, newdata=datos_nuevos, interval= "prediction")

datos_nuevos_con_pred=data.frame(datos_nuevos,pred_test)

## respuesta 8: para fila 1:  (10.367541, 14.80573) ## un intervalo por fila



residuales<-mod1$residuals

#1. prueba normalidad ####

##1.1 Normalidad - Pruebas gráficas #####

#Histograma

hist(residuales, main="Histograma residuales", breaks=20)




#qqplot fancy
qqPlot(residuales)


boxplot(residuales, horizontal=T)

##1.2 Normalidad -pruebas estadísticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

stats::ks.test(x=residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

goftest::ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling

goftest::cvm.test(residuales,pnorm, 0, sd(residuales)) ###Cramer von mises

  
  ### Respuesta 9: SEgún las gráficas parece que los datos del error No se distribuyen normal,
  ### En las pruebas de hipotesis todos los valores p son menores a una significancia de 0.05 entonces se puede rechazar la hipotesis nula
  ### Por lo tanto los residuales No provienen de una distribución normal y no cumplen el supuestos

#2. prueba varianza constante - Homocedasticidad ####

##2.1 Homocedasticidad -Pruebas gráficas #####


plot(mod1$fitted.values,residuales)
abline(h=0, col="red")

plot(datos$precio,residuales)
abline(h=0)

##2.2 Homocedasticidad -Pruebas estadísticas #####

bptest(mod1)


### Respuesta 9: SEgún las gráficas parece que los datos del error no tienen varianza constante
### En la prueba de hipotesis el valor p no es  mayores a una significancia de 0.05 entonces se rechaza la hipotesis nula
### Por lo tanto los residuales no tienen varianza constante y no cumplen el supuesto de homecedasticidad



#3. prueba independencia  #####


##3.1 Independencia-Pruebas GRáficas #####

plot(residuales)
abline(h=0)
par(mfrow=c(1,2)) ###Separar la ventana de los gráficos
acf(residuales)
pacf(residuales)

##3.2 Independencia-Pruebas estadísticas #####

dwtest(mod1) 

bgtest(mod1)


### Respuesta 9: SEgún las gráficas parece que los datos del error no son independientes
### En las dos pruebas de hipotesis el valor p es no mayores a una significancia de 0.05 entonces no se rechaza la hipotesis nula
### Por lo tanto, los residuales son indpendientes y cumplen el supuesto





