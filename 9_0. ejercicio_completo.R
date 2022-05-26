






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
###### El modelo noes muy apropiado para elconjunto de datos porque hay puntos inusuales 
###### y  además parece que la relación no es líneal por lo que debería hacerse una transformación.

###### corrección de incumplimientos


#### Eliminar atípicos

  lim_sup<-quantile(datos$precio,0.75) + 1.5*(IQR(datos$precio))
  datos_dep<-subset(datos, datos$precio <=lim_sup)
  
  
  lim_sup<-quantile(datos_dep$m2,0.75) + 1.5*(IQR(datos_dep$m2))
  datos_dep2<-subset(datos_dep, datos_dep$m2 <=lim_sup)

  plot(datos$m2, datos$precio)
  plot(datos_dep2$m2,datos_dep2$precio)
  
  mod2<-lm(data=datos_dep2, precio~m2)
  abline(mod2)
  
#### Eliminar utilizando función de medidas de influencia:
  
  
  datos_influyentes<- influence.measures(mod1)
  #####Extraer filas de los datos influyentes 
  
  influyentes<-summary(datos_influyentes)
  
  filas_filtrar<-as.numeric(row.names(influyentes))
  
  
  ####Base filtrada sin datos influyentes
  
  base_modelo2<-datos[-filas_filtrar, ]
  
  plot(base_modelo2$m2,base_modelo2$precio)

#### TRasnformar variable respuesta
  
  pt=powerTransform(mod2)
  
  summary(pt)
  
  ymod<-datos_dep2$precio^2
 
  
  mod3<-lm(ymod~m2,data=datos_dep2)
  plot(datos_dep2$m2, ymod)
  abline(mod3,col="red")
  
  ### Despues de las transformaciones el modelo ajusta muy bien a RLS
  
  summary(mod2)
  summary(mod3)
  
  mean(datos_dep2$precio)

  ###Respuesta 2: 
  ### Modelo  y=29.68  + 0.30x +e
  ###Valores predichos y_gorro= 29.68  + 0.30x
  
  #Respuesta 3: Bo  -> Ho: Bo=0 y Ha: Bo <> 0
  # estadístico t para Bo: 85.08
  # valor p <2e-16, como es menora 0.05 se rechaza Ho y se dice que el intercepto es significativo
  
  
  #Respuesta 3: B1  -> Ho: B1=0 y Ha: B1 <> 0
  # estadístico t para B1: 605.84
  # valor p <2e-16, como es menora 0.05 se rechaza Ho y se dice que el coeficiente B1 es significativo
  
  
  # Respuesta 4:  Bo: 0.35, B1: 4.964e-04
  
  # Respuesta 5: 4.961
  
  


confint(mod3,level=0.95)



####Respuesta 6. Bo: (28.99, 30.36)
################ B1: (0.2997, 0.3017)


predict(mod3,  interval= "confidence")

## respuesta 7: para fila 1:  (364.68, 365.85) ## un intervalo por fila

predict(mod3, newdata=datos_nuevos, interval= "prediction")

## respuesta 8: para fila 1:  ( 106.46488, 125.96026) ## un intervalo por fila



residuales<-mod3$residuals

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

ks.test(x=residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling

cvm.test(residuales,pnorm, 0, sd(residuales)) ###Cramer von mises

  
  ### Respuesta 9: SEgún las gráficas parece que los datos del error se distribuyen normal,
  ### En las pruebas de hipotesis todos los valores p son mayores a una significancia de 0.05 entonces no se puede rechazar la hipotesis nula
  ### Por lo tanto los residuales provienen de una distribución normal

#2. prueba varianza constante - Homocedasticidad ####

##2.1 Homocedasticidad -Pruebas gráficas #####


plot(mod3$fitted.values,residuales)
abline(h=0, col="red")

plot(ymod,residuales)
abline(h=0)

##2.2 Homocedasticidad -Pruebas estadísticas #####

bptest(mod3)


### Respuesta 9: SEgún las gráficas parece que los datos del error tienen varianza constante
### En la prueba de hipotesis el valor p es  mayores a una significancia de 0.05 entonces no se puede rechazar la hipotesis nula
### Por lo tanto los residuales tienen varianza constante y cumplen el supuesto de homecedasticidad



#3. prueba independencia  #####


##3.1 Independencia-Pruebas GRáficas #####

plot(residuales)
abline(h=0)
par(mfrow=c(1,2)) ###Separar la ventana de los gráficos
acf(residuales)
pacf(residuales)

##3.2 Independencia-Pruebas estadísticas #####

dwtest(mod3) 

bgtest(mod3)


### Respuesta 9: SEgún las gráficas parece que los datos del error son independientes
### En las dos pruebas de hipotesis el valor p es  mayores a una significancia de 0.05 entonces no se puede rechazar la hipotesis nula
### Por lo tanto, los residuales son indpendientes y cumplen el supuesto



