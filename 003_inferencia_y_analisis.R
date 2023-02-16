

####leer datos de git hub

url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/resistencia.csv"
datos=read.csv(url,sep=";")

####graficar variables

plot(datos$porcentaje,datos$resistencia)


#### Ajustar modelo

modelo=lm(formula=resistencia~porcentaje,data=datos)

#### Ver resumen

summary(modelo)
mean(datos$resistencia)


#### calcular I.C para coeficientes

confint(object=modelo,level=.95)

##Intervalo de confianza para datos con los que se entreno
predict(object=modelo,interval="confidence")

####generar datos nuevos
datos_resistencia2<-data.frame("porcentaje"=runif(5,max=35,min=24))


####Intervalo de confianza datos nuevos ####

predict(object=modelo, newdata = datos_resistencia2,interval="prediction")



library(tidyverse)
data(Orange)

