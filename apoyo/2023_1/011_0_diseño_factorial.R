

###########

library(graphics)  ### grafico de interacción, plto.design
library(gplots) ## gráfico de medias 
library(lmtest) ## supuestos
library(agricolae) ## comparación de pares de tratamientos



###############################Ejercicio: baterias.csv

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/bateria.csv'

datos=read.csv('D:/trabaja_Regresion/datos.csv', sep=";")
datos$temperatura=as.factor(datos$temperatura) ## los factores deben ser tipo de variable factor
datos$material=as.factor(datos$material)
###### Análisis exploratorio ####


### Efectos principales
boxplot(tiempo_horas~material+temperatura, data=datos)### para ver tratamientos
boxplot(tiempo_horas~material, data=datos) ### para ver efecto principal
boxplot(tiempo_horas~temperatura, data=datos) ### para ver efecto principal


####### comparación de efecto de factores

plot.design(data=datos, tiempo_horas~material+temperatura) ## para comparar efectos individuales de los factores

#### Efectos de interaccion

attach(datos) ## lee las columnas de data frame como variables

par(mfrow=c(1,2))
interaction.plot(material, temperatura, tiempo_horas)
interaction.plot( temperatura, material, tiempo_horas)

#### Ajsutar modelo y mirar singificancia de los efectos ###
an1=aov(data=datos, tiempo_horas~material*temperatura)## se pone la interaccion y 
##el agrega los efectos principales de los factores que forman la interaccion
summary(an1)

### tabla de efectos

model.tables(an1, type="effects", group =F)



### tabla de medias
model.tables(an1, type="means", group =F)

#### Verificación de supuestos con residuales ###



###### ejercicio

## 1. Calcular la potencia de las inferencias con el tamaño de la muestra que se tiene
## 2. Identificar entre qué pares de tratamientos hay diferncias singificativas

###



