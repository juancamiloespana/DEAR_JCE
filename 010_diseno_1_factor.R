
#####Paquetes
#install.packages("gplots") ####Paquete para hacer gráfico de medias
#install.packages("agricolae") ### para comparación de tratamientos
#install.packages("pwr") ### para calculo de muestra 

##### librerias
library(pwr) ### tamaño de muestra
library(gplots) ## gráfico de medias
library(agricolae) ### comparación de tratamiento
library(lmtest) ### prueba de hipótesis

#########cargar datos

### Para analizar los datos en r, solo puede haber una columna por cada variable
### cargar datos oxigeno
### los factores debene estar en formato factor

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/oxigeno.csv'

datos= read.csv(ruta, sep=';', dec=",")

datos$Punto=as.factor(datos$Punto)

## Punto 2 y 3

plotmeans(data=datos, oxigeno_disuelto~Punto)



### resumen promedios #######


### Grafico de medias  ##################################




# Punto 4  anova y significancia del modelo###############




###### Punto 5: modelo de efectos y de medias

### tabla de medias

### tabla de efectos


####punto 6 validar supuestos ##########


#### punto 7 Comparación de tratamientos  #############


#### punto 8 tamaño de muestras ###################

