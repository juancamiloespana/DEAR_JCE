## 1.Comentarios del código:

 ## se antepone la tecla numeral

## 2.Verificar y Actualizar versi?n de R y Rstudio.
## 3.Instalar R y Rstudio / Rstudio_cloud / Google Colab
## 4.Opciones de apariencia.
## 5.Componentes de la interfaz
## 6.Crear proyecto y guardar proyecto, componentes.
## 7.Ejecutar c?digo ctrtl + enter
## 8.Instalaci?n de paquetes, Carga de paquetes

#install.packages("plotly") ### esta funci?n instala paquetes

# estas dos funciones cargan el paquete a la memoria temporal funcionan igual
library(plotly)
require(plotly)

##9.Argumentos de una funci?n, valores por defecto,ayudas sobre funciones


  ### estos c?digos sirven para crear variables ficticias

  ## el argumentoes el nombre fijo que tienen los valores de entrada de una funci?n

set.seed(100)
anos_exp2=runif(n=30, max=10, min=1) ### Funcion para crear npumeros aleatorios uniformes
salario=2.5+ 1.0*anos_exp2 + rnorm(n=30, mean=0, sd=1) # crea la variable salario en funci?n de los a?os de experiencia

  #a la ayuda se accede por la pesta?a help de la ventana derecha inferior
help(runif) # la funci?n help tambi?n abre la ayuda
?runif #cuando se antepone el simbolo ? antes del nombre de la funci?n tambi?n abre la ayuda


## 10.Vectores, nombre de variables, dataframe, 

edades=c(19, 25, 21,18) ## se crea poniendo una c antes del parentesis

datos<-data.frame(anos_exp2,salario) ###Con base en variables aleatorioas o vectores crear una tabla tipo data.frame

datos$anos_exp2 # el simbolo pesos sirve para accede a elementos del objeto


##11 gr?ficos basicos de variables num?ricas hist, plot


hist(datos$salario) ###para gráficas hsitograma

plot(datos$anos_exp2,datos$salario) ###para gráfico de dispersión x y y



  


### para ajustar solo se identifica la variable respuesta y se pone antes de la virgulilla

modelo1<-lm(datos$salario~datos$anos_exp2)###para ajustar modelo de regresión lineal desde variables y no desde tabla

modelo2<-lm(formula=salario~anos_exp2, data=datos)  ###para ajustar modelo de regresión lineal desde data frame

summary(modelo2) ### para ver resumen de modelo de regresi?n lineal
summary(modelo1)















