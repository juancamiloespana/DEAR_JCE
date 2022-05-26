


library(randomForest)

############ Cargar paquetes ####

library(dplyr)  ### librería con funciones para manupular datos case_when
library(COUNT) #### librerías con tablas para análisis


packageVersion("COUNT")


############# reagrupar categorías con pocas observaciones ########

data(fishing) ## base de datos con información de peces en zonas con unas catacterísticas identificadas

fishing$year= as.factor(fishing$year) ### convertir año a categórica

table(fishing$year) ## ver conteo por categoría


fishing$year2 =
  case_when (
    fishing$year == "2002" ~"2002",
    fishing$year == "2001" ~"2001",
    fishing$year == "1982" ~"1982",
    fishing$year == "1981" ~"1981",
    fishing$year == "1980" ~"1980",
    fishing$year == "1979" ~"1979",
    TRUE ~ "Otros"
    
  )

table(fishing$year2)


###Convertir en categórica #########


table(fishing$meandepth)


fishing$color_rojo =
  case_when (
    fishing$color == "Rojo" ~ 1,
    TRUE ~ 0
    
  )
fishing$color_verde =
  case_when (
    fishing$color == "Verde" ~ 1,
    TRUE ~ 0
    
  )


data$marron2=NA


table(fishing$meandepth)
##################################Tratamiento de fechas ###################33


data_rlm<-read.csv("datos_los_train.csv",dec=".",sep=",", header= T, fileEncoding = "UTF-8")
str(data_rlm)

length(unique(data_rlm$vdate))

data_rlm$vdate2<-as.Date(data_rlm$vdate,"%m/%d/%Y")


months.Date(data_rlm$vdate2, abbreviate = T) ### para extraer el mes
weekdays(data_rlm$vdate2, abbreviate = T) ###para extraer el día de la semana
quarters(data_rlm$vdate2, abbreviate = T) ### Para extraer el trimestre
format(data_rlm$vdate2, format="%d") ###Para extraer el día del mes
format(data_rlm$vdate2, format="%Y") ## para extraer el año




#################


####### Estimar parámetros/ajustar modelo de regresión simple ####


modelo_regresion<-lm(fishing$totabund~fishing$density)
summary(modelo_regresion)


modelo_regresion<-lm(fishing$totabund~fishing$meandepth)
summary(modelo_regresion)




###################### Datos Faltantes

x=runif(n=1000,min=20,max=80)
x[901:1000]= NA
e=rnorm(1000,0,5)

y= 30 + 0.5*x + e

y[901:1000]= NA

base_modelo=data.frame(y,x)

####Comprobar si hay datos faltantes #####

is.na(base_modelo$y)
is.na(base_modelo)
table(is.na(base_modelo))

plot(x,y)

###### Imputación con random_forest #####

modelo_regresion_lineal=lm(base_modelo$y~base_modelo$x)
modelo_random_forest=randomForest(base_modelo$y~base_modelo$x)


base_modelo<-na.roughfix(base_modelo)
table(is.na(base_modelo))
plot(base_modelo$x,base_modelo$y)

modelo_random_forest=randomForest(base_modelo$y~base_modelo$x)



