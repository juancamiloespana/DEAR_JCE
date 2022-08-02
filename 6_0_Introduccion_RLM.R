#####CArgar paquetes #############3

library(regclass) ### Para calcular VIF
library(dplyr)  ### para case_when
library(plotly)  ###para gráficos Fancys

###############



####### ##### Ajustar regresión lineal multiple

### Cargar datos ####



##############################
### Ajustar modelo RLM###
###################################



######Ajsutar anova ####


#####extraer R2 y R2 ajustado


################################################
### Variable cualitativa crear base ficticia  ####
################################################

### crear edad como seq de 20 a 50
### crear variable antibiotico el 50% es no, el 20% es Bajo y el 30% es ALTO
#### variable respuesta si toma antibiotico b0= 0b1 es 0,7 si no es -0,5 y bo es 50
### error estándar para y
### crear data frame



####ajustar modelo y analizar resumen de cualitativa



############################################
################Interacción ##############
##########################################

##################Crear una base de datos ficticia

### crear edad como seq de 20 a 50
###crear variable antibiotico el 50% es 1 (si), el 50% es 0 (No)
#### variable respuesta si toma antibiotico b0= 0b1 es 0,7 si no es -0,5 y bo es 50
### error estándar para y
### crear data frame


################ Analizar gráficamente ############

###ver correlación entre variables explicativa




#####ver gráfico de interacción en plotly



####ajustar modelo rl con interacción




##### analizar resume





###############################
######### Analizar de VIF #####
###############################


#### Problema

####Suponga y utilidad de empresa
#### suponga x gasto en publicidad
#### Suponga x2 gasto en calidad

###### se quiere comprobar si aumentar gasto en publicidad mejora utilidades

#### Crear variables ficticias 
#### establecer semilla
### x distribución uniforme entre 15 70
### x2 x + componente aleatoria media 0 y varianza 10
### y con b1 =3 y error normal estánar 



###### Ajuste un modelo con sólo el gasto en calidad
###### Cuánto crece la utilidad si se aumento una unidad la inversion en calidad




### Agregue a ese modelo la variable publicidad
### ¿Qué pasa con la relación entre utilidad y calidad?


### Verifique la multicolinealidad.









