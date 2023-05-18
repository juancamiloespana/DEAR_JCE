
######### Isntalar paquetes ##########
install.packages("datarium")  ###Paquete con datasets
install.packages("tidyverse") #### para utilizar funciones de manipulación de datos y  operador %>%
install.packages("ggplot") #### para plotmeasn
install.packages("rstatix") ### para función shapiro_test agrupada
install.packages("ggpubr") ####Gráficos para vraibles agrupadas
install.packages("forcats")
install.packages("WRS2") ###Base de datos hangover

########### cargar librerías
library(datarium)
library(tidyverse) 
library(gplots) 
library(rstatix)
library(ggpubr)
library(WRS2) ###Paquete con conjunto de datos ejemplo




############################################
########## E.1 CArgar datos  Intra sujetos puro 1 vía- 1 factor ##########
############################################


#############Convertir formato de datos de wide a long




### factores y sujetos deben estar en formato factor




############# Análisis exploratorio



#### Interacción entre tratamiento y sujeto es el error en medidas repetidas ####

### Supuesto de normalidad método gráfico


########### Supuestos de normalidad prueba de hipotesis



###########ANOVAs


#### ajustar modelo funcion 1






### segunda  anova esta es otra anova que muestra información adicional



###Comparación de tratamientos #



############################################
###### Ejercicio 2  análisis de sudoku ###
###########################################3


### realizar ejercicio de sudokus  


############################################
######E3. Cargar datos Intra sujetos puro 2 vías #####
############################################





############# Análisis exploratorio

##########Supuesto de normalidad método gráfico


########### Supuestos de normalidad prueba de hipotesis




###########ANOVAs



###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #


############################################
##### E4. Cargar Datos Mixto 2 vías ############
############################################


####Convertir a formato long y cambiar formatos



############# Análisis exploratorio



##########Supuesto de normalidad método gráfico


########### Supuestos de normalidad prueba de hipotesis



###########ANOVAs



###Comparación de tratamientos #


############################################
#####################E5 hagover ##########
############################################


