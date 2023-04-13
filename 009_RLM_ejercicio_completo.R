

#####Cargar librerías, las librerías siempre se deberían cargar al principio 

  library(corrplot) ###Gráfico de correlaciones
  library(plotly) ###para gráficos bonitos
  library(Metrics) ### para calcular mape
  library(regclass) ### para VIF
  library(car)  ### validacion supuestos
  library(MASS) ### para stepAIC
  library(tseries) ### validacion supuestos
  library(nortest) ### validacion supuestos
  library(goftest) ### validacion supuestos
  library(lmtest) ### validacion supuestos
  library(randomForest) #### para imputación de datos faltantes
  library(dplyr) #### para case_when para reagrupar variables
  library(skimr) ### para descripcion de los datos 


#######Cargar Base de Datos-ejemplo ########3
  


##########################################################################################################
#### 1. Descripción y limpieza de los datos #############################################################
#########################################################################################################


### Describir datos y analizar los tipos de variables 
### Revisar que las variables queden en el formato correcto 
### Exploración inicial, verificar faltantes, números de categorías

#### Tratar datos faltantes 


### revisar categorías en variables categóricas 

#### Una variable con muchas categorías es difícil de analizar, puede hacer el modelo lento y 
#### generar sobre ajustes, aunque no hay un número exacto para saber qué son muchas, podría utilizarse más de 20 o 30 como referencia
#### según el estudio este valor se puede aumentar o disminuir
#### Además se debe revisar que ninguna categoría tenga pocas observaciones
####Tratamiento de fechas 
##se convierte la columna a formato fecha
###los formatos se pueden encontrar en la ayuda de la función strptime
  
### para extraer el mes
###para extraer el día de la semana
### Para extraer el trimestre
###Para extraer el día del mes

  
  
###############################################################################################
## 2. Análisis exploratorio #####################################################################
################################################################################################


#### Explorar variable respuesta 
####Histograma sencillo 
  
####Histograma fancy

#### separar variables categóricas y numéricas
#### analizar categoricas concluir cuáles tiene influencia

### Analizar correlaciones de numericas 



#############################################################################################
#### 3. Ajsutar mejor modelo posible ########################################################
#############################################################################################


### Separar la muestra de datos en entrenamiento y prueba #####
#### ajustar modelo y probar mape de evaluacion y entrenamiento ####
### Eliminar atipicos  y medir impacto en evaluacion y entrenamiento
#### verificar la colinealidad de variables a eliminar con VIF
####  seleccionar variables
#### validar interacciones 
  
  

############################################################################################
#### 4. Análisis de supuestos ##############################################################
############################################################################################



############################################################################################
#### 5. Inferencias y análisis #############################################################
############################################################################################

  
 
 