#install.packages('caTools')
#install.packages('plotly')
#install.packages('regclass')
#install.packages('skimr')
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
library(caTools) ###separar muestra entrenamiento y evaluacion


#######Cargar Base de Datos-ejemplo ########3

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/base_supermercado2.csv'
df= read.csv(ruta,stringsAsFactors = T)
##########################################################################################################
#### 1. Descripción y limpieza de los datos #############################################################
#########################################################################################################


### Describir datos y analizar los tipos de variables 
skim(df)
summary(df)


### Exploración inicial, verificar faltantes, números de categorías
table(is.na(df)) ##calcula los datos faltantes en todas la tabla
df2=na.roughfix(df) ##imputa mediana a numericas y moda a categóricas
table(is.na(df2))### verificar si eliminaron faltantes

### revisar categorías en variables categóricas:

table(df2$genero) ## está bien pocas cat y muchas observaciones en cada cat
table(df2$reportado_data_credito)

df2$reportado_data_credito=case_when(
  df2$reportado_data_credito=='SI' ~ 'SI',
  TRUE~'NO'
  
)
df2$reportado_data_credito=as.factor(df2$reportado_data_credito)

table(df2$medio_apgo)

df2$medio_apgo=case_when(
  df2$medio_apgo=='TD' ~ 'TD',
  df2$medio_apgo=='TDC' ~ 'TDC',
  TRUE~'Efectivo'
  
)

df2$medio_apgo=as.factor(df2$medio_apgo)


sorted_count(df2$producto_frecuente)

df2$producto_frecuente=case_when(
  df2$producto_frecuente=='Pollo' ~ 'Pollo',
  df2$producto_frecuente=='Carne' ~ 'Carne',
  TRUE~'Otro'
  
)

df2$producto_frecuente=as.factor(df2$producto_frecuente)


#### Una variable con muchas categorías es difícil de analizar, puede hacer el modelo lento y 
#### generar sobre ajustes, aunque no hay un número exacto para saber qué son muchas, podría utilizarse más de 20 o 30 como referencia
#### según el estudio este valor se puede aumentar o disminuir
#### Además se debe revisar que ninguna categoría tenga pocas observaciones


####Tratamiento de fechas 
df2$fecha_ultima_compra2=as.Date(df2$fecha_ultima_compra, format="%d/%m/%Y")
df2=df2[,-20] ### para eliminar columna de fecha repetida

df2$m_fuc=months.Date(df2$fecha_ultima_compra2) ##extrae el mes de una fecha

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



