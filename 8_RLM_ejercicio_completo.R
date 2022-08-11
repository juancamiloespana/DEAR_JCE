
#### 0. inicializar el proyecto #####

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
  library(skimr) ### para descripción de los datos 


#######Cargar Base de Datos-ejemplo 
  
url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/base_supermercado2.csv'

data_original<-read.csv(url, dec=",", stringsAsFactors = T)

#### 1. Descripción y limpieza de los datos #####


data_original=data_original[,-1] ### eliminar consecutivo
  
names(data_original)[8]="antiguedad_cliente"



### Describir datos y analizar los tipos de variables 
#####Revisar que las variables queden en el formato correcto 
  str(data_original)

# Exploración inicial, verificar atípicos, números de categorías
  skim(data_original)

  summary(data_original)


#### Tratar datos faltantes 

####Faltantes-imputación
table(is.na(data_original))

data_original<-na.roughfix(data_original) ###función de imputación de paquete randomforest
table(is.na(data_original))

############ revisar categorías en variables categóricas 

####Una variable con muchas categorías es difícil de analizar, puede hacer el modelo lento y 
#### generar sobre ajustes, aunque no hay un número exacto para saber qué son muchas, podría utilizarse más de 20 o 30 como referencia
#### según el estudio este valor se puede aumentar o disminuir
###Además se debe revisar que ninguna categoría tenga pocas observaciones


  obs_gen<-table(data_original$genero)
  obs_gen[order(obs_gen,decreasing=T)]
  
  obs_rep<-table(data_original$reportado_data_credito)
  obs_rep[order(obs_rep,decreasing=T)]
  
  ### Dado que tiene una categoría con una sola observación se reagrupa:
  ###Si la base de entrenamiento y evaluación está separada, la reagrupación se hace en las dos bases
  
  data_original$reportado_data_credito<-case_when(
    data_original$reportado_data_credito=="SI"~"Reportado",
    TRUE~"No reportado")
  
  table(data_original$reportado_data_credito)
  
  data_original$reportado_data_credito= as.factor(data_original$reportado_data_credito) ### para que en el summary quede bien
  
  
  
  obs_cat<-table(data_original$cat_principal)
  obs_cat[order(obs_cat,decreasing=T)]
  
  obs_med<-table(data_original$medio_apgo)
  obs_med[order(obs_med,decreasing=T)]
  
  ### Dado que tiene una categoría con pocas observación se reagrupa:
  
  data_original$medio_apgo<-case_when(
    data_original$medio_apgo=="TD"~"TD",
    data_original$medio_apgo=="TDC"~"TDC",
    
    TRUE~"Efectivo")
  
  table(data_original$medio_apgo)
  
  data_original$medio_apgo= as.factor(data_original$medio_apgo) ### para que en el summary quede bien
  
  
  
  obs_diasem<-table(data_original$dia_sem_mas_frecuente)
  obs_diasem[order(obs_diasem,decreasing=T)]
  
  
  obs_prod<-table(data_original$producto_frecuente)
  obs_prod[order(obs_prod,decreasing=T)]
  
  ##### Si tiene muchas categorias se debe reagrupar 
  
  data_original$producto_frecuente =
    case_when (
      data_original$producto_frecuente == "Pollo" ~"Pollo",
      data_original$producto_frecuente == "Carne" ~"Carne",
      data_original$producto_frecuente == "Cerveza tibetana Barley" ~"Cerveza",
      TRUE ~ "Otros"
      
    )
  
  data_original$producto_frecuente=as.factor(data_original$producto_frecuente)
  str(data_original)

#### slide 10 Tratamiento de fechas 

  table(data_original$fecha_ultima_compra)
  unique(data_original$fecha_ultima_compra)
  
  data_original$fecha_ultima_compra2<-as.Date(data_original$fecha_ultima_compra,"%d/%m/%Y") ##se convierte la columna a formato fecha
  ###los formatos se pueden encontrar en la ayuda de la función strptime
  
  data_original$fu_mes=months.Date(data_original$fecha_ultima_compra2, abbreviate = T) ### para extraer el mes
  data_original$fu_ds=weekdays(data_original$fecha_ultima_compra2, abbreviate = T) ###para extraer el día de la semana
  data_original$fu_tri=quarters(data_original$fecha_ultima_compra2, abbreviate = T) ### Para extraer el trimestre
  
  data_original$fu_dm=format(data_original$fecha_ultima_compra2, format="%d") ###Para extraer el día del mes
  data_original$fu_a=format(data_original$fecha_ultima_compra2, format="%Y") ## para extraer el año
  
  
  data_original$fu_mes=as.factor(data_original$fu_mes)
  data_original$fu_ds=as.factor(data_original$fu_ds)
  data_original$fu_tri=as.factor(data_original$fu_tri)
  data_original$fu_dm=as.factor(data_original$fu_dm)
  data_original$fu_a=as.factor(data_original$fu_a)
  
  str(data_original)


#### 2. Análisis exploratorio #######

#### Explorar variable respuesta 
####Histograma sencillo 
  
v_eliminar=c('id','fecha_ultima_compra','fecha_ultima_compra2')

data<-select(data_original,-v_eliminar) #Eliminar variables repetidas y id que no se va a usar.
  
hist(data$y/1000000, main= "Histograma de compras mensuales por cliente",
     ylab="Frecuencia", xlab="Promedio compras mensuales por cliente (millones de pesos)") ##

boxplot(data$y, horizontal = T)

qqPlot(data$y)

jarque.bera.test(data$y)



####Histograma fancy

plot_ly(data=data,x=~y  )%>%
  layout(title="Histograma de compras mensuales por cliente",
         yaxis=list(title="Frecuencia"),xaxis=list(title="Histograma de compras mensuales por cliente"))



####### separar variables categóricas y numéricas


cat<-c(2,8,17,16,9,19,20,21,22,23,24)

data_cat<-data[ ,cat]

data_num<-data[,-cat]



####### analizar categoricas concluir cuáles tiene influencia
boxplot(data$y~data$dia_sem_mas_frecuente)
boxplot(data$y~data$genero)

plot_ly(data=data, y=data$y, x=data$genero, type="box")

##### el género no tiene influencia


plot_ly(data=data, y=data$y, x=data$reportado_data_credito, type="box")

plot_ly(data=data, y=data$y, x=data$cat_principal, type="box")

plot_ly(data=data, y=data$y, x=data$medio_apgo, type="box")



###Analizar correlaciones
mc<-cor(data_num)
corrplot(mc,type="upper")


#### 3. Ajsutar mejor modelo posible #####



###  Eliminación de inusuales (podría ser parte de la limpieza si se quiere)

######La eliminación de atipicos en la variable respuesta es fundamental 

dim(data)

lim_sup<- quantile(data$y,0.75) +(IQR(data$y)*1.5 ) 
lim_inf<- quantile(data$y,0.25) - (IQR(data$y)*1.5 ) 

data=subset(data, data$y<=lim_sup & data$y>lim_inf)

dim(data)

#### Eliminar atipicos segun influencia del modelo (atípicos variables explicativas)

mod<-lm(y~.,data=data)
predichos<-predict(mod)
y1<-data$y

datos_influyentes<- influence.measures(mod)
influyentes<-summary(datos_influyentes)
filas_filtrar<-as.numeric(row.names(influyentes))
data2<-data[-filas_filtrar, ] ### datos eliminando atípicos

mod2<-lm(y~.,data=data2)
predichos2<-predict(mod2)
y2=data2$y

dim(data)
dim(data2)


mape(y1,predichos) # mape entrenamiento antes de eliminar inusuales

mape(y2,predichos2) #mape entrenamiento después de eliminar inusuales



#############Separar la muestra de datos en entrenamiento y prueba 

set.seed(987)
filas_train<-sample(1:length(data2$y),39000)


data_sinid<-data2[filas_train,]###asignar base de entrenamiento  eliminando variable id

data_test_sinid<-data2[-filas_train,] #### guardar variable de evaluaciÃ³n

# ajustar modelo en entrenamiento

modelo<-lm(data=data_sinid,y~.)

summary(modelo)

mape(data_test_sinid$y, predict(modelo,data_test_sinid))

###### depurar - seleccionar variables

# Eliminar variables que no pueda calcular cooeficiente


# verificar la colinealidad de variables a eliminar con VIF


data_dep=data_sinid[,-c(12,15,22)]
modelo2<-lm(data=data_dep,y~.)
summary(modelo2)
mape(data_test_sinid$y, predict(modelo2,data_test_sinid))



####### mirar efecto de multicolinealidad 

  vifs<-data.frame(VIF(modelo2))
  vifs[order(vifs$GVIF, decreasing=T),]
  
  
  data_dep2=data_dep[,-c(1,5)]
  modelo3<-lm(data=data_dep2,y~.)
  summary(modelo3)
  
  
  vifs<-data.frame(VIF(modelo3))
  vifs[order(vifs$GVIF, decreasing=T),]
  
  
  
  #evaluar sin con eliminación cambian indicadores
  
  #evaluación con variables VIf>5
  
  #alculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)
  
  predict_test<-predict(modelo2,data_test_sinid)
  
  mape(data_test_sinid$y,predict_test)
  rmse(data_test_sinid$y,predict_test)
  
  
  
  #evaluación sin variables VIf>5
  
  #Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)
  
  predict_test<-predict(modelo3,data_test_sinid)
  mape(data_test_sinid$y,predict_test)
  rmse(data_test_sinid$y,predict_test)
  
  #Como el mape no mejora, se dejan las variables con colinealidad data_dep, y modelo2
  

#  Usar StepWise sobre modelo 2 dejando las variables correlacionadas
  
  modelo_reducido<-stepAIC(modelo2)
  summary(modelo_reducido)
  
  
  #Calculo de indicador en muestra de entrenamiento
  mape(data_dep$y,predict(modelo_reducido))
  rmse(data_dep$y,predict(modelo_reducido))
  AIC(modelo_reducido)
  BIC(modelo_reducido)

  
  
  #Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)
  
  predict_test<-predict(modelo_reducido,data_test_sinid)
  mape(data_test_sinid$y,predict_test)
  rmse(data_test_sinid$y,predict_test)

  
  ### indicadores modelo anterior ##
  
  predict_test<-predict(modelo2,data_test_sinid)
  mape(data_test_sinid$y,predict_test) 
  rmse(data_test_sinid$y,predict_test)

#### 4. Análisis de supuestos #####
  
  ###Al modelo final validar supuestos y concluir sobre ellos, 
  ###en caso de que no se cumplan buscar correcciones para los incumplimientos o 
  ####justificar si el uso del moedelo no requiere cumplimiento de supuestos.
    

  #Supuestos para regresion 
  
  #Guardar residuales en viarble para analizarlo
  residuales<-modelo_reducido$residuals
  
  # 1 Supuesto de normalidad -Graficos  
  #Histograma
  par(mfrow=c(1,3))
  hist(residuales)
  
  #qqplot fancy
  qqPlot(residuales) # Si se salen de las bandas generalmente no cumple normalidad

  # boxplot 
  boxplot(residuales, horizontal = T)
  
  
  #  1 Supuesto de normalidad -pruebas estadísticas 
  
  #shapiro.test(residuales) #shapiro wilk no sirve para más de 5000 observaciones
  
  jarque.bera.test(residuales) #jarque bera test  
  ks.test(x=residuales,pnorm,0,sd(residuales)) #kolmogorov smirnov
  ad.test(residuales, pnorm, 0, sd(residuales)) #Anderson darling
  cvm.test(residuales,pnorm, 0, sd(residuales))  ###Cramer von mises
  
  
  #  2 supuesto Varianza constante - graficos

  plot(predict(modelo_reducido),residuales)
  abline(h=0)
  
  # 2 supuesto Varianza constante - prueba estadísticas 
  bptest(modelo_reducido) ##breusch pagan test

  


  
  # 3 supuestos independencia de errores pruebas de independencia 
  
  par(mfrow=c(1,3))
  
  #Orden vs residuales
  plot(residuales)
  #acf y pacf
  acf(residuales)
  pacf(residuales)
  
  #Prueba de durbin watson
  dwtest(modelo_reducido) 
  bgtest(modelo_reducido)
  
  
  
#### 5. Inferencias y análisis ####
  
  
  #### 1.Significancia de los coeficientes 
  
  #### 2. interpretación de los coeficientes 
  
  ##### 3. Intervalo de confianza para coeficientes

  
  
  
  