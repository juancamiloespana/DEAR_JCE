


#####Cargar librerías ######
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
library(skimr)


#######1. Leer Base de DAtos-ejemplo ##############


data_original<-read.csv("base_supermercado.csv",dec=",",sep=";",stringsAsFactors = T)


### 2. Describir datos y analizar los tipos de variables ####
#####Rrvisar que las variables queden en el formato correcto 
str(data_original)
skim(data_original)


###Convertir los variables que se consideren necesarias a factores




data_original$cat_principal= as.factor(data_original$cat_principal)
data_original$genero=as.factor(data_original$genero)
data_original$reportado_data_credito= as.factor(data_original$reportado_data_credito)
data_original$medio_apgo= as.factor(data_original$medio_apgo)
data_original$dia_sem_mas_frecuente= as.factor(data_original$dia_sem_mas_frecuente)
data_original$producto_frecuente = as.factor(data_original$producto_frecuente)
data_original$fecha_ultima_compra<-as.factor(data_original$fecha_ultima_compra) 

str(data_original)

####Una variable con muchas categorías es difícil de analizar, puede hacer el modelo lento y 
#### generar sobre ajustes, aunque no hay un número exacto para saber qué son muchas, podría utilizarse más de 20 o 30 como referencia
#### según el estudio este valor se puede aumentar o disminuir
###Además se debe revisar que ninguna categoría tenga pocas observaciones

summary(data_original)



#### 3. Tratar datos faltantes ####

####Faltantes-imputación
table(is.na(data_original))

data_original<-na.roughfix(data_original)
table(is.na(data_original))

#############  resumenes de datos verificar categorías cuántas tienen y cuántas observaciones
data=data_original
summary(data_original)


table(data$reportado_data_credito)

### dado que tiene una categoría con una sola observación se reagrupa:
###Si la base de entrenamiento y evaluación está separada, la reagrupación se hace en las dos bases

data$reportado_data_credito<-case_when(
  data$reportado_data_credito=="SI"~"Reportado",
  
  TRUE~"No reportado")

table(data$reportado_data_credito)

data$reportado_data_credito= as.factor(data$reportado_data_credito) ### para que en el summary quede bien




########## 4. Análisis exploratorio #####

#### Explorar variable respuesta 
####Histograma sencillo 

hist(data$y/1000000, main= "Histograma de compras mensuales por cliente",
     ylab="Frecuencia", xlab="Promedio compras mensuales por cliente (millones de pesos)") ##

boxplot(data$y, horizontal = T)

qqPlot(data$y)

jarque.bera.test(data$y)


####Histograma fancy
plot_ly(data=data,x=~y  )%>%
  layout(title="Histograma de compras mensuales por cliente",
         yaxis=list(title="Frecuencia"),xaxis=list(title="Histograma de compras mensuales por cliente"))


######La eliminación de atipicos en la variable respuesta es fundamental 

lim_sup<- quantile(data$y,0.75) +(IQR(data$y)*1.5 ) 
lim_inf<- quantile(data$y,0.25) - (IQR(data$y)*1.5 ) 

data=subset(data, data$y<=lim_sup & data$y>lim_inf)

##### como se vio en el boxplot e histograma no hay atipicos

####### separar variables categóricas y numéricas


cat<-c(3,9,18,17,10,20,21)

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


### 5. atipicos de variables explicativas ####
data=data[,-c(20,21)]

mod<-lm(data=data,y~.)
predichos<-predict(mod)
mape(data$y,predichos)


datos_influyentes<- influence.measures(mod)
influyentes<-summary(datos_influyentes)
filas_filtrar<-as.numeric(row.names(influyentes))
data=data[-filas_filtrar,]
length(filas_filtrar)


####Verificar si cambió el mape

mod<-lm(data=data,y~.)
predichos<-predict(mod)
mape(data$y,predichos)


#############6. Dividir la muestra de datos en entrenamiento y prueba



split_sample<-function(datos,perc)
{
  id_muestra<-sample(2,length(datos[,1]),replace = T,prob=c(perc,1-perc))
  fila<-seq(1:length(datos[,1]))
  muestra<-data.frame(id_muestra,fila)
  sub<-subset(muestra[,2],muestra$id_muestra==1)
  test<-subset(muestra$fila,muestra$id_muestra==2)
  datos_train<-datos[sub,]
  datos_test<-datos[test,]
  return(list(train=datos_train,test=datos_test))
}


data_split<-split_sample(data,0.8)




data_sinid<-data_split$train[,-1]###asignar base de entrenamiento  eliminando variable id

data_test_sinid<-data_split$test[,-1] #### guardar variable de evaluaciÃ³n



###### depurar - seleccionar variables


####### 7. mirar efecto de multicolinealidad #####

modelo1<-lm(data=data_sinid,y~.) ####ajustar el modelo con todas las variables
summary(modelo1) ## resumen del modelo 


####Calculo de indicador en muestra de entrenamiento 
mape(data_sinid$y,predict(modelo1)) ####estÃ¡n libreria Metrics
mae(data_sinid$y,predict(modelo1))
rmse(data_sinid$y,predict(modelo1)) 
AIC(modelo1)
BIC(modelo1)

#####Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)

predict_test<-predict(modelo1,data_split$test)

mape(data_test_sinid$y,predict_test)
mae(data_test_sinid$y,predict_test)
rmse(data_test_sinid$y,predict_test)

##### Se eliminan variables con singularidad


data_dep<-data_sinid[,-c(15,12)]
data_dep_test<-data_test_sinid[,-c(15,12)]

modelo2<-lm(data=data_dep,y~.)
summary(modelo2)

####Calculo de indicador en muestra de entrenamiento 
mape(data_dep$y,predict(modelo2))
rmse(data_dep$y,predict(modelo2))
AIC(modelo2)
BIC(modelo2)

#####Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)

predict_test<-predict(modelo2,data_split$test)

mape(data_test_sinid$y,predict_test)
rmse(data_test_sinid$y,predict_test)



######## verificar la colinealidad de variables a eliminar con VIF


vifs<-data.frame(VIF(modelo2))

vifs[order(vifs$GVIF),]

str(data_dep)
####eliminar las VIF > 5

vif_grande=c(1,5)
data_dep2<-data_dep[,-vif_grande]


modelo3<-lm(data=data_dep2,y~.)

summary(modelo3)

VIF(modelo3) ### para evaluar las variables posteriormente a la eliminaciÃ³n

####Calculo de indicador en muestra de entrenamiento 

mape(data_dep$y,predict(modelo3))
rmse(data_dep$y,predict(modelo3))
AIC(modelo3)
BIC(modelo3)

#####Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)

predict_test<-predict(modelo3,data_split$test)

mape(data_test_sinid$y,predict_test)
rmse(data_test_sinid$y,predict_test)


####Como el mape no mejora se dejan las variables con colinealidad data_dep<-data_sinid[,-c(15,12)]


############ 8. Usar StepWise sobre modelo 2 dejando las variables correlacionadas####

modelo_reducido<-stepAIC(modelo2)
summary(modelo_reducido)

#arbol<-rpart(data=data_dep,y~.,   control = rpart.control(cp = 0.00005))


#columnas_arbol<-row.names(data.frame(arbol$variable.importance))
#data_arbol_seleccion<-data[,columnas_arbol]


#modelo_arbol_seleccion<-lm(data=data_arbol_seleccion,data$y~.)

#summary(modelo_arbol_seleccion)

summary(modelo_reducido)


####Calculo de indicador en muestra de entrenamiento
mape(data_dep$y,predict(modelo_reducido))
rmse(data_dep$y,predict(modelo_reducido))
AIC(modelo_reducido)
BIC(modelo_reducido)

#####Calculo de indicadores en muestra de evaluaciÃ³n (datos que no se utilizan en ajuste)

predict_test<-predict(modelo_arbol_seleccion,data_split$test)

mape(data_test_sinid$y,predict_test)
rmse(data_test_sinid$y,predict_test)


####Para el árbol

#predict_test<-predict(modelo_reducido,data_split$test)

#mape(data_test_sinid$y,predict_test)


###### 9. Supuestos para regresiÃ³n #########

#Guardar residuales en viarble para analizarlo
residuales<-modelo_reducido$residuals
#### 9.1 Supuesto de normalidad -GrÃ¡ficos  ####
#Histograma
par(mfrow=c(1,3))
hist(residuales)

#qqplot sencillo

qqnorm(residuales)
qqline(residuales)

#qqplot fancy

qqPlot(residuales) # Si se salen de las bandas generalmente no cumple normalidad

# boxplot 

boxplot(residuales, horizontal = T)


#### 9.1 Supuesto de normalidad -pruebas estadÃ­sticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

ks.test(x=residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling

cvm.test(residuales,pnorm, 0, sd(residuales))  ###Cramer von mises


#### 9.2 supuesto Varianza constante - graficos #####
par(mfrow=c(1,2))
plot(data_sinid$y,residuales)
abline(h=0)


plot(predict(modelo_reducido),residuales)
abline(h=0)


#### 9.2 supuesto Varianza constante - prueba estadÃ­sticas ####


bptest(modelo_reducido) ##breusch pagan test


###### 9.3 supuestos independencia de errores -graficos ####
par(mfrow=c(1,3))
#orden vs residuales
plot(residuales)


#acf y pacf

acf(residuales)
pacf(residuales)


#### 9.3 supuestos independencia de errores pruebas de independencia ####

# Prueba de durbin watson

dwtest(modelo_reducido) 

# Prueba de Breusch y godfrey

bgtest(modelo_reducido)









