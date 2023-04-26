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
df2$ds_fuc=weekdays(df2$fecha_ultima_compra2) ##extrae el dia de la semana de una fecha
df2$q_fuc=quarters(df2$fecha_ultima_compra2) ### extrae el trimestre
df2$dm_fuc=format(df2$fecha_ultima_compra2, format='%d') ### extrae el día del mes


#### convertirlas a fomato facotr
df2$m_fuc=as.factor(df2$m_fuc)
df2$ds_fuc= as.factor(df2$ds_fuc)
df2$q_fuc=as.factor(df2$q_fuc)
df2$dm_fuc=as.numeric(df2$dm_fuc)

df2=df2[,-21 ] ### para eliminar la columna fecha ultima compra



###############################################################################################
## 2. Análisis exploratorio #####################################################################
################################################################################################


#### Explorar variable respuesta 
####Histograma sencillo 

hist(df2$y)

####Histograma fancy
plot_ly(data=df2, x=~y, type="histogram")%>%
layout(title='Histograma promedio de compras')

#### separar variables categóricas y numéricas
df_num=df2%>%select_if(is.numeric) ### selecciono solo las numericas
df_fac=df2%>%select_if(is.factor) ### selecciono solo las numericas

#### analizar categoricas concluir cuáles tiene influencia

boxplot(data=df2, y~genero)
plot_ly(data=df2, y=~y, x=~genero, type="box")%>%
  layout(title="Promedio de compra por género")


plot_ly(data=df2, y=~y, x=~reportado_data_credito, type="box")%>%
  layout(title="Promedio de compra por género")


plot_ly(data=df2, y=~y, x=~medio_apgo, type="box")%>%
  layout(title="Promedio de compra por género")


plot_ly(data=df2, y=~y, x=~producto_frecuente, type="box")%>%
  layout(title="Promedio de compra por género")


plot_ly(data=df2, y=~y, x=~ds_fuc, type="box")%>%
  layout(title="Promedio de compra por género")

### Analizar correlaciones de numericas 

mc=cor(df_num)

corrplot(mc)


#############################################################################################
#### 3. Ajsutar mejor modelo posible ########################################################
#############################################################################################
set.seed(123) ### para que resultado sea igual
df=sample.split(df2$id,SplitRatio = 0.8) ### separar muestra eval- entrenamiento
df_train=subset(df2[,-1], df==T) ## muestra entrenamiento
df_test=subset(df2[,-1], df==F) ### muestra de evaluacion

mod1=lm(y~. -prop_compras_bebe -q_fuc, data=df_train) ### primer modelo
summary(mod1)
AIC(mod1) ## calucla AIC

###ind entrenamiento####

pred=predict(mod1) ### predicciones entrenamiento
mape(df_train$y, pred)#### mape entrenamiento
rmse(df_train$y, pred) ##rmse entrenamiento

##### indicadores evaluacion

pred_test=predict(mod1, newdata=df_test) ### prediccion evaluación
mape(df_test$y, pred_test) ## mape evaluación
rmse(df_test$y, pred_test)

#
### Eliminar atipicos  y medir impacto en evaluacion y entrenamiento

im=influence.measures(mod1) ### tabla de datos de influencia
im_at=as.numeric(row.names(summary(im))) ### filas en las que identifco atipicos

df_train2=df_train[-im_at,] ### eliminar atipicos

mod2=lm(y~., data=df_train2)
AIC(mod2)

pred_train=predict(mod2)
mape(df_train2$y, pred_train)
rmse(df_train2$y, pred_train)

### evaluación
pred_test=predict(mod2, newdata=df_test)
mape(df_test$y, pred_test)
rmse(df_test$y, pred_test)


#### verificar la multicolinealidad de variables a eliminar con VIF

summary(mod1)

df_vif=data.frame(VIF(mod1))
arrange(df_vif, -GVIF)

mod1=lm(y~. -prop_compras_bebe -q_fuc-prop_compras_verduras-edad_cliente, data=df_train)

df_vif=data.frame(VIF(mod1))
arrange(df_vif, -GVIF)

pred_train=predict(mod1)
mape(df_train$y, pred_train)


pred_test=predict(mod1, newdata=df_test)
mape(df_test$y, pred_test)

####  seleccionar variables

mod_reducido=stepAIC(mod1)
summary(mod_reducido)

AIC(mod_reducido)
pred_test=predict(mod_reducido, newdata=df_test)
mape(df_test$y, pred_test)
#### validar interacciones 

mod1=lm(y~.-prop_compras_bebe -q_fuc-prop_compras_verduras-edad_cliente+num__prom_compras_mensuales*valor_promedio_por_compra, data=df_train)
summary(mod1)
### no es significativa la interacción

### validar transformaciones


neg=df_train[df_train$y>=0,]
mod1=lm(y~.-prop_compras_bebe -q_fuc-prop_compras_verduras-edad_cliente+num__prom_compras_mensuales*valor_promedio_por_compra, data=neg)
modelo_reducido=stepAIC(mod1)

powerTransform(modelo_reducido) ### muestra que no requiere transformación
### esta función sólo se puede aplicar a valores positivos de y


############################################################################################
#### 4. Análisis de supuestos ##############################################################
############################################################################################
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




