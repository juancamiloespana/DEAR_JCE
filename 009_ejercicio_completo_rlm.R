library(dplyr) ### para utilizar funcion select case when
library(forecast) ### para independencia 
library(car) ### para función de power transform

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/base_supermercado2.csv'


df=read.csv(url, stringsAsFactors = T)


### tener ciuidado con variables con muchascategorias
##vamos eliminar productos frecuentes

#### hace tratamiento a fecha

fuc= as.Date(df$fecha_ultima_compra, format='%d/%m/%Y')

df2=select(df, -fecha_ultima_compra,-producto_frecuente)

df2$dia_sem_fuc= as.numeric(format(fuc, format="%u"))
df2$mes_fuc= as.numeric(format(fuc, format="%m"))

df2$y=df2$y/1000 ### para que sea más facil de leer

###### datos faltantes o nulos

table(is.na(df2))

#### imputar datos faltantes 

#install.packages("randomForest") ### para imputación
library(randomForest)

df2=na.roughfix(df2)
table(is.na(df2))

###### ver datos atupicos en respuestaa


boxplot(df$y)

###eliminar de respuests

bigot_sup=quantile(df2$y,0.75) + 2*IQR(df2$y)
bigot_inf=quantile(df2$y,0.25) - 2*IQR(df2$y)

df3=subset(df2, df2$y<=bigot_sup & df2$y>bigot_inf)

boxplot(df3$y)

###### ajustar modelo inicial con todas las variables

mod1=lm(y~.-id, data=df3)

summary(mod1)
mean(df3$y)

#### validación rapida de supuestos para identifcar necesidad de transformaciones e interacciones 

hist(mod1$residuals) ### normalidad

plot(mod1$residuals, mod1$fitted.values) ### varainza constante deberia ser distribucion uniforme

###### para validar independencia de residuales

par(mfrow=c(1,2))
Acf(mod1$residuals)
Pacf(mod1$residuals)

##### estrategias para mejorar el modelo 

### transformacion de variable respuesta

trans=powerTransform(mod1)
summary(trans)
#### no hay necesidad de transformar


###probar inteaccion ###

mod2=lm(y~.-id+usa_tc*reportado, data=df3)
summary(mod2)

hist(mod2$residuals)

plot(mod2$residuals, mod2$fitted.values)

par(mfrow=c(1,2))
Acf(mod2$residuals)
Pacf(mod2$residuals)

vif(mod2)

###### modelo 3 eliminando variables con multicolinealidad 

mod3=lm(y~.-id+usa_tc*reportado-edad_cliente, data=df3)

summary(mod3)
vif(mod3)


##### 

library(Metrics)

mape(df3$y, mod3$fitted.values)

#### prediccciones para datos nuevos ####
url2='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/base_supermercado_actualizada.csv'

datos_nuevos= read.csv(url2, stringsAsFactors = T)



pred=predict(mod3, newdata = datos_nuevos,interval = 'prediction', level = 0.99999)

sum(pred[,1]) ### suma promedio compras clientes predichas jun-dic2023

sum(df3$y) ##### suma promedio compras reales en_jun2023

pred[5,1]

cliente_5_20=pred[5,1]*1.2


#### la variable que mpas influye 

summary(mod3)


mean(mod3$residuals)

library(lmtest)
bptest(mod3)

