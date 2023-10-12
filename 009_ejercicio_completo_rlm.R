library(dplyr) ### para utilizar funcion select case when


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

install.packages("randomForest") ### para imputación
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


