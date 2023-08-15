#####CArgar paquetes #############3

library(regclass) ### Para calcular VIF
library(plotly)  ###para gráficos Fancys
library(lmtest)
################################################
### Variable cualitativa  ####
################################################

####datos 'los_cual.csv'
### predecir los(y) con base en edad y antibitoicos
#####

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/los_cual.csv'
df=read.csv(ruta)

mod=lm(y~., data=df) ## el punto indica que utilice todas las variables menos la respuesta como explicativas
summary(mod)

res=mod$residuals

hist(res, breaks=15)

shapiro.test(res)
bptest(mod)
bgtest(mod)

############################################
################Interaccion ##############
##########################################

###datos 'los_intera.csv'
###predecir los(y) con base en edad y antibioticos
####

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/los_intera.csv'
df=read.csv(ruta)
################ Analizar gráficamente ############

plot_ly(data=df,y=~y, x=~edad, color=~antibiotico)

###ver correlación entre variables explicativas

boxplot(edad~antibiotico, data=df)

cor(df$antibiotico, df$edad)
####ajustar modelo rl con interacción

mod=lm(y~edad*antibiotico, data=df)
summary(mod)

mod2=lm(y~., data=df)
summary(mod2)

##### analizar resumen



###############################
######### Analizar de VIF #####
###############################


#### Problema

####Suponga y utilidad de empresa
#### suponga x gasto en publicidad
#### Suponga x2 gasto en calidad

###### se quiere comprobar si aumentar gasto en publicidad mejora utilidades


###### Ajuste un modelo con sólo el gasto en calidad
###### Cuánto crece la utilidad si se aumento una unidad la inversion en calidad



### Agregue a ese modelo la variable publicidad
### ¿Qué pasa con la relación entre utilidad y calidad?


### Verifique la multicolinealidad con funcion vif











