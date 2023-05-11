
#####Paquetes
#install.packages("gplots") ####Paquete para hacer gráfico de medias
#install.packages("agricolae") ### para comparación de tratamientos
#install.packages("pwr") ### para calculo de muestra 

##### librerias
library(pwr) ### tamaño de muestra
library(gplots) ## gráfico de medias
library(agricolae) ### comparación de tratamiento
library(lmtest) ### prueba de hipótesis

#########cargar datos

### Para analizar los datos en r, solo puede haber una columna por cada variable
### cargar datos oxigeno
### los factores debene estar en formato factor

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/oxigeno.csv'

datos= read.csv(ruta, sep=';', dec=",")

datos$Punto=as.factor(datos$Punto)

## Punto 2 y 3

plotmeans(data=datos, oxigeno_disuelto~Punto)



# Punto 4  anova y significancia del modelo###############

mod1=aov(data=datos,oxigeno_disuelto~Punto)
summary(mod1)



###### Punto 5: modelo de efectos y de medias

### tabla de medias

model.tables(mod1, type="means") ### calcular medias

unique(predict(mod1)) ### predicciones unicas, hay tantas predicciones como tratamientos

### tabla de efectos
model.tables(mod1, type="effects")

####punto 6 validar supuestos ##########
res=mod1$residuals

###
hist(res)

shapiro.test(res) ## se cumple supuesto de normalidad de residuales valor p > a 0.05(significancia)

bptest(mod1) ## se cumple supuesto de varianza constante valorP > 0.05

bgtest(mod1) ## se cumple supuesto de independencia

dwtest(mod1) ### se cumple supuesto de independencia



#### punto 7 Comparación de tratamientos  #############

lsd=LSD.test(y=mod1, trt="Punto",group=F)
lsd

hsd=HSD.test(y=mod1, trt="Punto",group=F)
hsd

#### punto 8 tamaño de muestras ###################

