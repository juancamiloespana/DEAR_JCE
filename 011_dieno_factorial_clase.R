#### librerias
library(agricolae) ### lsd, HSD compara pares de tratamientos
library(graphics) 
library(gplots)### grafica de medias
library(stats) ## interaccion de variables

library(lmtest) ### este para validación de supuestos

###
url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/bateria.csv"

datos_bat=read.csv(url, sep=";", stringsAsFactors = T )

datos_bat$temperatura=as.factor(datos_bat$temperatura) ## los factores deben estar en formato factor
datos_bat$material=as.factor(datos_bat$material)

###Efecto principal(individual) de los facotres

#### se realizan graficos por pares factor/respuesta
par(mfrow=c(1,2))

## para factor temperatura
plotmeans(datos_bat$tiempo_horas~ datos_bat$temperatura)
boxplot(datos_bat$tiempo_horas~ datos_bat$temperatura)


### para factor material
plotmeans(datos_bat$tiempo_horas~ datos_bat$material)
boxplot(datos_bat$tiempo_horas~ datos_bat$material)


### 

plot.design(tiempo_horas~ material+temperatura, data=datos_bat)


##### grafico de interacción

interaction.plot(x.factor=datos_bat$temperatura, trace.factor=datos_bat$material, response=datos_bat$tiempo_horas)

interaction.plot(x.factor=datos_bat$material, trace.factor=datos_bat$temperatura, response=datos_bat$tiempo_horas)


#### tamaño de muestra

desv=sd(datos_bat$tiempo_horas)
delta= 47
beta= 0.1
pot= 1-beta
n= 4

power.t.test( n=n,delta=delta,sd=desv, power=pot, sig.level = NULL)



######## ajustar modelo anova


an1=aov(tiempo_horas~temperatura*material, data=datos_bat)
summary(an1)

#######

model.tables(an1, type = "means")
model.tables(an1, type = "effects")


#### verificar supuestos
res=an1$residuals

#### normalidad
shapiro.test(res) ## se cumple supuesto porque se acepta la hipotesis nula 0.61 > 0.05


### varianza constante

bptest(an1)## se cumple supuesto porque se acepta la hipotesis nula 0.2032 > 0.05


### independencia 

bgtest(an1) #se cumple supuesto porque se acepta la hipotesis nula 0.89 > 0.05


######## comparación de tratamientos ####

lsd=LSD.test(y=an1, trt=c("temperatura","material"), group=F)
lsd



HSD=HSD.test(y=an1, trt=c("temperatura","material"), group=F)
HSD
