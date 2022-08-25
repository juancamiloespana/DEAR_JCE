
#####Paquetes
#install.packages("gplots") ####Paquete para hacer gráfico de medias
#install.packages("agricolae") ### para comparación de tratamientos
#install.packages("pwr") ### para calculo de muestra 




##### librerias
library(pwr)
library(gplots)
library(agricolae)
library(lmtest)

#########cargar datos

### Para analizar los datos en r, solo puede haber una columna por cada variable
url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/oxigeno.csv"

datos<-read.csv(url,sep=";",dec=",",stringsAsFactors = T)
str(datos)
names(datos)=c("punto", "muestra", "oxigeno")
datos$punto<-as.factor(datos$punto) ### los factores estudiados siempre deben estar en tipo de dato factor

attach(datos)


## Punto 2 y 3


#####Esta función tapply(x,indice,fun) para cada indice aplica la función fun sobre la variable x

tapply(oxigeno,punto,summary)

####equivale a :

summary(datos$oxigeno[datos$Punto==1])
summary(datos$oxigeno[datos$Punto==2])
summary(datos$oxigeno[datos$Punto==3])
summary(datos$oxigeno[datos$Punto==4])




##########  Grafico de medias  ##################################

par(mfrow=c(1,2))

boxplot(oxigeno~ punto)
plotmeans(oxigeno~punto,bars=T)

####Volver a presentacion ###


##############Punto 4###############


anova<-aov(oxigeno~punto,data=datos)
summary(anova)
predict(anova,interval="confidence")
datos<-data.frame(datos,predict(anova,interval="confidence"))


modelo<-lm(oxigeno~punto)
summary(modelo)
datos=data.frame(datos,predict(modelo,interval="confidence"))

#####Si los intervalos se cruzan no hay diferencia estásiticamente significativa entre los tratamient

unique(datos[,c(1,4:9)])

####esta función muestra valores sin repetidos, 
####el vector c indica las columnas que se deben mostrar
###se puede poner lista de columnas separada por ","
### o 4:9 para indicar todas las columnas entre esos números


##############Punto 4: modelo de efectos y de medias

### tabla de medias
medias<-model.tables(x=anova,type="means")
medias


### tabla de efectos
efectos<-model.tables(x=anova,type="effects")
efectos


### Validación de supuestos

###1 normalidad ####
residuales=anova$residuals

hist(residuales)
shapiro.test(residuales)

####2 varianza constatne

bptest(anova)

##3 independencia

bgtest(anova)
dwtest(anova)


### El procedimiento de validacion de supuestos es el mismo que en RLM



#########################Comparación de tratamientos#################

#####En la función se pone en "y" el nombre del modelo de anova
### en "trt" el nombre del trataimeinto en comillas
### En grupo se pone F si no quiero ver pareados los resultados 
#### Si no la lista de tratamientos y cuales no son diferentes



lsd<-LSD.test(y=anova, trt="punto",  group=F)
lsd

hsd<-HSD.test(y=anova,trt="punto",group=F)
hsd


thsd<-TukeyHSD(anova, "punto") ### es la misma que HSD pero con esta función se puede graficar
thsd
plot(thsd)


#################Tamaño muestral


sigma=sd(oxigeno)

hist(oxigeno)
delta=1
d= delta/sigma


#########


pwr.t.test( sig.level=.05, power = .90,n=5)

power.t.test(d=delta,sd=sigma, sig.level = .05,power=.90 )

#### determinar la potencia de la muestra observada


power.t.test(sd=sigma, sig.level = .10,power=0.85,n=5 )

##########En la segunda función se puede omitir
####### sigma pero la muestras tienden a hacer más grandes






