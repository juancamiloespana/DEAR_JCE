
rm(list=ls())
###########

library(graphics)  ### grafico de interacción
library(gplots) ## gráfico de medias 



########### ejercicio 1: Crecimiento de bacterias
url_crecbac='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/crecimiento_bacterias.csv'
datos<-read.csv(url_crecbac,sep=";", dec=",")


datos$tiempo=as.factor(datos$tiempo)
datos$medio=as.factor(datos$medio)



attach(datos)



#########

boxplot(crecimiento~tiempo)
boxplot(crecimiento~medio)

#######

plotmeans(crecimiento~tiempo,bars=T)
plotmeans(crecimiento~medio,bars=T)



formula <- crecimiento  ~ tiempo + medio
plot.design(formula, col= "coral", xlab="Efectos", ylab="Promedio de crecimiento")


interaction.plot(medio, tiempo,crecimiento, ylab="Promedio de crecimiento")



###############################Ejercicio 2: baterias

rm(list=ls())
url_bat='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/bateria.csv'

bateria<-read.csv(url_bat,sep=";")


bateria$temperatura=as.factor(bateria$temperatura)
bateria$material=as.factor(bateria$material)

attach(bateria)


###### Análisis exploratorio ####


### Efectos principales


par(mfrow=c(1,2))

boxplot(tiempo_horas~temperatura)
boxplot(tiempo_horas~material)

#######

plotmeans(tiempo_horas~temperatura,bars=T)
plotmeans(tiempo_horas~material,bars=T)


formula <- tiempo_horas~temperatura+material
plot.design(formula, col= "coral", xlab="Efectos", ylab="Promedio de crecimiento")

#### Efectos de interaccio

par(mfrow=c(1,2))

interaction.plot(temperatura, material, tiempo_horas, ylab="Tiempo en horas")

interaction.plot( material,temperatura, tiempo_horas, ylab="Tiempo en horas")


#### Ajsutar modelo y mirar singificancia de los efectos ###

anova_bat=aov(tiempo_horas~material+temperatura+temperatura*material)
summary(anova_bat)

### tabla de efectos
efectos<-model.tables(x=anova_bat,type="effects")


### tabla de medias
medias<-model.tables(x=anova_bat,type="means")


#### Verificación de supuestos con residuales ###


residuales= anova_bat$residuals


