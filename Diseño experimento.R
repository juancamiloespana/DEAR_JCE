library(dplyr)
library(ggplot2)

#Base de Datos
DF<-DF



#Analisis

aditivo<-factor(DF$`Tipo de adictivo`)
tipo_made<-factor(DF$`Tipo de madera`)
tipo_pega<-factor(DF$`Tipo de pegamento`)

#SUPUESTOS DE ANOVA
install.packages("car")
library(car)

Modelo<-lm(DF$Resistencia~ DF$`Tipo de adictivo`+DF$`Tipo de madera`+DF$`Tipo de pegamento`)
Anova<-aov(Modelo)
summary(Anova)


#Tukey

library(multcompView)

Tukey<-TukeyHSD(Anova)


#Diagrama de dispersion

plot(Modelo$residuals, type = "p")

#Normalidad
qqnorm(Modelo$residuals)
qqline(Modelo$residuals)


hist(Modelo$residuals)

shapiro.test(Modelo$residuals)

#Homocedasticidad

library(lmtest)
bptest(Modelo)

#ANOVA
#Anova tipo de madera y tipo de aditivo


Modelo1<-lm(DF$Resistencia~ DF$`Tipo de adictivo`+DF$`Tipo de madera`)
Anova1<-aov(Modelo1)
summary(Anova1)

#Anova tipo de pegamento y tipo aditivo

Modelo2<-lm(DF$Resistencia~ DF$`Tipo de adictivo`+ DF$`Tipo de pegamento`)
Anova2<-aov(Modelo2)
summary(Anova2)





#PROCEDIMIENTO LSD

install.packages(agricolae)

library(agricolae)


#Se analizan las diferencias por medio del procedimiento LSD
#Tipo de pegamento

LSD_Pega<-LSD.test(y=Anova,trt="DF$`Tipo de adictivo`", group=T , console=T)
bar.group(x=LSD_Pega$groups, horiz=T , col="red",
          xlab="resis",
          ylab="tipo_pega",
          xlim=c(0,30),
          main="Comparaciones de los tipos de pegamento \n Procedimiento LSD")

#Se analizan las diferencias por medio del procedimiento LSD
#Alturas

LSD_Pega<-LSD.test(y=Anova,trt="DF$`Tipo de pegamento`", group=T , console=T)
bar.group(x=LSD_Pega$groups, horiz=T , col="red",
          xlab="resis",
          ylab="alturas",
          xlim=c(0,30),
          main="Comparaciones de los alturas \n Procedimiento LSD")         

