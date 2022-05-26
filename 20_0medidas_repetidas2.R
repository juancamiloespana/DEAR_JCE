############# Borrar datos en memoria

rm(list=ls())

######### Isntalar paquetes ##########
install.packages("datarium")  ###Paquete con datasets
install.packages("tidyverse") #### para utilizar funciones de manipulación de datos y  operador %>%
install.packages("ggplot") #### para plotmeasn
install.packages("rstatix") ### para función shapiro_test agrupada
install.packages("ggpubr") ####Gráficos para vraibles agrupadas
install.packages("forcats")

########### cargar librerías
library(datarium)
library(tidyverse) 
library(gplots) 
library(rstatix)
library(ggpubr)

########## E.1 CArgar datos  Intra sujetos puro 1 vía ##########

data("selfesteem")

selfesteem$id=as.factor(selfesteem$id)


#############Convertir formato de datos de wide a long

selfesteem <- selfesteem %>%
gather(key = "time", value = "score", t1, t2, t3)

selfesteem$time=as.factor(selfesteem$time)


attach(selfesteem)


############# Análisis exploratorio

par(mfrow=c(1,2))
boxplot(score~time)
plotmeans(score~time)

interaction.plot(time,id,score)
interaction.plot(id,time,score)

##########Supuesto de normalidad método gráfico


ggqqplot(selfesteem, "score", facet.by = "time")


########### Supuestos de normalidad prueba de hipotesis

selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)


###########ANOVAs

automestima1<-anova_test(data=selfesteem,dv=score,wid=id,within=time)
automestima1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)


automestima2=aov(score~time+Error(id))
summary(automestima2)

###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #

pairwise.t.test(score,time,paired=T,p.adjust.method = "bonferroni") ### para un solo factor


######E2. Cargar datos Intra sujetos puro 2 vías #####

data("selfesteem2", package = "datarium")
head(selfesteem2)
selfesteem2$id=as.factor(selfesteem2$id)


selfesteem2 <- selfesteem2 %>%
  gather(key = "time", value = "score", t1, t2, t3)

selfesteem2$time=as.factor(selfesteem2$time)
attach(selfesteem2)

############# Análisis exploratorio

par(mfrow=c(1,2))
boxplot(score~time)
plotmeans(score~time)

par(mfrow=c(1,2))
boxplot(score~treatment)
plotmeans(score~treatment)


boxplot(score~treatment+time)



interaction.plot(time,treatment,score)
interaction.plot(treatment, time,score)

##########Supuesto de normalidad método gráfico


ggqqplot(selfesteem2, "score", facet.by = c("time","treatment"))


########### Supuestos de normalidad prueba de hipotesis

selfesteem2 %>%
  group_by(time,treatment) %>%
  shapiro_test(score)


###########ANOVAs

automestima1<-anova_test(data=selfesteem2,dv=score,wid=id,within=c(time,treatment))
automestima1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

automestima2=aov(score~time*treatment+Error(id))
summary(automestima2)

###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #

pwc <- selfesteem2 %>%
  group_by(c(treatment)) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

table(selfesteem2)

table(selfesteem2$treatment)

##### E3. Cargar Datos Mixto 2 vías ############

data("anxiety", package = "datarium")
head(anxiety)


####Convertir a formato long y cambiar formatos

anxiety <- anxiety %>%
  gather(key = "time", value = "score", t1, t2, t3)

anxiety$id<-as.factor(anxiety$id)
anxiety$time<-as.factor(anxiety$time)

attach(anxiety)

############# Análisis exploratorio

par(mfrow=c(1,2))
boxplot(score~time)
plotmeans(score~time)

par(mfrow=c(1,2))
boxplot(score~group)
plotmeans(score~group)


boxplot(score~time+group)


  interaction.plot(time,group,score)
interaction.plot(group, time,score)

##########Supuesto de normalidad método gráfico


ggqqplot(anxiety, "score", facet.by = c("time","group"))


########### Supuestos de normalidad prueba de hipotesis

anxiety%>%
  group_by(time,group) %>%
  shapiro_test(score)


###########ANOVAs

ansiedad1<- anova_test(
  data = anxiety, dv = score, wid = id,
  between = group, within = time
)
ansiedad1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

ansiedad2=aov(score~time*group+Error(id))
summary(ansiedad2)

###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #
pwc <- anxiety %>%
  group_by(group) %>%
  pairwise_t_test(score ~ time, p.adjust.method = "bonferroni", paired=T)
pwc


