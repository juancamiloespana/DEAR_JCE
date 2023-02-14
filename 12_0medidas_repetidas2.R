
######### Isntalar paquetes ##########
install.packages("datarium")  ###Paquete con datasets
install.packages("tidyverse") #### para utilizar funciones de manipulación de datos y  operador %>%
install.packages("ggplot") #### para plotmeasn
install.packages("rstatix") ### para función shapiro_test agrupada
install.packages("ggpubr") ####Gráficos para vraibles agrupadas
install.packages("forcats")
install.packages("WRS2") ###Base de datos hangover

########### cargar librerías
library(datarium)
library(tidyverse) 
library(gplots) 
library(rstatix)
library(ggpubr)
library(WRS2) ###Paquete con conjunto de datos ejemplo




############################################
########## E.1 CArgar datos  Intra sujetos puro 1 vía- 1 factor ##########
############################################


data("selfesteem")




#############Convertir formato de datos de wide a long

selfesteem <- selfesteem %>%
gather(key = "time", value = "score", t1, t2, t3)


### factores y sujetos deben estar en formato factor
selfesteem$time=as.factor(selfesteem$time)
selfesteem$id=as.factor(selfesteem$id)


attach(selfesteem)


############# Análisis exploratorio

par(mfrow=c(1,2))
boxplot(score~time)
plotmeans(score~time)

interaction.plot(time,id,score)
interaction.plot(id,time,score)

####Interacción entre tratamiento y sujeto es el error en medidas repetidas ####

##########Supuesto de normalidad método gráfico


ggqqplot(selfesteem, "score", facet.by = "time")


########### Supuestos de normalidad prueba de hipotesis

selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)


###########ANOVAs

###ajustar sin formula

automestima1<-anova_test(data=selfesteem,dv=score,wid=id,within=time)
#### ajustar con formula


f=score~time+Error(id/time)
automestima1.1<-anova_test(data=selfesteem,formula=f)

### estas dos anovas ajustadas son la misma, son dos formasde ajsutar la misma anova, usar cualquiera de las dos
automestima1
automestima1.1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

### esta es otra anova que muestra información adicional
automestima2=aov(data=selfesteem,f)
summary(automestima2)

###La anova de aov permite ver las suma de cuadrados, los cuadrados medios y la suma de cuadrados de los sujetos


###Comparación de tratamientos #

pairwise.t.test(score,time,paired=T,p.adjust.method = "bonferroni") ### para un solo factor





############################################
###### Ejercicio 2  análisis de sudoku ###
###########################################3

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/tiempos_sudokus.csv'
sudoku=read.csv(url,sep=";",stringsAsFactors = T)


### realizar ejercicio de sudokus  




############################################
######E3. Cargar datos Intra sujetos puro 2 vías #####
############################################

data("selfesteem2", package = "datarium")
head(selfesteem2)



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

par(mfrow=c(1,1))
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


f=score~time*treatment+Error(id/(treatment*time))

automestima1<-anova_test(data=selfesteem2,f)
automestima1


#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

automestima2=aov(f, data=selfesteem2)
summary(automestima2)

###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #

pwc <- selfesteem2 %>%
  group_by(treatment) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


pwc <- selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc


############################################
##### E4. Cargar Datos Mixto 2 vías ############
############################################


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


f=score~time*group+Error(id/time)

ansiedad1<- anova_test( data = anxiety, f)
ansiedad1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

ansiedad2=aov(f, data=anxiety)
summary(ansiedad2)

###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #
pwc <- anxiety %>%
  group_by(group) %>%
  pairwise_t_test(score ~ time, p.adjust.method = "bonferroni", paired=T)
pwc


pwc <- anxiety %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group, p.adjust.method = "bonferroni", paired=T)
pwc

############################################
#####################E5 hagover ##########
############################################


#####Cargar base de datos y convertir a factor ###

data(hangover)
attach(hangover)