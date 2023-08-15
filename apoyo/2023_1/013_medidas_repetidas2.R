
######### Isntalar paquetes ##########
install.packages("datarium")  ###Paquete con datasets
install.packages("tidyverse") #### para utilizar funciones de manipulación de datos y  operador %>%
install.packages("ggplot") #### para plotmeasn
install.packages("rstatix") ### para función shapiro_test agrupada
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
data("selfesteem") ## carga datos

#############Convertir formato de datos de wide a long

df_auto=gather(selfesteem, key='time', 'auto', t1, t2, t3)



### factores y sujetos deben estar en formato factor
df_auto$id=as.factor(df_auto$id)
df_auto$time=as.factor(df_auto$time)

############# Análisis exploratorio
boxplot(data=df_auto, auto~time)

attach(df_auto) ## reconozca columnas como variabels
#### Interacción entre tratamiento y sujeto es el error en medidas repetidas ####
interaction.plot(id, time, auto)

### Supuesto de normalidad método gráfico
ggqqplot(df_auto, 'auto', facet.by = 'time')

########### Supuestos de normalidad prueba de hipotesis

df_auto%>%group_by(time)%>%shapiro_test(auto)

###########ANOVAs

formula=auto~time +Error(id/time)

an1=anova_test(df_auto, formula)
an1

an2=aov(data=df_auto, formula)
summary(an2)



###Comparación de tratamientos #

pairwise.t.test(auto,time, paired = T, p.adjust.method = 'bonferroni')



############################################
###### Ejercicio 2  análisis de sudoku ###
###########################################3


### realizar ejercicio de sudokus  


############################################
######E3. Cargar datos Intra sujetos puro 2 vías #####
############################################
data("selfesteem2")


df_auto2=gather(selfesteem2, key=  'time', 'auto', t1, t2, t3)
attach(df_auto2)
############# Análisis exploratorio

par(mfrow=c(1,2))
boxplot(auto~time)
boxplot(auto~treatment)

interaction.plot(time, treatment, auto)
interaction.plot( treatment,time, auto)

##########Supuesto de normalidad método gráfico
ggqqplot(df_auto2, 'auto', facet.by=c('treatment', 'time'))

########### Supuestos de normalidad prueba de hipotesis

df_auto2%>%group_by(treatment, time)%>%shapiro_test(auto)


###########ANOVAs

formula2=auto~time*treatment + Error(id/(time*treatment))
an1=anova_test(df_auto2,formula2 )
an1


an2=aov(data=df_auto2, formula2)
summary(an2)
###LA anova de aov permite ver las suma de cuadrados

###Comparación de tratamientos #
pwc=df_auto2%>%group_by(treatment)%>%
  pairwise_t_test(auto~time,paired=T, p.adjust.method = 'bonferroni')

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

table(anxiety$time)

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


f=score~time*group+Error(id/time) ## solo se pone en el eror el factor intrasujeto

### el grupo es entre sujeto

ansiedad1<- anova_test( data = anxiety, f)
ansiedad1

#####la columna ges (Generalize effect size ses la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)

ansiedad2=aov(f, data=anxiety)
summary(ansiedad2)


###Comparación de tratamientos #
pwc <- anxiety %>%
  group_by(group) %>%
  pairwise_t_test(score ~ time, p.adjust.method = "bonferroni", paired=T)
pwc


pwc <- anxiety %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group, p.adjust.method = "bonferroni", paired=T)
pwc
