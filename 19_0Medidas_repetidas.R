


install.packages("WRS2") ###Base de datos hangover
install.packages("ggpubr") ####Gráficos para vraibles agrupadas
install.packages("rstatix") ### para función shapiro_test agrupada


library(gplots) ### para gráfico de plotmeans
library(car) ###función aov
library(agricolae) ###Para comparación de tratamientos
library(WRS2) ###Paquete con conjunto de datos ejemplo
library(ggpubr) ####Gráficos para vraibles agrupadas
library(rstatix) ### para función shapiro_test agrupada


#####################Ejercicio hagover

#####Cargar base de datos y convertir a factor ###

data(hangover)
attach(hangover)

hangover$id=as.factor(hangover$id)

######## Análisis exploratorio

par(mfrow=c(1,2))

boxplot(hangover$symptoms~hangover$time)
plotmeans(hangover$symptoms~hangover$time)

boxplot(hangover$symptoms~hangover$group)
plotmeans(hangover$symptoms~hangover$group)


########### Interacciones ########

####entre factores ####

interaction.plot(time,group,symptoms)

interaction.plot(group,hangover$time,symptoms)


############## Validación de supuestos medidas repetidas ########

#####Normalidad por tratamiento, gráfico

ggqqplot(hangover, "symptoms", facet.by = c("time","group"))


#### Normalidad por tratamiento pruebas

hangover %>%
  group_by(time, group) %>%
  shapiro_test(symptoms)



#####modelo para supuestos de esfericidad


anova_hang<-anova_test(data=hangover,dv=symptoms,wid=id,within= time, between=group)
anova_hang


anova_hang2=aov(symptoms~ time*group +Error(id))
summary(anova_hang2)


#####la columna ges (Generalize effect size es la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)



#####Comparación de tratamientos

pairwise.t.test(symptoms,time,paired=T,p.adjust.method = "bonferroni") ### para un solo factor



### prueba para varios factores
pwc <- hangover %>%
  group_by(c(time)) %>%
  pairwise_t_test(
    symptoms ~ group, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

pwc

############################################
###### Analisis de sudoku ###
###########################################3


sudoku=read.csv("tiempos_sudokus_nn.csv",sep=";",stringsAsFactors = T)
sudoku$num=as.factor(sudoku$num)
attach(sudoku)



######## Análisis exploratorio



boxplot(tiempo~tipo)
plotmeans(tiempo~tipo)





########### Interacciones ########

####entre factores ####

interaction.plot(num,tipo,tiempo)

interaction.plot(tipo,num,tiempo)


############## Validación de supuestos medidas repetidas ########

#####Normalidad por tratamiento, gráfico

ggqqplot(sudoku, "tiempo", facet.by = "tipo")


#### Normalidad por tratamiento pruebas

sudoku %>%
  group_by(tipo) %>%
  shapiro_test(tiempo)



#####modelo para supuestos de esfericidad


anova_sudoku<-anova_test(data=sudoku,dv=tiempo,wid=num,within= tipo)
anova_sudoku


anova_sudoku2=aov(tiempo~ tipo +Error(num))
summary(anova_sudoku2)


#####la columna ges (Generalize effect size es la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)



#####Comparación de tratamientos

pairwise.t.test(tiempo,tipo,paired=T,p.adjust.method = "bonferroni") ### para un solo factor









