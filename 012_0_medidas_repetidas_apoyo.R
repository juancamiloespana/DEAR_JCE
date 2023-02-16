



sudoku$cod=as.factor(sudoku$cod)
attach(sudoku)


######## Análisis exploratorio


boxplot(tiempo~tipo)
plotmeans(tiempo~tipo)


########### Interacciones ########

####entre factores ####

interaction.plot(cod,tipo,tiempo)

interaction.plot(tipo,cod,tiempo)

### no es necesario analizar la interacción entre sujetos y tratamiento

############## Validación de supuestos medidas repetidas ########

#####Normalidad por tratamiento, gráfico

ggqqplot(sudoku, "tiempo", facet.by = "tipo")


#### Normalidad por tratamiento pruebas

sudoku %>%
  group_by(tipo) %>%
  shapiro_test(tiempo)



##### modelo para supuestos de esfericidad


anova_sudoku<-anova_test(data=sudoku,dv=tiempo,wid=cod,within= tipo)
anova_sudoku


anova_sudoku2<-anova_test(data=sudoku,formula=tiempo~tipo +Error(cod/tipo))
anova_sudoku2


anova_sudoku2=aov(tiempo~ tipo +Error(cod/tipo))
summary(anova_sudoku2)



#####la columna ges (Generalize effect size es la cantidad de variabilidad explicada Intra sujetos
###EL GES se calcula con SSTR/(SST)



#####Comparación de tratamientos

pairwise.t.test(tiempo,tipo,paired=T,p.adjust.method = "bonferroni") ### para un solo factor




#####################E5 hagover

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

f=symptoms~ time*group +Error(id/time)
anova_hang<-anova_test(data=hangover,f)
anova_hang


anova_hang2=aov(f, data=hangover)
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



