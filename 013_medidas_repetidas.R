### instalar paquetes

install.packages('datarium') ### para conjuntos de datos
install.packages("tidyverse") ### para operaciones con datos
install.packages("ggpubr") ### graficas de normalidad agrupadas
install.packages("rstatix") ### shapiro test agrupada


#### cargarlos
library(datarium)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gplots)


data('selfesteem')

auto_df=selfesteem%>%gather(key= "tiempo", value= "autoestima", t1, t2, t3) ### tranpuesto parcial

auto_df$id=as.factor(auto_df$id)
auto_df$tiempo=as.factor(auto_df$tiempo)



####### exploratoria analizar efecto factor (tiempo) sobre respuesta (autoestima)

boxplot(autoestima~tiempo, data=auto_df)
plotmeans(autoestima~tiempo, data=auto_df)


###### validar supuestos

##### 1 normalidad por tratimiento
#### 2 esfercidad  -> anova

### normalidad
attach(auto_df)

### grafico de normalidad por tratamiento
ggqqplot(data=auto_df, 'autoestima', facet.by ='tiempo' )

## prueba e hipotesis de normalidad
auto_df%>%group_by(tiempo)%>%shapiro_test(autoestima)

##supuesto de normalidad se cumple porque valor p > 0.05, se acepta la hipotesis nula

f=autoestima~tiempo + Error(id/tiempo)  ### id es nombre de variable de sujetos y en el denominador factores intrasujetos

anova1=anova_test(data=auto_df, formula = f)
anova1

### se cumple supuesto de esfericidad porque valor P de prueba de mauchly es mayor a 0.05


anova2= aov(data=auto_df, formula = f)
summary(anova2)


### comprobar entre qué pares de tratamientos hay diferencias

pairwise.t.test(autoestima, tiempo, paired = T, p.adjust.method = 'bonferroni')


##### ejercicio 2

data("selfesteem2")

auto_df=selfesteem2%>%gather(key= "tiempo", value= "autoestima", t1, t2, t3) ### tranpuesto parcial

auto_df$id=as.factor(auto_df$id)
auto_df$tiempo=as.factor(auto_df$tiempo)
attach(auto_df)
#### efecto de cada factor

boxplot(autoestima~tiempo)
boxplot(autoestima~treatment)

#####interaccion

interaction.plot(x.factor=tiempo, trace.factor=treatment, autoestima)

### supuestos 

ggqqplot(data= auto_df, 'autoestima', facet.by = c("tiempo", 'treatment'))

auto_df%>%group_by(treatment, tiempo)%>%shapiro_test(autoestima)

#### ajustar modelo para mauchly

f=autoestima~treatment*tiempo +Error(id/(treatment*tiempo))

anova1=anova_test(data=auto_df,f)
anova1

anova2= aov(data=auto_df, f)
summary(anova2)

pairwise.t.test(autoestima, interaction(treatment, tiempo), paired = T, p.adjust.method = "bonferroni")


########

data('anxiety')

auto_df=anxiety%>%gather(key= "tiempo", value= "ansiedad", t1, t2, t3) ### tranpuesto parcial

auto_df$tiempo=as.factor(auto_df$tiempo)

attach(auto_df)
######

boxplot(ansiedad~group)
boxplot(ansiedad~tiempo)

interaction.plot(x.factor=tiempo, trace.factor = group, ansiedad)


### el de normalidad igual al anterior ##

f=ansiedad~group*tiempo +Error(id/tiempo)

anova1=anova_test(data=auto_df, f)
anova1

anova2=aov(data=auto_df, f)
summary(anova2)


#####

pairwise.t.test(ansiedad, interaction(group, tiempo), paired = T, p.adjust.method = "bonferroni")
