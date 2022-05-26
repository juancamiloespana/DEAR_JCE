#####CArgar paquetes #############3

library(regclass) ### Para calcular VIF
library(dplyr)  ### para case_when
library(plotly)  ###para gráficos Fancys



####### 
##################Crear una base de datos ficticia

edad<-seq(20,50)

antibiotico<-case_when(
  runif(31,0,1) >=0.5 ~1,
  TRUE ~0
)


y<-case_when(
  antibiotico=="SI"~ edad*0.7,
  TRUE ~ 50-edad*0.5
)


############################

boxplot(edad~antibiotico)

datos<-data.frame(y,edad,antibiotico)

plot_ly(data=datos,y=~y,x=~edad, color=~antibiotico, type="scatter")

inter=antibiotico*edad

reg<-lm(y~edad+antibiotico+inter)
reg<-lm(y~edad*antibiotico)
reg<-lm(y~.,data=datos)



summary(reg)
anova<-aov(reg)
summary(anova)




#####VIF  

VIF(reg)
vifs<-data.frame(VIF(reg))


######### Mirar efectos de VIF

#### Otro ejemplo

####Suponga y utilidad de empresa
#### suponga x gasto en publicidad
#### Suponga x2 gasto en calidad


#### Crear variables ficticias

set.seed(2)
x=runif(1000,15,70)
y=3*x +rnorm(1000,0,1)
x2=x+rnorm(5,0,10)

###### Ajuste un modelo con sólo el gasto en calidad
###### Cuánto crece la utilidad si se aumento una unidad la inversion en calidad
modelo1=lm(y~x2)
summary(modelo1)

### Agregue a ese modelo la variable publicidad
### ¿Qué pasa con la relación entre utilidad y calidad?

modelo2=lm(y~x2+x)
summary(modelo2)
### Verifique la multicolinealidad.

VIF(modelo2)





