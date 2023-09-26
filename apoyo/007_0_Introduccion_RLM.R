#####CArgar paquetes #############3

library(regclass) ### Para calcular VIF
library(dplyr)  ### para case_when
library(plotly)  ###para gráficos Fancys

###############


##############################
### Ajustar modelo RLM###
###################################


### Cargar datos microsoft
url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/los_microsoft.csv'
datos_losm=read.csv(url)

### ajustar modelo con pulse y glucose

mod=lm(data=datos_losm,lengthofstay~pulse+glucose)

sum_rl=summary(mod)

######Ajsutar anova ####

an=aov(mod)
summary(an)

#####extraer R2 y R2 ajustado

sum_rl$adj.r.squared

################################################
### Variable cualitativa crear base ficticia  ####
################################################

### crear edad como seq de 20 a 50
### crear variable antibiotico el 50% es no, el 20% es Bajo y el 30% es ALTO
#### variable respuesta si no toma antibiotico b0= 0, b1 es 0,7 si sí es -0,5 y bo es 50
### error estándar para y
### crear data frame

set.seed(123)
edad=seq(20,50)
antibiotico<-case_when(
  runif(31,0,1)<=0.5 ~ "NO",
  runif(31,0,1)<=0.7~"BAJO",
  TRUE~"ALTO"
  
)
table(antibiotico)

y=case_when(antibiotico=="NO"~ 0.7*edad,
            TRUE~ 50 -0.5*edad
              )
y=y+rnorm(31)

datos=data.frame(y,edad,antibiotico)



####ajustar modelo y analizar resumen de cualitativa
mod2=lm(data=datos,y~.)
summary(mod2)


############################################
################Interacción ##############
##########################################

##################Crear una base de datos ficticia

### crear edad como seq de 20 a 50
###crear variable antibiotico el 50% es 1 (si), el 50% es 0 (No)
#### variable respuesta si toma antibiotico b0= 0b1 es 0,7 si no es -0,5 y bo es 50
### error estándar para y
### crear data frame


set.seed(123)

edad=seq(20,50)

antibiotico<-case_when(
  runif(31,0,1)<=0.5 ~ 0,
  TRUE~1
)

table(antibiotico)

y=case_when(antibiotico==0~ 0.7*edad,
            TRUE~ 50 -0.5*edad
)
y=y+rnorm(31)

datos=data.frame(y,edad,antibiotico)


################ Analizar gráficamente ############

###ver correlación entre variables explicativa

plot_ly(data=datos, y=~y, x=~edad, color=~antibiotico)

boxplot(edad~antibiotico)

#####ver gráfico de interacción en plotly



####ajustar modelo rl con interacción

mod3=lm(data=datos, y~edad*antibiotico)
summary(mod3)

##### analizar resume





###############################
######### Analizar de VIF #####
###############################


#### Problema

####Suponga y utilidad de empresa
#### suponga x gasto en publicidad
#### Suponga x2 gasto en calidad

###### se quiere comprobar si aumentar gasto en publicidad mejora utilidades

#### Crear variables ficticias 
#### establecer semilla
### x distribución uniforme entre 15 70
### x2 x + componente aleatoria media 0 y varianza 10
### y con b1 =3 y error normal estánar 


x=runif(50,15,70)
x2=x+rnorm(50,0,10)

y=3*x +rnorm(50)

datos=data.frame(y,x,x2)
write.csv(datos, "data\\multicolinealidad.csv", row.names=F )


###### Ajuste un modelo con sólo el gasto en calidad
###### Cuánto crece la utilidad si se aumento una unidad la inversion en calidad

mod1=lm(data=datos,y~x2)
summary(mod1)


### Agregue a ese modelo la variable publicidad
### ¿Qué pasa con la relación entre utilidad y calidad?

mod2=lm(data=datos,y~x+x2)
summary(mod2)
### Verifique la multicolinealidad.


VIF(mod2)







