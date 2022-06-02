
###Carga de paquetes

library(nortest)
require(nortest)


##### crear base de datos ficticia

x=runif(n=500,min=1,max=20)
y= 2.48 + 1.01*x + rnorm(n=500)

#####Crear dataset con variables creadas
data=data.frame("salario"=y,"experiencia"=x)

###Entrenar modelo ####
modelo=lm(data=data,formula=salario~experiencia)


#####Ver resultados del modelo
summary(modelo)
