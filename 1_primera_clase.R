
###Carga de paquetes

library(nortest)
require(nortest)


#####
x=runif(n=500,min=1,max=20)
y= 2.48 + 1.01*x + rnorm(n=500)


data=data.frame("salario"=y,"experiencia"=x)

modelo=lm(data=data,formula=salario~experiencia)

summary(modelo)
