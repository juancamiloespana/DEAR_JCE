library(Mcomp)

plot(M1$YAF2)

plot(M1$MND1$x)
plot(M1$MND4$x)
plot(M1$MND6$x)
library(tseries)
data(bev)
plot(lynx)
plot(bev)

plot(M1$MND4$x)


acf(M1$MND1$x,plot=F)
acf
print(M1$MND2$x)

m<-window(M1$MND7$x, start=c(1977,12), end=c(1981,2))
m

###### ejercicios en ajuste modelos de regresión en R
install.packages("datasets")
library(datasets)
data(iris)
head(iris)

library(help="datasets")
data(mpg)

plot(iris$Sepal.Length,iris$sepal.Length)


########## Puede copiar y pegar a partir de aquí##################



########## Puede copiar y pegar a partir de aquí##################

set.seed(1)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)
library(datasets)
data(iris)
a<-iris$Sepal.Length*r1
b<-iris$Petal.Length*r2
plot(b,a)
datos1=data.frame("presupuesto_mercadeo"=b, "ventas_totales"=a)

#### acá finaliza el código de r