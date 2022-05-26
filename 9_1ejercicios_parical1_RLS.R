########## Puede copiar y pegar a partir de aquí##################

library(datasets)


set.seed(222)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)

plot(r1)
graphics.off()

data(iris)


a<-iris$Sepal.Length*r1
b<-iris$Petal.Length*r2

plot(b,a)
datos1=data.frame("presupuesto_mercadeo"=b, "ventas_totales"=a)
#### acá finaliza el código de r


modelo=lm(a~b)
summary(modelo)

confint(modelo,level=0.50)


predict(modelo, interval="confidence")


residuales=modelo$residuals

mod=modelo

###############Cargar paquetes


library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)

#1. prueba normalidad ####

##1.1 Normalidad - Pruebas gráficas #####

#Histograma

hist(residuales, main="Histograma residuales", breaks=20)

#qqplot sencillo

qqnorm(residuales)
qqline(residuales)

#qqplot fancy
qqPlot(residuales)


boxplot(residuales, horizontal = T)

##1.2 Normalidad -pruebas estadísticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

stats::ks.test(residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling
help(ad.test)
cvm.test(residuales,pnorm, 0, sd(residuales)) ###Cramer von mises

#2. prueba varianza constante - Homocedasticidad ####

##2.1 Homocedasticidad -Pruebas gráficas #####


plot(mod$fitted.values,residuales)
abline(h=0, col="red")

plot(a,residuales)
abline(h=0)

##2.2 Homocedasticidad -Pruebas estadísticas #####

bptest(mod)



#3. prueba independencia  #####


##3.1 Independencia-Pruebas GRáficas #####

plot(residuales)
abline(h=0)
par(mfrow=c(1,2)) ###Separar la ventana de los gráficos
acf(residuales)
pacf(residuales)

##3.2 Independencia-Pruebas estadísticas #####

dwtest(mod) 

bgtest(mod)





#####Ejercicio 2 

########## Puede copiar y pegar a partir de aquí##################

library(datasets)


set.seed(222)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)


data(iris)


a<-iris$Sepal.Width*r1
b<-iris$Petal.Width*r2

plot(b,a)
datos1=data.frame("presupuesto_mercadeo"=b, "ventas_totales"=a)
  #### acá finaliza el código de r


modelo=lm(a~b)
summary(modelo)

confint(modelo,level=0.50)


predict(modelo, interval="confidence")


residuales=modelo$residuals

mod=modelo

###############Cargar paquetes


library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)

#1. prueba normalidad ####

##1.1 Normalidad - Pruebas gráficas #####

#Histograma

hist(residuales, main="Histograma residuales", breaks=20)

#qqplot sencillo

qqnorm(residuales)
qqline(residuales)

#qqplot fancy
qqPlot(residuales)


boxplot(residuales, horizontal = T)

##1.2 Normalidad -pruebas estadísticas #########


shapiro.test(residuales) ##shapiro wilk

jarque.bera.test(residuales) ###jarque bera test

stats::ks.test(residuales,pnorm,0,sd(residuales)) ####kolmogorov smirnov

ad.test(residuales, pnorm, 0, sd(residuales))  ###Anderson darling
help(ad.test)
cvm.test(residuales,pnorm, 0, sd(residuales)) ###Cramer von mises

#2. prueba varianza constante - Homocedasticidad ####

##2.1 Homocedasticidad -Pruebas gráficas #####


plot(mod$fitted.values,residuales)
abline(h=0, col="red")

plot(a,residuales)
abline(h=0)

##2.2 Homocedasticidad -Pruebas estadísticas #####

bptest(mod)



#3. prueba independencia  #####


##3.1 Independencia-Pruebas GRáficas #####

plot(residuales)
abline(h=0)
par(mfrow=c(1,2)) ###Separar la ventana de los gráficos
acf(residuales)
pacf(residuales)

##3.2 Independencia-Pruebas estadísticas #####

dwtest(mod) 

bgtest(mod)


