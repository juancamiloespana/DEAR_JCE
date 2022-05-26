##################Cargar paquetes


library(car)
library(tseries)
library(nortest)
library(goftest)
library(lmtest)


############ Crear base de datos ficticia

set.seed(50)
x=runif(1000,-80,80)
e=rnorm(1000,0,500)
y= -30 + 20*x + e



plot(x,y)
mod<-lm(y~x)
abline(mod, col="red")


summary(mod)


residuales=mod$residuals


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

plot(y,residuales)
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




