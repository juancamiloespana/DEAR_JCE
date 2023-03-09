
####cargar paquetes
install.packages('car')
library(car) ###qqPlot
library(tseries) ###jarquebera
library(goftest)##adtest, cvm.test
library(lmtest) ###bptest, bgtest, dwtest

##### cargar datos utilidad desde github

ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/utilidad.csv'
df=read.csv(ruta)
names(df)=c('y','x') ### para simplificar codigo

#####graficar y vs x ####
plot(df$y~df$x)


######### ajustar modelo y analizar resumen ###

mod=lm(y~x, data=df)
summary(mod)
##### guardar residuales ####
res=mod$residuals

####Validar supuestos:

#####1.1normalidad - grafico

par(mfrow=c(1,3)) ### dividir pantalla de graficos
hist(res)
boxplot(res)
qqPlot(res)


#####1.2 normalidad:pruebas estadisticas 
### pruebas shapiro, jarquebera, kstest, ad test, cvm test

shapiro.test(res)
jarque.bera.test(res)

ks.test(res,pnorm,0, sd(res))
goftest::ad.test(res,pnorm,0, sd(res))
goftest::cvm.test(res,pnorm,0, sd(res))
#####2.1 Varianza constante procedimiento grafico
par(mfrow=c(1,2))
plot(res~mod$fitted.values)
plot(res~df$x)


####prueba estadísticas VArianza constante: homocedasticidad

lmtest::bptest(mod)

###Independencia gráficos
par(mfrow=c(1,3))
plot(res)
acf(res)
pacf(res)


######Independencia pruebas estadísticas

lmtest::bgtest(mod)
lmtest::dwtest(mod)








