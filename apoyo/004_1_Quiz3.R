########## Puede copiar y pegar a partir de aquí##################

library(datasets)
library(car) ###qqPlot
library(lmtest)
library(tseries)
library(nortest) 
library(goftest)### anderson darling
library(lmtest)


library(datasets)


set.seed(1)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)


data(iris)


a<-iris$Sepal.Length*r1
b<-iris$Petal.Length*r2


datos1=data.frame("n_defectos_promedio"=b, "n_productos_devueltos"=a)





mod=lm(n_productos_devueltos ~n_defectos_promedio,data=datos1)

summary(mod)
confint(mod,level=0.99)

predict(mod,interval="confidence")

res=mod$residuals

goftest::ad.test(res, pnorm, 0, sd(res))

jarque.bera.test(res)
shapiro.test(res)

bptest(mod)

qqPlot(res)

dwtest(mod)
bgtest(mod)

acf(res)


########ventas totales#####

Para obtener la base de datos con la información que utilizará para los siguientes puntos ejecute los siguientes comandos en R.

########## Puede copiar y pegar a partir de aquí##################

library(datasets)


set.seed(1)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)


data(iris)


a<-iris$Sepal.Length*r1
b<-iris$Petal.Length*r2


datos1=data.frame("presupuesto_mercadeo"=b, "ventas_totales"=a)

plot(datos1$ventas_totales~datos1$presupuesto_mercadeo)

write.csv(datos1,'data\\ventas_presupuesto.csv')

#### acá finaliza el código de r

mod=lm(ventas_totales~presupuesto_mercadeo, datos1)
summary(mod)

123+(0.81726*2)
123 +(0.82*2)

confint(mod, level=0.5)


predict(mod, interval='confidence')
residuales=mod$residuals
goftest::ad.test(residuales,pnorm,0,sd(residuales))

qqPlot(residuales)

bptest(mod)
bgtest(mod)
dwtest(mod)
