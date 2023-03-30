######n_ing_empleados

library(datasets)

set.seed(222)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)



data(iris)


a<-iris$Sepal.Width*r1
b<-iris$Petal.Width*r2
datos1=data.frame("n_empleados"=b, "ingresos"=a)

mod=lm(ingresos~n_empleados, data=datos1)
summary(mod)

write.csv(datos1,'data\\ing_empleados.csv')

confint(mod,level=0.99)

###al reves
mod=lm(n_empleados~ingresos, data=datos1)
summary(mod)
confint(mod,level=0.99)
predict(mod, interval='confidence')
shapiro.test(mod$residuals)
acf(mod$residuals)

bptest(mod)
bgtest(mod)
dwtest(mod)
##############

x= rnorm(30, mean=100, sd=15)
y=-0.05*x+ rnorm(30)

turistas_robos=data.frame('n_robos'=x,'flujo_turistas'=y)
#write.csv(turistas_robos, 'data\\turistas_robos.csv')

d=read.csv('data\\turistas_robos.csv')
plot(y~x)
mod=lm(flujo_turistas~n_robos, data=d)
summary(mod)
abline(mod)

shapiro.test(mod$residuals)
bptest(mod)
library(lmtest)


confint(mod, level=0.89)

predict(mod, interval='confidence')

shapiro.test(mod$residuals)

acf(mod$residuals)

bptest(mod)
bgtest(mod)
dwtest(mod)


######

mod=lm(n_robos~flujo_turistas, data=d)
summary(mod)
abline(mod)

shapiro.test(mod$residuals)
bptest(mod)
library(lmtest)


confint(mod, level=0.89)

predict(mod, interval='confidence')

shapiro.test(mod$residuals)

acf(mod$residuals)

bptest(mod)
bgtest(mod)
dwtest(mod)
