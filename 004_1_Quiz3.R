########## Puede copiar y pegar a partir de aquí##################

library(datasets)
library(car)
library(lmtest)
library(tseries)
library(nortest)
library(goftest)
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





