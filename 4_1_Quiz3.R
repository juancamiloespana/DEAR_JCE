########## Puede copiar y pegar a partir de aquí##################

library(datasets)


set.seed(1)
r1=runif(1,min=10, max=80)
r2=runif(1,min=5, max=30)


data(iris)


a<-iris$Sepal.Length*r1
b<-iris$Petal.Length*r2


datos1=data.frame("presupuesto_mercadeo"=b, "ventas_totales"=a)


mod=lm(presupuesto_mercadeo~ventas_totales,data=datos1)

summary(mod)
confint(mod,level=0.5)

predict(mod,interval="confidence")

res=mod$residuals

goftest::ad.test(res, pnorm, 0, sd(res))
bptest(mod)

dwtest(mod)
bgtest(mod)
