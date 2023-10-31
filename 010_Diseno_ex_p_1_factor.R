library(gplots) ### grafica de medias


url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/6bbd6e7beb1d6bafda3f129d50a87e0ee4c0cd21/data/oxigeno.csv"

datos=read.csv(url, sep=";", dec=",")


####convertir factores estudiados a tipo factor

datos$Punto=as.factor(datos$Punto)

#### análisis exploratorio ###

plotmeans(datos$oxigeno_disuelto~datos$Punto)
boxplot(datos$oxigeno_disuelto~datos$Punto)

###calculo manual de la media tratamiento 1, para construir modelo medias

u1j= (6.1+5.9+6.3+6.1+6.0)/5


### ajustar modelo en R

an1=aov(oxigeno_disuelto~Punto, data=datos) ### ajusta modelo anova

summary(an1)


### para calcular modelos en R

model.tables(an1, type="mean") ## calcula los mu
model.tables(an1, type="effect") ### calcula los tao



### validar supuestos ###


res=an1$residuals

 ### 1 normalidad y media cero

hist(res)
shapiro.test(res)

### 2 varianza constante

library(lmtest)
bptest(an1)


### 3 independencia

bgtest(an1)


### comparación de medias de los tratamientos

library(agricolae)

lsd=LSD.test(y=an1, trt= "Punto", group=F) ## para comprobar entre qué tratamientos están las diferencias

lsd$comparison


hsd=HSD.test(y=an1, trt= "Punto", group=F)
hsd$comparison

#### tamaño muestral 


sig= 0.05
beta=0.2
pote=1-beta
n= 5
desv= sd(datos$oxigeno_disuelto)

mean(datos$oxigeno_disuelto)

power.t.test(n=n, sig.level = sig, power=pote, sd=desv)


###punto 2


sig= 0.1

n= 5
desv= sd(datos$oxigeno_disuelto)
delta=2
mean(datos$oxigeno_disuelto)

power.t.test(n=n, sig.level = sig,delta=2, sd=desv)


### escenario 3 tamaño de muestra


sig= 0.1 ## porque es menos caro este error
beta=0.05 ## este error implica la multa 
pote=1-beta
desv= sd(datos$oxigeno_disuelto)
delta=2

power.t.test(power=pote, sig.level = sig,delta=2, sd=desv)

