url <- "https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/salario.csv"
# Cargar datos desde url de github
datos <- read.csv(url, header = T, sep=",", dec=".")

plot(datos$salario~datos$experiencia)


modelo=lm(salario~experiencia, data=datos)

lines(datos$experiencia,modelo$fitted.values, col="red")

### como se calculan residuales manualmente
residuales= datos$salario -modelo$fitted.values

summary(modelo)

###como sacarlos del modelo
res=modelo$residuals


###1 normalidad

hist(res)
library(car) ### para qqPlot cargar librerias siempre al principio
qqPlot(res)


##pruebas de hipotesis normalidad

shapiro.test(res)
library(tseries) ## para jarquebera
jarque.bera.test(res)

##### 2. homocedasticidad

plot(modelo$fitted.values, res)
plot(datos$experiencia, res)

library(lmtest)
bptest(modelo)
gqtest(modelo)


### 3. independencia
par(mfrow=c(1,3)) ### para dividir pantalla de salida de gráficos
plot(res)
acf(res)
pacf(res)



###hipotesis

bgtest(modelo)
dwtest(modelo)
