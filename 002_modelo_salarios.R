url <- "https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/salario.csv"
# Cargar datos desde url de github
datos <- read.csv(url, header = T, sep=",", dec=".")

# Diagrama de dispersión para mirar si es de regresión lineal
# Si los datos no varian con el eje x no es un modelo sdecuado para regresion lineal
plot(datos$salario~datos$experiencia)



#Con esta funcion se ajusta/entrean un modelo de regresion lineal
mod1 <- lm(salario~experiencia, data = datos)
# lwd = para que la linea sea mas gruesa
abline(mod1, col="red", lwd = 3)
# Con esto miramos el beta0 y beta1 (los datos del ajuste los vemos con este comando)
summary(mod1)


### calcular manulmente la desviación del error

y_est=mod1$fitted.values ## son los valores de y predichos para los datos de entrenamiento

datos$y_est= y_est
datos$error=datos$salario-datos$y_est

mean(datos$error)
sd(datos$error)

mean(datos$salario)

#### intervalo de confianza es para coeficientes de regresion

confint(mod1, level=0.95)

### intervalo confianza valores ajustados

datos$va=predict(mod1, interval = "confidence")

### intervalo de predicciones o datos nuevso
xnuevo=datos$experiencia+1
xnuevo=data.frame(experiencia=xnuevo[1]) ## para convertir columna de x nuevos en data frame y garantizar que el nombre de columna es igual al nombre de columna con el que se entrenó


pred=predict(mod1, newdata=xnuevo, interval="prediction")
newdataframe=data.frame(xnuevo, pred)
