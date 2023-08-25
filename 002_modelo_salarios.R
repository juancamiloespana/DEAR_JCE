url <- "https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/salario.csv"
# Cargar datos desde url de github
datos <- read.csv(url, header = T, sep=",", dec=".")

# Diagrama de dispersión para mirar si es de regresión lineal
# Si los datos no varian con el eje x no es un modelo sdecuado para regresion lineal
plot(datos$salario~datos$experiencia)




### pregunt<as###

#Ajuste un modelo de regresión lineal y genere el resumen para el modelo de salarios vs años de experiencia
#Analice e interprete los coeficientes estimados y su desviación
#Analice la desviación estándar del error
#Establezca la ecuación del modelo y la ecuación del valor predicho
#Plantee las pruebas de hipótesis para verificar la significancia del modelo y concluya sobre la significancia de cada coeficiente y del modelo en general indique cuáles son los estadísticos de prueba utilizados y los valores p
#Indique los intervalos de confianza para la estimación de los coeficientes
#Indique los intervalos de confianza para los valores ajustados de la base histórica (predicción de años de experiencia de la base de entrenamiento del modelo)
#Indique un intervalo de predicción para un conjunto de datos nuevos con los que no se entrenó el modelo (dados en R: cambio experiencia)
#Diga cuál es el coeficiente de determinación y analícelo.

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