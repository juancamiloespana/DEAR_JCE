

######## 1. Leer datos ######
###Cargar los datos desde un archivo .csv

###Si los datos no estÃ¡n dentro del proyecto, se debe poner la ruta completa donde estÃ¡:
### "D:/catedra/Salary_Data.csv"



datos_resistencia<-read.csv("resistencia.csv",dec=".",sep=";", header= T, fileEncoding = "UTF-8")


datos_resistencia2<-data.frame("porcentaje"=runif(5,max=35,min=24))


##El separador de columnas 'sep' y separador decimal 'dec' pueden variar con la configuracion del pc
###Si se pone mal el separador decimal, puede leer un valor nÃºmerico como texto
### Si se pone mal el separador de columnas, reconoce todo como si fuera una columna

##### 1. Diagrama de dispersión

plot(datos_resistencia$porcentaje,datos_resistencia$resistencia)


#### 2. Ajustar modelo de regresión lineal
modelo<-lm(formula=resistencia~porcentaje, data=datos_resistencia)


###2,3, 4, 5 REsumen del modelo para establecer la ecuación

summary(modelo)




####GRafico de dispersión con modelo ajustado
plot(datos_resistencia$porcentaje,datos_resistencia$resistencia)
abline(modelo, col="red")



####6. Intervalos de confinza para B1 y B0

confint(modelo,level=0.95)


###### 7 y 8 intervalos de confianza y Predicciones

predict(modelo)###Valores predichos de la base utilizada, se puede usar para predecir conjunto de datos nuevo(lo veremos en regresión lineal múltiple)
data.frame(datos_resistencia$porcentaje,predict(modelo))
modelo$fitted.values ## valores ajustados iguales a los de función predict (no aplica para datos nuevos)

int_confidencia<-predict(modelo, interval="confidence")### 7.Intervalo de confianza para valores predichos de datos con los que se ajustó el modelo


int_prediccion<-predict(modelo, newdata=datos_resistencia2, interval="prediction")  ### 8.ntervalo de predicción para valores predichos de datos nuevos a los que se utilizaron para ajustar el modelo

int_confidencia 

int_prediccion

data.frame(datos_resistencia2,int_prediccion)



