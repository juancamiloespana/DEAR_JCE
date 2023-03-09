
url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/salario.csv'
datos=read.csv(url, sep=',', dec='.') ## se configura los argumentos de acuerdo al archivo que se va a vcargar

plot(datos$salario~datos$experiencia)

modelo1=lm(data=datos,formula = salario~experiencia ) ## si se usa nombre de los argumentos, no importa el orden en que se escriban
modelo2=lm( salario~experiencia, datos)  ###se puede omitir el nombre del argumento pero se tiene que garantizar orden de los argumentos
####solo se usa uno de los dos codigos anteriores par ajustar el modelo, se ponen dos como ejemplo de formas diferentes de usar la funcion


plot(datos$salario~datos$experiencia)
abline(modelo1, col='red' )

summary(modelo1)
###esta funcion permite ver la informacion del modelo ajustado


y_pred=predict(modelo1) 
## esta funcion permite predecir para todas las x los valores de y


datos_full=data.frame(datos,y_pred)

##formar una tabla con datos originales y predicciones

2.273 + 1.025*1.92 ### y_pred cuando x vale 1.92 verificar que da igual que funcion predict

datos_full$error= datos_full$salario-datos_full$y_pred  ### error de prediccion, prediccion menos variable respuesta

val_abs_erro= abs(datos_full$error) ### sacar el valor absoluto de errores para que al promediarlos no se anulen
mean(val_abs_erro) ### mae: mean absolute error, medida de en que valor se desvia la prediccion de los valores reales
mean(datos_full$salario)### para identificar que tan alto es el mae, se compara con este valor


cuadrado_error=datos_full$error**2 ## en lugar de valor absoluto se puede usar el error al cuadrado
cuadrado_medio_error= mean(cuadrado_error) ##mse: mean square error, sirve de medida de desempeño del modelo
rmse= sqrt(cuadrado_medio_error) ### rmse: root mean square error, para quitar el efecto del cuadrado se saca raiz, asi las unidades de medida vuelven a ser las originales (unidades de variable respuesta)

######### resolver ejercicio 2 completo 

### punto 1


url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/salario.csv'
datos=read.csv(url, sep=',', dec='.') ## se configura los argumentos de acuerdo al archivo que se va a vcargar


modelo=lm(salario~experiencia, data=datos)
summary(modelo)
mean(datos$salario) ## para comparar contra la desviacion de errores
### con base en el resumen se hacen los puntos: 2, 3, 4, 5


confint(modelo, level=0.95) ## punto 6

int_conf_va=predict(modelo,interval='confidence') ###punto 7

df_pred=data.frame(datos,int_conf_va) ### para analizar intervalos de valores ajustados


###punto 8: cargar datos nuevos y hacer predicciones
ruta='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/cambio_experiencia.csv'
datos_nuevos= read.csv(ruta)
colnames(datos_nuevos)=c('x','experiencia') ## cambiar nombre a x, debe ser igual a como se llama en la funcion lm

int_pred=predict(modelo, newdata=datos_nuevos, interval='prediction') ###punto 8


### punto 9 ####

summary(modelo1)

