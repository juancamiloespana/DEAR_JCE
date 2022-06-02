

####leer datos de git hub

url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/resistencia.csv"
datos=read.csv(url,sep=";")

####graficar variables

plot(datos$porcentaje,datos$resistencia)


#### Ajustar modelo

modelo=lm(formula=resistencia~porcentaje,data=datos)

#### Ver resumen

summary(modelo)

mean(datos$resistencia)

