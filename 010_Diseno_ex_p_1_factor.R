library(gplots) ### grafica de medias


url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/6bbd6e7beb1d6bafda3f129d50a87e0ee4c0cd21/data/oxigeno.csv"

datos=read.csv(url, sep=";", dec=",")



#### análisis exploratorio ###

plotmeans(datos$oxigeno_disuelto~datos$Punto)
boxplot(datos$oxigeno_disuelto~datos$Punto)

### convertir a factor
