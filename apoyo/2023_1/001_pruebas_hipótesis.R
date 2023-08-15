
mu=30 ### media poblacional y media de las medias muestrales por teorema central del limite
sdm=3/sqrt(100) ### estimar la desviacion de xbarra (no es la misma desviacion que la variable original), 
alpha=0.05 ### error tipo 1 limite para comparar valor P

### Error tipo 1 (ortodoxo): Probabilidad de equivocarse si se rechaza Ho
### Error tipo 1 (Nemotecnia): Probabilidad de que Ho sea verdadera

### Valor p y alpha son el mismo tipo de error, alpha es el valor mas bajo que necesito del valor p para aprobar Ho


val_crit=qnorm(p=0., mean= mu, sd=sdm, lower.tail = T) ## valor critico para alpha 0.05

url='https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/muestras_inferencias.csv'

muestra1= read.csv(url)  ## tabla con muestras

xbarra=mean(muestra1$muestra1) ## calculo del estadistico de prueba

val_p=pnorm( 29.54422, mean=mu, sd=sdm, lower.tail = T) ### probabilidad de que con ese estadístico Ho se cumpla (probabilidad de equivocarse si rechaza Ho dado ese esttadistico) 


mean(muestra1$muestra1)



qnorm(0.05, 30, sdm, lower.tail = T)
qnorm(0.95, 29.5442, sdm, lower.tail = T)

