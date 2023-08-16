

## se guarda la url entre comillas para cargarla desde internet(github)
url="https://raw.githubusercontent.com/juancamiloespana/DEAR_JCE/master/data/muestras_inferencias.csv"

## verificar formato del archivo que se carga
datos=read.csv(url,dec=".", sep=",", header=T)

### se calcula el estadistico de prueba que es la media muestral
###la columna muestra 1 tiene dias de duracion de una muestra de baterias
x_barra=mean(datos$muestra1)

#### la media poblacional a probar, hipótesis nula

mu=30
### la desviacion del estadístico de prueba basado en teorema central del limite
sd_m=0.3

### funcion para calcular la probabilidad del estadístico de prueba (cola izquiera)
valor_p=pnorm(x_barra, mean=mu, sd=sd_m, lower.tail = T)

### valor del error inferior y superior del intervalo

sign=0.05


int_lower=qnorm(sign, mean=x_barra, sd=sd_m, lower.tail =T )
int_upper=qnorm(sign, mean=x_barra, sd=sd_m, lower.tail =F )


