




cat_feat=function(cat,n,name){
x_cat=sample(cat,n,replace=T)
order=unique(x_cat)
x_num <- as.numeric(factor(x_cat, levels = order, exclude = NULL))
x_df=data.frame(x_cat,x_num)
names(x_cat)=c(name,'x_num')

return(x_df)
}


n=2000
name='ingles'
cat=c("A1",'A2','B1','B2','C1','C2')
x1=cat_feat(cat,n,name)

x1$x_num=case_when(
  x1$x_cat %in%  c("A1",'A2') ~ -1,
  x1$x_cat %in%  c("C1",'C2') ~ 2,
  TRUE ~0
  
  )

n=2000
name='genero'
cat=c("F",'M')
x2=cat_feat(cat,n,name)

n=2000
name='ciudad'
cities <- c("Bogota", "Lima", "Mexico City", "Santiago", "Buenos Aires", 
            "Caracas", "Montevideo", "Quito", "Guatemala City", "San Jose")


library(dplyr)
# remove accents from city names
cat <- iconv(cities, to="ASCII//TRANSLIT")
x3= cat_feat(cat,n,name)
x3$x_num<-case_when(
  x3$x_num<=3 ~ 1,
  TRUE~0
  
)

x4=data.frame('edad'=runif(2000,24,59))

x0=data.frame('ID'=seq(1:2000))

x5=data.frame(x4*(0.2)+rnorm(2000))
names(x5)='experiencia'

n=2000
name='genero'
cat=c('PhD','Master','Especialización', 'Pregrado', 'Bachillerato')
x6=cat_feat(cat,n,name)

x7= data.frame('prom_anos_por_emp'=rnorm(2000, 10, 2))

x8= data.frame('score_inteligencia_emoc'=rnorm(2000, 50, 8))

x9= data.frame('score_program'=runif(2000, 10,100))

x10 = data.frame('num_conex'=runif(2000, 5, 1000))

x11 = data.frame('num_publ'=runif(2000, 5, 2000))

x12=data.frame('num_reacc'=runif(2000, 10, 3000))

n=2000
name='tipo_cargo'
cat=c('Junior','Senior')
x13=cat_feat(cat,n,name)

b1=5
b2=1
b3=6
b4=4
b5=2

salario= x13$x_num*x5$experiencia*b1  +x8^2*b2 + x9*b3 +x1$x_num + x3$x_num*b5

salario=salario + rnorm(2000,salario*0.05, salario*0.01)

hist(salario$score_inteligencia_emoc)

