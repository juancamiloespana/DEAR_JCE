




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
x8$score_inteligencia_emoc[sample(c(1:200),50)]=-99
hist(x8$score_inteligencia_emoc)

x9= data.frame('score_program'=runif(2000, 10,100))

x10 = data.frame('num_conex'=runif(2000, 5, 1000))
x10[1]=round(x10[1],0)

x11 = data.frame('num_publ'=runif(2000, 5, 2000))
x11[1]=round(x11[1],0)

x12=data.frame('num_reacc'=runif(2000, 10, 3000))
x12[1]=round(x12[1],0)
n=2000
name='tipo_cargo'
cat=c('Junior','Senior')
x13=cat_feat(cat,n,name)
x13$x_num=case_when(
  x13$x_cat=='Senior'~1,
  TRUE~-1
)

b1=15
b2=0.0001
b3=6
b4=14
b5=7


set.seed(123)
salario= x13$x_num*x5$experiencia*b1  +x8^4*b2 + x9*b3 +x1$x_num*b4 + x3$x_num*b5


names(salario)='salario'

min(salario)
plot(salario$salario~x8$score_inteligencia_emoc)


m=0
sd=1500

salario=6000 +salario + rnorm(2000, 0, sd)
min(salario)
max(salario)                        
                        
hist(salario$salario, breaks=30)
shapiro.test(salario$salario)

datos_salario=data.frame(salario,x1$x_cat,x2$x_cat,x3$x_cat, x4$edad, x5$experiencia, x6$x_cat,x7$prom_anos_por_emp,x8$score_inteligencia_emoc,
                         x9$score_program,x10$num_conex, x11$num_publ, x12$num_reacc, x13$x_cat)
names(datos_salario)=c('salario', 'ingles','genero', 'ciudad', 'edad','experiencia','estudio','anos_prom_emps','test_intel_emocional','test_program','amigos_en_plat','num_publ_plat','num_reacc_plat','tipo_de_cargo')

datos_salario$salario[sample(c(1:2000),50)]=50000

modelo=lm(salario~., data=datos_salario)

summary(modelo)

library(Metrics)
mape(salario$salario,modelo$fitted.values)
mae(salario$salario,modelo$fitted.values)
mean(df_error$salario.1)
library(lmtest)

hist(modelo$residuals)
shapiro.test(modelo$residuals)
bptest(modelo)
plot(modelo$residuals)
bgtest(mod)

hist(datos_salario$salario)
hist(datos_salario$x8.score_inteligencia_emoc)
datos_salario$x8


write.csv(datos_salario,"trabajo1\\datos_salario.csv", row.names = F)
