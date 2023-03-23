




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

x5=data.framex4*(0.2)+(rnorm(2000)))
names(x5)='experiencia'




