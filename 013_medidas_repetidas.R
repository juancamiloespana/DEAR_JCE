### instalar paquetes

install.packages('datarium') ### para conjuntos de datos
install.packages("tidyverse") ### para operaciones con datos
install.packages("ggpubr") ### graficas de normalidad agrupadas
install.packages("rstatix") ### shapiro test agrupada


#### cargarlos
library(datarium)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(gplots)


data('selfesteem')

auto_df=selfesteem%>%gather(key= "tiempo", value= "autoestima", t1, t2, t3) ### tranpuesto parcial

auto_df$id=as.factor(auto_df$id)
auto_df$tiempo=as.factor(auto_df$tiempo)

#######

boxplot(autoestima~tiempo, data=auto_df)
plotmeans(autoestima~tiempo, data=auto_df)


######




