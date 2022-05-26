
#####Cargar librerías ######

library(Metrics)




####### 0.Ejemplo predicción para kaggle #############

####Leer base para entrenar

str(data_rlm)

data_rlm$vdate=as.factor(data_rlm$vdate)

data_rlm$vdate<-as.Date(data_rlm$vdate,"%m/%d/%Y")


months.Date(data_rlm$vdate2, abbreviate = T) ### para extraer el mes
weekdays(data_rlm$vdate2, abbreviate = T) ###para extraer el día de la semana
quarters(data_rlm$vdate2, abbreviate = T) ### Para extraer el trimestre
format(data_rlm$vdate2, format="%d") ###Para extraer el día del mes
format(data_rlm$vdate2, format="%Y") ## para extraer el año

####Leer base para evaluación
data_rlm_test<-read.csv("datos_test_nooutput.csv",dec=".",sep=",", header= T, fileEncoding = "UTF-8")


#######Ajustar modelo en base de entrenamiento ###
data_rlm<-read.csv("datos_los_train.csv",dec=".",sep=",", header= T, fileEncoding = "UTF-8")
modelo<-lm(data=data_rlm, lengthofstay ~ pneum+sodium)

predicho_test<-predict(modelo,data_rlm_test) ##### para predicciones que se montan en kaggle



######Se construye base para envío

submission=data.frame("eid"=data_rlm_test$eid, "lengthofstay"=predicho_test)
write.csv(submission, "submission3.csv", row.names=F)

