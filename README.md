setwd("c:/users/camila/desktop/tarea2")
file<- paste0(getwd(),"/tarea.csv")
data<-read.csv2(file=file,head= TRUE)
str(data)

#aqui obtengo el promedio de las variables
Mbesalco<- mean(data$besalco)
Mandina<- mean(data$andina)
Msantander<-mean(data$santander)
Migpa<- mean(data$igpa)

#obtengo la desviacion estandar
Dbesalco<-sd(data$besalco)
Dandina<-sd(data$andina)
Dsantander<-sd(data$santander)
Digpa<-sd(data$igpa)


#aqui obtengo las regresiones lineales
lmbesalco<- lm(data$besalco~ data$igpa, data=data)
summary(lmbesalco)

lmandina<-lm(data$andina~data$igpa,data=data)
summary(lmandina)

lmsantander<-lm(data$santander~data$igpa, data=data)
summary(lmsantander)


#con esto obtengo la correlacion entre los retornos
x<- data.frame(data$igpa, data$besalco, data$andina, data$santander)
matrizcor<-cor(x)

#aqui instalo el paquete para graficar
install.packages("ggplot2")
library(ggplot2)


#con este codigo grafico
qplot(data$z, data$besalco, data=data)
qplot(data$z, data$andina, data=data)
qplot(data$z, data$santander, data=data)
qplot(data$z, data$igpa, data=data)
