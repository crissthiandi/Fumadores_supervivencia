library(survival)
library(KMsurv)
library(survminer)
setwd("C:/Users/Acer E15/Google Drive/7° semestre/Analisis de supervivencia/Proyecto")

datos<-read.csv("datosEquipo10.csv", header = T)
str(datos)
head(datos)

datos$Fumar<-factor(datos$Fumar, labels = c("No fuma","Si fuma")) #cero no fuma, 1 si fuma
datos$Sexo<-factor(datos$Sexo, labels=c("Mujer","Hombre"))

#analisis preliminar 
attach(datos)
tapply(Fumar,Sexo,summary)
tapply(Tiempo,Fumar,summary)
summary(Tiempo)
detach(datos)
#Censura
summary(as.factor(datos$censura))
        

#CREANDO VARIABLES DUMMYS  
datos$Term = (datos$Producto == 1)
datos$Whole.life = (datos$Producto == 2)
datos$Universal.life = (datos$Producto == 3)
datos$Other = (datos$Producto == 4)

#para producto

producto.ph<-coxph(Surv(Tiempo, censura)~factor(Producto), datos
                   , method="breslow", na.action=na.exclude)
summary(producto.ph)



#para la edad

age.ph<-coxph(Surv(Tiempo, censura)~Edad, datos
              , method="breslow", na.action=na.exclude)

print(summary(age.ph))

#Para fumador

fumador.ph<-coxph(Surv(Tiempo, censura)~Fumar, datos
                  , method="breslow", na.action=na.exclude)

summary(fumador.ph)

# Para sexo

sexo.ph<-coxph(Surv(Tiempo, censura)~Sexo, datos
               , method="breslow", na.action=na.exclude)

summary(sexo.ph)

# solo el modelo con edad resulto significativo, verificaremos si mantiene su significancia al
#al agregar las variables una por una

#modelo con sexo que resulto no significativa
modelo1.ph<-coxph(Surv(Tiempo, censura)~Sexo+Edad, datos
                  , method="breslow", na.action=na.exclude)

summary(modelo1.ph)

#modelo con fumador que resulto no significativo

modelo2.ph<-coxph(Surv(Tiempo, censura)~Fumar+Edad, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo2.ph)

#modelo con los dos no significativos
modelo3.ph<-coxph(Surv(Tiempo, censura)~Fumar+Sexo, datos
                 , method="breslow", na.action=na.exclude)

summary(modelo3.ph)
# Se checa si un modelo con todas las variables afecta en la significancia

modelo4.ph<-coxph(Surv(Tiempo, censura)~Fumar+Sexo+Edad, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo4.ph)


# modelo con las interacciones de las 3 covariables
modelo5.ph<-coxph(Surv(Tiempo, censura)~Fumar+Sexo+Edad+Fumar:Sexo+Fumar:Edad+Fumar:Edad, datos
                  , method="breslow", na.action=na.exclude)

summary(modelo5.ph)

#modelo con interacciones de fumar y edad
modelo6.ph<-coxph(Surv(Tiempo, censura)~Fumar*Edad, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo6.ph)
# modelo con interacciones de sexo y edad
modelo7.ph<-coxph(Surv(Tiempo, censura)~Sexo*Edad, datos
                  , method="breslow", na.action=na.exclude)

summary(modelo7.ph)

#modelo con edad y sexo solamente

modelo8.ph<-coxph(Surv(Tiempo, censura)~Fumar*Sexo, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo8.ph)  

#ingresando producto factores  
  
datos.ph<-coxph(Surv(Tiempo, censura)~Fumar+Sexo+Edad+factor(Producto), datos
                  , method="breslow", na.action=na.exclude)
summary(datos.ph)

#como resulto significante producto, se realizan modelos para ver si alguna cambia su significancia
#lo suficiente para ser aceptada en el modelo

modelo1.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Whole.life+Universal.life, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo1.ph)

#parece que todos tienen posibilidad de estar en el modelo final, se agregan nuevas variables

#sexo
modelo2.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Whole.life+Universal.life+Sexo, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo2.ph)
#fumador
modelo3.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Whole.life+Universal.life+Fumar, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo3.ph)
#si se agrega fumar whole.life se sale del modelo, se esperaba era el más cercano a salir

#modelo sin whole.life
#con Fumar
modelo4.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Universal.life+Fumar, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo4.ph)
#aunque es cercano, es menor a el valor que tenia whole.life, pero tiene mayor fuerza por la literatura
#a tener espacio en el modelo

#con sexo
modelo5.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Universal.life+Sexo, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo5.ph)
#no pasa sexo para el modelo, aunque sabemos por la literatura que es una variable importante 
#al igual que el fumar

#interacciones 

modelo6.ph<-coxph(Surv(Tiempo, censura)~Edad+Term+Universal.life+Whole.life*Fumar, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo6.ph)

#Fumar y edad

modelo7.ph<-coxph(Surv(Tiempo, censura)~Edad*Fumar+Term+Whole.life+Universal.life, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo7.ph)

#todas con fumador

modelo8.ph<-coxph(Surv(Tiempo, censura)~Edad+Term*Fumar+Whole.life*Fumar+Universal.life*Fumar, datos
                  , method="breslow", na.action=na.exclude)
summary(modelo8.ph)

#se concluye el modelo final, ya que es el que muestra más sentido con la literatura

modelofinal.ph<-modelo4.ph
summary(modelofinal.ph)


# checando riesgos proporcionales
cox.modelo.ph<-cox.zph(modelofinal.ph)
cox.modelo.ph
summary(cox.modelo.ph)

#dado que el p valor global es mayor que .05 no se tiene evidencia en contra de riesgos
#proporcionales, más aun el p valor de cada covariable
# tambien es mayor que o.o5 por lo que ninguna de ellas viola el supuesto
# de riesgos proporcionales

par(mfrow=c(2,3))
plot(cox.modelo.ph)

ggcoxzph(cox.modelo.ph)


# checando linealidad
j=1
par(mfrow=c(1,2))

datos$Fumar<-as.integer(datos$Fumar, labels = c("0","1"))
datos$Edad<-datos$Edad-min(datos$Edad)

modelo<-coxph(Surv(Tiempo, censura)~Edad+Term+Universal.life+Fumar
              , datos, method="breslow", na.action=na.exclude)
mar.modelo<-residuals(modelo, type='martingale')

X<-as.matrix(datos[,c("Edad", "Fumar")]) 
head(X)


for (j in 1:2){ #graficas de residuales martingalas
  # plot (X[,j],mar.modelo9, xlab=c("age","ndrugtx")[j], ylab="residuos martingalas")
  #abline(h=0, lty=2)
  #lines(lowess(X[,j],mar.modelo9, iter=0))
  scatter.smooth(X[,j],mar.modelo, type="p", pch=".",xlab=c("Edad","Fumar")[j]
                 , ylab="residuos martingalas")
}



ggcoxdiagnostics(modelo, type = "martingale", ox.scale = "time")


#Checando datos influyentes

dfbeta.modelo<-residuals(modelo, type='dfbeta')
par(mfrow=c(2,3))
for (j in 1:4){
  plot(dfbeta.modelo[,j], ylab=names(coef(modelo))[j])
  abline(h=0, lty=2)
}

par(mfrow=c(1,1))

BRiesgo<-basehaz(modelo,centered=T)
XX<-as.matrix(BRiesgo)
plot(XX[,2],XX[,1],type="s",xlab="Tiempo",ylab="Riesgo",
     main="Función  Riesgo Base")

legend(locator(1),legend="Baseline Hazard",lty=1)

legend(20,3,legend="Baseline Hazard",lty=1)


#hacer para todas las bases de datos
