
#######################################################################################################
#----------------------------CARGA DE LAS BIBLIOTECAS NECESARIAS--------------------------------------#
#######################################################################################################

setwd("D:/AnalisisEstadistico")

library(quadprog) 
library(zoo) 
library(tseries) 
library(forecast)
library(stats)

#######################################################################################################
#------------------------------------ CARGA DE DATOS -------------------------------------------------#
#######################################################################################################

MILK=read.csv("monthly-milk-production-pounds-p (1).csv")

head(MILK)
str(MILK)
summary(MILK) # observo que hay una observacion con valo nulo
MILK[!complete.cases(MILK),] # veo la observación con valor nulo

MILK <- MILK[complete.cases(MILK),] # serie sin el valor nulo
summary(MILK)
MILK


#######################################################################################################
#------------------------------------ANÁLISIS DE DATOS------------------------------------------------#
#######################################################################################################


plot(MILK) # pintamos los datos
lines(MILK) # se puede ver que existe tandencia y estacionalidad

tsdisplay(MILK[,2])
# observamos saltos regulares, lo cual indica escacionalidad.   

MILKSERIE <- ts(MILK[,2],start=c(1962,1),frequency=12) # creo la serie temporal
MILKSERIE
plot(MILKSERIE, main="Milk per cow", xlab="time", ylab="liter", col="blue") # la serie no es estacionaria

acf(MILKSERIE) # acf no decrece hasta cero, vamos a diferenciar los datos
pacf(MILKSERIE)

#######################################################################################################
#----------------------------DESCOMPOSICIÓN DE LA SERIE TEMPORAL--------------------------------------#
#######################################################################################################


MILKDES <- stl(MILKSERIE,s.window=12) # descomponemos la serie en componente estacional, tendencia y resto
MILKDES
plot(MILKDES, main="Descomposición de la serie", col="blue")

# otro metodo
dec<- decompose(MILKSERIE,type="additive") # descomponemos la serie en componente estacional, tendencia y resto
dec
plot(dec)

dec1<- decompose(MILKSERIE,type="multiplicative")
plot(dec1)



#######################################################################################################
# ------------------------------- DIFERENCIAMOS LOS DATOS---------------------------------------------#
#######################################################################################################



MILKSERIE.dif.12 <- diff(MILKSERIE, lag = 12)  # quitamos estacionalidad
MILKSERIE.stac <- diff(MILKSERIE.dif.12, diff = 1) # quitamos tendencia [trend (d=1)]

tsdisplay(MILKSERIE.stac,lag=36,main="Milk per cow stationary (d=1, D=1)", col="blue") 
#la serie ya parece estacionaria

acf(MILKSERIE.stac) # ACF indica el orden del proceso MA
#observamos que acf decrece ya a cero, pero no decrece rapidamente
pacf(MILKSERIE.stac) # pacf indica el orden del proceso AR
# pacf tampoco decrece rapidamente


# analizando estacionalidad
time <- c(1:length(MILKSERIE)) # vector de tiempo
trend <- lm(MILKSERIE~time) # regresión lineal: milk - time
reg.wart <- trend$fitted.values #valores estimados de la regresión
par(mfrow = c(1, 2),cex=0.8,cex.main=1.1,cex.lab=1.1) #ubicación de los graficos
seasonplot(MILKSERIE, 12, main="Grafico estacional",col="red3") #grafico estacional
seasonplot(MILKSERIE/reg.wart, 12, main="Grafico estacional sin tendencia", col="blue") # Grafico estacional sin tendencia


# voy a tener que construir un modelo sarima: arima con estacionalidad

#######################################################################################################
# ------------------------------- CONSTRUCCION DEL MODELO---------------------------------------------#
#######################################################################################################


autoarimamilk <- auto.arima(MILKSERIE,seasonal=TRUE,trace=TRUE) # construcción del modelo Auto Arima
autoarimamilk

# Best model: ARIMA(0,1,1)(0,1,1)[12]

autoarimamilklog <- auto.arima(log(MILKSERIE),seasonal=TRUE,trace=TRUE) # prueba con modelo log
# Best model: ARIMA(0,1,0)(0,0,1)[12]

#######################################################################################################
#------------------------------------DIAGNOSTICO DEL MODELO-------------------------------------------#
#######################################################################################################

tsdiag(autoarimamilk) # diagnostico del modelo

residuos <- autoarimamilk$resid #residuos del modelo
par(mfrow = c(1, 2),cex=0.8,cex.main=1.1,cex.lab=1.1) #wizualne aspekty wykresu
hist(residuos, main="Histograma de los residuos",ylab="Densidad",xlab="",prob=T) # histograma
curve(dnorm(x,mean=mean(residuos),sd=sd(residuos)),col="red",add=T) # curva de distribucion normal
qqnorm(residuos,main="Quartiles de los residuos", xlab="Quartiles teoricos", ylab="Quartiles de la muestra")

shapiro.test(residuos) # rechazo normalidad de residuos
-------------------------------------------------------------------------------------------------------
#prueba modelo log
tsdiag(autoarimamilklog) # diagnostico del modelo

residuoslog <- autoarimamilklog$resid #residuos del modelo
par(mfrow = c(1, 2),cex=0.8,cex.main=1.1,cex.lab=1.1) #wizualne aspekty wykresu
hist(residuoslog, main="Histograma de los residuos",ylab="Densidad",xlab="",prob=T) # histograma
curve(dnorm(x,mean=mean(residuoslog),sd=sd(residuoslog)),col="red",add=T) # curva de distribucion normal
qqnorm(residuoslog,main="Quartiles de los residuos", xlab="Quartiles teoricos", ylab="Quartiles de la muestra")

shapiro.test(residuoslog) # tampoco sale normalidad de residuos

-------------------------------------------------------------------------------------------------------
  
names(autoarimamilk) #informacje jakie zawiera obiekt

coeficientes <- autoarimamilk$coef # valores de los coeficientes estimados
coeficientes
varcoeficientes <- diag(autoarimamilk$var.coef) #varianza de los coeficientes estimados
varcoeficientes
t.stat <- abs(coeficientes/sqrt(varcoeficientes)) # Se testea hipotesis H0: coeficientes=0
p.value <- 2*(1-pnorm(t.stat)) #p-value para estadistico t.stat con distribución normal
p.value # p value bajo rechaza H0


#######################################################################################################
# ----------------------------------------- PREDICCIÓN------------------------------------------------#
#######################################################################################################

prog <- forecast(autoarimamilk,h=24) # prognostico del modelo para 24 meses
prog # valores prognosticados
par(mfrow = c(1, 1),cex=0.8,cex.main=1.1,cex.lab=1.1)
plot(prog) # gráfico del prognostico
