setwd("D:/AnalisisEstadistico")

################## CARGA DE DATOS #############################

CASAS=read.csv("house_train.csv")

head(CASAS)
str(CASAS)

############# FORMATEO DE LAS VARIABLES #################################

CASAS$zipcode=as.factor(CASAS$zipcode)
CASAS$bedrooms=as.factor(CASAS$bedrooms)
CASAS$bathrooms=as.factor(CASAS$bathrooms)
CASAS$waterfront=as.factor(CASAS$waterfront)
CASAS$view=as.factor(CASAS$view)
CASAS$floors=as.factor(CASAS$floors)
CASAS$grade=as.factor(CASAS$grade)
CASAS$condition=as.factor(CASAS$condition)

str(CASAS)
#summary(CASAS)


############# ANÁLISIS DEL PRECIO Y SUPERFICIE DE LAS VIVIENDAS #################################

library(ggplot2)

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=zipcode )) # observando la distribución de las variables por códogo postal veo que son casas de Seattle

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=price)) # precio en ciertas zonas en el centro es mayor

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=sqft_living)) # en el este las casas son mas grandes que en el oeste

# voy a introducir una variable ueva: precio por metro (pie) cuadrado
library(dplyr) # necesitaré paquete dplyr
CASAS3 <- mutate(CASAS, priceXsqft_living = price/sqft_living)

f <- ggplot(CASAS3, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) #el precio por metro cuadrado es definitivamente mayor en el centro. Es algo que podía indicar una brecha estructural.


############# ANÁLISIS DEL CAMBIO ESTRUCTURAL #################################

summary(CASAS3) # que existen outliers en el data sest

# divido el data set en dos partes: casas en el centro y casas en afueras para ver si puedo obtener un resultado mejor

CASAS_CENTRO = CASAS3[CASAS3$long< -122.125 & CASAS3$long> -122.4 & CASAS3$lat>47.53 & CASAS3$lat<47.7 ,]
CASAS_AFUERAS = CASAS3[((CASAS3$long> -122.125 | CASAS3$long< -122.4 ) & (CASAS3$lat<47.53 | CASAS3$lat>47.7) )    | ((CASAS3$lat>=47.53 | CASAS3$lat<=47.7) &(CASAS3$long> -122.125 | CASAS3$long< -122.4 ))| ((CASAS3$lat<=47.53 | CASAS3$lat>=47.7) &(CASAS3$long< -122.125 | CASAS3$long> -122.4 )),]

f <- ggplot(CASAS_CENTRO, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) 


f <- ggplot(CASAS_AFUERAS, aes(long,lat))
f + geom_point(aes(color=priceXsqft_living)) 

# compruebo si existe cambio de estructura de precios en función de la ubicación de las viviendas

modelotodo=lm(price~sqft_living,data=CASAS3)
modelocentro=lm(price~sqft_living,data=CASAS_CENTRO)
modeloafueras=lm(price~sqft_living,data=CASAS_AFUERAS)

plot(CASAS$sqft_living,CASAS$price)
abline(modelotodo,col="red",lty = "dashed")
abline(modelocentro,col="blue")
abline(modeloafueras,col="green") # encuentro que la pendiente de cambio de preio en finción de superficie es mayor para las casas en el centro que para las casas en afueras


# adicionalmente realizo chow test
if (!require("gap")){
  install.packages("gap") 
  library(gap)
}

chow.test(log(CASAS_CENTRO$price),CASAS_CENTRO$sqft_living,log(CASAS_AFUERAS$price),CASAS_AFUERAS$sqft_living)

chow.test(CASAS_CENTRO$price,CASAS_CENTRO$sqft_living,CASAS_AFUERAS$price,CASAS_AFUERAS$sqft_living) # el resultado da siginificativo

# compruebo la distribución de los dos data sets:
summary(CASAS_CENTRO)
summary(CASAS_AFUERAS)# aunque parece que sigue habiendo valores extremos las series están mas homogeneas que la serie principal



#################### constucción del modelo ########################################

# -------------------------- modelo básico ---------------------------------------

#casas del centro

modelocentrolog=lm(log(price)~sqft_living,data=CASAS_CENTRO)
summary(modelocentrolog)
qqnorm(modelocentrolog$residuals); qqline(modelocentrolog$residuals,col=2)

plot(modelocentrolog$residuals)
smoothScatter(modelocentrolog$residuals)
hist(modelocentrolog$residuals)
confint(modelocentrolog,level=0.95)

if (!require("MASS")){
  install.packages("MASS") 
  library(MASS)
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}
modelocentrologrobusto=rlm(log(price)~sqft_living,data=CASAS_CENTRO)
summary(modelocentrologrobusto)
qqnorm(modelocentrologrobusto$residuals); qqline(modelocentrolog$residuals,col=2)

plot(modelocentrologrobusto$residuals)
smoothScatter(modelocentrologrobusto$residuals)
hist(modelocentrologrobusto$residuals)
confint.default(modelocentrologrobusto,level=0.95)

AIC(modelocentrolog)
AIC(modelocentrologrobusto)
BIC(modelocentrolog)
BIC(modelocentrologrobusto)


#casas de las afueras

modeloafueras=lm(log(price)~sqft_living,data=CASAS_AFUERAS)
summary(modeloafueras)
qqnorm(modeloafueras$residuals); qqline(modeloafueras$residuals,col=2)

modeloafuerasrobusto=rlm(log(price)~sqft_living,data=CASAS_AFUERAS)
summary(modeloafuerasrobusto)
qqnorm(modeloafuerasrobusto$residuals); qqline(modeloafueras$residuals,col=2)
confint.default(modeloafuerasrobusto,level=0.95)


plot(modeloafueras$residuals)
smoothScatter(modeloafueras$residuals)
hist(modeloafueras$residuals)
confint(modeloafueras,level=0.95)



AIC(modeloafueras)
AIC(modeloafuerasrobusto)
BIC(modeloafueras)
BIC(modeloafuerasrobusto)

# ------------------ analizamos si añadir variables--------------------------------

# variables adicionales

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=waterfront)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=view)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=yr_built )) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=yr_renovated)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=bedrooms)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=bathrooms)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=floors)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=grade)) 

f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=condition)) 

head(CASAS3)
head(CASAS3[,c(3:12,15:16,22)])
plot(CASAS3[,c(3:12,15:16,22)])


# modelo casas centro

#busco patrones en los residuos
plot(modelocentrolog$residuals,col=CASAS3$yr_renovated)
plot(modelocentrolog$residuals,col=CASAS3$sqft_lot)
plot(modelocentrolog$residuals,col=CASAS3$bedrooms)
plot(modelocentrolog$residuals,col=CASAS3$floors)
plot(modelocentrolog$residuals,col=CASAS3$grade)
plot(modelocentrolog$residuals,col=CASAS3$condition)
plot(modelocentrolog$residuals,col=CASAS3$waterfront)
plot(modelocentrolog$residuals,col=CASAS3$view)
plot(modelocentrolog$residuals,col=CASAS3$yr_built)
plot(modelocentrolog$residuals,col=CASAS3$sqft_living15)
plot(modelocentrolog$residuals,col=CASAS3$sqft_lot15)
plot(modelocentrolog$residuals,col=CASAS3$sqft_above)

modelocentrologgrade=lm(log(price)~sqft_living+grade,data=CASAS_CENTRO)
modelocentrologcond=lm(log(price)~sqft_living+grade+condition,data=CASAS_CENTRO)
modelocentrologwaterfront=lm(log(price)~sqft_living+grade+condition+waterfront,data=CASAS_CENTRO)
modelocentrologwiew=lm(log(price)~sqft_living+grade+condition+waterfront+view,data=CASAS_CENTRO)
modelocentrologyrbuild=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built,data=CASAS_CENTRO)
modelocentrologneighbours=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15,data=CASAS_CENTRO)

AIC(modelocentrolog)
AIC(modelocentrologgrade)
AIC(modelocentrologcond)
AIC(modelocentrologwaterfront)
AIC(modelocentrologwiew)
AIC(modelocentrologyrbuild)
AIC(modelocentrologneighbours)

BIC(modelocentrolog)
BIC(modelocentrologgrade)
BIC(modelocentrologcond)
BIC(modelocentrologwaterfront)
BIC(modelocentrologwiew)
BIC(modelocentrologyrbuild)
BIC(modelocentrologneighbours)

summary(modelocentrologneighbours)
confint(modelocentrologneighbours,level=0.95)
qqnorm(modelocentrologneighbours$residuals); qqline(modelocentrologneighbours$residuals,col=2)
hist(modelocentrologneighbours$residuals)


# modelo casas afueras

#busco patrones en los residuos

plot(modeloafueras$residuals,col=CASAS3$yr_renovated)
plot(modeloafueras$residuals,col=CASAS3$bedrooms)
plot(modeloafueras$residuals,col=CASAS3$grade)
plot(modeloafueras$residuals,col=CASAS3$condition)
plot(modeloafueras$residuals,col=CASAS3$waterfront)
plot(modeloafueras$residuals,col=CASAS3$view)
plot(modeloafueras$residuals,col=CASAS3$yr_built)
plot(modeloafueras$residuals,col=CASAS3$sqft_living15)
plot(modeloafueras$residuals,col=CASAS3$sqft_lot15)
plot(modeloafueras$residuals,col=CASAS3$sqft_above)

modeloafueras=lm(log(price)~sqft_living,data=CASAS_AFUERAS)
modeloafuerasgrade=lm(log(price)~sqft_living+grade,data=CASAS_AFUERAS)
modeloafuerascondition=lm(log(price)~sqft_living+grade+condition,data=CASAS_AFUERAS)
modeloafueraswaterfront=lm(log(price)~sqft_living+grade+condition+waterfront,data=CASAS_AFUERAS)
modeloafuerasview=lm(log(price)~sqft_living+grade+condition+waterfront+view,data=CASAS_AFUERAS)
modeloafuerasyrbuild=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built,data=CASAS_AFUERAS)
modeloafuerasneihgbours=lm(log(price)~sqft_living+grade+condition+waterfront+view+yr_built+sqft_living15,data=CASAS_AFUERAS)

AIC(modeloafueras)
AIC(modeloafuerasgrade)
AIC(modeloafuerascondition)
AIC(modeloafueraswaterfront)
AIC(modeloafuerasview)
AIC(modeloafuerasyrbuild)
AIC(modeloafuerasneihgbours)

BIC(modeloafueras)
BIC(modeloafuerasgrade)
BIC(modeloafuerascondition)
BIC(modeloafueraswaterfront)
BIC(modeloafuerasview)
BIC(modeloafuerasyrbuild)
BIC(modeloafuerasneihgbours)



summary(modeloafuerasneihgbours)
confint(modeloafuerasneihgbours,level=0.95)
qqnorm(modeloafuerasneihgbours$residuals); qqline(modeloafuerasneihgbours$residuals,col=2)
hist(modeloafuerasneihgbours$residuals)








plot(CASAS2[,c(1:10)])

CASAS2<-CASAS[,c(3:10, 15:19)]

head(CASAS2)

hist(CASAS$zipcode)
modelo1=lm(price~sqft_living,data=CASAS)
summary(modelo1)

modelo2=lm(price~sqft_living+zipcode,data=CASAS)
summary(modelo2)

modelo3=lm(price~sqft_living+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo3)

modelo4=lm(price~sqft_lot+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo4)

modelo5=lm(log(price)~log(sqft_living)+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo5)

modelo6=rlm(log(price)~log(sqft_living)+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo6)

modelo7=rlm(log(price)~sqft_living+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo7)


modelo4=lm(price~sqft_lot+zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo4)

modelo7=lm(price~sqft_living*zipcode,data=CASAS)
summary(modelo7)

modelo8=lm(price~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo8)

modelo9=rlm(price~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo9)

modelo10=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo10)

modelo11=rlm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated,data=CASAS)
summary(modelo11)


modelo12=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade,data=CASAS)
summary(modelo12)

modelo13=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS)
summary(modelo13)

modelo13=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS)
summary(modelo13)

modelo14=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition + bedrooms,data=CASAS)
summary(modelo14)

modelo15=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS2)
summary(modelo15)

modelo15=rlm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS2)
summary(modelo15)

modelo16=lm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASASnoLUJO)
summary(modelo16)

modelo16=rlm(log(price)~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition +floors,data=CASAS)
summary(modelo16)

modelo16=rlm(log(price)~log(sqft_living)*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS)
summary(modelo16)

modelo17=rlm(log(price)~log(sqft_living) ,data=CASAS)
qqnorm(modelo17$residuals); qqline(modelo17$residuals,col=2)

summary(modelo16)

plot(modelo16$residuals,col=CASAS$sqft_living)
plot(modelo16$residuals,col=CASAS$price)

plot(modelo3$residuals,col=CASAS$yr_renovated)
plot(modelo5$residuals,col=CASAS$sqft_lot)
plot(modelo5$residuals,col=CASAS$bedrooms)
plot(modelo5$residuals,col=CASAS$floors)
hist(modelo5$residuals)
qqnorm(modelo5$residuals); qqline(modelo5$residuals,col=2)

plot(CASAS$price,CASAS$sqft_living)

str(CASAS)

boxplot(CASAS$sqft_living)


library(dplyr) # primero cargamos paquete dplyr
#casas2 <- mutate(CASAS, bathxbedroom =bathrooms/bedrooms)
CASAS3 <- mutate(CASAS, priceXsqft_living = price/sqft_living)

CASAS2=CASAS[
  CASAS$bathrooms ==("0")|
  CASAS$bathrooms ==("0.5")|
  CASAS$bathrooms ==("0.75")|
  CASAS$bathrooms ==("1")|
  CASAS$bathrooms ==("1.25")|
  CASAS$bathrooms ==("1.5")|
  CASAS$bathrooms ==("1.75")|
  CASAS$bathrooms ==("2")|
  CASAS$bathrooms ==("2.25")|
  CASAS$bathrooms ==("2.5")|
  CASAS$bathrooms ==("2.75")|
  CASAS$bathrooms ==("3")
  ,]





CASASnoLUJO = CASAS [
  CASAS$zipcode != ("98004") &
    CASAS$zipcode != ("98006") &
    CASAS$zipcode != ("98008") &
    CASAS$zipcode != ("98039") &
    CASAS$zipcode != ("98040") &
    CASAS$zipcode != ("98102")
  ,]
head(CASASnoLUJO)
summary (CASASnoLUJO)


CASASLUJO = CASAS [
  CASAS$zipcode != ("98004") |
    CASAS$zipcode != ("98006") |
    CASAS$zipcode != ("98008") |
    CASAS$zipcode != ("98039") |
    CASAS$zipcode != ("98040") |
    CASAS$zipcode != ("98102")
  ,]

modelo=lm(price~sqft_living*zipcode+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS)
modeloLOG=lm(log(price)~sqft_living+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASAS)

modeloLUJO=lm(price~sqft_living+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASASLUJO)
modeloLUJOlog=lm(log(price)~sqft_living+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASASLUJO)

modeloNOLUJO=lm(price~sqft_living+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASASnoLUJO)
modeloNOLUJOlog=lm(log(price)~sqft_living+yr_built+view+waterfront+yr_renovated + grade + condition,data=CASASnoLUJO)

modelotodo=lm(price~sqft_living,data=CASAS)
modelotodoLOG=lm(log(price)~sqft_living,data=CASAS)
summary(modelotodoLOG)



qqnorm(modelo$residuals); qqline(modelo$residuals,col=2)
qqnorm(modeloLOG$residuals); qqline(modeloLOG$residuals,col=2)

qqnorm(modeloLUJO$residuals); qqline(modeloLUJO$residuals,col=2)
qqnorm(modeloLUJOlog$residuals); qqline(modeloLUJOlog$residuals,col=2)

qqnorm(modeloNOLUJO$residuals); qqline(modeloNOLUJO$residuals,col=2)
qqnorm(modeloNOLUJOlog$residuals); qqline(modeloNOLUJOlog$residuals,col=2)




AIC(modelo1)
AIC(modeloLOG)
AIC(modeloLUJO)
AIC(modeloLUJOlog)
AIC(modelo5)
AIC(modelo6)
AIC(modelo7)
AIC(modelo8)
AIC(modelo9)
AIC(modelo10)
AIC(modelo11)
AIC(modelo12)
AIC(modelo13)
AIC(modelo16)


BIC(modelo1)
BIC(modelo2)
BIC(modelo3)
BIC(modelo4)
BIC(modelo5)
BIC(modelo6)
BIC(modelo7)
BIC(modelo8)
BIC(modelo9)
BIC(modelo10)
BIC(modelo11)
BIC(modelo12)
BIC(modelo13)
BIC(modelo16)



CASAS2M = CASAS[CASAS$price>=2000000,]
CASAS2M

CASASno2M = CASAS[CASAS$price<2000000,]
summary(CASASno2M)


CASAS3 = CASAS[CASAS$price>=3000000,]
CASAS3

CASASno3 = CASAS[CASAS$price<3000000,]
summary(CASASno3)


CASAS7 = CASAS[CASAS$price>=7000000,]
CASAS7

CASASno7 = CASAS[CASAS$price<7000000,]
summary(CASASno7)

CASASno5 = CASAS[CASAS$price<5000000,]
summary(CASASno5)

CASAS4 = CASAS[CASAS$price>=4000000,]
CASAS4

CASASno4 = CASAS[CASAS$price<4000000,]
summary(CASASno4)

plot(CASAS$bedrooms,CASAS$price)
plot(CASAS$bathrooms,CASAS$price)
plot(CASAS$waterfront,CASAS$price)
plot(CASAS$view,CASAS$price)
plot(CASAS$zipcode,CASAS$price,col=CASAS$bedrooms)
plot(CASAS$zipcode,CASAS$price,col=CASAS$sqft_living)

library(ggplot2)
h <- ggplot(CASAS, aes(bedrooms, bathrooms)) 
h + geom_jitter(aes(color=floors)) 

h <- ggplot(CASAS, aes(bedrooms, bathrooms)) 
h + geom_jitter(aes(color=sqft_living)) 

h <- ggplot(CASAS, aes(zipcode,bedrooms)) 
h + geom_jitter(aes(color=price)) 


f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=bedrooms)) 

f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=sqft_living)) 

f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=waterfront)) 

f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=view)) 

f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=yr_renovated)) 

f <- ggplot(CASAS, aes(zipcode,price))
f + geom_point(aes(color=yr_built)) 






f <- ggplot(CASAS, aes(long,lat))
f + geom_point(aes(color=sqft_basement)) 


 



head(CASAS)
summary(CASAS)

library(dplyr)
by_zipcode <- group_by(CASAS, zipcode)
pr_med_quil_by_zipcode <- summarise(by_zipcode, median(price))
pr_med_quil_by_zipcode  # precio por quilate parece mayor para valor
plot(pr_med_quil_by_zipcode)
