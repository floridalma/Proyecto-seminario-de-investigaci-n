# ================================================
# Analizando las Encuestas de Hogares 2019
# ================================================

# 1) Instalacion de paquetes 
install.packages(c("tidyverse","psych","haven", "PerformanceAnalytics"))
library(tidyverse)
library(haven)
library(psych)
library(PerformanceAnalytics)

# 2) Importacion de los datos (Encuesta de Hogares 2019 )
EH2019 <- read_sav("EH2019_Persona.sav")
attach(EH2019) # Permite trabajar con los nombres de la varibales dentro del data frame 
names(EH2019) # Nombres de la varibles dentro del data frame
EH2019
str(EH2019) # Resumen del tipo de objeto 
class(EH2019) # Revisando la clase o tipo de objeto 

# 3) Observando las variables y Estadistica descriptiva
EH2019$ylab
EH2019$s02a_02

Genero<-EH2019$s02a_02
mean(EH2019$ylab, na.rm=TRUE) # Promedio con observaciones faltantes 
mean(ylab, na.rm=TRUE)

describe(ylab)
summary(ylab)

table(s02a_02) # Se puede agrupar variables cualitativas
str(s02a_02)
Genero[s02a_02 == 2] <- 0
str(Genero)
table(Genero)

Salario<-ylab
Edu <- aestudio
Edad <- s02a_03
Horas<-tothrs

Data <- data.frame(Salario, Edu, Edad, Horas, Genero, depto)
DF <- na.omit(Data) 
attach(DF)
DF

Salario_df<-DF$Salario
Horas_df<-DF$Horas
Edad_df<-DF$Edad
edu_df<-DF$Edu
Genero_df<-DF$Genero
depto_df<-DF$depto

#algoritmo del salario
Ln_salario <-log(Salario_df)-log(Horas_df*4)
describe(Salario)
describe(Ln_salario)
hist(Ln_salario)

# 3) 
## ggplot2 

hist(Salario_df)

## `ggplot2 'ejemplo de histograma:
install.packages("ggplot2")
library(ggplot2)

ggplot(DF, aes(x = Salario_df)) +
  geom_histogram()

hist1 <- ggplot(DF, aes(x=Salario_df)) + geom_histogram() + 
  xlab("Salario Mensuales (Mes)") + 
  ylab("Frecuencia Absoluta") +
  ggtitle("Distribucion del Salario, EH 2019")
hist1

# 4) Analisis de correlaciones

Exp <- Edad_df-edu_df-6
Exp2 <- Exp*Exp

plot(Ln_salario ~ Exp)
plot(Ln_salario ~ Edad_df)
cor.test(Ln_salario,Exp)

Data1 <- data.frame(Ln_salario, Edad_df, edu_df, Exp, Exp2, Genero_df, depto_df,Salario_df)
DF1 <- na.omit(Data1) 
attach(DF1)
DF1

ggplot(DF1,
       aes(y = Ln_salario, x = Exp)) +
  geom_point()

DF1$pred.Salario <- predict(lm(Ln_salario ~ Exp, data = DF1))
p1 <- ggplot(DF1, aes(x = Exp, y = Ln_salario))
p1 + geom_point() + geom_line(aes(y = pred.Salario)) + geom_smooth()

# 5)Generar el modelo lineal multiple
#RLM
m1 <- lm(DF1$Ln_salario ~ DF1$edu_df, DF1)
summary(m1)


m2 <- lm(DF1$Ln_salario ~ DF1$Exp+DF1$edu_df+DF1$Genero_df,DF1)
summary(m1)

m3 <- lm(DF1$Ln_salario ~ DF1$Exp+DF1$Exp2+DF1$edu_df+DF1$Genero_df,DF1)
summary(m3)
#Condiciones para la regresión múltiple lineal

######## Modelos lineales generalizados ########
install.packages("Metrics")
library(Metrics) #error cuadratico
install.packages("DescTools")
library(DescTools) #


#construir un modelo lineal de regresion  múltiple 
m3 <- lm(DF1$Ln_salario ~ DF1$Exp+DF1$Exp2+DF1$edu_df+DF1$Genero_df,DF1)
summary(m3)

plot(m3)

#Modelo Lineal Generalizado con distribución de Poisson y función link Log

m4<-glm(DF1$Ln_salario~DF1$Exp2+DF1$edu_df+DF1$Genero_df,family = poisson(link="log"))
m4

######################################

#construir un modelo lineal de regresion  múltiple
m3 <- lm(DF1$edu_df ~DF1$Genero_df,DF1)
summary(m3)
plot(m3)
#Modelo Lineal Generalizado con distribución de Poisson y función link Log

m4<-glm(DF1$edu_df ~DF1$Genero_df,family = poisson(link="log"))
summary(m4)
plot(m4)

#####################################

AIC(m3)
AIC(m4)

plot(DF1$edu_df~predict(m3)) #graficar predichos en funcion de obsrvados para modelo de regresion normal
plot(DF1$edu_df~exp(predict(m4))) #graficar predichos en función  de observados para  modelo Poisson

rmse(DF1$edu_df,predict(m3)) #error medio cuadrático para modelo3
rmse(DF1$edu_df,exp(predict(m4))) #error medio cuadrático para modelo Poisson


#el modelo Poisson con link log tiene mejor valor AIC, por lo tanto nos quedamos con este
#ahora vamos a explorar este modelo


#analisis de componentes principales
install.packages("GPArotation")

p1<-principal(DF1,nfactors = 5,rotate = "none")
p1
library(stats)

pca_nci <- prcomp(DF1, scale = TRUE)
pca_nci


#Para obtener los autovalores
pca_nci$sdev^2

#Importance of components:
summary(pca_nci)
#Porcentaje de varianza total
barplot(summary(pca_nci)$importance[2, ])

#eleccion de componente 
head(pca_nci$rotation)[, 1:3]

# Presentando los resulatados de la regresiones
install.packages("stargazer")
library(stargazer)

stargazer(m1, type="text")
stargazer(m2, type="text")
stargazer(m3, type="text")
stargazer(m4, type="text")

stargazer(m1, m2, m3,m4 , type="text",
          title = "Resultados de las regresiones")

