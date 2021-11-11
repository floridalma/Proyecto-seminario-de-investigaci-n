 #================================================
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

EH2019$s06b_21b

EH2019$ylab
EH2019$s02a_02

Genero<-EH2019$s02a_02
mean(EH2019$ylab)
mean(ylab)

mean(EH2019$ylab, na.rm=TRUE) # Promedio con observaciones faltantes 
mean(ylab, na.rm=TRUE)

describe(ylab)
summary(ylab)

table(s02a_02) # Se puede agrupar variables cualitativas
str(s02a_02)
Genero[s02a_02 == 2] <- 0
str(Genero)
table(Genero)
#variable respuesta
y<-ifelse(s06b_21b<3,1,0)
y
table(y)
Salario<-ylab
edu<-aestudio
Nivel_Edu <- niv_ed_g
Edad <- s02a_03
Horas<-tothrs
Data <- data.frame(y,Salario, Nivel_Edu,edu, Edad, Horas, Genero)
DF <- na.omit(Data) 
attach(DF)
DF
Tam_Empresa<-DF$y
Tam_Empresa

Salario_df<-DF$Salario
Horas_df<-DF$Horas
Edad_df<-DF$Edad
EDAD_df <- case_when(Edad_df %in% c(12:14) ~ "12 a 14",      #<<
                     Edad_df %in% c(15:18) ~"15 a 18",       #<<
                     Edad_df %in% c(19:24) ~ "19 a 24",      #<<
                     Edad_df %in% c(25:29) ~ "25 a 29",  #<<
                     Edad_df %in% c(30:34) ~ "30 a 34",
                     Edad_df %in% c(35:39) ~ "35 a 39",
                     Edad_df %in% c(40:44) ~ "40 a 44",
                     Edad_df %in% c(45:49) ~ "45 a 49",
                     Edad_df %in% c(50:54) ~ "50 a 54",
                     Edad_df %in% c(55:59) ~ "55 a 59",
                     Edad_df %in% c(60:64) ~ "60 a 64",
                     Edad_df %in% c(65:100) ~ "65 y mas")#<<
edu_df<-DF$edu
Genero_df<-DF$Genero
Ln_salario <-log(Salario_df)-log(Horas_df*4)
Nivel_edu_df<-DF$Nivel_Edu
Nivel_EDU<-case_when(Nivel_edu_df %in% c(0)~ "Sin nivel",
                     Nivel_edu_df %in% c(1)~ "Primaria",
                     Nivel_edu_df %in% c(2)~ "Secundaria",
                     Nivel_edu_df %in% c(3) ~ "Superior",
                     Nivel_edu_df %in% c(4) ~ "otros")
Exp <- Edad_df-edu_df-6
Exp2 <- Exp*Exp


#Data frame
Data1 <- data.frame(Tam_Empresa,Genero_df,Edad_df,Nivel_edu_df,Exp,Ln_salario)
DF1 <- na.omit(Data1) 
attach(DF1)
DF1

datos.entrenamiento<-DF1[1:10706,]
datos.prueba<-DF1[10707:15295,]

#analisis exploratorio

#analisis univariado
prop.table(table(Tam_Empresa))
prop.table(table(Nivel_edu_df))

#analisis vibariado
a<-table(Genero_df,Tam_Empresa)
a
summary(a)
prop.table(a)
w<-prop.table(a,2)
w
table(Nivel_edu_df)
b<-table(Nivel_edu_df,Tam_Empresa)
summary(b)

prop.table(b,2)
##################################
#Distribucion de Ocupados formales e informales

piepercent <- round(prop.table(table(Tam_Empresa))*100)
pie(table(Tam_Empresa),
    main="Distribución de los Trabajadores de EH2019",
    #título 
    col=c("mistyrose","lightblue"), # damos color a los sectores
    labels=paste0(piepercent,"%")) 
legend("topright", c("Trabajadores formales","Trabajadores informales"), cex=0.8, fill=c(2,4)) # añadimos la leyenda al gráfico
prop.table(Tam_Empresa)
#################################
#Trabajadores informales por genero

piepercent <- round(w[,2]*100)
pie(w[,2],
    main="Distribución de los ocupados por genero,Trabajadores formales por genero",
    #título 
    col=c("mistyrose","lightblue"), # damos color a los sectores
    labels=paste0(piepercent,"%")) 
legend("topright", c("Mujer","Hombre"), cex=0.8, fill=c(2,4)) # añadimos la leyenda al gráfico

#Trabajadores formales por genero

piepercent <- round(w[,1]*100)
pie(w[,1],
    main="Distribución de los ocupados por genero,Trabajadores formales por genero",
    #título 
    col=c("red","blue"), # damos color a los sectores
    labels= paste0(piepercent,"%") )
legend("topright", c("Mujer","Hombre"), cex=0.8, fill=c(2,4)) # añadimos la leyenda al gráfico
#######################################
#INFORMALIDAD SEGUN EDAD

table(EDAD_df)

prop.table(table(EDAD_df,Tam_Empresa),2)
hist(prop.table(table(EDAD_df,Tam_Empresa),1))

######################################
z<-prop.table(table(Tam_Empresa,EDAD_df),1)
z
barplot(z[2,])
######################################
#Tasa de informalidad por nivel educativo y sexo
table(Nivel_edu_df,Genero_df,Tam_Empresa)
prop.table(table(Nivel_edu_df,Genero_df,Tam_Empresa),2)
barplot(prop.table(table(Nivel_edu_df,Genero_df,Tam_Empresa)))

hist(EDAD_df)
table(EDAD_df)
Edad_df
prop.table(table(EDAD_df,Tam_Empresa),1)
hist(prop.table(table(EDAD_df,Tam_Empresa),1))

prop.table(table(Edad_df,Tam_Empresa))
hist(prop.table(table(Edad_df)))

histPercent <- function(Nivel_edu_df) {
  H <- hist(prop.table(table(Nivel_edu_df)), plot = FALSE)
  H
  labs <- paste(round(H, "%", sep=""))
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H)))
}

histPercent(islands, col="red")

#############################################
#INFORMALIDAD SEGUN NIVEL DE INGRESOS

table(SALARIO_df,Tam_Empresa)
X<-prop.table(table(SALARIO_df,Tam_Empresa),2)
X

library(scales)
g<- ggplot(Tam_Empresa, aes(SALARIO_df, fill=Trabajadores) ) + 
  labs(title = "Distribucio'n de los ocupados segun nivel de ingresos")+ylab("") +
  theme(plot.title = element_text(size = rel(2), colour = "blue")) 
g+geom_bar(position="dodge") + 
  scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
  theme(axis.title.x = element_text(face="bold", size=10))


#############################################

table(Ln_salario)

prop.table(table(Ln_salario))
plot(prop.table(table(Ln_salario)))
############################################



#MODELOS LOGIT Y PROBIT


# Modelo logit
mod_logit <- glm(Tam_Empresa~Genero_df+Edad_df+Nivel_edu_df+Exp+Ln_salario,data=datos.entrenamiento, family = "binomial")
mod_logit
summary(mod_logit)
###
#Bondad de ajuste 
library(DescTools)
PseudoR2(mod_logit)
AIC(mod_logit)
BIC(mod_logit)



#install.packages("ROCR")
library(ROCR)
#Porcentaje de clasificaciones correctas.

# Cálculo de la probabilidad predicha por el modelo con los datos de entrenamiento
prob.modelo <- predict(mod_logit, newdata = datos.prueba, type = "response")

# Vector de elementos “negativos”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “negativos” por “positivos” si la p > 0.5
pred.modelo[prob.modelo > 0.5] <- "1"

# Matriz de confusión
tabla.clasif <- table(pred.modelo, datos.prueba$Tam_Empresa)
tabla.clasif

error1<- 100*sum(tabla.clasif[1,2], tabla.clasif[2,1])/sum(tabla.clasif)
error1

tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc

sensibilidad <-100*sum(tabla.clasif[2,2])/sum(tabla.clasif[2,2], tabla.clasif[2,1])
sensibilidad

Especificidad<- 100*sum(tabla.clasif[1,1])/sum(tabla.clasif[1,2], tabla.clasif[1,1])
Especificidad

################

#Estadistico devianza
sum(residuals(mod_logit,type="deviance")^2)
1 - pchisq(sum(residuals(mod_logit,type="deviance")^2),1)
#anova(mod_logit,test="Chisq")
#Estadistico chi cuadrado
sum(residuals(mod_logit,type="pearson")^2)
1 - pchisq(sum(residuals(mod_logit,type="pearson")^2),1)
#Estadisticos de  Hosmer y Lemeshow, junto el de le Cessie y Van Houwelingen.
#install.packages("MKclass")
library(MKclass)
HLgof.test(fit = fitted(mod_logit), obs = as.numeric(as.character(datos.entrenamiento$Tam_Empresa)),
           X= model.matrix(datos.entrenamiento$Tam_Empresa ~ datos.entrenamiento$Genero_df+
                             datos.entrenamiento$Edad_df+datos.entrenamiento$Nivel_edu_df+
                             datos.entrenamiento$Exp+datos.entrenamiento$Ln_salario))


##################################
# Modelo probit
##################################
mod_probit <- glm(Tam_Empresa~Genero_df+Edad_df+Nivel_edu_df+Exp+Ln_salario,data=datos.entrenamiento,family=binomial(link="probit"))
summary(mod_probit)

#Bondad de ajuste 
library(DescTools)
PseudoR2(mod_probit)
AIC(mod_probit)
BIC(mod_probit)

#Porcentaje de clasificaciones correctas.

# Cálculo de la probabilidad predicha por el modelo con los datos de entrenamiento
prob.modelo <- predict(mod_probit, newdata = datos.prueba, type = "response")

# Vector de elementos “Negativos”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “negativos” por “positivos” si la p > 0.5
pred.modelo[prob.modelo > 0.5] <- "1"

#TamEmpresa = Tam_Empresa[1:10706]

# Matriz de confusión
tabla.clasif <- table(pred.modelo, datos.prueba$Tam_Empresa)
tabla.clasif


error1<- 100*sum(tabla.clasif[1,2], tabla.clasif[2,1])/sum(tabla.clasif)
error1

tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc

sensibilidad <-100*sum(tabla.clasif[2,2])/sum(tabla.clasif[2,2], tabla.clasif[2,1])
sensibilidad

Especificidad<- 100*sum(tabla.clasif[1,1])/sum(tabla.clasif[1,2], tabla.clasif[1,1])
Especificidad

###############

#Estadistico deviance
sum(residuals(mod_probit,type="deviance")^2)
1 - pchisq(sum(residuals(mod_probit,type="deviance")^2),1)
#anova(mod_logit,test="Chisq")
#Estadistico chi cuadrado
sum(residuals(mod_probit,type="pearson")^2)
1 - pchisq(sum(residuals(mod_probit,type="pearson")^2),1)
#Estadisticos de  Hosmer y Lemeshow, junto el de le Cessie y Van Houwelingen.
#install.packages("MKmisc")
library(MKclass)
HLgof.test(fit = fitted(mod_probit), obs = as.numeric(as.character(datos.entrenamiento$Tam_Empresa)),
           X= model.matrix(datos.entrenamiento$Tam_Empresa ~ datos.entrenamiento$Genero_df+
                             datos.entrenamiento$Edad_df+datos.entrenamiento$Nivel_edu_df+
                             datos.entrenamiento$Exp+datos.entrenamiento$Ln_salario))

###################################################################################
library(memisc)
mtable(mod_logit, mod_probit)
###################################################################################

#Evaluacion del modelo con los 
#DATOSDE PRUEBA

# Modelo logit
mod_logit <- glm(Tam_Empresa~Genero_df+Edad_df+Nivel_edu_df+Exp+Ln_salario,data=datos.prueba, family = "binomial")
mod_logit
summary(mod_logit)
###
#Bondad de ajuste 
library(DescTools)
PseudoR2(mod_logit)
AIC(mod_logit)
BIC(mod_logit)

#Porcentaje de clasificaciones correctas.

# Cálculo de la probabilidad predicha por el modelo con los datos de test
prob.modelo <- predict(mod_logit, newdata = datos.prueba, type = "response")

# Vector de elementos “Negativos”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “negativos” por “positivos” si la p > 0.5
pred.modelo[prob.modelo > 0.5] <- "1"

TamEmpresa = Tam_Empresa[10707:15295]

# Matriz de confusión
tabla.clasif <- table(pred.modelo, TamEmpresa)
tabla.clasif


error1<- 100*sum(tabla.clasif[1,2], tabla.clasif[2,1])/sum(tabla.clasif)
error1

tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc

sensibilidad <-100*sum(tabla.clasif[2,2])/sum(tabla.clasif[2,2], tabla.clasif[2,1])
sensibilidad

Especificidad<- 100*sum(tabla.clasif[1,1])/sum(tabla.clasif[1,2], tabla.clasif[1,1])
Especificidad

##################################


# Modelo probit
mod_probit <- glm(Tam_Empresa~Genero_df+Edad_df+Nivel_edu_df+Exp+Ln_salario,data=datos.prueba,family=binomial(link="probit"))
summary(mod_probit)

#Bondad de ajuste 
library(DescTools)
PseudoR2(mod_probit)
AIC(mod_probit)
BIC(mod_probit)

#Porcentaje de clasificaciones correctas.

# Cálculo de la probabilidad predicha por el modelo con los datos de test
prob.modelo <- predict(mod_probit, newdata = datos.prueba, type = "response")

# Vector de elementos “Negativos”
pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de “negativos” por “positivos” si la p > 0.5
pred.modelo[prob.modelo > 0.5] <- "1"

TamEmpresa = Tam_Empresa[10707:15295]

# Matriz de confusión
tabla.clasif <- table(pred.modelo, TamEmpresa)
tabla.clasif

error1<- 100*sum(tabla.clasif[1,2], tabla.clasif[2,1])/sum(tabla.clasif)
error1

tcc <- 100 * sum(diag(tabla.clasif))/sum(tabla.clasif)
tcc

sensibilidad <-100*sum(tabla.clasif[2,2])/sum(tabla.clasif[2,2], tabla.clasif[2,1])
sensibilidad

Especificidad<- 100*sum(tabla.clasif[1,1])/sum(tabla.clasif[1,2], tabla.clasif[1,1])
Especificidad
################


#Estadistico deviance
sum(residuals(mod_probit,type="deviance")^2)
1 - pchisq(sum(residuals(mod_probit,type="deviance")^2),1)
#anova(mod_logit,test="Chisq")
#Estadistico chi cuadrado
sum(residuals(mod_probit,type="pearson")^2)
1 - pchisq(sum(residuals(mod_probit,type="pearson")^2),1)
#Estadisticos de  Hosmer y Lemeshow, junto el de le Cessie y Van Houwelingen.
#install.packages("MKmisc")
library(MKclass)
HLgof.test(fit = fitted(mod_probit), obs = as.numeric(as.character(datos.prueba$Tam_Empresa)),
           X= model.matrix(datos.prueba$Tam_Empresa ~ datos.prueba$Genero_df+
                             datos.prueba$Edad_df+datos.prueba$Nivel_edu_df+
                             datos.prueba$Exp+datos.prueba$Ln_salario))




###########################################################################################
library(memisc)
mtable(mod_logit, mod_probit)
###########################################################################################
