library(parallel)
library(doParallel)
library(plyr)
library(dummies)
library(ggplot2)
library(naniar)
library(MASS)
library(caret)
library(MXM)
library(gridExtra)
library(randomForest)
library(catboost)
library(xgboost)

# Directorio de trabajo 
setwd("C:/Users/Vero/Desktop/Machine Final")

source("funcion steprepetido binaria.R")
source ("cruzadas avnnet y log binaria.R") 
source ("cruzada arbolbin.R") 
source ("cruzada rf binaria.R") 

library(parallel)
library(doParallel)
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

# Carga de datos
df <- read.csv("datos_finales.csv")
dput(names(df))
str(df)

# Variables de mi df
c("age", "balance", "duration", "campaign", "pdays", "previous", 
  "marital", "education", "default", "housing", "loan", "contact", 
  "poutcome", "week_of_month", "quincena", "working", "season", 
  "deposit")

# Separación
listconti <- c("age", "balance", "duration", "campaign", "pdays", "previous")
listclass <- c("marital", "education", "default", "housing", "loan", "contact", "poutcome","week_of_month", "working", "quincena", "season")
vardep <- c("deposit")

germanbien <- df

# Estandarización de las variables continuas
means <-apply(germanbien[,listconti],2,mean)
sds <- sapply(germanbien[,listconti],sd)

# Estandarizo solo las continuas y uno con las categoricas
germanbien <- df

germancon<-scale(germanbien[,listconti], center = means, scale = sds)
numerocont<-which(colnames(germanbien)%in%listconti)
germanbien<-cbind(germancon,germanbien[,-numerocont,drop=FALSE ])

germanbien[,vardep]<-as.factor(germanbien[,vardep])

# Frecuencia de cada nivel 
frecu<-ldply(germanbien[,listclass],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)
frecu

# Creación de dummies
germanbis <- dummy.data.frame(germanbien, listclass, sep = ".")

# Para borrar las dummies con menos de k observaciones se utiliza el 
# listado de frecuencias frecu obtenido anteriormente

# 1) Obtengo filas de frecu con frecuencia menor que k=20 como ejemplo
frecu20 <- frecu[frecu$frecuencia<20,]

# 2) Obtengo listado de los niveles en el mismo formato que las dummies,
# con separador .
frecu20$dum<-paste(frecu20$variable,frecu20$nivel,sep=".")
listamal<-dput(frecu20$dum)

# Borro las dummies de amesbis que coinciden con la lista
germanbis[,listamal]<-NULL

# Se guardan los datos depurados y con dummies
save(germanbis,file="Bank_Marketing.Rda")

dput(names(germanbis))

# Lo mismo con ggplot2 ya que no deja guardar el png la anterior
library(ggplot2)

deposit_counts <- table(df$deposit)
deposit_counts_df <- as.data.frame(deposit_counts)
colnames(deposit_counts_df) <- c("deposit", "count")

# Creación del gráfico
fig <- ggplot(deposit_counts_df, aes(x = deposit, y = count, fill = deposit)) +
  geom_bar(stat = "identity", color = "black", size = 1.5) +  # Añadir borde negro grueso
  scale_fill_manual(values = c("Yes" = "#41B7C4", "No" = "#f8766d")) +
  labs(title = "Contrata el depósito a plazo fijo", x = "", y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Arial", size = 14),
    axis.title = element_text(family = "Arial", size = 12),
    axis.text = element_text(family = "Arial", size = 10),
    legend.position = "none"
  )

print(fig)

# Variables tras crear las dummies

listconti<- c("age", "balance", "duration", "campaign", "pdays", "previous", 
              "marital.divorced", "marital.married", "marital.single", "education.primary", 
              "education.secondary", "education.tertiary", "education.unknown", 
              "default.no", "default.yes", "housing.no", "housing.yes", "loan.no", 
              "loan.yes", "poutcome.failure", "poutcome.other", "poutcome.success", 
              "poutcome.unknown", "week_of_month.1", "week_of_month.2", "week_of_month.3", 
              "week_of_month.4", "week_of_month.5", "working.No", "working.Yes", "season.Autumn", 
              "season.Spring", "season.Summer", "season.Winter")

listclass <- c("") 
vardep <- "deposit" 

categorical_barplot <- function(var) {
  ggplot(df, aes_string(x = var, fill = "deposit")) +
    geom_bar(position = "dodge", color = "black", size = 1) +  # Añadir borde negro grueso
    labs(title = var, x = "", y = "") +  # Eliminar título del eje x
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Título en negrita
      axis.title = element_text(face = "bold"),               # Títulos de ejes en negrita
      axis.text = element_text(family = "Arial", size = 10),
      legend.position = "none"  # Eliminar la leyenda
    )
}

# Variables categóricas a graficar
categorical_vars <- c("marital", "education", "default", "housing", 
                      "loan", "poutcome", "week_of_month", "quincena", 
                      "working", "season")

# Dividir las variables en chunks para múltiples gráficos
chunks <- split(categorical_vars, ceiling(seq_along(categorical_vars) / 4))

# Generar y mostrar los gráficos en una cuadrícula
for (chunk in chunks) {
  plots <- lapply(chunk, categorical_barplot)
  grid.arrange(grobs = plots, ncol = 2)
}

# Boxplot


plot_boxplot1 <- ggplot(df, aes(x = deposit, y = balance, fill = deposit)) +
  geom_boxplot() +
  labs(title = "", x = "Deposit", y = "Balance") +
  theme_minimal()

plot_boxplot2 <- ggplot(df, aes(x = deposit, y = age, fill = deposit)) +
  geom_boxplot() +
  labs(title = "", x = "Deposit", y = "Age") +
  theme_minimal()

plot_boxplot3 <- ggplot(df, aes(x = deposit, y = duration, fill = deposit)) +
  geom_boxplot() +
  labs(title = "", x = "Deposit", y = "Duration") +
  theme_minimal()

plot_boxplot4 <- ggplot(df, aes(x = deposit, y = campaign, fill = deposit)) +
  geom_boxplot() +
  labs(title = "", x = "Deposit", y = "Campaign") +
  theme_minimal()

# Organizar los gráficos en una cuadrícula de 2x2
grid.arrange(plot_boxplot1, plot_boxplot2, plot_boxplot3, plot_boxplot4, ncol = 2)

# 
germanbis <- germanbis[, !names(germanbis) %in% "quincena"]
archivo1 <- germanbis
archivo1$depositBin <- ifelse(archivo1$deposit == "Yes", 1, 0)

write.csv(archivo1, file = "archivoSAS.csv", row.names = FALSE)

vardep<-"deposit"
nombres1 <- c("age", "balance", "duration", "campaign", "pdays", "previous", 
            "marital.divorced", "marital.married", "marital.single", "education.primary", 
            "education.secondary", "education.tertiary", "education.unknown", 
            "default.no", "default.yes", "housing.no", "housing.yes", "loan.no", 
            "loan.yes", "poutcome.failure", "poutcome.other", "poutcome.success", 
            "poutcome.unknown", "week_of_month.1", "week_of_month.2", "week_of_month.3", 
            "week_of_month.4", "week_of_month.5", "working.No", "working.Yes", "season.Autumn", 
            "season.Spring", "season.Summer", "season.Winter")

y<-archivo1[,vardep]
x<-archivo1[,nombres1]

# En total, hay 34 variables
length(nombres1)

# --------------------------------------------------------
# INICIO DE LA SELECCIÓN DE VARIABLES
# -------------------------------------------------------

# CON AIC: (1)

full<-glm(deposit~.,data=archivo1,family = binomial(link="logit"))
null<-glm(deposit~1,data=archivo1,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = binomial(link="logit"),trace=FALSE)

vec<-(names(selec1[[1]]))

length(vec)
dput(vec)

# 17 variables
c("(Intercept)", "duration", "poutcome.success", "housing.no", 
"poutcome.unknown", "campaign", "working.No", "education.tertiary", 
"loan.no", "season.Summer", "marital.single", "week_of_month.3", 
"season.Autumn", "education.primary", "balance", "week_of_month.5", 
"marital.divorced", "previous")

# CON BIC (2), ponemos k=log(n) en stepAIC, en este caso n=11162 observaciones, k=9

full<-glm(deposit~.,data=archivo1,family = binomial(link="logit"))
null<-glm(deposit~1,data=archivo1,family = binomial(link="logit"))

selec1<-stepAIC(null,scope=list(upper=full),
                direction="both",family = binomial(link="logit"),trace=FALSE,k=9)

vec<-(names(selec1[[1]]))

length(vec)
dput(vec)

# 15 variables -1 (intercept)
c("duration", "poutcome.success", "housing.no", 
  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
  "season.Autumn", "education.primary", "balance")


# Para controlar que en los paquetes se tome deposit como binaria categorica, la pasamos a factor
archivo1$deposit<-as.factor(archivo1$deposit)
# BORUTA (3)

# También vale como Filter
library(Boruta)
out.boruta <- Boruta(deposit~., data = archivo1)

print(out.boruta)

summary(out.boruta)

sal<-data.frame(out.boruta$finalDecision)

sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
dput(row.names(sal2))

length(dput(row.names(sal2)))

# 30 variables
c("age", "balance", "duration", "campaign", "pdays", "previous", 
  "marital.married", "marital.single", "education.primary", "education.secondary", 
  "education.tertiary", "housing.no", "housing.yes", "loan.no", 
  "loan.yes", "poutcome.failure", "poutcome.other", "poutcome.success", 
  "poutcome.unknown", "week_of_month.1", "week_of_month.2", "week_of_month.3", 
  "week_of_month.4", "week_of_month.5", "working.No", "working.Yes", "season.Autumn", 
  "season.Spring", "season.Summer", "season.Winter")

# PRUEBAS RFE (4) (he puesto size hasta 15 pero se pueden poner más)

control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(x, y, sizes=c(1:15), rfeControl=control)

cosa<-as.data.frame(results$results)

ggplot(cosa,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+ 
  scale_y_continuous(breaks = cosa$Accuracy) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")


# Pongo a partir de 4 variables
cosa2<-cosa[c(4:16),]
ggplot(cosa,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+ 
  scale_y_continuous(breaks = cosa$Accuracy) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")

selecrfe<-results$optVariables[1:7]  

dput(selecrfe)

# Elijo 12 variables
c("duration", "age", "poutcome.success", "season.Summer", "housing.no", 
  "pdays", "housing.yes", "balance", "season.Spring", "season.Autumn", 
  "season.Winter", "week_of_month.3")

# MXM

library(MXM)
# (5)
mmpc2 <- MMPC(vardep, archivo1, max_k = 3, hash = TRUE,
              test = "testIndLogistic")

mmpc2@selectedVars

a<-dput(names(archivo1[,c(mmpc2@selectedVars)]))
length(a)
a

# 12 variables
c("balance", "duration", "campaign", "marital.single", "education.tertiary", 
  "default.no", "housing.yes", "loan.yes", "poutcome.success", 
  "poutcome.unknown", "working.Yes", "season.Autumn"
)

# (6)
SES1 <- SES(vardep, archivo1, max_k = 3, hash = TRUE,
            test = "testIndLogistic")

SES1@selectedVars

dput(names(archivo1[,c(SES1@selectedVars)]))
a<-dput(names(archivo1[,c(SES1@selectedVars)]))
length(a)

# 12 variables y las mismas de antes
c("balance", "duration", "campaign", "marital.single", "education.tertiary", 
  "default.no", "housing.yes", "loan.yes", "poutcome.success", 
  "poutcome.unknown", "working.Yes", "season.Autumn"
)

# (7) Step repetido AIC
# SIEMPRE VAMOS A PONER Yes,No, LA VARIABLE DEPENDIENTE EN LAS FUNCIONES a partir de ahora 
archivo1<-germanbis

lista<-steprepetidobinaria(data=archivo1,vardep=c("deposit"),
                           listconti=nombres1,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# 16 variables
c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
  "campaign", "working.No", "education.tertiary", "loan.no", "season.Summer", 
  "marital.single", "week_of_month.3", "season.Autumn", "education.primary", 
  "balance", "week_of_month.5", "marital.divorced")


# (8) Step repetido BIC
lista<-steprepetidobinaria(data=archivo1,vardep=c("deposit"),
                           listconti=nombres1,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# 14 variables
c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
  "working.No", "campaign", "loan.no", "marital.married", "education.tertiary", 
  "season.Summer", "week_of_month.3", "season.Autumn", "education.primary", 
  "balance")

dput(lista[[2]][[2]])
# 14  variables
c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
  "campaign", "working.No", "education.tertiary", "loan.no", "season.Summer", 
  "marital.single", "week_of_month.3", "season.Autumn", "education.primary", 
  "balance")

# Con selección Random Forest
# Selección de variables Random Forest
rfgrid<-expand.grid(mtry=c(22))
set.seed(12345)
data$aleatoria <- sample(1000, size = nrow(data), replace = TRUE)

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- train(factor(deposit)~.,data=archivo1,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf


# CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseGini),]
tabla

tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla


lista<-dput(rownames(tabla))

set.seed(1234)
vacio2<-data.frame()

for (i in (4:20))
{
  varis<-lista[1:i]
  data2<-data[,c(varis,vardep)]
  rfgrid<-expand.grid(mtry=c(i))
  
  control<-trainControl(method = "cv",number=5,savePredictions = "all") 
  
  rf<- train(factor(deposit)~.,data=data2,
             method="rf",trControl=control,tuneGrid=rfgrid,
             linout = F,ntree=500,nodesize=10,replace=TRUE,
             importance=TRUE)
  
  a<-rf$results$Accuracy
  vacio <- data.frame(Variables = i, Accuracy= a)
  vacio2<-rbind(vacio,vacio2)
  
}
vacio2 <- arrange(vacio2, Variables)

ggplot(vacio2,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+  
  scale_x_continuous(breaks = vacio2$Variables)+
  scale_y_continuous(breaks = vacio2$Accuracy) +  labs(title="")

# Tomo 15 pero es cierto que se podrían tomar sets de más variables

selecrandomforest<-lista[1:15]

dput(selecrandomforest)

c("duration", "age", "season.Summer", "poutcome.success", "season.Autumn", 
  "week_of_month.3", "pdays", "season.Winter", "balance", "housing.yes", 
  "week_of_month.4", "housing.no", "season.Spring", "week_of_month.2", 
  "campaign")

set.seed(12345) 
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11)) 
control<-trainControl(method = "cv",number=10,savePredictions = "all", 
                      classProbs=TRUE)  
rf<- train(factor(deposit)~.,data=archivo1, 
           method="rf",trControl=control,tuneGrid=rfgrid, 
           linout = FALSE,ntree=3000,nodesize=10,replace=TRUE, 
           importance=TRUE) 
rf 

final<-rf$finalModel 
tabla<-as.data.frame(importance(final)) 
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),] 
tabla 
barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla)) 


# ---------------------------------------------------
# Una vez hecha la selección de variables
# ---------------------------------------------------

# Modelos para las variables elegidas
# AQUÍ USO DETACH PUES HAY PAQUETES CON FUNCIONES CON LOS MISMOS nombres1Y DAN PROBLEMAS

detach(package:MXM)
detach(package:Boruta)

data <- germanbis
source ("cruzadas avnnet y log binaria.R")

# Medias 1: STEP AIC con 17 variables
medias1<-cruzadalogistica(data=data,
                          vardep="deposit",listconti= c("duration", "poutcome.success", "housing.no", 
                                                        "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                                        "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                                        "season.Autumn", "education.primary", "balance", "week_of_month.5", 
                                                        "marital.divorced", "previous"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias1$modelo="STEPAIC-17"

# Medias 2: STEP BIC con 15 variables
medias2<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", 
                              "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                              "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                              "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                            listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias2$modelo="STEPBIC-15"

# Medias 3 Boruta con 30 variables
medias3<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("age", "balance", "duration", "campaign", "pdays", "previous", 
                              "marital.married", "marital.single", "education.primary", "education.secondary", 
                              "education.tertiary", "housing.no", "housing.yes", "loan.no", 
                              "loan.yes", "poutcome.failure", "poutcome.other", "poutcome.success", 
                              "poutcome.unknown", "week_of_month.1", "week_of_month.2", "week_of_month.3", 
                              "week_of_month.4", "week_of_month.5", "working.No", "working.Yes", "season.Autumn", 
                              "season.Spring", "season.Summer", "season.Winter"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias3$modelo="Boruta-30"
# Medias 4 con RFE 12 variables
medias4<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "age", "poutcome.success", "season.Summer", "housing.no", 
                              "pdays", "housing.yes", "balance", "season.Spring", "season.Autumn", 
                              "season.Winter", "week_of_month.3")
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias4$modelo="RFE-12"
# Medias 5 MXM con 12 variables
medias5<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("balance", "duration", "campaign", "marital.single", "education.tertiary", 
                              "default.no", "housing.yes", "loan.yes", "poutcome.success", 
                              "poutcome.unknown", "working.Yes", "season.Autumn"
                            )
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias5$modelo="MXM-12"
# Medias 6 StepRep1 con 16 variables
medias6<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
                              "campaign", "working.No", "education.tertiary", "loan.no", "season.Summer", 
                              "marital.single", "week_of_month.3", "season.Autumn", "education.primary", 
                              "balance", "week_of_month.5", "marital.divorced")
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias6$modelo="SES-16"

# Medias 7: Steprep AIC con 16 
medias7<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
  "campaign", "working.No", "education.tertiary", "loan.no", "season.Summer", 
  "marital.single", "week_of_month.3", "season.Autumn", "education.primary", 
  "balance", "week_of_month.5", "marital.divorced")
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias7$modelo="STEPrep-AIC-16"

# Medias 7: Steprep BIC con 14 
medias8<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", "poutcome.unknown", 
                              "campaign", "working.No", "education.tertiary", "loan.no", "season.Summer", 
                              "marital.single", "week_of_month.3", "season.Autumn", "education.primary", 
                              "balance")
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias8$modelo="STEPrep-BIC2-14"

# Boxplot resultados
union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8)

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")

medias11<-cruzadalogistica(data=data,
                           vardep="deposit",listconti=
                             c("duration", "age", "season.Summer", "poutcome.success", "season.Autumn", 
                               "week_of_month.3", "pdays", "season.Winter", "balance", "housing.yes", 
                               "week_of_month.4", "housing.no", "season.Spring", "week_of_month.2", 
                               "campaign")
                           ,
                           listclass=c(""),grupos=4,sinicio=1234,repe=5)

medias11$modelo="RF-Selec-15"

c("duration", "poutcome.success", "housing.no", 
  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
  "season.Autumn", "education.primary", "balance", "week_of_month.5")

# Boxplot resultados
union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8, medias11)

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")



# ---------------------------------------------------
# Modelos Tema 6: redes neuronales
# ---------------------------------------------------

# Se comparará con la logística 
medias2$modelo="Logistica"

# Tuneo de la red con la mejor selección de variables
vardep="deposit"
variables<-c("duration", "poutcome.success", "housing.no", 
             "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
             "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
             "season.Autumn", "education.primary", "balance", "week_of_month.5")

data2<-data[,c(variables,vardep)]

# Calculos:
#   en realidad hay solo 300 obs de la categoria objetivo. y 6 variables input.
# Si consideramos 10 obs como minimo por cada obs de la clase objetivo:
#   30 parametros maximo, k=6, como mucho 5 nodos.

# En este caso, como mucho 25

control<-trainControl(method = "cv",
                      number=5,savePredictions = "all") 

set.seed(1234)
nnetgrid <-  expand.grid(size=c(5, 10, 15, 20),decay=c(0.01,0.1, 0.01),bag=F)

completo<-data.frame()
listaiter<-c(50,100,200,500,1000,2000)

for (iter in listaiter)
{
  rednnet<- train(deposit~.,
                  data=data2,
                  method="avNNet",linout = FALSE,maxit=iter,
                  trControl=control,repeats=5,tuneGrid=nnetgrid,trace=F)
  # Añado la columna del parametro de iteraciones
  rednnet$results$itera<-iter
  # Voy incorporando los resultados a completo
  completo<-rbind(completo,rednnet$results)
  
  
}

completo<-completo[order(completo$Accuracy),]

ggplot(completo, aes(x=factor(itera), y=Accuracy, 
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# Una vez salga el ggplot, elijo los parametros 

# Las diferencias son muy pequeñas por mucho que variemos (probar a cambiar la semilla)
# Cuanto mas accuracy mejor

# itera=2000, size=5, decay=0.1

medias9 <- cruzadaavnnetbin(data=archivo1,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", 
                              "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                              "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                              "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=6,repeticiones=5,itera=500,
                          size=c(5),decay=c(0.01))

medias9$modelo="Red-5n"
# Me quedo con red-5n 

medias10 <- cruzadaavnnetbin(data=data2,
                            vardep="deposit",listconti=
                              c("duration", "poutcome.success", "housing.no", 
                                "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                            listclass=c(""),grupos=4,sinicio=1234,repe=6,repeticiones=5,itera=200,
                            size=c(10),decay=c(0.01))

medias10$modelo="Red-10n"

union1<-rbind(medias2, medias9, medias10)

par(cex.axis=0.8)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")


par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")

# Metiendo logística con RF
medias99<-cruzadalogistica(data=archivo1,
                           vardep="deposit",listconti=
                             c("duration", "age", "season.Summer", "poutcome.success", "season.Autumn", 
                               "week_of_month.3", "pdays", "season.Winter", "balance", "housing.yes", 
                               "week_of_month.4", "housing.no", "season.Spring", "week_of_month.2", 
                               "campaign"),
                           listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias99$modelo="Log-RF"

union1<-rbind(medias2, medias9, medias10, medias99)

par(cex.axis=0.8)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")
# ---------------------------------------------------------------------
# Modelos Tema 7
# Random Forest, bagging, xbg, catboost
# ---------------------------------------------------------------------

# Selección de variables Random Forest
rfgrid<-expand.grid(mtry=c(22))
set.seed(12345)
data$aleatoria <- sample(1000, size = nrow(data), replace = TRUE)

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

rf<- train(factor(deposit)~.,data=archivo1,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf


# CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseGini),]
tabla

tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla


lista<-dput(rownames(tabla))

set.seed(1234)
vacio2<-data.frame()

for (i in (4:20))
{
  varis<-lista[1:i]
  data2<-data[,c(varis,vardep)]
  rfgrid<-expand.grid(mtry=c(i))
  
  control<-trainControl(method = "cv",number=5,savePredictions = "all") 
  
  rf<- train(factor(deposit)~.,data=data2,
             method="rf",trControl=control,tuneGrid=rfgrid,
             linout = F,ntree=500,nodesize=10,replace=TRUE,
             importance=TRUE)
  
  a<-rf$results$Accuracy
  vacio <- data.frame(Variables = i, Accuracy= a)
  vacio2<-rbind(vacio,vacio2)
  
}
vacio2 <- arrange(vacio2, Variables)

ggplot(vacio2,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+  
  scale_x_continuous(breaks = vacio2$Variables)+
  scale_y_continuous(breaks = vacio2$Accuracy) +  labs(title="")

# Tomo 15 pero es cierto que se podrían tomar sets de más variables

selecrandomforest<-lista[1:15]

dput(selecrandomforest)

c("duration", "age", "season.Summer", "poutcome.success", "season.Autumn", 
  "week_of_month.3", "pdays", "season.Winter", "balance", "housing.yes", 
  "week_of_month.4", "housing.no", "season.Spring", "week_of_month.2", 
  "campaign")


medias11<-cruzadalogistica(data=data,
                          vardep="deposit",listconti=
                            c("duration", "age", "season.Summer", "poutcome.success", "season.Autumn", 
                              "week_of_month.3", "pdays", "season.Winter", "balance", "housing.yes", 
                              "week_of_month.4", "housing.no", "season.Spring", "week_of_month.2", 
                              "campaign")
                          ,
                          listclass=c(""),grupos=4,sinicio=1234,repe=5)

medias11$modelo="RF-Selec-15"

# Boxplot resultados
union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8, medias9, medias10, medias11)

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")

par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")

# -------------------------------------

# BAGGING 
set.seed(123456)
rfgrid<-expand.grid(mtry=c(15))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                     classProbs=TRUE) 

rf<- train(data=archivo1,
           factor(deposit)~duration + poutcome.success + housing.no + 
             poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
             marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=5000,nodesize=10,replace=TRUE)

rf

# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest

library(randomForest)
set.seed(12345)

# A veces da error por el parallel
# stopCluster(cluster)

rfbis<-randomForest(factor(deposit)~duration + poutcome.success + housing.no + 
                      poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
                      marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
                    data=archivo1,
                    mtry=15,ntree=1000,sampsize=300,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

predictions <- predict(rfbis, archivo1)
conf_matrix <- table(Actual = archivo1$deposit, Predicted = predictions)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
n <- sum(conf_matrix)
nc <- ncol(conf_matrix)
rowsums <- rowSums(conf_matrix)
colsums <- colSums(conf_matrix)
expected <- rowsums %*% t(colsums) / n
po <- sum(diag(conf_matrix)) / n
pe <- sum(diag(expected)) / n
kappa <- (po - pe) / (1 - pe)
cat("Accuracy:", accuracy, "\n")
cat("Kappa:", kappa, "\n")


# Se prueba un solo arbol para ver el error
arbolgrid <-expand.grid(cp=c(0)) 
arbol<- train(factor(deposit)~duration + poutcome.success + housing.no + 
                poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
                marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5, 
              data=archivo1, 
              method="rpart",trControl=control,tuneGrid=arbolgrid, minbucket=10)
arbol 


# A continuación, se plotea el error:
union1<-rbind(medias2, medias9, medias10)

par(cex.axis=0.8)
boxplot(data=union1,col="cyan",tasa~modelo,main="TASA DE FALLOS")


par(cex.axis=0.7)
boxplot(data=union1,col="cyan",auc~modelo,main="AUC")


medias3<-cruzadaarbolbin(data=archivo1,
                         vardep="deposit",listconti=c("duration", "poutcome.success", "housing.no", 
                                                      "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                                      "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                                      "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                         listclass=c(""),grupos=4,sinicio=1234,repe=5,
                         cp=c(0),minbucket =5)

medias3$modelo="arbol"


medias4<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=5,nodesize=20,
                      mtry=15,ntree=1000,replace=TRUE)

medias4$modelo="bagging"


union1<-rbind(medias2, medias9, medias10,medias3,medias4)

par(cex.axis=0.9)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="cyan")
boxplot(data=union1,auc~modelo,main="AUC",col="cyan")

# Probamos variaciones sobre el tamaño muestral en bagging
# con 10 grupos de CV, maximo 415 sampsize
# En mi caso 11000*0.9

medias1<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE,sampsize=50)

medias1$modelo="b-50"


medias2<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE,sampsize=500)

medias2$modelo="b-500"

medias3<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE,sampsize=1500)


medias3$modelo="b-1500"

medias4<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE,sampsize=3000)

medias4$modelo="b-3000"

medias5<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE,sampsize=5000)


medias5$modelo="b-5000"

medias6<-cruzadarfbin(data=archivo1, vardep="deposit",
                      listconti=c("duration", "poutcome.success", "housing.no", 
                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=20,nodesize=10,
                      mtry=15,ntree=1000,replace=TRUE)

# En este modelo al no usar sampsize usa el que tiene por defecto 
# caret, que son todas las observaciones con reemplazamiento (415) 

medias6$modelo="b-BASE"

# Medias 2: STEP BIC con 15 variables
mediasLog<-cruzadalogistica(data=archivo1,
                          vardep="deposit",listconti=
                            c("duration", "poutcome.success", "housing.no", 
                              "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                              "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                              "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                          listclass=c(""),grupos=10,sinicio=1234,repe=10)

mediasLog$modelo="Logistica"

union1<-rbind(mediasLog,medias1,medias2,medias3,medias4,medias5,medias6)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="cyan")

uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=1)
boxplot(data=uni,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=uni,auc~modelo,main="AUC",col="cyan")

# ---------------------------------------------------------------------
# Random Forest
# ---------------------------------------------------------------------

set.seed(12345)
rfgrid<-expand.grid(mtry=c(9,10,11,12,13,14,15,16,16,18,19,20))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(deposit)~.,data=archivo1,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla

nombres = rownames(tabla)
barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))
barplot(tabla$MeanDecreaseAccuracy, names.arg = nombres, las = 1, cex.names = 0.8)
axis(side = 1, at = barplot(tabla$MeanDecreaseAccuracy), labels = nombres, las = 2, cex.axis = 0.8)

# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest
# Elegir el mtry seleccionado antes

set.seed(12345)

rfbis<-randomForest(factor(deposit)~.,
                    data=archivo1,
                    mtry=10,ntree=3000,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

listconti<-c("duration", "age", "season.Summer",
             "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring")
paste(listconti,collapse="+")

set.seed(12345)
rfgrid<-expand.grid(mtry=c(8,9,10,11,12))

control<-trainControl(method = "cv",number=10,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(deposit)~duration+age+season.Summer+season.Autumn+poutcome.success+season.Winter+pdays+week_of_month.3+balance+season.Spring,data=archivo1,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=500,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# Se prueban diferentes sampsize
mediasz8<-cruzadarfbin(data=archivo1, 
                      vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                   "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=10,nodesize=10,
                      mtry=10,ntree=800,replace=TRUE,sampsize=150)

mediasz8$modelo="rf-150"

mediasz9<-cruzadarfbin(data=archivo1, 
                      vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                   "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                      listclass=c(""),
                      grupos=10,sinicio=1234,repe=10,nodesize=10,
                      mtry=10,ntree=800,replace=TRUE,sampsize=150)

mediasz9$modelo="rf-500"

mediasz10<-cruzadarfbin(data=archivo1, 
                       vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                    "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                       listclass=c(""),
                       grupos=10,sinicio=1234,repe=10,nodesize=10,
                       mtry=10,ntree=800,replace=TRUE,sampsize=250)

mediasz10$modelo="rf-2500"

mediasz11<-cruzadarfbin(data=archivo1, 
                        vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                     "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,nodesize=10,
                        mtry=10,ntree=800,replace=TRUE,sampsize=5000)

mediasz11$modelo="rf-5000"

mediasz12<-cruzadarfbin(data=archivo1, 
                        vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                     "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,nodesize=10,
                        mtry=10,ntree=800,replace=TRUE,sampsize=8000)

mediasz12$modelo="rf-8000"

mediasz13<-cruzadarfbin(data=archivo1, 
                        vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                     "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,nodesize=10,
                        mtry=10,ntree=800,replace=TRUE,sampsize=3000)

mediasz13$modelo="rf-3000"

mediasz14<-cruzadarfbin(data=archivo1, 
                        vardep="deposit",listconti=c("duration", "age", "season.Summer",
                                                     "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,nodesize=10,
                        mtry=10,ntree=800,replace=TRUE,sampsize=6000)

mediasz14$modelo="rf-6000"

medias9 <- cruzadaavnnetbin(data=data2,
                            vardep="deposit",listconti=
                              c("duration", "poutcome.success", "housing.no", 
                                "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                            listclass=c(""),grupos=4,sinicio=1234,repe=6,repeticiones=5,itera=500,
                            size=c(5),decay=c(0.01))

medias9$modelo="Red-5n"

union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz8,mediasz9,mediasz10,mediasz11,mediasz12, mediasz13, mediasz14)
uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=1)
boxplot(data=uni,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=uni,auc~modelo,main="AUC",col="cyan")

# ---------------------------------------------------------------------
# Gradient Boosting
# ---------------------------------------------------------------------

set.seed(12345)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(5,10,20),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,data=archivo1,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm

plot(gbm)

gbmgrid<-expand.grid(shrinkage=c(0.05, 0.1),
                     n.minobsinnode=c(10),
                     n.trees=c(500,800,1000,1200, 1500, 2000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=10,savePredictions = "all",
                      classProbs=TRUE) 

gbm<- train(factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,data=archivo1,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

gbm
plot(gbm)

# IMPORTANCIA DE VARIABLES
par(cex=1.3)
summary(gbm)

tabla<-summary(gbm)
par(cex=1.2,las=2)
barplot(tabla$rel.inf,names.arg=row.names(tabla))

par(cex=1.0, cex.names = 0.5, las=1)

barplot(tabla$rel.inf,
        horiz = FALSE,  # Gráfico horizontal
        names.arg = row.names(tabla),  # Nombres de las variables
        main = "Importancia de Variables",  # Título del gráfico
        xlab = "Importancia Relativa")  # Etiqueta del eje x
text(x = par("usr")[1] - 0.1, y = par("usr")[3], labels = rownames(tabla), srt = 90, adj = 1, cex = 0.7)

source ("cruzada gbm binaria.R")
mediasGB <- cruzadagbmbin(data=archivo1, vardep="deposit",
                       listconti=c("duration", "poutcome.success", "housing.no", 
                                   "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                   "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                   "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=10,
                       n.minobsinnode=10,shrinkage=0.05,n.trees=1200,interaction.depth=2)

mediasGB$modelo="gbm"

# Comparativa con los mejores modelos 
union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz13, mediasGB)

par(cex.axis=0.8,las=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=union1,auc~modelo,main="AUC",col="cyan")

#union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz8,mediasz9,mediasz10,mediasz11,mediasz12, mediasz13, mediasz14)
uni<-union1
uni$modelo <- with(uni,
                   reorder(modelo,tasa, mean))
par(cex.axis=0.8,las=1)
boxplot(data=uni,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=uni,auc~modelo,main="AUC",col="cyan")

# ---------------------------------------------------------------------
# XGB
# ---------------------------------------------------------------------

set.seed(12345)

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(deposit)~.,data=archivo1,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)

# Early stopping
xgbmgrid<-expand.grid(eta=c(0.05),
                      min_child_weight=c(10),
                      nrounds=c(100, 200, 300,400,450, 500, 550, 600),
                      max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(deposit)~.,data=archivo1,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)

set.seed(1369)
control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(deposit)~.,data=archivo1,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

plot(xgbm)

varImp(xgbm)
plot(varImp(xgbm))

# Con las variables y parámetros:

xgbmgrid<-expand.grid(
  min_child_weight=c(5,10,20),
  eta=c(0.1,0.05,0.03,0.01,0.001),
  nrounds=c(100,500,1000,5000),
  max_depth=6,gamma=0,colsample_bytree=1,subsample=1)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

xgbm<- train(factor(deposit)~duration+poutcome.success+housing.no+pdays+balance+age+season.Summer+campaign+week_of_month.3,
             data=archivo1,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)

xgbm

plot(xgbm)

# Se evalúa el modelo para la mejor selección y para la xgb
source ("cruzada xgboost binaria.R")

mediasXGB1<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
                        listconti=c("duration", "age", "season.Summer",
                                    "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,
                        min_child_weight=10,eta=0.05,nrounds=423,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)

mediasXGB1$modelo="xgbm"

# La otra opción recomendada por caret

mediasXGB2<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
                        listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=10,
                        min_child_weight=20,eta=0.01,nrounds=1000,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)

mediasXGB2$modelo="xgbm2"


union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz13, mediasGB, mediasXGB1, mediasXGB2)

par(cex.axis=0.8,las=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=union1,auc~modelo,main="AUC",col="cyan")

# Para reducir la varianza se sortean variables u observaciones
mediasXBGvar<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
                        listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                        listclass=c(""),
                        grupos=10,sinicio=1234,repe=5,
                        min_child_weight=20,eta=0.10,nrounds=200,max_depth=6,
                        gamma=0,colsample_bytree=0.8,subsample=1,
                        alpha=0,lambda=0)

mediasXBGvar$modelo="xgbmvar"

# Repitiendo con los parámetros afinados
mediasXBGvar2<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
                             listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                             listclass=c(""),
                             grupos=10,sinicio=1234,repe=10,
                             min_child_weight=20,eta=0.010,nrounds=1000,max_depth=6,
                             gamma=0,colsample_bytree=0.8,subsample=1,
                             alpha=0,lambda=0)

mediasXBGvar2$modelo="xgbmvar2"

mediasXGBobs<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
                         listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                         listclass=c(""),
                         grupos=10,sinicio=1234,repe=5,
                         min_child_weight=20,eta=0.10,nrounds=200,max_depth=6,
                         gamma=0,colsample_bytree=1,subsample=0.8,
                         alpha=0,lambda=0)

mediasXGBobs$modelo="xgbmobs"

union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz13, mediasGB, mediasXGB1, mediasXGB2, mediasXBGvar, mediasXBGvar2, mediasXGBobs)

union1<-rbind(mediasXGB1, mediasXGB2, mediasXBGvar, mediasXBGvar2, mediasXGBobs)
par(cex.axis=0.8,las=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=union1,auc~modelo,main="AUC",col="cyan")

# ---------------------------------------------------------------------
# Catboost
# ---------------------------------------------------------------------

x<-archivo1[,c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn")]
y<-archivo1[,c("deposit")]


fit_control <- trainControl(method = "cv",
                            number = 10)
set.seed(12345)
catgrid<-expand.grid(
  learning_rate=c(0.01,0.05,0.1),
  iterations=c(50,100, 150, 200),
  depth=6,rsm=c(0.8),l2_leaf_reg=c(1,5,10), border_count=c(100,200, 300))


report <- train(x,y,
                method=catboost.caret, 
                logging_level = 'Verbose', preProc = NULL,
                tuneGrid = catgrid, trControl = fit_control,  min_data_in_leaf=c(10),subsample=1)

print(report$results)

preditest<-report$pred


print(report)

plot(report)
source("cruzada catboost binaria.R")

mediasCat<-cruzadacatboostbin(data=archivo1, vardep="deposit",
                            listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                            listclass=c(""),
                            grupos=4,sinicio=1234,repe=10,
                            learning_rate=0.05,
                            iterations=200,
                            depth=6,rsm=0.8,l2_leaf_reg=10, border_count=200,
                            min_data_in_leaf=10,subsample=1)

mediasCat$modelo="catboost"

union1<-rbind(medias9, medias10, mediasLog, medias3, mediasz13, mediasGB, mediasXGB2, mediasXBGvar, mediasCat)

par(cex.axis=0.8,las=1)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=union1,auc~modelo,main="AUC",col="cyan")

# --------------
# Tema 8: SVM (lineal, polinomial y RBF)
# --------------

install.packages ("kernlab")
library(kernlab)
# ---------------------------------------------------------------------
# SVM LINEAL: SOLO PARÁMETRO C
# ---------------------------------------------------------------------

c("duration", "poutcome.success", "housing.no", 
  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
  "season.Autumn", "education.primary", "balance", "week_of_month.5")

set.seed(12345)
SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=archivo1,factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)

# C  Accuracy     Kappa AccuracySD    KappaSD
# 1  0.01 0.7992272 0.5958683 0.00957593 0.01922311
# 2  0.05 0.7989584 0.5955085 0.01004838 0.02011149
# 3  0.10 0.7994957 0.5965885 0.01098751 0.02203516
# 4  0.20 0.7995852 0.5967814 0.01138233 0.02279795
# 5  0.50 0.8000332 0.5976947 0.01104305 0.02210069
# 6  1.00 0.7997646 0.5971582 0.01070023 0.02140147
# 7  2.00 0.8002126 0.5980793 0.01088930 0.02176431
# 8  5.00 0.8000334 0.5977186 0.01069623 0.02138200
# 9 10.00 0.8000334 0.5977186 0.01069623 0.02138200

# La mayor precisión se obtiene para C = 2

# Rehago el grid para observar mejor el intervalo de C entre 0 y 0.6
SVMgrid<-expand.grid(C=c(1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(data=archivo1,factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)

# ---------------------------------------------------------------------
# SVM Polinomial: PARÁMETROS C, degree, scale
# ---------------------------------------------------------------------

SVMgrid <- expand.grid(C=c(0.2,0.5,1,2.5,5,7.5, 10),
                     degree=c(2),scale=c(0.1,0.5,1,2,5))

control <- trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM <- train(data=archivo1,factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results

# Representaciones
dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# ---------------------------------------------------------------------
# SVM RBF: PARÁMETROS C, degree, scale
# ---------------------------------------------------------------------


SVMgrid<-expand.grid(C=c(0.5,1,2,5,10,30, 40),
                     sigma=c(0.0001,0.005,0.01,0.05, 0.1))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(data=archivo1,factor(deposit)~duration + poutcome.success + housing.no + 
              poutcome.unknown + campaign + working.No + education.tertiary + loan.no + season.Summer + 
              marital.single + week_of_month.3 + season.Autumn + education.primary + balance + week_of_month.5,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)


dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3) + 
theme(
  axis.text.x = element_text(size = 12),  # Tamaño de etiquetas del eje x
  axis.text.y = element_text(size = 12)   # Tamaño de etiquetas del eje y
)

source ("cruzada SVM binaria lineal.R")
source ("cruzada SVM binaria polinomial.R")
source ("cruzada SVM binaria RBF.R")
library(caret)

# Creo boxplot con los modelos, como de algunos hay varios buenos, se comparan antes entre ellos.

mediasSVM<-cruzadaSVMbin(data=archivo1, vardep="deposit",
                       listconti=c("duration", "poutcome.success", "housing.no", 
                                   "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                   "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                   "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                       listclass=c(""),
                       grupos=10,sinicio=1234,repe=5,C=2.1)

mediasSVM$modelo="SVM"


mediasSVMPoly<-cruzadaSVMbinPoly(data=archivo1, vardep="deposit",
                           listconti=c("duration", "poutcome.success", "housing.no", 
                                       "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                       "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                       "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                           listclass=c(""),
                           grupos=10,sinicio=1234,repe=5,C=0.5,degree=2,scale=2)

mediasSVMPoly$modelo="SVMPoly"


mediasSVMRBF<-cruzadaSVMbinRBF(data=archivo1, vardep="deposit",
                           listconti=c("duration", "poutcome.success", "housing.no", 
                                       "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                       "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                       "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                           listclass=c(""),
                           grupos=10,sinicio=1234,repe=5,
                           C=2,sigma=0.05)

mediasSVMRBF$modelo="SVMRBF"

mediasSVMRBF2<-cruzadaSVMbinRBF(data=archivo1, vardep="deposit",
                               listconti=c("duration", "poutcome.success", "housing.no", 
                                           "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
                                           "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
                                           "season.Autumn", "education.primary", "balance", "week_of_month.5"),
                               listclass=c(""),
                               grupos=10,sinicio=1234,repe=5,
                               C=5,sigma=0.01)

mediasSVMRBF2$modelo="SVMRBF2"

mediasRed5 <- medias9
mediasRed10 <- medias10
mediasRF <- mediasz13

union3<-rbind(mediasLog, mediasXGB2, mediasXBGvar, mediasCat, mediasGB, mediasRed5, mediasRed10, mediasSVM, mediasSVMPoly, mediasSVMRBF, mediasSVMRBF2, mediasz13)
par(cex.axis=0.9,las=1)
boxplot(data=union3,tasa~modelo,main="TASA FALLOS", col="cyan")
boxplot(data=union3,auc~modelo,main="AUC",col="cyan")

# ---------------------------------------------------------------------
# Tema 9: Ensamblado
# ---------------------------------------------------------------------

# **************************************
# IMPORTANTE: AQUÍ HAY QUE DECIDIR ANTES LOS PARÁMETROS A UTILIZAR
# EN CADA ALGORITMO, NO VALE GRID
# Importante, la dependiente en letras Yes, No
# Preparación de archivo, variables y CV. 
# Esto se cambia para cada archivo.
# Necesario haber cambiado la var dep a Yes,No.
# **************************************

# LEER LAS CRUZADAS DE ENSAMBLADO, SON LIGERAMENTE DIFERENTES
# A LAS UTILIZADAS ANTERIORMENTE AUNQUE SE LLAMAN IGUAL
library(parallel)
library(doParallel)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing

source("cruzadas ensamblado binaria fuente.R")

#archivo<-saheartbis
archivo <- archivo1

vardep<-"deposit"
listconti<-c("duration", "poutcome.success", "housing.no", 
             "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
             "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
             "season.Autumn", "education.primary", "balance", "week_of_month.5")
listclass<-c("")
grupos<-4
sinicio<-1234
repe<-50

# APLICACIÓN CRUZADAS PARA ENSAMBLAR

medias1<-cruzadalogistica(data=archivo1,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"Logistica"
predi1<-as.data.frame(medias1[2])
predi1$logi<-predi1$Yes

# listclass=c(""),grupos=4,sinicio=1234,repe=6,repeticiones=5,itera=500,
# size=c(5),decay=c(0.01))

medias2<-cruzadaavnnetbin(data=archivo1,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                          size=c(5),decay=c(0.01),repeticiones=5,itera=500)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"avnnet5"
predi2<-as.data.frame(medias2[2])
predi2$avnnet5<-predi2$Yes

medias6<-cruzadaavnnetbin(data=archivo1,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                          size=c(10),decay=c(0.01),repeticiones=5,itera=200)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"avnnet10"
predi6<-as.data.frame(medias6[2])
predi6$avnnet10<-predi6$Yes


# grupos=10,sinicio=1234,repe=20,nodesize=10,
# mtry=15,ntree=1000,replace=TRUE,sampsize=3000)
# mediasz13<-cruzadarfbin(data=archivo1, 
#                         vardep="deposit",listconti=c("duration", "age", "season.Summer",
#                                                      "season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
#                         listclass=c(""),
#                         grupos=10,sinicio=1234,repe=10,nodesize=10,
#                         mtry=10,ntree=800,replace=TRUE,sampsize=3000)

medias3<-cruzadarfbin(data=archivo1,
                      vardep=vardep,listconti=c("duration", "age", "season.Summer","season.Autumn",  "poutcome.success","season.Winter","pdays", "week_of_month.3", "balance", "season.Spring"),
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      mtry=10,ntree=800,nodesize=10,replace=TRUE)


medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias3[2])
predi3$rf<-predi3$Yes

# listclass=c(""),
# grupos=4,sinicio=1234,repe=10,
# n.minobsinnode=10,shrinkage=0.05,n.trees=1200,interaction.depth=2)

medias4<-cruzadagbmbin(data=archivo1,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       n.minobsinnode=10,shrinkage=0.05,n.trees=1200,interaction.depth=2)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"xgbm"
predi4<-as.data.frame(medias4[2])
predi4$xgbm<-predi4$Yes

# grupos=10,sinicio=1234,repe=10,
# min_child_weight=20,eta=0.01,nrounds=1000,max_depth=6,
# gamma=0,colsample_bytree=1,subsample=1,

# mediasXGB2<-cruzadaxgbmbin(data=archivo1, vardep="deposit",
#                            listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
#                            listclass=c(""),
#                            grupos=10,sinicio=1234,repe=10,
#                            min_child_weight=20,eta=0.01,nrounds=1000,max_depth=6,
#                            gamma=0,colsample_bytree=1,subsample=1,
#                            alpha=0,lambda=0)
# 
# mediasXGB2$modelo="xgbm2"

medias5<-cruzadaxgbmbin(data=archivo1,
                        vardep=vardep,listconti=c("duration","poutcome.success","housing.no","pdays","balance","age","season.Summer","campaign","week_of_month.3", "season.Spring", "season.Autumn"),
                        listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                        min_child_weight=20,eta=0.01,nrounds=100,max_depth=6,
                        gamma=0,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)


medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"gbm"
predi5<-as.data.frame(medias5[2])
predi5$gbm<-predi5$Yes

# Hasta aquí funciona, con SVM hay que mergear filas comunes
# Pero no hay forma
# Por lo que no se incluyen estos modelos
# set.seed(1234)
# 
# medias6<-cruzadaSVMbin(data=archivo1,
#                        vardep=vardep,listconti=c("duration", "poutcome.success", "housing.no", 
#                                                  "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
#                                                  "loan.no", "season.Summer", "marital.single", "week_of_month.3", 
#                                                  "season.Autumn", "education.primary", "balance", "week_of_month.5"),
#                        listclass=listclass,grupos=grupos,
#                        sinicio=sinicio,repe=repe,C=2.2)
# 
# medias6bis<-as.data.frame(medias6[1])
# medias6bis$modelo<-"svmLinear"
# predi6<-as.data.frame(medias6[2])
# predi6$svmLinear<-predi6$Yes

# cruzadaSVMbinPoly(data=archivo1, vardep="deposit",
#                   grupos=10,sinicio=1234,repe=5,C=0.5,degree=2,scale=2)

# medias7<-cruzadaSVMbinPoly(data=archivo,
#                           vardep=vardep,listconti=listconti,
#                           listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
#                           C=0.5,degree=2, scale = 2)
# 
# medias7bis<-as.data.frame(medias7[1])
# medias7bis$modelo<-"svmPoly"
# predi7<-as.data.frame(medias7[2])
# predi7$svmPoly<-predi7$Yes

# grupos=10,sinicio=1234,repe=5,
# C=5,sigma=0.01)

# medias8<-cruzadaSVMbinRBF(data=germanbis,
#                           vardep=vardep,listconti=listconti,
#                           listclass=listclass,grupos=grupos,
#                           sinicio=sinicio,repe=repe,
#                           C=5,sigma=0.01)
# 
# medias8bis<-as.data.frame(medias8[1])
# medias8bis$modelo<-"svmRadial"
# predi8<-as.data.frame(medias8[2])
# predi8$svmRadial<-predi8$Yes

union1<-rbind(medias1bis,medias2bis,
              medias3bis,medias4bis,medias5bis, medias6bis)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,col="cyan",main='TASA FALLOS')
boxplot(data=union1,auc~modelo,col="cyan",main='AUC')

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5, predi6)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados, cambiar al gusto

unipredi$predi9<-(unipredi$logi+unipredi$avnnet5)/2
unipredi$predi10<-(unipredi$logi+unipredi$rf)/2
unipredi$predi11<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi12<-(unipredi$logi+unipredi$xgbm)/2
unipredi$predi13<-(unipredi$logi+unipredi$avnnet10)/2
unipredi$predi14<-(0.5*unipredi$logi+1.5*unipredi$avnnet5)/2
unipredi$predi15 <- (0.7*unipredi$logi + 0.3*unipredi$rf)

unipredi$predi16<-(unipredi$avnnet5+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet5+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet5+unipredi$xgbm)/2
unipredi$predi19<-(unipredi$avnnet5+unipredi$avnnet10)/2
unipredi$predi20 <- (0.6*unipredi$logi + 0.4*unipredi$avnnet5)
unipredi$predi21 <- (0.7*unipredi$logi + 0.3*unipredi$rf)

unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi23<-(unipredi$rf+unipredi$xgbm)/2
unipredi$predi24<-(unipredi$rf+unipredi$avnnet10)/2
unipredi$predi25 <- (0.4*unipredi$logi + 0.6*unipredi$avnnet10)
unipredi$predi26<-(0.8*unipredi$gbm+1.2*unipredi$xgbm)/2

unipredi$predi27<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi28<-(unipredi$gbm+unipredi$avnnet10)/2

unipredi$predi31<-(unipredi$logi+unipredi$avnnet5+unipredi$rf)/3
unipredi$predi32<-(unipredi$logi+unipredi$avnnet5+unipredi$gbm)/3
unipredi$predi33<-(unipredi$logi+unipredi$avnnet5+unipredi$xgbm)/3
unipredi$predi34<-(unipredi$logi+unipredi$avnnet5+unipredi$avnnet10)/3
unipredi$predi35 <- (0.4*unipredi$logi + 0.3*unipredi$avnnet5 + 0.3*unipredi$rf)
unipredi$predi36 <- (0.4*unipredi$logi + 0.3*unipredi$avnnet5 + 0.3*unipredi$gbm)

unipredi$predi37<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi38<-(unipredi$logi+unipredi$rf+unipredi$xgbm)/3
unipredi$predi39<-(unipredi$logi+unipredi$rf+unipredi$avnnet10)/3

unipredi$predi40 <- (0.4*unipredi$logi + 0.3*unipredi$gbm + 0.3*unipredi$xgbm)
unipredi$predi41 <- (0.5*unipredi$logi + 0.25*unipredi$gbm + 0.25*unipredi$xgbm)

unipredi$predi42<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi43<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi44<-(unipredi$logi+unipredi$gbm+unipredi$avnnet10)/3

unipredi$predi45 <- (0.4*unipredi$logi + 0.3*unipredi$xgbm + 0.3*unipredi$avnnet10)
unipredi$predi46 <- (0.4*unipredi$logi + 0.3*unipredi$xgbm + 0.3*unipredi$avnnet5)
unipredi$predi47<-(unipredi$logi+unipredi$xgbm+unipredi$avnnet10)/3

unipredi$predi48 <- (0.5*unipredi$rf + 0.3*unipredi$gbm + 0.2*unipredi$avnnet10)
unipredi$predi49 <- (0.4*unipredi$rf + 0.4*unipredi$gbm + 0.2*unipredi$avnnet5)

unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$avnnet10)/3
unipredi$predi51 <- (0.5*unipredi$rf + 0.2*unipredi$avnnet5 + 0.3*unipredi$gbm)
unipredi$predi52 <- (0.3*unipredi$rf + 0.4*unipredi$avnnet5 + 0.3*unipredi$xgbm)
unipredi$predi53<-(unipredi$rf+unipredi$xgbm+unipredi$avnnet10)/3
unipredi$predi54 <- (0.3*unipredi$rf + 0.4*unipredi$xgbm + 0.3*unipredi$avnnet5)
unipredi$predi55 <- (0.4*unipredi$rf + 0.3*unipredi$avnnet10 + 0.3*unipredi$avnnet5)

unipredi$predi56<-(unipredi$rf+unipredi$avnnet5+unipredi$gbm)/3
unipredi$predi57<-(unipredi$rf+unipredi$avnnet5+unipredi$xgbm)/3
unipredi$predi58<-(unipredi$rf+unipredi$avnnet5+unipredi$avnnet10)/3

unipredi$predi61<-(unipredi$avnnet5+unipredi$gbm+unipredi$avnnet10)/3
unipredi$predi62 <- (0.3*unipredi$logi + 0.2*unipredi$rf + 0.3*unipredi$gbm + 0.2*unipredi$avnnet5)
unipredi$predi63 <- (0.4*unipredi$logi + 0.1*unipredi$rf + 0.2*unipredi$xgbm + 0.3*unipredi$avnnet5)
unipredi$predi64<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$avnnet5)/4
unipredi$predi65<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet5)/4
unipredi$predi66<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet5)/4

unipredi$predi67<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet5+unipredi$avnnet10)/5
unipredi$predi68 <- (0.3*unipredi$logi + 0.2*unipredi$rf + 0.2*unipredi$xgbm + 0.15*unipredi$avnnet5 + 0.15*unipredi$avnnet10)
unipredi$predi69 <- (0.25*unipredi$logi + 0.25*unipredi$rf + 0.2*unipredi$xgbm + 0.15*unipredi$avnnet5 + 0.15*unipredi$avnnet10)
unipredi$predi70 <- (0.2*unipredi$logi + 0.2*unipredi$rf + 0.2*unipredi$xgbm + 0.2*unipredi$avnnet5 + 0.2*unipredi$avnnet10)


# Listado de modelos a considerar, cambiar al gusto

dput(names(unipredi))

listado<-c("logi", "avnnet5", "avnnet10",
           "rf","gbm",  "xgbm",  "predi9", "predi10", "predi11", "predi12", "predi13", "predi14", "predi15", 
           "predi16", "predi17", "predi18", "predi19", "predi20", "predi21", "predi22", 
           "predi23", "predi24", "predi25", "predi26", "predi27", "predi28", "predi31", 
           "predi32", "predi33", "predi34", "predi35", "predi36", "predi37", "predi38", 
           "predi39", "predi40", "predi41", "predi42", "predi43", "predi44", "predi45", 
           "predi46", "predi47", "predi48", "predi49", "predi50", "predi51", "predi52", 
           "predi53", "predi54", "predi55", "predi56", "predi57", "predi58", "predi61", 
           "predi62", "predi63", "predi64", "predi65", "predi66", "predi67", "predi68", 
           "predi69", "predi70")

# Cambio a Yes, No, todas las predicciones

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())
for (prediccion in listado)
{
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# Finalmente boxplot

par(cex.axis=0.5,las=2)
boxplot(data=medias0,tasa~modelo,col="pink",main="TASA FALLOS")

# Para AUC se utiliza la variable auc del archivo medias0

boxplot(data=medias0,auc~modelo,col="pink",main="AUC")

# PRESENTACION TABLA MEDIAS

library(dplyr)
tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(tasa=mean(tasa))     

tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,tasa~modelo,col="pink", main='TASA FALLOS')

# PRESENTACION TABLA MEDIAS

library(dplyr)
tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(tasa=mean(tasa))     

tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,tasa~modelo,col="pink", main='TASA FALLOS')

# ************************************
# PARA AUC
# ************************************

# PRESENTACION TABLA MEDIAS

tablamedias2<-medias0 %>%
  group_by(modelo) %>%
  summarize(auc=mean(auc))     

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN AUC
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,auc~modelo,col="pink", main='AUC')


# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("logi", "avnnet", 
              "rf","gbm",  "xgbm", "predi61", "predi56", "predi17", "predi62") 

medias0$modelo<-as.character(medias0$modelo)

mediasver<-medias0[medias0$modelo %in% listadobis,]


mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))

par(cex.axis=0.9,las=2)
boxplot(data=mediasver,auc~modelo,col="pink",main='AUC')

# GRÁFICOS DE APOYO PARA OBSERVAR COMPORTAMIENTO DE LOS MODELOS

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5)
# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]
# Añadir ensamblados

unipredi$predi58 <- 0.33*unipredi$rf+0.33*unipredi$avnnet5+0.33*unipredi$avnnet10
unipredi$predi52 <- 0.3*unipredi$rf + 0.4*unipredi$avnnet5 + 0.3*unipredi$xgbm
unipredi$predi53 <- 0.33*unipredi$rf+0.33*unipredi$xgbm+0.33*unipredi$avnnet10

unigraf<-unipredi[unipredi$Rep=="Rep1",]

# Correlaciones entre predicciones de cada algoritmo individual
solos<-c("rf","rfxgbm","avnnet5")
mat<-unigraf[,solos]
matrizcorr<-cor(mat)
matrizcorr
library(corrplot)
corrplot(matrizcorr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,cl.lim=c(0.7,1),is.corr=FALSE)

library(ggplot2)

qplot(avnnet5,rf,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)

qplot(avnnet10,rf,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)

qplot(gbm,xgbm,data=unigraf,colour=obs)+
  geom_hline(yintercept=0.5, color="black", size=1)+
  geom_vline(xintercept=0.5, color="black", size=1)


# Árboles
library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)
library(caret)

dput(names(archivo1))

###saheart$chd<-ifelse(saheart$chd==1,"Yes","No")

arbol1 <- rpart(factor(deposit) ~ duration+age+season+poutcome+pdays+housing, data = df,
                minbucket =40,method = "class",parms=list(split="gini"))

summary(arbol1)


# EL MÁS COMPLETO
rpart.plot(arbol1,extra=105,nn=TRUE) 


################################################################
##############################BAGGIN


#CAMBIAMOS LOS DATOS DE LA VARIABLE DEPENDIENTE DE YES & NO A 1 & 0.
archivoAA <- archivo1
archivoAA$deposit <- ifelse(archivoAA$deposit == "Yes", 1, 0)

archivoAA$deposit<-ifelse(archivoAA$deposit==1,"Yes","No")

# EJEMPLO PROBANDO UN SOLO PARÁMETRO MTRY

library(caret)

rfgrid<-expand.grid(mtry=c(12))

set.seed(1234)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(data=archivoAA,
           factor(deposit)~duration+poutcome.success+housing.no+pdays+balance+age+season.Summer+campaign+week_of_month.3+season.Spring+season.Autumn,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=3000,nodesize=10,replace=TRUE)

rf


# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest
library(randomForest)
set.seed(12345)

rfbis<-randomForest(factor(deposit)~duration+poutcome.success+housing.no+pdays+balance+age+season.Summer+campaign+week_of_month.3+season.Spring+season.Autumn,
                    data=archivo1,
                    mtry=12,ntree=5000,sampsize=2000,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

# Uso de visualpred para comprobar la capacidad de nuestro modelo y la 
# separabilidad de nuestros datos

# USO DE VISUALPRED 
library(visualpred)

listconti <- c("duration", "age", "pdays", "balance")
listclass<-c("season", "poutcome")
vardep <- c("deposit")

result_rf<-famdcontour(dataf=df,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="Random Forest",title2=" ",selec=0,modelo="rf",classvar=0,
                    sampsize=400,mtry=10)
g_rf <- result_rf[[4]]+theme(legend.position = "none")+xlab("")+ylab("")
g_rf

result_glm<-famdcontour(dataf=df,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="GLM",title2=" ",selec=0,modelo="glm",classvar=0)

g_glm <- result_glm[[4]]+theme(legend.position = "none")+xlab("")+ylab("")
g_glm

result_gbm<-famdcontour(dataf=df,listconti=listconti,listclass=listclass,vardep=vardep,
                                title="GBM",title2=" ",selec=0,modelo="gbm",classvar=0)

g_gbm <- result_gbm[[4]]+theme(legend.position = "none")+xlab("")+ylab("")
g_gbm

result_net <-famdcontour(dataf=df,listconti=listconti,listclass=listclass,vardep=vardep,
                    title="NNET",title2=" ",selec=0,modelo="nnet",classvar=0)

g_net <- result_net[[4]]+theme(legend.position = "none")+xlab("")+ylab("")
g_net

result[[1]]
result[[2]]
result[[3]]
result[[4]]
result[[5]]
result[[6]]

# No funciona con par(mfrow = c(2, 2))
# Ni con layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mfrow = c(2, 2))
plot(result_rf[[1]])
plot(result_glm[[1]])
plot(result_gbm[[1]])
plot(result_net[[1]])

# Gráficos individuales sin títulos

result_rf[[5]]
result_glm[[5]]
result_gbm[[5]]
result_net[[5]]


# Restablecer el diseño original
par(mfrow = c(1, 1))

datos<-df[,c(listconti,listclass,vardep)]

datos$season<-as.factor(datos$season)
datos$poutcome<-as.factor(datos$poutcome)
datos$black<-as.factor(datos$week_of_month)




# RANDOM FOREST EXPLAINER
library(randomForest)
library(randomForestExplainer)


# Para ver todas las opciones completas y explicación de las medidas de importancia ver esta página:

# https://cran.r-project.org/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

set.seed(2017)
forest <- randomForest(factor(deposit) ~ ., data = germanbien, localImp = TRUE,mtry=10,sampsize=300)


# *************************************************************************************
# Esto crea un html con información completa, que suele ser suficiente. Tarda un poco:
# *************************************************************************************

explain_forest(forest, interactions = TRUE, data = germanbien)

# Plot OOB

plot(forest, main = "Learning curve of the forest")
legend("topright", c("error for 'Yes'", "misclassification error", "error for 'No'"),
       lty = c(1,1,1), col = c("green", "black", "red"))

# *************************************************************************************

# Gráficos individuales del estilo del html creado con explain_forest :

# *************************************************************************************

min_depth_frame <- min_depth_distribution(forest)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)


importance_frame <- measure_importance(forest)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "gini_decrease",
                          y_measure = "mean_min_depth",
                          size_measure = "p_value", no_of_labels = 10)


plot_importance_ggpairs(importance_frame)

plot_importance_rankings(importance_frame)

vars <- important_variables(importance_frame, k = 5, 
                            measures = c("mean_min_depth", "gini_decrease"))

vars

# "age"      "balance"  "campaign" "duration" "housing"  "pdays"   
interactions_frame <- min_depth_interactions(forest, vars)
save(interactions_frame, file = "interactions_frame.rda")
load("interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ]

plot_min_depth_interactions(interactions_frame)


plot_predict_interaction(forest, germanbien, "duration", "housing")
plot_predict_interaction(forest, germanbien, "duration", "housing")
plot_predict_interaction(forest, germanbien, "housing", "age")


# ************************************************************
# COMPARACIÓN A NIVEL DE CONTRASTES DE HIPÓTESIS
# ************************************************************

# Tras realizar ensamblado, se obtiene el data frame medias0, 
# Aquí se pone un ejemplo de contrastar si la diferencia entre dos modelos es significativa

# Se realiza el contraste de medias de la t de Student , comparando si la media del AUC
# es significativamente diferente en los resultados de cv de los dos algoritmos

# Por ejemplo comparamos predi46 con logística

# listadobis<-c("logi", "avnnet", 
#               "rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
#               "svmRadial","predi45", "predi14", "predi46", "predi47") 

listamodelos<-c("avnnet5","avnnet10")

datacontraste<-medias0[which(medias0$modelo%in%listamodelos),]

# Para Tasa de fallos

res <- t.test(datacontraste$tasa ~datacontraste$modelo)
res

# Para auc

res <- t.test(datacontraste$auc ~datacontraste$modelo)
res

# El p-valor es muy alto, de 0.88, con lo que se concluye que la diferencia de medias
# no es significativamente diferente de cero.
# Por lo tanto al ser similares los resultados, nos quedaríamos con el modelo más sencillo, la logística
# 
# data:  datacontraste$auc by datacontraste$modelo
# t = 0.13956, df = 97.71, p-value = 0.8893
# alternative hypothesis: true difference in means between group logi and group predi46 is not equal to 0
# 95 percent confidence interval:
#   -0.001838600  0.002116745
# sample estimates:
#   mean in group logi mean in group predi46 
# 0.7778642             0.7777252 


# Como ejemplo, repetimos el contraste entre avnnet y logi

listamodelos<-c("logi","predi52")

datacontraste<-medias0[which(medias0$modelo%in%listamodelos),]

# Para Tasa de fallos

res <- t.test(datacontraste$tasa ~datacontraste$modelo)
res

# Para auc

res <- t.test(datacontraste$auc ~datacontraste$modelo)
res

# AQUÍ SI HAY DIFERENCIA SIGNIFICATIVA 

# data:  datacontraste$auc by datacontraste$modelo
# t = -8.2883, df = 87.897, p-value = 1.201e-12
# alternative hypothesis: true difference in means between group gbm and group logi is not equal to 0
# 95 percent confidence interval:
#   -0.01224739 -0.00751006
# sample estimates:
#   mean in group gbm mean in group logi 
# 0.7679855          0.7778642 


# ===========================================================================================================
# Decisión en clasificación binaria

library(caret)
library(pROC)

# c("sbp", "tobacco", "ldl", "adiposity", "famhist", "typea", "obesity", 
# "alcohol", "age", "chd")

vardep<-"deposit"
listconti<-c("duration", "poutcome.success", "housing.no", "poutcome.unknown", "campaign", "working.No", "education.tertiary", 
             "loan.no", "season.Summer", "marital.single", "week_of_month.3", "season.Autumn", "education.primary", "balance", "week_of_month.5")                                             
                                                
listclass<-c("")

archivo2 <- archivo1
archivo2 <- archivo2[,c(listconti,vardep)]

table(archivo2$deposit)
# No Yes 
# 302 160 
prop.table(table(archivo2$deposit))
# No       Yes 
# 0.6536797 0.3463203 

set.seed(12345)
control<-trainControl(method = "cv",number=8,savePredictions = "all",classProbs=TRUE) 

logi<- train(factor(deposit)~.,data=archivo2,method="glm",trControl=control)

logi

sal<-logi$pred

# MEDIDAS CON PUNTO DE CORTE 0.5

confusionMatrix(reference=sal$obs,data=sal$pred, positive="Yes")

# CON PUNTO DE CORTE 0.4

corte<-0.5

sal$predcorte<-ifelse(sal$Yes>corte,"Yes","No")
sal$predcorte<-as.factor(sal$predcorte)

confusionMatrix(reference=sal$obs,data=sal$predcorte, positive="Yes")

# Estimación final del error

set.seed(12345)
control<-trainControl(method = "cv",number=10,savePredictions = "all",classProbs=TRUE) 

logi<- train(factor(deposit)~.,data=archivo2,method="glm",trControl=control)

logi
sal<-logi$pred

roc(sal$obs,sal$Yes)


# Hago los boxplot

auc_predi58 <- medias0$auc[medias0$modelo == "predi58"]
auc_predi52 <- medias0$auc[medias0$modelo == "predi52"]
auc_predi53 <- medias0$auc[medias0$modelo == "predi53"]

# Crear el boxplot con mediasCat y mediasXBGvar2 y añadir predi70
boxplot(list(mediasCat = mediasCat$auc, 
             mediasXBGvar2 = mediasXBGvar2[[1]]$auc,
             medias9 = medias9[[1]]$auc,
             predi58 = auc_predi58,
             predi52 = auc_predi52,
             predi53 = auc_predi53),
        main = "AUC mejores modelos",
        ylab = "AUC",
        names = c("CatBoost", "XBGvar", "Red5n", "predi58", "predi52", "predi53"),
        col = c("cyan", "lightgreen", "pink", "yellow", "lightcoral", "lightcyan"))

# Para las tasas
# Hago los boxplot

auc_predi58 <- medias0$tasa[medias0$modelo == "predi58"]
auc_predi52 <- medias0$tasa[medias0$modelo == "predi52"]
auc_predi53 <- medias0$tasa[medias0$modelo == "predi53"]

# Crear el boxplot con mediasCat y mediasXBGvar2 y añadir predi70
boxplot(list(mediasCat = mediasCat$tasa, 
             mediasXBGvar2 = mediasXBGvar2[[1]]$tasa,
             medias9 = medias9[[1]]$tasa,
             predi58 = auc_predi58,
             predi52 = auc_predi52,
             predi53 = auc_predi53),
        main = "Tasa de error mejores modelos",
        ylab = "tasa error",
        names = c("CatBoost", "XBGvar", "Red5n", "predi58", "predi52", "predi53"),
        col = c("cyan", "lightgreen", "pink", "yellow", "lightcoral", "lightcyan"))


# Añadir una leyenda
legend("topleft", 
       legend = c("mediasCat", "mediasXBGvar2", "predi70"),
       fill = c("lightblue", "lightgreen", "pink"))

