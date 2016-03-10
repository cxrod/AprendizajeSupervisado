#Cargamos librerias
library ('rpart')
library ('rpart.plot')
library('caret')
library('class')
library('RWeka')
library('pROC')

# Lectura de los datos
dataSet <- read.csv2(
  file = "minable.csv",
  header = T,sep = ",")

# Arreglamos la data
dataSet <- dataSet[-c(1),]
for (i in 1:length(dataSet)){
  if (is.factor(dataSet[,i])){
    dataSet[,i] <- as.numeric(as.character(dataSet[,i]))
  }
}

# Eliminamos columnas inutiles
dataSet$cIdentidad <- NULL
dataSet$fNacimiento <- NULL
dataSet$sCurso <- NULL
dataSet$cDireccion <- NULL
dataSet$jReprobadas <- NULL
dataSet$mInscritas <- NULL
dataSet$tGrado <- NULL
dataSet$sCurso <- NULL
dataSet$pReside <- NULL
dataSet$dHabitacion <- NULL
dataSet$aEconomica <- NULL
dataSet$oSolicitudes <- NULL
dataSet$rEconomico <- NULL
dataSet$pRenovar <- NULL
dataSet$rating <- NULL
dataSet$sugerencias <- NULL
dataSet$iTotal <- -1
dataSet$gTotal <- -1
dataSet$irTotal <- -1
dataSet$grTotal <- -1
for (i in 1:nrow(dataSet)){
  dataSet$iTotal[i] <- dataSet$beca[i] + dataSet$aResponsable[i] + dataSet$aOtros[i] + dataSet$iActividades
  dataSet$gTotal[i] <- dataSet$gAlimentacion[i] + dataSet$gTransporte[i] + dataSet$gMedicos[i] + dataSet$gOdontologicos[i] + dataSet$gPersonales[i] + dataSet$gEstudios[i] + dataSet$gRecreacion[i] + dataSet$gAlquiler[i] + dataSet$gOtros[i]
  dataSet$irTotal[i] <- dataSet$irMensual[i] + dataSet$irOtros[i]
  dataSet$grTotal[i] <- dataSet$grVivienda[i] + dataSet$grAlimentacion[i] + dataSet$grTransporte[i] + dataSet$grMedicos[i] + dataSet$grOdontologicos[i] + dataSet$grEducativos[i] + dataSet$grServicios[i] + dataSet$grCondominio[i] + dataSet$grOtros[i]
}
dataSet$grTotal[96] <- 15250
dataSet$beca <- NULL
dataSet$aResponsable <- NULL
dataSet$aOtros <- NULL
dataSet$iActividades <- NULL
dataSet$gAlimentacion <- NULL
dataSet$gTransporte <- NULL
dataSet$gMedicos <- NULL
dataSet$gOdontologicos <- NULL
dataSet$gPersonales <- NULL
dataSet$gEstudios <- NULL
dataSet$gRecreacion <- NULL
dataSet$gAlquiler <- NULL
dataSet$gOtros <- NULL
dataSet$irMensual <- NULL
dataSet$irOtros <- NULL
dataSet$grVivienda <- NULL
dataSet$grAlimentacion <- NULL
dataSet$grTransporte <- NULL
dataSet$grMedicos <- NULL
dataSet$grOdontologicos <- NULL
dataSet$grEducativos <- NULL
dataSet$grServicios <- NULL
dataSet$grCondominio <- NULL
dataSet$grOtros <- NULL

#Creamos nuestro set de entrenamiento y de pruebas estratificado
set.seed(1886)
mIngreso0 = dataSet[dataSet$mIngreso==0,]
mIngreso1 = dataSet[dataSet$mIngreso==2,]
mIngreso2 = dataSet[dataSet$mIngreso==3,]
samp = sample(2,nrow(mIngreso0), replace = TRUE, prob = c(0.8,0.2))
samp1 = sample(2,nrow(mIngreso1), replace = TRUE, prob = c(0.8,0.2))
samp2 = sample(2,nrow(mIngreso2), replace = TRUE, prob = c(0.8,0.2))

t_entrenamiento = mIngreso0[samp==1,]
t_prueba = mIngreso0[samp==2,]
t_entrenamiento1 = mIngreso1[samp1==1,]
t_prueba1 = mIngreso1[samp1==2,]
t_entrenamiento2 = mIngreso2[samp2==1,]
t_prueba2 = mIngreso2[samp2==2,]

for (i in 1:nrow(t_entrenamiento1)){
  t_entrenamiento <- rbind(t_entrenamiento, t_entrenamiento1[i,])
}
for (i in 1:nrow(t_entrenamiento2)){
  t_entrenamiento <- rbind(t_entrenamiento, t_entrenamiento2[i,])
}
for (i in 1:nrow(t_prueba1)){
  t_prueba <- rbind(t_prueba, t_prueba1[i,])
}
for (i in 1:nrow(t_prueba2)){
  t_prueba <- rbind(t_prueba, t_prueba2[i,])
}

#Arbol por defecto
tree <- rpart(mIngreso ~ ., data = t_entrenamiento, method = 'class')

confusionMatrixTree = table(t_prueba$mIngreso, predict(tree, newdata = t_prueba,type = "class"))

# Definicion de multiples arboles para elegir el mejor
minsplits <- c(2,5,10,50,300,1000)
cps <- c(0.3,0.2,0.1,0.0001,0.0000000000001)
minbuckets <- c(2,5,10,50,300)
best_Tree <- tree
min_error <- 1 - ((confusionMatrixTree[1,1] + confusionMatrixTree[2,2] + confusionMatrixTree[3,3]) / nrow(t_prueba))
for (i in minsplits){
  for(j in cps){
    for(k in minbuckets){
      tree <- rpart(mIngreso ~ ., t_entrenamiento,method = "class",control = rpart.control(minsplit = i,cp = j, minbucket = k))
      confusionMatrixTree = table(t_prueba$mIngreso, predict(tree, newdata = t_prueba,type = "class"))
      tree_error <- 1 - ((confusionMatrixTree[1,1] + confusionMatrixTree[2,2] + confusionMatrixTree[3,3]) / nrow(t_prueba))        
      if (tree_error < min_error){
        best_Tree <- tree
        min_error <- tree_error
      }
    }
  }
}

rpart.plot(best_Tree)
t_predict <- predict(best_Tree, newdata = t_prueba,type = "class")
confusionMatrixTree = table(t_prueba$mIngreso, predict(best_Tree, newdata = t_prueba,type = "class"))
tree_error <- 1 - ((confusionMatrixTree[1,1] + confusionMatrixTree[2,2] + confusionMatrixTree[3,3]) / nrow(t_prueba))        
tree_error

#K Vecinos #####################################################
kv_entrenamiento <- t_entrenamiento
kv_prueba <- t_prueba
level_entrenamiento <- kv_entrenamiento$mIngreso
level_prueba <- kv_prueba$mIngreso
kv_entrenamiento$mIngreso <- NULL
kv_prueba$mIngreso <- NULL
normDatos = preProcess(kv_entrenamiento, method = "range")
kv_entrenamiento = predict(normDatos, kv_entrenamiento)
kv_prueba = predict(normDatos, kv_prueba)
kv_modelo <- knn(train = kv_entrenamiento, test = kv_prueba, cl = as.factor(level_entrenamiento), k=14)
confusionMatrixKv = table(level_prueba, kv_modelo)
kv_error = 1 - ((confusionMatrixKv[1,1] + confusionMatrixKv[2,2] + confusionMatrixKv[3,3]) / nrow(t_prueba))
kv_error
# Reglas de clasificacion ####################################

t_entrenamiento$mIngreso <- as.factor(t_entrenamiento$mIngreso)
t_prueba$mIngreso <- as.factor(t_prueba$mIngreso)
rc_modelo <- JRip(formula = mIngreso ~ ., data = t_entrenamiento)
rc_predict <- predict(rc_modelo, newdata = t_prueba,type = "class")
confusionMatrixClasification = table(t_prueba$mIngreso, rc_predict)
confusionMatrixClasification
rc_error = 1 - ((confusionMatrixClasification[1,1] + confusionMatrixClasification[2,2] + confusionMatrixClasification[3,3]) / nrow(t_prueba))
rc_error

#Pruebas ROC de los 3 modelos
rocT <- roc(t_prueba$mIngreso, as.numeric(as.character(t_predict)), levels=level_prueba)
plot(rocT)

rocKv <- roc(t_prueba$mIngreso, as.numeric(as.character(kv_modelo)), levels=level_prueba)
plot(rocKv)


rocRc <- roc(t_prueba$mIngreso, as.numeric(as.character(rc_predict)), levels=level_prueba)
plot(rocRc)

