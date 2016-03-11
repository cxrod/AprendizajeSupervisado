#install.packages('curl')
#install.packages('xlsx')
library('curl')
library('xlsx')

source('google_api.R')
#Cargamos la data
dataSet = read.xlsx("hogares.xlsx", sheetIndex = 1, startRow = 1, endRow = 104, header = T, encoding = 'UTF-8')

dataSet$Dirección <- as.character(dataSet$Dirección)
dataSet$Distrito <- as.character(dataSet$Distrito)
dataSet$Dirección[11] <- "Galliate"
dataSet$Dirección[33] <- "Via San Roberto Bellarmino"
dataSet$Dirección[61] <- "Via di Monte Verde"

dataSet$Duracion <- 0
googleApiKey <- 'AIzaSyBekPZxKRhaB4kbaa90sx4ZoK4-OJImWAM'
destino <- 'Piazzale Aldo Moro' #Sapienza Universidad de Roma

for (i in 1:nrow(dataSet)){
  dataSet$Dirección[i] <- gsub("\n", " ", dataSet$Dirección[i])
  dataSet$Distrito[i] <- gsub("\n", " ", dataSet$Distrito[i])
  origen <- paste(c(dataSet$Dirección[i]," ",dataSet$Distrito[i]), collapse="") 
  googleUrl <- get_url(origen, destino, googleApiKey)
  googleData <- get_data(googleUrl)
  googleJson = parse_data(googleData)
  if(googleJson$status == "OK"){
    dataSet$Duracion[i] <- googleJson$duration$value
    
  }else{
    origen <- dataSet$Dirección[i]
    googleUrl <- get_url(origen, destino, googleApiKey)
    googleData <- get_data(googleUrl)
    googleJson = parse_data(googleData)
    if(googleJson$status == "OK"){
      #Making split to transform hours to mins
      dataSet$Duracion[i] = googleJson$duration$value
    }
  }
}

dataSet$Tipo.de.Inmueble <- as.character(dataSet$Tipo.de.Inmueble)
dataSet$Tipo.de.Inmueble[grepl("Ap", dataSet$Tipo.de.Inmueble)] <- '0'
dataSet$Tipo.de.Inmueble[grepl("Mini", dataSet$Tipo.de.Inmueble)] <- '1'
dataSet$Tipo.de.Inmueble[grepl("Monolocale", dataSet$Tipo.de.Inmueble)] <- '1'
dataSet$Tipo.de.Inmueble <- as.numeric(dataSet$Tipo.de.Inmueble)

dataSet$Entrada <- 0
dataSet$Entrada[grepl("(ngresso)", dataSet$Descripción)] <- 1

dataSet$Cocina <- 0
dataSet$Cocina[grepl("(cucina)", dataSet$Descripción)] <- 1

dataSet$Bano <- 0
dataSet$Bano[grepl("(bagno)", dataSet$Descripción)] <- 1
dataSet$Bano[grepl("(2.?bagni)", dataSet$Descripción)] <- 2
dataSet$Bano[grepl("(4.?bagni)", dataSet$Descripción)] <- 3
dataSet$Bano[grepl("(3.?bagni)", dataSet$Descripción)] <- 4

dataSet$Calefaccion <- 0
dataSet$Calefaccion[grepl("(riscaldamento)", dataSet$Precio.Mensual)] <- 1

dataSet$condominio <- 0
dataSet$condominio[grepl("(condominio)", dataSet$Precio.Mensual)] <- 1

dataSet$NoServicios <- 0
dataSet$NoServicios[grepl("(escluse)", dataSet$Precio.Mensual)] <- 1

dataSet$Genero.admitido <- -1
dataSet$Genero.admitido[grepl("(ragazzi)", dataSet$Notas)] <-  0
dataSet$Genero.admitido[grepl("(ragazze)", dataSet$Notas)] <-  1
dataSet$Genero.admitido[grepl('(ragazze.i) | (ragazzi.e) | (ragazzi.ragazze) | (ragazze.ragazzi)', dataSet$Notas)] <-  2
dataSet$Genero.admitido[39] <- 2

dataSet$Precio <- -1
dataSet$Habitacion <- -1
dataSet$Habitaciones.Disponibles <- as.character(dataSet$Habitaciones.Disponibles)
dataSet$Precio.Mensual <- as.character(dataSet$Precio.Mensual)
for (i in 1:nrow(dataSet)){
  aux <- strsplit(dataSet$Precio.Mensual[i], '[^[:digit:]]')
  aux <- matrix(unlist(aux), ncol=1, byrow=TRUE)
  aux <- aux[aux!='',]
  if (length(aux) == 1){
    dataSet$Precio[i] <- as.numeric(aux[1])
    if (grepl("(ingol)", dataSet$Habitaciones.Disponibles[i])) dataSet$Habitacion[i] <- 0
    if (grepl("(doppi)", dataSet$Habitaciones.Disponibles[i])) dataSet$Habitacion[i] <- 1
    if (grepl("(posto)", dataSet$Habitaciones.Disponibles[i])) dataSet$Habitacion[i] <- 2
    if (grepl("(ntero)", dataSet$Habitaciones.Disponibles[i])) dataSet$Habitacion[i] <- 3
  }
}

#El resto a pata
aux <-  dataSet[4,]
aux$Habitacion <- 1
aux$Precio <- as.numeric(450)
dataSet$Habitacion[4] <- 0
dataSet$Precio[4] <- as.numeric(300)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[5,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(450)
dataSet$Habitacion[5] <- 0
dataSet$Precio[5] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[7,]
aux$Habitacion <- 1
aux$Precio <- as.numeric(450)
dataSet$Habitacion[7] <- 0
dataSet$Precio[7] <- as.numeric(250)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[10,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(350)
dataSet$Habitacion[10] <- 0
dataSet$Precio[10] <- as.numeric(550)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[16,]
dataSet$Habitacion[16] <- 0
dataSet$Precio[16] <- as.numeric(460)
aux$Habitacion <- 0
aux$Precio <- as.numeric(460)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(430)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(430)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[18,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(450)
dataSet$Habitacion[18] <- 0
dataSet$Precio[18] <- as.numeric(400)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[21,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(475)
dataSet$Habitacion[21] <- 0
dataSet$Precio[21] <- as.numeric(525)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(575)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[22,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(525)
dataSet$Habitacion[22] <- 0
dataSet$Precio[22] <- as.numeric(575)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[23,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(475)
dataSet$Habitacion[23] <- 0
dataSet$Precio[23] <- as.numeric(525)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(575)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[24,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(380)
dataSet$Habitacion[24] <- 0
dataSet$Precio[24] <- as.numeric(350)
dataSet <- rbind(dataSet, aux)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[26,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(400)
dataSet$Habitacion[26] <- 0
dataSet$Precio[26] <- as.numeric(420)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(380)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[31,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(250)
dataSet$Habitacion[31] <- 0
dataSet$Precio[31] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[41,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(350)
dataSet$Habitacion[41] <- 0
dataSet$Precio[41] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[42,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(450)
dataSet$Habitacion[42] <- 0
dataSet$Precio[42] <- as.numeric(500)
dataSet <- rbind(dataSet, aux)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(420)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[43,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(450)
dataSet$Habitacion[43] <- 0
dataSet$Precio[43] <- as.numeric(500)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[44,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(400)
dataSet$Habitacion[44] <- 0
dataSet$Precio[44] <- as.numeric(370)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(340)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[56,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(550)
dataSet$Habitacion[56] <- 0
dataSet$Precio[56] <- as.numeric(500)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[61,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(425)
dataSet$Habitacion[61] <- 0
dataSet$Precio[61] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(475)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[72,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(350)
dataSet$Habitacion[72] <- 0
dataSet$Precio[72] <- as.numeric(430)
dataSet <- rbind(dataSet, aux)
###############################################+++
dataSet$Habitacion[74] <- 0
dataSet$Precio[74] <- as.numeric(450)
###############################################+++
aux <-  dataSet[76,]
aux$Habitacion <- 3
aux$Precio <- as.numeric(850)
dataSet$Habitacion[76] <- 0
dataSet$Precio[76] <- as.numeric(425)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[77,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(350)
dataSet$Habitacion[77] <- 0
dataSet$Precio[77] <- as.numeric(550)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[81,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(325)
dataSet$Habitacion[81] <- 0
dataSet$Precio[81] <- as.numeric(550)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[84,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(475)
dataSet$Habitacion[84] <- 2
dataSet$Precio[84] <- as.numeric(375)
dataSet <- rbind(dataSet, aux)
aux$Precio <- as.numeric(525)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[85,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(300)
dataSet$Habitacion[85] <- 0
dataSet$Precio[85] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[87,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(475)
dataSet$Habitacion[87] <- 0
dataSet$Precio[87] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[88,]
aux$Habitacion <- 2
aux$Precio <- as.numeric(300)
dataSet$Habitacion[88] <- 0
dataSet$Precio[88] <- as.numeric(450)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[93,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(600)
dataSet$Habitacion[93] <- 0
dataSet$Precio[93] <- as.numeric(500)
dataSet <- rbind(dataSet, aux)
###############################################+++
aux <-  dataSet[99,]
aux$Habitacion <- 0
aux$Precio <- as.numeric(600)
dataSet$Habitacion[99] <- 0
dataSet$Precio[99] <- as.numeric(550)
dataSet <- rbind(dataSet, aux)
###############################################
dataSet$Habitacion[45] <- 3
dataSet$Habitacion[90] <- 3

dataSet$Distrito <- NULL
dataSet$Dirección <- NULL 
dataSet$Notas <- NULL
dataSet$Descripción <- NULL
dataSet$Foto <- NULL
dataSet$Piso <- NULL

dataSet$Habitaciones.Disponibles <- NULL
dataSet$Precio.Mensual <- NULL
dataSet$Piso <- NULL


regresion <- lm(Precio ~ . , data = dataSet)

myEntrada = as.numeric(readline("Quieres que tenga Entrada? 0 no, 1 si:"))
myTipoImnueble = as.numeric(readline("Quieres que sea tipo estudio? 0 no, 1 si:"))
myCocina = as.numeric(readline("Quieres que tenga Cocina? 0 no, 1 si:"))
myBano = as.numeric(readline("Cuantos baños quieres que tenga?:"))
myCalefaccion = as.numeric(readline("Quieres que tenga calefaccion? 0 no, 1 si:"))
myCondominio = as.numeric(readline("Quieres pagar condominio? 0 no, 1 si:"))
myNoServicios = as.numeric(readline("Quieres que NO incluya cargos por servicios? 0 no, 1 si:"))
myGenero = as.numeric(readline("Quieres que admitan a que genero? 0 para hombres, 1 mujeres y 2 para ambos:"))
myHabitacion = as.numeric(readline("Que tipo de habitacion quieres? 0 para simple, 1 doble, 2 cama en habitacion doble y 3 para apartamento completo:"))
myDireccion = as.character(readline("Direccion aproximada donde quieres el hogar?:"))

aux <- dataSet[1,]
aux$Tipo.de.Inmueble <- myTipoImnueble
aux$Entrada <- myEntrada
aux$Cocina <- myCocina
aux$Bano <- myBano
aux$Calefaccion <- myCalefaccion
aux$condominio <- myCondominio
aux$NoServicios <- myNoServicios
aux$Genero.admitido <- myGenero
aux$Habitacion <- myHabitacion

googleUrl <- get_url(myDireccion, destino, googleApiKey)
googleData <- get_data(googleUrl)
googleJson <- parse_data(googleData)
if(googleJson$status == "OK"){
  myDuracion <- googleJson$duration$value
}

aux$Duracion <- myDuracion

prediccion <- predict(regresion, newdata = aux)
prediccion
