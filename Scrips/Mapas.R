# Librerías requeridas ----------------------------------

library(rgdal) #1.3-6"
library(leaflet) #2.0.2
library(htmlwidgets) #1.3
library(tidyverse) #1.2.1
library(dplyr) #0.7.8
library(rjson) #0.2.2.0
library(readxl) #1.1.0
library(extrafont) #0.17


# Importar datos ----------------------------------------

divipola.R <- read.table("Geodata/DIVIPOLA_20160930.csv", sep=";", header=T, encoding = "UTF-8")
municipios <-divipola.R[,5]
departamentos <- divipola.R[,4]
latitud <- divipola.R[,9]
longitud <- divipola.R[,8]
tipo_centro <- divipola.R[,7]
code_mun <- divipola.R[,2]
code_dept <- divipola.R[,1]
poblados <- data.frame(municipios, code_mun, code_dept, departamentos, longitud, latitud, tipo_centro)

# Base de datos con solo cabeceras municipales 

cabeceras <- poblados[tipo_centro=="CABECERA MUNICIPAL (CM)",]

# Lectura de datos de matirantes por lugar de residencia

tipovar <- c("text", "text", "numeric", "numeric", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "numeric", "text", "numeric", 
             "text", "text", "text", "numeric", "text", "numeric", "text", "text", 
             "text", "text", "text", "numeric", "text", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "text", "text", "text", "text", 
             "text", "text", "numeric", "text", "text", "text", "numeric", "text", "text", 
             "text", "numeric", "numeric", "text")

procedencia.R <- read_excel("Data/matriculados.xlsx",  col_types = tipovar)  %>% filter(YEAR == 2019, SEMESTRE == 1)


depart_mat <- procedencia.R[,13]
codept_mat<-procedencia.R[,14]
ciudad_mat <- procedencia.R[,15]
codecity_mat <-procedencia.R[,16]
#codecity_mat <-as.integer(as.character(codecity_mat))
long_mat <- as.numeric(str_replace(procedencia.R$LON_CIU_PROC, ",", "."))
lat_mat <- as.numeric(str_replace(procedencia.R$LAT_CIU_PROC, ",", "."))
# long_mat <- as.numeric(gsub(".", ",", as.character(procedencia.R[,17])))
# lat_mat <- as.numeric(gsub(".", ",", as.character(procedencia.R[,18])))

mod_mat <- procedencia.R[,38] # si fue inscripción regular o irregular
tipo_mat <- procedencia.R[,39] # si es paes, peama o regular la inscripción
tipopeama_mat <- procedencia.R[,41]  
sexo <- procedencia.R[,24]
#indi <- procedencia.R[,25]<-1

# matriculados<-data.frame(depart_mat,codecity_mat,codept_mat, ciudad_mat,long_mat, lat_mat, sexo,  mod_mat, tipo_mat, tipopeama_mat)
matriculados<-na.omit(data.frame(depart_mat,codecity_mat,codept_mat, ciudad_mat,long_mat, lat_mat, sexo,  mod_mat, tipo_mat, tipopeama_mat))

cantesp_mat <-matriculados %>% group_by(DEP_PROC, COD_DEP_PROC)%>% summarise (n = n()) 
cantesp_mat_mun <-  matriculados %>% group_by(CIU_PROC, COD_CIU_PROC)%>% summarise (n = n()) 

#Extraer lista de códigos de los municipios con la respectiva cantidad de matriculados

json_file <- "Geodata/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 


for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS=as.integer(json_data$features[[i]]$properties$MPIOS)
}

codigos <- matrix(0, nrow=1122,ncol=2)

for(i in 1:1122){
  codigos[i,1]=json_data$features[[i]]$properties$MPIOS
}


for(i in cantesp_mat_mun$COD_CIU_PROC){
  codigos[codigos[,1]==i,2]= cantesp_mat_mun$n[cantesp_mat_mun$COD_CIU_PROC==i]
}

codigos


######### Json por jurisdicciones de municipios

cities_col <- rgdal::readOGR("Geodata/mpio5.json",use_iconv = T, encoding="UTF-8")

cities_col@data <- cities_col@data[-c(1,2,3,4,5,7,9,10,11,12,13,14,15)]



#Agregar información al Spatial Data Frame

cities_col@data$CODE_MPI=codigos[,1]
cities_col@data$CANT_ADM=codigos[,2]





#Lectura de JSON de Colombia por departamentos

colombia.R<- rgdal::readOGR("Geodata/depto4.json", use_iconv = T, encoding= "UTF-8")

colombia.R@data<-colombia.R@data[-c(3,4,5)] #Se omite información complementaria

colombia.R@data$DPTO=as.integer(as.character(colombia.R@data$DPTO))


# Buscar el centroide de cada departamento en la base


x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}


#Centroides de los departamentos
centro_dept <- data.frame(x,y)
centro_dept
colnames(centro_dept) <- c("dept", "lon", "lat")

#ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(2,10,11)]
esri
names.esri <- c("Street","Satélite" , "Ligero")


#Leer en formato excel la base de datos de DIVIPOLA

divipola2.R <- read_excel("Geodata/DIVIPOLA_20160930.xlsx")
cabeceras2 <- divipola2.R %>% filter(divipola2.R$`Tipo Centro Poblado` == "CABECERA MUNICIPAL (CM)")

#####

check.integer <- function(x) {
  x == round(x)
}

#Sedes de Presencia Nacional

capitaless <- cabeceras2[check.integer((cabeceras2$`Código Municipio`-1)/1000)==T,]

#Suprimir Agua de Dios
capitaless <- capitaless%>%filter(`Código Municipio`!="25001")

#Filtrar sedes de la Universidad Nacional de Colombia
sedes <- rbind(cabeceras2[cabeceras2$`Código Municipio`== "5001",],
               cabeceras2[cabeceras2$`Código Municipio`== "11001",], 
               cabeceras2[cabeceras2$`Código Municipio`== "76520",], 
               cabeceras2[cabeceras2$`Código Municipio`== "52835",],
               cabeceras2[cabeceras2$`Código Municipio`== "17001",],
               cabeceras2[cabeceras2$`Código Municipio`== "88001",],
               cabeceras2[cabeceras2$`Código Municipio`== "91001",],
               cabeceras2[cabeceras2$`Código Municipio`== "20621",])

sedes2 <- rbind(cabeceras2[cabeceras2$`Código Municipio`== "81001",])

Sede <- c("Medellín", "Bogotá", "Palmira", "Tumaco", "Manizales",   "Caribe", "Amazonía", "La Paz" )
Sede2<- c("Orinoquia")
sedes <- cbind(sedes, Sede)
sedes2 <- cbind(sedes2, Sede2)
sedes2

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

sedeIcon2 <- makeAwesomeIcon (markerColor = "purple", iconColor = "white", 
                              fontFamily = "Leonardian", text = "un")

label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

label_sede2 <- sprintf("<strong>%s %s</strong>", 
                       "Sede ", sedes2$Sede)%>% lapply(htmltools::HTML)

#Función para mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


## Filtrar tipos de población especial

tipo <- matriculados %>% group_by(DEP_PROC, COD_DEP_PROC, TIPO_ADM)%>% summarise (n = n())
PEAMA <-  tipo %>% filter(TIPO_ADM=="PEAMA")

tipopeama <- matriculados %>% group_by(DEP_PROC, COD_DEP_PROC, PEAMA)%>% summarise (n = n())
orinoquia <- tipopeama %>%  filter(PEAMA=="PEAMA - Orinoquía")


codigoscol <- data.frame(matrix(0, nrow=33,ncol=4))

for(i in 1:33){
  codigoscol[i,1]=colombia.R@data$DPTO[i]
}

cantesp_mat <- cantesp_mat[-12,]


for(i in cantesp_mat$COD_DEP_PROC){
  codigoscol[codigoscol[,1]==i,2]= cantesp_mat$n[cantesp_mat$COD_DEP_PROC==i][1]
}


for(i in PEAMA$COD_DEP_PROC){
  codigoscol[codigoscol[,1]==i,3]= PEAMA$n[PEAMA$COD_DEP_PROC==i]
}


for(i in orinoquia$COD_DEP_PROC){
  codigoscol[codigoscol[,1]==i,4]= orinoquia$n[orinoquia$COD_DEP_PROC==i][1]
}


colnames (codigoscol) = c("code_dept", "Total", "PEAMA", "orinoquia")

codigoscol

colombia.R$PEAMA = codigoscol$PEAMA

colombia.R$ORINOQUIA = codigoscol$orinoquia

colombia.R@data$CANT_ASP = codigoscol$Total

# Localizaciones idividuos poblacion especial


tipopeama <- matriculados %>% group_by(DEP_PROC, COD_DEP_PROC, PEAMA, lat_mat, long_mat, SEXO)
orinoquia <- tipopeama %>%  filter(PEAMA=="PEAMA - Orinoquía")


centroidecol <- centro_dept%>% filter(dept =="CUNDINAMARCA")
centroidecol
