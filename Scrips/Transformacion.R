# Librerías Requeridas ----

library("googledrive")
library(tidyverse)

# Funciones ----

# Función para consolidar información por desagregaciones temáticas

Clases <- function(Base, varc){
  Base %>% group_by(.dots = list("YEAR", "SEMESTRE", all_of(varc))) %>%
    summarise(Total = n()) %>% rename("Clase"=varc) %>%
    mutate(Variable = varc) %>% select(Variable, YEAR, SEMESTRE, Clase, Total) %>%
    complete(Variable, YEAR, SEMESTRE, Clase) %>%
    replace_na(list(Total = 0)) %>% ungroup()
}

# Función para consolidar Series de tiempo por poblaciones

Total <- function(Base){
  Base %>% group_by(YEAR, SEMESTRE) %>%  summarise(Total = n()) %>% ungroup() %>%
    mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total) %>% ungroup()
}

# Importar registros ----

drive_download("Agregados SPN/Aspirantes/AspirantesSPN.csv", overwrite = TRUE)
drive_download("Agregados SPN/Matriculados/MatriculadosSPN.csv", overwrite = TRUE)
drive_download("Agregados SPN/Graduados/GraduadosSPN.csv", overwrite = TRUE)
drive_download("Agregados SPN/Docentes/DocentesSPN.csv", overwrite = TRUE)
drive_download("Agregados SPN/Administrativos/AdministrativosSPN.csv", overwrite = TRUE)


Aspirantes <- read_csv("AspirantesSPN.csv", locale = readr::locale(encoding = "latin1"))
Aspirantes <- Aspirantes %>% select(-X1)
Matriculados <- read_csv("MatriculadosSPN.csv", locale = readr::locale(encoding = "latin1"))
Matriculados <- Matriculados %>% select(-X1)
Graduados <- read_csv("GraduadosSPN.csv", locale = readr::locale(encoding = "latin1"))
Graduados <- Graduados %>% select(-X1)
Docentes <- read_csv("DocentesSPN.csv", locale = readr::locale(encoding = "latin1"))
Docentes <- Docentes %>% select(-X1)
Administrativos <- read_csv("AdministrativosSPN.csv", locale = readr::locale(encoding = "latin1"))
Administrativos <- Administrativos %>% select(-X1)


# Poblaciones - Sede Orinoquía ----

AspirantesPRE <- Aspirantes %>% filter(INS_SEDE_NOMBRE == "Orinoquía", TIPO_NIVEL == "Pregrado")
AdmitidosPRE <- Aspirantes %>% filter(INS_SEDE_NOMBRE == "Orinoquía", TIPO_NIVEL == "Pregrado", ADMITIDO == "Sí")
# AspirantesPOS <- Aspirantes %>% filter(INS_SEDE_NOMBRE == "Orinoquía", TIPO_NIVEL == "Postgrado")
# AdmitidosPOS <- Aspirantes %>% filter(INS_SEDE_NOMBRE == "Orinoquía", TIPO_NIVEL == "Postgrado", ADMITIDO == "Sí")

MatriculadosPRE <- Matriculados %>% filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == "Orinoquía")
MatriculadosPREFI <- Matriculados %>% filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == "Orinoquía", MOV_PEAMA == "Etapa Inicial") 
MatriculadosPREFM <- Matriculados %>% filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == "Orinoquía", MOV_PEAMA == "Etapa de movilidad")
# MatriculadosPOS <- Matriculados %>% filter(TIPO_NIVEL == "Postgrado", SEDE_NOMBRE_ADM == "Orinoquía", !(CONVENIO == "Sí" & TIP_CONVENIO == "Interno"))

GraduadosPRE <- Graduados %>% filter(TIPO_NIVEL == "Pregrado", SEDE_NOMBRE_ADM == 'Orinoquía')
# GraduadosPOS <- Graduados %>% filter(TIPO_NIVEL == "Postgrado", SEDE_NOMBRE_ADM == 'Orinoquía')

Docentes <- Docentes %>% filter(SEDE == 'Orinoquía')
Administrativos <- Administrativos %>% filter(SEDE == 'Orinoquía')

# Eliminar archivos CSV ----

unlink(c('AspirantesSPN.csv','MatriculadosSPN.csv', 
         'GraduadosSPN.csv', 'DocentesSPN.csv',
         'AdministrativosSPN.csv'))

# Transformaciones y factores ----

# Transformaciones

AspirantesPRE <- mutate(AspirantesPRE, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
AspirantesPRE <- mutate(AspirantesPRE, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 
AdmitidosPRE <- mutate(AdmitidosPRE, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
AdmitidosPRE <- mutate(AdmitidosPRE, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 


MatriculadosPRE <- mutate(MatriculadosPRE, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
MatriculadosPRE <- mutate(MatriculadosPRE, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 
MatriculadosPREFI <- mutate(MatriculadosPREFI, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
MatriculadosPREFI <- mutate(MatriculadosPREFI, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 
MatriculadosPREFM <- mutate(MatriculadosPREFM, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
MatriculadosPREFM <- mutate(MatriculadosPREFM, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 

GraduadosPRE <- mutate(GraduadosPRE, CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) 
GraduadosPRE <- mutate(GraduadosPRE, CAT_EDAD = ifelse(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)) 

GraduadosPRE <- GraduadosPRE %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
  mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))

# Factores

AspirantesPRE$CAT_EDAD <- factor(AspirantesPRE$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
AspirantesPRE$ESTRATO <- factor(AspirantesPRE$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
AspirantesPRE$ADMITIDO <- factor(AspirantesPRE$ADMITIDO, levels = c('No', 'Sí'))
# AspirantesPOS$CAT_EDAD <- factor(AspirantesPOS$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
# AspirantesPOS$ESTRATO <- factor(AspirantesPOS$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
# AspirantesPOS$ADMITIDO <- factor(AspirantesPOS$ADMITIDO, levels = c('No', 'Sí'))
AdmitidosPRE$CAT_EDAD <- factor(AdmitidosPRE$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
AdmitidosPRE$ESTRATO <- factor(AdmitidosPRE$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
# AdmitidosPOS$CAT_EDAD <- factor(AdmitidosPOS$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
# AdmitidosPOS$ESTRATO <- factor(AdmitidosPOS$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))


MatriculadosPRE$CAT_EDAD <- factor(MatriculadosPRE$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
MatriculadosPRE$ESTRATO <- factor(MatriculadosPRE$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
MatriculadosPRE$MOV_PEAMA <- factor(MatriculadosPRE$MOV_PEAMA, levels = c('Etapa de movilidad', 'Etapa Inicial'))
MatriculadosPRE$ADM_PEAMA_ANDINA <- factor(MatriculadosPRE$ADM_PEAMA_ANDINA, levels = c('Bogotá', 'Manizales', 'Medellín', 'Palmira'))
MatriculadosPREFI$CAT_EDAD <- factor(MatriculadosPREFI$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
MatriculadosPREFI$ESTRATO <- factor(MatriculadosPREFI$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
MatriculadosPREFI$ADM_PEAMA_ANDINA <- factor(MatriculadosPREFI$ADM_PEAMA_ANDINA, levels = c('Bogotá', 'Manizales', 'Medellín', 'Palmira'))
MatriculadosPREFM$CAT_EDAD <- factor(MatriculadosPREFM$CAT_EDAD, levels = c('17 años o menos', '18 a 20 años', '21 a 25 años', '26 o más años', 'Sin información'))
MatriculadosPREFM$ESTRATO <- factor(MatriculadosPREFM$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
MatriculadosPREFM$ADM_PEAMA_ANDINA <- factor(MatriculadosPREFM$ADM_PEAMA_ANDINA, levels = c('Bogotá', 'Manizales', 'Medellín', 'Palmira'))
# MatriculadosPOS$CAT_EDAD <- factor(MatriculadosPOS$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))
# MatriculadosPOS$SEXO <- factor(MatriculadosPOS$SEXO, levels = c('Hombres', 'Mujeres'))


GraduadosPRE$ADM_PEAMA_ANDINA <- factor(GraduadosPRE$ADM_PEAMA_ANDINA, levels = c('Bogotá', 'Medellín', 'Manizales', 'Palmira'))
GraduadosPRE$SEXO <- factor(GraduadosPRE$SEXO, levels = c('Hombres', 'Mujeres'))
GraduadosPRE$CAT_EDAD <- factor(GraduadosPRE$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))
GraduadosPRE$ESTRATO <- factor(GraduadosPRE$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
# GraduadosPOS$CAT_EDAD <- factor(GraduadosPOS$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


Docentes$SEXO <- factor(Docentes$SEXO, levels = c('Hombres', 'Mujeres'))
Docentes$CAT_EDAD <- factor(Docentes$CAT_EDAD, levels = c('29 años o menos', '30 a 39 años', '40 a 49 años', '50 a 59 años', '60 o más años'))
Docentes$FORMACION <- factor(Docentes$FORMACION, levels = c('Pregrado', 'Especialización', 'Especialidad Médica', 'Maestría', 'Doctorado'))

Administrativos$SEXO <- factor(Administrativos$SEXO, levels = c('Hombres', 'Mujeres'))
Administrativos$CAT_EDAD <- factor(Administrativos$CAT_EDAD, levels = c('29 años o menos', '30 a 39 años', '40 a 49 años', '50 a 59 años', '60 o más años'))
Administrativos$FORMACION <- factor(Administrativos$FORMACION, levels = c('Doctorado', 'Especialidad Médica', 'Especialización', 'Maestría', 'Pregrado', 'Secundaria o menos', 'Sin información', 'Técnico', 'Tecnología'))


# ASPIRANTES Y ADMITIDOS ----

# Aspirantes Pregrado ----

DT1 <- Clases(AspirantesPRE, "SEXO")
DT2 <- Clases(AspirantesPRE, "CAT_EDAD")
DT3 <- Clases(AspirantesPRE, "ESTRATO")
DT4 <- Clases(AspirantesPRE, "ADMITIDO")
TotalAspPre <- Total(AspirantesPRE)

Agregado_Asp_Pre <- bind_rows(DT1, DT2, DT3, DT4, TotalAspPre)

# Tablas aspirantes a pregrado

Asp_Pre_Municipios <- AspirantesPRE %>% group_by(YEAR, SEMESTRE, DEP_RES, CIU_RES) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_RES, CIU_RES, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   


# Admitidos Pregrado ----

DT1 <- Clases(AdmitidosPRE, "SEXO")
DT2 <- Clases(AdmitidosPRE, "CAT_EDAD")
DT3 <- Clases(AdmitidosPRE, "ESTRATO")
TotalAdmPre <- Total(AdmitidosPRE)

Agregado_Adm_Pre <- bind_rows(DT1, DT2, DT3, DT4, TotalAdmPre)

# Tablas matriculados en pregrado 

Adm_Pre_Municipios <- AdmitidosPRE %>% group_by(YEAR, SEMESTRE,DEP_RES, CIU_RES) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_RES, CIU_RES, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN")) 
Adm_Pre_Carreras <- AdmitidosPRE %>% group_by(YEAR, SEMESTRE, ADM_ANDINA_PEAMA, FACULTAD, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, ADM_ANDINA_PEAMA, FACULTAD, PROGRAMA, Total) 


# # Aspirantes Postgrado ----
# 
# DT1 <- Clases(AspirantesPOS, "SEXO") %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- expand.grid(Variable = unique(DT1$Variable), YEAR = unique(DT1$YEAR), SEMESTRE = unique(DT1$SEMESTRE), Clase = unique(DT1$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- left_join(DT1_C, select(DT1, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT1$YEAR[nrow(DT1)], DT1$SEMESTRE[nrow(DT1)]) != paste0(DT1_C$YEAR[nrow(DT1_C)], DT1_C$SEMESTRE[nrow(DT1_C)])) {DT1_C <- DT1_C[1:(nrow(DT1_C) - nrow(distinct(DT1_C, Clase))), ]}
# 
# DT2 <- Clases(AspirantesPOS, "CAT_EDAD") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT2_C <- expand.grid(Variable = unique(DT2$Variable), YEAR = unique(DT2$YEAR), SEMESTRE = unique(DT2$SEMESTRE), Clase = unique(DT2$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT2_C <- left_join(DT2_C, select(DT2, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT2$YEAR[nrow(DT2)], DT2$SEMESTRE[nrow(DT2)]) != paste0(DT2_C$YEAR[nrow(DT2_C)], DT2_C$SEMESTRE[nrow(DT2_C)])) {DT2_C <- DT2_C[1:(nrow(DT2_C) - nrow(distinct(DT2_C, Clase))), ]}
# 
# DT3 <- Clases(AspirantesPOS, "ADMITIDO") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT3_C <- expand.grid(Variable = unique(DT3$Variable), YEAR = unique(DT3$YEAR), SEMESTRE = unique(DT3$SEMESTRE), Clase = unique(DT3$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT3_C <- left_join(DT3_C, select(DT3, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT3$YEAR[nrow(DT3)], DT3$SEMESTRE[nrow(DT3)]) != paste0(DT3_C$YEAR[nrow(DT3_C)], DT3_C$SEMESTRE[nrow(DT3_C)])) {DT3_C <- DT3_C[1:(nrow(DT3_C) - nrow(distinct(DT3_C, Clase))), ]}
# 
# DT4 <- Clases(AspirantesPOS, "ESTRATO") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT4_C <- expand.grid(Variable = unique(DT4$Variable), YEAR = unique(DT4$YEAR), SEMESTRE = unique(DT4$SEMESTRE), Clase = unique(DT4$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT4_C <- left_join(DT4_C, select(DT4, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT4$YEAR[nrow(DT4)], DT4$SEMESTRE[nrow(DT4)]) != paste0(DT4_C$YEAR[nrow(DT4_C)], DT4_C$SEMESTRE[nrow(DT4_C)])) {DT4_C <- DT4_C[1:(nrow(DT4_C) - nrow(distinct(DT4_C, Clase))), ]}
# 
# TotalAspPos <- Total(AspirantesPOS) %>% unite("Id", Variable:Clase, remove = FALSE)
# TotalAspPos_C <- expand.grid(Variable = unique(TotalAspPos$Variable), YEAR = unique(TotalAspPos$YEAR), SEMESTRE = unique(TotalAspPos$SEMESTRE), Clase = unique(TotalAspPos$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# TotalAspPos_C <- left_join(TotalAspPos_C, select(TotalAspPos, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(TotalAspPos$YEAR[nrow(TotalAspPos)], TotalAspPos$SEMESTRE[nrow(TotalAspPos)]) != paste0(TotalAspPos_C$YEAR[nrow(TotalAspPos_C)], TotalAspPos_C$SEMESTRE[nrow(TotalAspPos_C)])) {TotalAspPos_C <- TotalAspPos_C[1:(nrow(TotalAspPos_C) - nrow(distinct(TotalAspPos_C, Clase))), ]}
# 
# Agregado_Asp_Pos <- bind_rows(DT1_C, DT2_C, DT3_C, DT4_C, TotalAspPos_C)
# Agregado_Asp_Pos$Clase <- as.character(Agregado_Asp_Pos$Clase)
# 
# # Tablas aspirantes a postgrado
# 
# Asp_Pos_Municipios <- AspirantesPOS %>% group_by(YEAR, SEMESTRE, DEP_RES, CIU_RES) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_RES, CIU_RES, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   
# 
# 
# # Admitidos a Postgrado ----
# 
# DT1 <- Clases(AdmitidosPOS, "SEXO") %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- expand.grid(Variable = unique(DT1$Variable), YEAR = unique(DT1$YEAR), SEMESTRE = unique(DT1$SEMESTRE), Clase = unique(DT1$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- left_join(DT1_C, select(DT1, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT1$YEAR[nrow(DT1)], DT1$SEMESTRE[nrow(DT1)]) != paste0(DT1_C$YEAR[nrow(DT1_C)], DT1_C$SEMESTRE[nrow(DT1_C)])) {DT1_C <- DT1_C[1:(nrow(DT1_C) - nrow(distinct(DT1_C, Clase))), ]}
# 
# DT2 <- Clases(AdmitidosPOS, "CAT_EDAD") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT2_C <- expand.grid(Variable = unique(DT2$Variable), YEAR = unique(DT2$YEAR), SEMESTRE = unique(DT2$SEMESTRE), Clase = unique(DT2$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT2_C <- left_join(DT2_C, select(DT2, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT2$YEAR[nrow(DT2)], DT2$SEMESTRE[nrow(DT2)]) != paste0(DT2_C$YEAR[nrow(DT2_C)], DT2_C$SEMESTRE[nrow(DT2_C)])) {DT2_C <- DT2_C[1:(nrow(DT2_C) - nrow(distinct(DT2_C, Clase))), ]}
# 
# DT3 <- Clases(AdmitidosPOS, "ESTRATO") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT3_C <- expand.grid(Variable = unique(DT3$Variable), YEAR = unique(DT3$YEAR), SEMESTRE = unique(DT3$SEMESTRE), Clase = unique(DT3$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT3_C <- left_join(DT3_C, select(DT3, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT3$YEAR[nrow(DT3)], DT3$SEMESTRE[nrow(DT3)]) != paste0(DT3_C$YEAR[nrow(DT3_C)], DT3_C$SEMESTRE[nrow(DT3_C)])) {DT3_C <- DT3_C[1:(nrow(DT3_C) - nrow(distinct(DT3_C, Clase))), ]}
# 
# TotalAdmPos <- Total(AdmitidosPOS) %>% unite("Id", Variable:Clase, remove = FALSE)
# TotalAdmPos_C <- expand.grid(Variable = unique(TotalAdmPos$Variable), YEAR = unique(TotalAdmPos$YEAR), SEMESTRE = unique(TotalAdmPos$SEMESTRE), Clase = unique(TotalAdmPos$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# TotalAdmPos_C <- left_join(TotalAdmPos_C, select(TotalAdmPos, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(TotalAdmPos$YEAR[nrow(TotalAdmPos)], TotalAdmPos$SEMESTRE[nrow(TotalAdmPos)]) != paste0(TotalAdmPos_C$YEAR[nrow(TotalAdmPos_C)], TotalAdmPos_C$SEMESTRE[nrow(TotalAdmPos_C)])) {TotalAdmPos_C <- TotalAdmPos_C[1:(nrow(TotalAdmPos_C) - nrow(distinct(TotalAdmPos_C, Clase))), ]}
# 
# Agregado_Adm_Pos <- bind_rows(DT1_C, DT2_C, DT3_C, TotalAdmPos_C)
# Agregado_Adm_Pos$Clase <- as.character(Agregado_Adm_Pos$Clase)
# 
# # Tablas admitidos a postgrado
# 
# Adm_Pos_Municipios <- AdmitidosPOS %>% group_by(YEAR, SEMESTRE,DEP_RES, CIU_RES) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_RES, CIU_RES, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN")) 
# Adm_Pos_Carreras <- AdmitidosPOS %>% group_by(YEAR, SEMESTRE, NIVEL, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, NIVEL, PROGRAMA, Total) 

# MATRICULADOS ----

# Matriculados Pregrado ----

DT1 <- Clases(MatriculadosPRE, "SEXO")
DT2 <- Clases(MatriculadosPRE, "CAT_EDAD")
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Clases(MatriculadosPRE, "ESTRATO")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT4 <- Clases(MatriculadosPRE, 'MOV_PEAMA')
DT5 <- Clases(MatriculadosPRE, 'ADM_PEAMA_ANDINA')
TotalMatPre <- Total(MatriculadosPRE)

Agregado_MatPre <- bind_rows(DT1, DT2, DT3, DT4, DT5, TotalMatPre)


# Tablas matriculados en pregrado 

Mat_Pre_Municipios <- MatriculadosPRE %>% group_by(YEAR, SEMESTRE,DEP_PROC, CIU_PROC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_PROC, CIU_PROC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN")) 
Mat_Pre_Carreras <- MatriculadosPRE %>% group_by(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA, Total) 


# Matriculados Pregrado Inicial ----

DT1 <- Clases(MatriculadosPREFI, "SEXO")
DT2 <- Clases(MatriculadosPREFI, "CAT_EDAD")
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Clases(MatriculadosPREFI, "ESTRATO")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT4 <- Clases(MatriculadosPREFI, 'ADM_PEAMA_ANDINA')
TotalMatPreFI <- Total(MatriculadosPREFI)

Agregado_MatPreFI <- bind_rows(DT1, DT2, DT3, DT4, TotalMatPreFI)


# Tablas matriculados en pregrado Inicial

Mat_Pre_FI_Municipios <- MatriculadosPREFI %>% group_by(YEAR, SEMESTRE,DEP_PROC, CIU_PROC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_PROC, CIU_PROC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   
Mat_Pre_FI_Carreras <- MatriculadosPREFI %>% group_by(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA, Total) 

# Matriculados Pregrado Movilidad ----

DT1 <- Clases(MatriculadosPREFM, "SEXO")
DT2 <- Clases(MatriculadosPREFM, "CAT_EDAD")
DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT3 <- Clases(MatriculadosPREFM, "ESTRATO")
DT3 <- mutate(DT3, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
DT4 <- Clases(MatriculadosPREFM, 'ADM_PEAMA_ANDINA')
TotalMatPreFM <- Total(MatriculadosPREFM)

Agregado_MatPreFM <- bind_rows(DT1, DT2, DT3, DT4, TotalMatPreFM)


# Tablas matriculados en pregrado Movilidad

Mat_Pre_FM_Municipios <- MatriculadosPREFM %>% group_by(YEAR, SEMESTRE,DEP_PROC, CIU_PROC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_PROC, CIU_PROC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   
Mat_Pre_FM_Carreras <- MatriculadosPREFM %>% group_by(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA, Total) 

# # Matriculados Postgrado----
# 
# DT1 <- Clases(MatriculadosPOS, "SEXO")
# DT2 <- Clases(MatriculadosPOS, "CAT_EDAD")
# DT2 <- mutate(DT2, Total = ifelse(YEAR == 2011 & SEMESTRE == 2, NA, Total))
# TotalMatPos <- Total(MatriculadosPOS)
# 
# Agregado_MatPos <- bind_rows(DT1, DT2, TotalMatPos)
# 
# # Tablas matriculados en postgrado
# 
# Mat_Pos_Municipios <- MatriculadosPOS %>% group_by(YEAR, SEMESTRE,DEP_PROC, CIU_PROC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE,DEP_PROC, CIU_PROC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN")) 
# Mat_Pos_Carreras <- MatriculadosPOS %>% group_by(YEAR, SEMESTRE, NIVEL, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, NIVEL, PROGRAMA, Total) 
# 

# GRADUADOS ----

# Graduados Pregrado ----

DT1 <- Clases(GraduadosPRE, "ADM_PEAMA_ANDINA") %>% unite("Id", Variable:Clase, remove = FALSE)
DT1_C <- expand.grid(Variable = unique(DT1$Variable), YEAR = unique(DT1$YEAR), SEMESTRE = unique(DT1$SEMESTRE), Clase = unique(DT1$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
DT1_C <- left_join(DT1_C, select(DT1, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
if(paste0(DT1$YEAR[nrow(DT1)], DT1$SEMESTRE[nrow(DT1)]) != paste0(DT1_C$YEAR[nrow(DT1_C)], DT1_C$SEMESTRE[nrow(DT1_C)])) {DT1_C <- DT1_C[1:(nrow(DT1_C) - nrow(distinct(DT1_C, Clase))), ]}

DT2 <- Clases(GraduadosPRE, "SEXO") %>% unite("Id", Variable:Clase, remove = FALSE)
DT2_C <- expand.grid(Variable = unique(DT2$Variable), YEAR = unique(DT2$YEAR), SEMESTRE = unique(DT2$SEMESTRE), Clase = unique(DT2$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
DT2_C <- left_join(DT2_C, select(DT2, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
if(paste0(DT2$YEAR[nrow(DT2)], DT2$SEMESTRE[nrow(DT2)]) != paste0(DT2_C$YEAR[nrow(DT2_C)], DT2_C$SEMESTRE[nrow(DT2_C)])) {DT2_C <- DT2_C[1:(nrow(DT2_C) - nrow(distinct(DT2_C, Clase))), ]}

DT3 <- Clases(GraduadosPRE, "CAT_EDAD") %>% unite("Id", Variable:Clase, remove = FALSE)
DT3_C <- expand.grid(Variable = unique(DT3$Variable), YEAR = unique(DT3$YEAR), SEMESTRE = unique(DT3$SEMESTRE), Clase = unique(DT3$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
DT3_C <- left_join(DT3_C, select(DT3, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
if(paste0(DT3$YEAR[nrow(DT3)], DT3$SEMESTRE[nrow(DT3)]) != paste0(DT3_C$YEAR[nrow(DT3_C)], DT3_C$SEMESTRE[nrow(DT3_C)])) {DT3_C <- DT3_C[1:(nrow(DT3_C) - nrow(distinct(DT3_C, Clase))), ]}

DT4 <- Clases(GraduadosPRE, "ESTRATO") %>% unite("Id", Variable:Clase, remove = FALSE)
DT4_C <- expand.grid(Variable = unique(DT4$Variable), YEAR = unique(DT4$YEAR), SEMESTRE = unique(DT4$SEMESTRE), Clase = unique(DT4$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
DT4_C <- left_join(DT4_C, select(DT4, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
if(paste0(DT4$YEAR[nrow(DT4)], DT4$SEMESTRE[nrow(DT4)]) != paste0(DT4_C$YEAR[nrow(DT4_C)], DT4_C$SEMESTRE[nrow(DT4_C)])) {DT4_C <- DT4_C[1:(nrow(DT4_C) - nrow(distinct(DT4_C, Clase))), ]}

TotalGraPre <- Total(GraduadosPRE) %>% unite("Id", Variable:Clase, remove = FALSE)
TotalGraPre_C <- expand.grid(Variable = unique(TotalGraPre$Variable), YEAR = unique(TotalGraPre$YEAR), SEMESTRE = unique(TotalGraPre$SEMESTRE), Clase = unique(TotalGraPre$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
TotalGraPre_C <- left_join(TotalGraPre_C, select(TotalGraPre, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
if(paste0(TotalGraPre$YEAR[nrow(TotalGraPre)], TotalGraPre$SEMESTRE[nrow(TotalGraPre)]) != paste0(TotalGraPre_C$YEAR[nrow(TotalGraPre_C)], TotalGraPre_C$SEMESTRE[nrow(TotalGraPre_C)])) {TotalGraPre_C <- TotalGraPre_C[1:(nrow(TotalGraPre_C) - nrow(distinct(TotalGraPre_C, Clase))), ]}

Agregado_GraPre <- bind_rows(DT1_C, DT2_C, DT3_C, DT4_C, TotalGraPre_C)
Agregado_GraPre$Clase <- as.character(Agregado_GraPre$Clase)

# Tablas Graduados en pregrado

Gra_Pre_Municipios <- GraduadosPRE %>% group_by(YEAR, SEMESTRE, DEP_NAC, CIU_NAC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, DEP_NAC, CIU_NAC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   
Gra_Pre_Carreras <- GraduadosPRE %>% group_by(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, ADM_PEAMA_ANDINA, FACULTAD, PROGRAMA, Total) 


# # Graduados Postgrado ----
# 
# DT1 <- Clases(GraduadosPOS, "SEXO") %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- expand.grid(Variable = unique(DT1$Variable), YEAR = unique(DT1$YEAR), SEMESTRE = unique(DT1$SEMESTRE), Clase = unique(DT1$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT1_C <- left_join(DT1_C, select(DT1, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT1$YEAR[nrow(DT1)], DT1$SEMESTRE[nrow(DT1)]) != paste0(DT1_C$YEAR[nrow(DT1_C)], DT1_C$SEMESTRE[nrow(DT1_C)])) {DT1_C <- DT1_C[1:(nrow(DT1_C) - nrow(distinct(DT1_C, Clase))), ]}
# 
# DT2 <- Clases(GraduadosPOS, "CAT_EDAD") %>% unite("Id", Variable:Clase, remove = FALSE)
# DT2_C <- expand.grid(Variable = unique(DT2$Variable), YEAR = unique(DT2$YEAR), SEMESTRE = unique(DT2$SEMESTRE), Clase = unique(DT2$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# DT2_C <- left_join(DT2_C, select(DT2, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(DT2$YEAR[nrow(DT2)], DT2$SEMESTRE[nrow(DT2)]) != paste0(DT2_C$YEAR[nrow(DT2_C)], DT2_C$SEMESTRE[nrow(DT2_C)])) {DT2_C <- DT2_C[1:(nrow(DT2_C) - nrow(distinct(DT2_C, Clase))), ]}
# 
# TotalGraPos <- Total(GraduadosPOS) %>% unite("Id", Variable:Clase, remove = FALSE)
# TotalGraPos_C <- expand.grid(Variable = unique(TotalGraPos$Variable), YEAR = unique(TotalGraPos$YEAR), SEMESTRE = unique(TotalGraPos$SEMESTRE), Clase = unique(TotalGraPos$Clase)) %>% arrange(YEAR) %>% unite("Id", Variable:Clase, remove = FALSE) 
# TotalGraPos_C <- left_join(TotalGraPos_C, select(TotalGraPos, c("Id", "Total")), by = "Id") %>% replace_na(list(Total = 0)) %>% select(-c('Id')) %>% arrange(YEAR, SEMESTRE)
# if(paste0(TotalGraPos$YEAR[nrow(TotalGraPos)], TotalGraPos$SEMESTRE[nrow(TotalGraPos)]) != paste0(TotalGraPos_C$YEAR[nrow(TotalGraPos_C)], TotalGraPos_C$SEMESTRE[nrow(TotalGraPos_C)])) {TotalGraPos_C <- TotalGraPos_C[1:(nrow(TotalGraPos_C) - nrow(distinct(TotalGraPos_C, Clase))), ]}
# 
# Agregado_GraPos <- bind_rows(DT1_C, DT2_C, TotalGraPos_C)
# Agregado_GraPos$Clase <- as.character(Agregado_GraPos$Clase)
# 
# 
# # Tablas Graduados en postgrado
# 
# Gra_Pos_Municipios <- GraduadosPOS %>% group_by(YEAR, SEMESTRE, DEP_NAC, CIU_NAC) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, DEP_NAC, CIU_NAC, Total) %>% ungroup() %>% mutate(across(where(is.character), replace_na,  replace = "SIN INFORMACIÓN"))   
# Gra_Pos_Carreras <- GraduadosPOS %>% group_by(YEAR, SEMESTRE, NIVEL, PROGRAMA) %>% summarise(Total=n()) %>% select(YEAR, SEMESTRE, NIVEL, PROGRAMA, Total) 

# DOCENTES ----

# Docentes Carrera ----

DT1 <- Clases(Docentes, "SEXO") 
DT2 <- Clases(Docentes, "CAT_EDAD") 
DT3 <- Clases(Docentes, "FORMACION") 
TotalDoc <- Total(Docentes)

Agregado_Doc <- bind_rows(DT1, DT2, DT3, TotalDoc)

# ADMINISTRATIVOS ----

# Administrativos Carrera ----

DT1 <- Clases(Administrativos, "SEXO")
DT2 <- Clases(Administrativos, "CAT_EDAD") 
DT3 <- Clases(Administrativos, "FORMACION") 

TotalAdmi <- Total(Administrativos)

Agregado_Admi <- bind_rows(DT1, DT2, DT3, TotalAdmi)
