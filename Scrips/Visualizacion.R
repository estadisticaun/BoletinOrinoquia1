library(highcharter) # version 0.5.0.9999
library(DT)

# ASPIRANTES Y ADMITIDOS----

# Aspirantes a Pregrado ----

ano <- max(Agregado_Asp_Pre %>% select(YEAR))
semestre <- Agregado_Asp_Pre[[nrow(Agregado_Asp_Pre), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total
SERIE_ASP_PRE <- series(
  datos = Agregado_Asp_Pre,
  categoria = "TOTAL",
  colores = col,
  titulo = "Evolución histórica del número total de aspirantes a pregrado",
  eje = "Número de aspirantes"
)           

# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

# Serie de tiempo

SEXO_SERIE_ASP_PRE <- series(
  datos = Agregado_Asp_Pre,
  categoria = "SEXO",
  colores = col,
  titulo = "Evolución del número de aspirantes a pregrado por sexo",
  eje = "Número de aspirantes"
)

# Estado actual 

SEXO_ACTUAL_ASP_PRE <- torta(
  datos = Agregado_Asp_Pre,
  variable = "SEXO",
  colores = col,
  titulo = "Distribución de aspirantes a pregrado por sexo",
  etiqueta = "Número de aspirantes",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
)

# Edad ----


col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información

# Serie de tiempo

EDAD_SERIE_ASP_PRE <- series(
  datos = Agregado_Asp_Pre, 
  categoria = "CAT_EDAD", 
  colores = col, 
  titulo = "Evolución del número de aspirantes a pregrado por grupos de edad", 
  eje = "Número de aspirantes")
  

# Estado actual

EDAD_ACTUAL_ASP_PRE <- barra_vertical_ord(
  datos = Agregado_Asp_Pre, 
  categoria = "CAT_EDAD", 
  colores = col, 
  ano = ano, 
  periodo = semestre, 
  periodo_titulo = periodo_actual_titulo, 
  titulo = "Distribución de aspirantes a pregrado por grupos de edad", 
  eje = "Número de aspirantes")


# Estrato ----

col <-   c( "#8cc63f", # verde, estrato 2 o menos
            "#f15a24", # naranja, estrato 3
            "#0071bc", # azul vivo, estrato 4 o más
            "#6d6666") # gris, ND/NE

# Serie de tiempo

ESTRATO_SERIE_ASP_PRE <- series(
  datos = Agregado_Asp_Pre, 
  categoria = "ESTRATO", 
  colores = col, 
  titulo = "Evolúción del número de aspirantes a pregrado por estrato socioeconómico", 
  eje = "Número de aspirantes")

# Estado actual

ESTRATO_ACTUAL_ASP_PRE <- torta(
  datos = Agregado_Asp_Pre, 
  variable = "ESTRATO", 
  colores = col, 
  titulo = "Distribución de aspirantes a pregrado por estrato socioeconómico",
  etiqueta = "Número de aspirantes", 
  ano = ano, 
  periodo = semestre, 
  periodo_titulo = periodo_actual_titulo)

# Cobertura ----


col <-   c( "#8cc63f", # verde, No
            "#f15a24") # naranja, Sí

# Serie de tiempo

ADMITIDO_SERIE_ASP_PRE <- series(
  datos = Agregado_Asp_Pre, 
  categoria = "ADMITIDO", 
  colores = col, 
  titulo = "Evolución del número de aspirantes admitidos a pregrado", 
  eje = "Número de aspirantes")

# Estado actual

ADMITIDO_ACTUAL_ASP_PRE <- torta(
  datos = Agregado_Asp_Pre, 
  variable = "ADMITIDO",
  colores = col, 
  titulo = "Distribución de aspirantes admitidos a pregado",
  etiqueta = "Número de aspirantes", 
  ano = ano,
  periodo = semestre, 
  periodo_titulo = periodo_actual_titulo)


# Tablas ----

TABLA_MUNICIPIOS_ASP_PRE <- tabla_n(
  datos = Asp_Pre_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de residencia del aspirante',
  mensaje = "Número de aspirantes por lugar de residencia",
  titulo = "Histórico total de aspirantes por departamento y municipio de residencia"  
)


# Admitidos a Pregrado ----

ano <- max(Agregado_Adm_Pre %>% select(YEAR))
semestre <- Agregado_Adm_Pre[[nrow(Agregado_Adm_Pre), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total

SERIE_ADM_PRE <- series(
  datos = Agregado_Adm_Pre,
  categoria = "TOTAL",
  colores = col,
  titulo = "Evolución histórica del número total de admitidos a pregrado",
  eje = "Número de admitidos"
)           

# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

# Serie de tiempo

SEXO_SERIE_ADM_PRE <- series(
  datos = Agregado_Adm_Pre,
  categoria = "SEXO",
  colores = col,
  titulo = "Evolución del número de admitidos a pregrado por sexo",
  eje = "Número de admitidos"
)

# Estado actual 

SEXO_ACTUAL_ADM_PRE <- torta(
  datos = Agregado_Adm_Pre,
  variable = "SEXO",
  colores = col,
  titulo = "Distribución de admitidos a pregrado por sexo",
  etiqueta = "Número de admitidos",
  ano = ano,
  periodo = semestre,
  periodo_titulo = periodo_actual_titulo
)

# Edad ----


col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información

# Serie de tiempo

EDAD_SERIE_ADM_PRE <- series(
  datos = Agregado_Adm_Pre, 
  categoria = "CAT_EDAD", 
  colores = col, 
  titulo = "Evolución del número de admitidos a pregrado por grupos de edad", 
  eje = "Número de admitidos")


# Estado actual

EDAD_ACTUAL_ADM_PRE <- barra_vertical_ord(
  datos = Agregado_Adm_Pre, 
  categoria = "CAT_EDAD", 
  colores = col, 
  ano = ano, 
  periodo = semestre, 
  periodo_titulo = periodo_actual_titulo, 
  titulo = "Distribución de admitidos a pregrado por grupos de edad", 
  eje = "Número de admitidos")


# Estrato ----

col <-   c( "#8cc63f", # verde, estrato 2 o menos
            "#f15a24", # naranja, estrato 3
            "#0071bc", # azul vivo, estrato 4 o más
            "#6d6666") # gris, ND/NE

# Serie de tiempo

ESTRATO_SERIE_ADM_PRE <- series(
  datos = Agregado_Adm_Pre, 
  categoria = "ESTRATO", 
  colores = col, 
  titulo = "Evolúción del número de admitidos a pregrado por estrato socioeconómico", 
  eje = "Número de admitidos")

# Estado actual

ESTRATO_ACTUAL_ADM_PRE <- torta(
  datos = Agregado_Adm_Pre, 
  variable = "ESTRATO", 
  colores = col, 
  titulo = "Distribución de admitidos a pregrado por estrato socioeconómico",
  etiqueta = "Número de admitidos", 
  ano = ano, 
  periodo = semestre, 
  periodo_titulo = periodo_actual_titulo)


# Tablas ----


TABLA_MUNICIPIOS_ADM_PRE <- tabla_n(
  datos = Adm_Pre_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de residencia del admitido',
  mensaje = "Número de admitidos por lugar de residencia",
  titulo = "Histórico total de admitidos por departamento y municipio de residencia"  
)

TABLA_CARRERAS_ADM_PRE <- tabla_n(
  datos = Adm_Pre_Carreras,
  categorias = c("Año","Semestre","Sede Andina","Facultad","Programa", "Total"),
  variable = 'Sede Andina, Facultad y Programas Académicos del matriculado en pregrado',
  mensaje = "Número de admitidos a pregrado por Sede, Facultad y Programa",
  titulo = "Histórico total de admitidos a pregrado por sede andina, facultad y programas"
)


# # Aspirantes a Postgrado ----
# 
# ano <- max(Agregado_Asp_Pos %>% select(YEAR))
# semestre <- Agregado_Asp_Pos[[nrow(Agregado_Asp_Pos), "SEMESTRE"]]
# periodo_actual_titulo <- paste0(" ", ano, "-", semestre)
# 
# # Serie histórica ----
# 
# col <-   c("#f15a24") # verde, Total
# 
# SERIE_ASP_POS <- series(
#   datos = Agregado_Asp_Pos,
#   categoria = "TOTAL",
#   colores = col,
#   titulo = "Evolución histórica del número total de aspirantes a postgrado",
#   eje = "Número de aspirantes"
# )           
# 
# # Sexo ----
# 
# col <-   c( "#f15a24", # naranja, hombres
#             "#8cc63f") # verde, mujeres
# 
# # Serie de tiempo
# 
# SEXO_SERIE_ASP_POS <- series(
#   datos = Agregado_Asp_Pos,
#   categoria = "SEXO",
#   colores = col,
#   titulo = "Evolución del número de aspirantes a postgrado por sexo",
#   eje = "Número de aspirantes"
# )
# 
# # Estado actual 
# 
# SEXO_ACTUAL_ASP_POS <- torta(
#   datos = Agregado_Asp_Pos,
#   variable = "SEXO",
#   colores = col,
#   titulo = "Distribución de aspirantes a postgrado por sexo",
#   etiqueta = "Número de aspirantes",
#   ano = ano,
#   periodo = semestre,
#   periodo_titulo = periodo_actual_titulo
# )
# 
# # Edad ----
# 
# col <-   c( "#6d6666", # gris, 25 años o menos
#             "#8cc63f", # verde, 26 a 30 años
#             "#0071bc", # azul vivo, 31 a 35 años
#             "#f15a24", # naranja, 36 años o más
#             "#fbb03b" ) # amarillo, sin información
# 
# # Serie de tiempo
# 
# EDAD_SERIE_ASP_POS <- series(
#   datos = Agregado_Asp_Pos, 
#   categoria = "CAT_EDAD", 
#   colores = col, 
#   titulo = "Evolución del número de aspirantes a postgrado por grupos de edad", 
#   eje = "Número de aspirantes")
# 
# 
# # Estado actual
# 
# EDAD_ACTUAL_ASP_POS <- barra_vertical_ord(
#   datos = Agregado_Asp_Pos, 
#   categoria = "CAT_EDAD", 
#   colores = col, 
#   ano = ano, 
#   periodo = semestre, 
#   periodo_titulo = periodo_actual_titulo, 
#   titulo = "Distribución de aspirantes a postgrado por grupos de edad", 
#   eje = "Número de aspirantes")
# 
# # Estrato ----
# 
# col <-   c( "#8cc63f", # verde, estrato 2 o menos
#             "#f15a24", # naranja, estrato 3
#             "#0071bc", # azul vivo, estrato 4 o más
#             "#6d6666") # gris, ND/NE
# 
# # Serie de tiempo
# 
# ESTRATO_SERIE_ASP_POS <- series(
#   datos = Agregado_Asp_Pos, 
#   categoria = "ESTRATO", 
#   colores = col, 
#   titulo = "Evolúción del número de aspirantes a postgrado por estrato socioeconómico", 
#   eje = "Número de aspirantes")
# 
# # Estado actual
# 
# ESTRATO_ACTUAL_ASP_POS <- torta(
#   datos = Agregado_Asp_Pos, 
#   variable = "ESTRATO", 
#   colores = col, 
#   titulo = "Distribución de aspirantes a postgrado por estrato socioeconómico",
#   etiqueta = "Número de aspirantes", 
#   ano = ano, 
#   periodo = semestre, 
#   periodo_titulo = periodo_actual_titulo)
# 
# 
# # Cobertura ----
# 
# 
# col <-   c( "#8cc63f", # verde, No
#             "#f15a24") # naranja, Sí
# 
# # Serie de tiempo
# 
# ADMITIDO_SERIE_ASP_POS <- series(
#   datos = Agregado_Asp_Pos, 
#   categoria = "ADMITIDO", 
#   colores = col, 
#   titulo = "Evolución del número de aspirantes admitidos a postgrado", 
#   eje = "Número de aspirantes")
# 
# # Estado actual
# 
# ADMITIDO_ACTUAL_ASP_POS <- torta(
#   datos = Agregado_Asp_Pos, 
#   variable = "ADMITIDO",
#   colores = col, 
#   titulo = "Distribución de aspirantes admitidos a postgrado",
#   etiqueta = "Número de aspirantes", 
#   ano = ano,
#   periodo = semestre, 
#   periodo_titulo = periodo_actual_titulo)
# 
# # Tablas ----
# 
# TABLA_MUNICIPIOS_ASP_POS <- tabla_n(
#   datos = Asp_Pos_Municipios,
#   categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
#   variable = 'Lugar de residencia del aspirante',
#   mensaje = "Número de aspirantes por lugar de residencia",
#   titulo = "Histórico total de aspirantes por departamento y municipio de residencia"  
# )
# 
# 
# # Admitidos a Postgrado ----
# 
# ano <- max(Agregado_Adm_Pos %>% select(YEAR))
# semestre <- Agregado_Adm_Pos[[nrow(Agregado_Adm_Pos), "SEMESTRE"]]
# periodo_actual_titulo <- paste0(" ", ano, "-", semestre)
# 
# # Serie histórica ----
# 
# col <-   c( "#8cc63f") # verde, Total
# 
# SERIE_ADM_POS <- series(
#   datos = Agregado_Adm_Pos,
#   categoria = "TOTAL",
#   colores = col,
#   titulo = "Evolución histórica del número total de admitidos a postgrado",
#   eje = "Número de admitidos"
# )           
# 
# # Sexo ----
# 
# col <-   c( "#f15a24", # naranja, hombres
#             "#8cc63f") # verde, mujeres
# 
# # Serie de tiempo
# 
# SEXO_SERIE_ADM_POS <- series(
#   datos = Agregado_Adm_Pos,
#   categoria = "SEXO",
#   colores = col,
#   titulo = "Evolución del número de admitidos a postgrado por sexo",
#   eje = "Número de admitidos"
# )
# 
# # Estado actual 
# 
# SEXO_ACTUAL_ADM_POS <- torta(
#   datos = Agregado_Adm_Pos,
#   variable = "SEXO",
#   colores = col,
#   titulo = "Distribución de admitidos a postgrado por sexo",
#   etiqueta = "Número de admitidos",
#   ano = ano,
#   periodo = semestre,
#   periodo_titulo = periodo_actual_titulo
# )
# 
# # Edad ----
# 
# 
# col <-   c( "#8cc63f", # verde, 17 o menos
#             "#f15a24", # naranja,  18 a 20 
#             "#0071bc", # azul vivo, 21 a 25
#             "#6d6666", # gris, 26 o más
#             "#fbb03b" ) # amarillo, sin información
# 
# # Serie de tiempo
# 
# EDAD_SERIE_ADM_POS <- series(
#   datos = Agregado_Adm_Pos, 
#   categoria = "CAT_EDAD", 
#   colores = col, 
#   titulo = "Evolución del número de admitidos a postgrado por grupos de edad", 
#   eje = "Número de admitidos")
# 
# 
# # Estado actual
# 
# EDAD_ACTUAL_ADM_POS <- barra_vertical_ord(
#   datos = Agregado_Adm_Pos, 
#   categoria = "CAT_EDAD", 
#   colores = col, 
#   ano = ano, 
#   periodo = semestre, 
#   periodo_titulo = periodo_actual_titulo, 
#   titulo = "Distribución de admitidos a postgrado por grupos de edad", 
#   eje = "Número de admitidos")
# 
# 
# # Estrato ----
# 
# col <-   c( "#8cc63f", # verde, estrato 2 o menos
#             "#f15a24", # naranja, estrato 3
#             "#0071bc", # azul vivo, estrato 4 o más
#             "#6d6666") # gris, ND/NE
# 
# # Serie de tiempo
# 
# ESTRATO_SERIE_ADM_POS <- series(
#   datos = Agregado_Adm_Pos, 
#   categoria = "ESTRATO", 
#   colores = col, 
#   titulo = "Evolúción del número de admitidos a postgrado por estrato socioeconómico", 
#   eje = "Número de admitidos")
# 
# # Estado actual
# 
# ESTRATO_ACTUAL_ADM_POS <- torta(
#   datos = Agregado_Adm_Pos, 
#   variable = "ESTRATO", 
#   colores = col, 
#   titulo = "Distribución de admitidos a postgrado por estrato socioeconómico",
#   etiqueta = "Número de admitidos", 
#   ano = ano, 
#   periodo = semestre, 
#   periodo_titulo = periodo_actual_titulo)
# 
# # Tablas ----
# 
# TABLA_MUNICIPIOS_ADM_POS <- tabla_n(
#   datos = Adm_Pos_Municipios,
#   categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
#   variable = 'Lugar de residencia del admitido',
#   mensaje = "Número de admitidos por lugar de residencia",
#   titulo = "Histórico total de admitidos por departamento y municipio de residencia"  
# )
# 
# TABLA_CARRERAS_ADM_POS <- tabla_n(
#   datos = Adm_Pos_Carreras,
#   categorias = c("Año","Semestre","Nivel de formación","Programa académico", "Total"),
#   variable = 'Nivel de formación y programas académicos de admitidos a postgrado',
#   mensaje = "Número de admitidos a postgrado por Nivel de Formación y Programas",
#   titulo = "Histórico total de admitidos a postgrado por Nivel de Formación y Programas"
# )

# MATRICULADOS ----

# Matriculados en Pregrado ----

ano <- max(Agregado_MatPre %>% select(YEAR))
semestre <- Agregado_MatPre[[nrow(Agregado_MatPre), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total

SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de matriculados en pregrado", eje = "Número de matriculados")           

# Etapa ----

col <-   c( "#f15a24", # naranja, Etapa de movilidad
            "#8cc63f") # verde, Etapa inicial

MOV_PEAMA_SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "MOV_PEAMA", colores = col, titulo = "Evolución del total de matriculados en pregrado según etapa de formación", eje = "Número de estudiantes (k: miles)")
MOV_PEAMA_ACTUAL_MAT_PRE <- torta(datos = Agregado_MatPre, variable = "MOV_PEAMA", colores = col, titulo = "Distribución del total de matriculados en pregrado según etapa de formación", etiqueta = "Número de estudiantes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)

# Sede Andina ----


col <-   c( "#8cc63f", # verde, Bogotá
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#93278f"  # Morado, Palmira
          
) 


SEDE_ANDINA_SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "ADM_PEAMA_ANDINA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según sede andina de ubicación", eje = "Número de estudiantes (k: miles)")
SEDE_ANDINA_ACTUAL_MAT_PRE <- barra_vertical(datos = Agregado_MatPre, categoria = "ADM_PEAMA_ANDINA",colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por sede andina de ubicación", eje = "Número de estudiantes")


# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "SEXO", colores = col, titulo = "Evolución del número de matriculados en pregrado por sexo", eje = "Número de matriculados")
SEXO_ACTUAL_MAT_PRE <- torta(datos = Agregado_MatPre, variable = "SEXO", colores = col, titulo = "Distribución de matriculados en pregrado por sexo", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)


# Edad ----

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


EDAD_SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)")
EDAD_ACTUAL_MAT_PRE <- barra_vertical_ord(datos = Agregado_MatPre, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad", eje = "Número de estudiantes (k: miles)")


# Estrato ----

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_SERIE_MAT_PRE <- series(datos = Agregado_MatPre, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico", eje = "Número de estudiantes (k: miles)")
ESTRATO_ACTUAL_MAT_PRE <- barra_vertical_ord(datos = Agregado_MatPre, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato", eje = "Número de estudiantes (k: miles)")


# Tablas ----


TABLA_MUNICIPIOS_MAT_PRE <- tabla_n(
  datos = Mat_Pre_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de residencia del matriculado en pregrado',
  mensaje = "Número de matriculados en pregrado por lugar de procedencia",
  titulo = "Histórico total de matriculados en pregrado por departamento y municipio de procedencia"  
)


TABLA_CARRERAS_MAT_PRE <- tabla_n(
  datos = Mat_Pre_Carreras,
  categorias = c("Año","Semestre","Sede Andina","Facultad","Programa", "Total"),
  variable = 'Sede Andina, Facultad y Programas Académicos del matriculado en pregrado',
  mensaje = "Número de matriculados en pregrado por Sede, Facultad y Programa",
  titulo = "Histórico total de matriculados en pregrado por sede andina, facultad y programas"
)

# Matriculados Pregrado Etapa Inicial ----

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total

SERIE_MAT_PRE_FI <- series(datos = Agregado_MatPreFI, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de matriculados en pregrado - Etapa Inicial", eje = "Número de matriculados")           

# Sede Andina ----


col <-   c( "#8cc63f", # verde, Bogotá
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#93278f"  # Morado, Palmira
            
) 


SEDE_ANDINA_SERIE_MAT_PRE_FI <- series(datos = Agregado_MatPreFI, categoria = "ADM_PEAMA_ANDINA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según sede andina de ubicación - Etapa Inicial", eje = "Número de estudiantes (k: miles)")
SEDE_ANDINA_ACTUAL_MAT_PRE_FI <- barra_vertical(datos = Agregado_MatPreFI, categoria = "ADM_PEAMA_ANDINA",colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por sede andina de ubicación - Etapa Inicial -", eje = "Número de estudiantes")


# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_SERIE_MAT_PRE_FI <- series(datos = Agregado_MatPreFI, categoria = "SEXO", colores = col, titulo = "Evolución del número de matriculados en pregrado por sexo - Etapa Inicial", eje = "Número de matriculados")
SEXO_ACTUAL_MAT_PRE_FI <- torta(datos = Agregado_MatPreFI, variable = "SEXO", colores = col, titulo = "Distribución de matriculados en pregrado por sexo - Etapa Inicial -", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)


# Edad ----

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


EDAD_SERIE_MAT_PRE_FI <- series(datos = Agregado_MatPreFI, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - Etapa Inicial", eje = "Número de estudiantes (k: miles)")
EDAD_ACTUAL_MAT_PRE_FI <- barra_vertical_ord(datos = Agregado_MatPreFI, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - Etapa Inicial -", eje = "Número de estudiantes (k: miles)")


# Estrato ----

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_SERIE_MAT_PRE_FI <- series(datos = Agregado_MatPreFI, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - Etapa Inicial", eje = "Número de estudiantes (k: miles)")
ESTRATO_ACTUAL_MAT_PRE_FI <- barra_vertical_ord(datos = Agregado_MatPreFI, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - Etapa Inicial -", eje = "Número de estudiantes (k: miles)")


# Tablas ----


TABLA_MUNICIPIOS_MAT_PRE_FI <- tabla_n(
  datos = Mat_Pre_FI_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de residencia del matriculado en pregrado - Etapa Inicial -',
  mensaje = "Número de matriculados en pregrado por lugar de procedencia - Etapa Inicial -",
  titulo = "Histórico total de matriculados en pregrado por departamento y municipio de procedencia - Etapa Inicial -"  
)


TABLA_CARRERAS_MAT_PRE_FI <- tabla_n(
  datos = Mat_Pre_FI_Carreras,
  categorias = c("Año","Semestre","Sede Andina","Facultad","Programa", "Total"),
  variable = 'Sede Andina, Facultad y Programas Académicos del matriculado en pregrado -Etapa Inciial -',
  mensaje = "Número de matriculados en pregrado por Sede, Facultad y Programa - Etapa Inicial -",
  titulo = "Histórico total de matriculados en pregrado por sede andina, facultad y programas - Etapa Inicial -"
)

# Matriculados Preg. Etapa Movilidad ----

# Serie histórica ----

col <-   c("#f15a24") # Naranja, Total

SERIE_MAT_PRE_FM <- series(datos = Agregado_MatPreFM, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de matriculados en pregrado - Etapa Movilidad", eje = "Número de matriculados")           

# Sede Andina ----


col <-   c( "#8cc63f", # verde, Bogotá
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#93278f"  # Morado, Palmira
            
) 


SEDE_ANDINA_SERIE_MAT_PRE_FM <- series(datos = Agregado_MatPreFM, categoria = "ADM_PEAMA_ANDINA", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado según sede andina de ubicación - Etapa Movilidad", eje = "Número de estudiantes (k: miles)")
SEDE_ANDINA_ACTUAL_MAT_PRE_FM <- barra_vertical(datos = Agregado_MatPreFM, categoria = "ADM_PEAMA_ANDINA",colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por sede andina de ubicación - Etapa Movilidad -", eje = "Número de estudiantes")


# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_SERIE_MAT_PRE_FM <- series(datos = Agregado_MatPreFM, categoria = "SEXO", colores = col, titulo = "Evolución del número de matriculados en pregrado por sexo - Etapa Movilidad", eje = "Número de matriculados")
SEXO_ACTUAL_MAT_PRE_FM <- torta(datos = Agregado_MatPreFM, variable = "SEXO", colores = col, titulo = "Distribución de matriculados en pregrado por sexo - Etapa Movilidad -", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)


# Edad ----

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


EDAD_SERIE_MAT_PRE_FM <- series(datos = Agregado_MatPreFM, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por grupos de edad - Etapa Movilidad", eje = "Número de estudiantes (k: miles)")
EDAD_ACTUAL_MAT_PRE_FM <- barra_vertical_ord(datos = Agregado_MatPreFM, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por grupos de edad - Etapa Movilidad -", eje = "Número de estudiantes (k: miles)")


# Estrato ----

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_SERIE_MAT_PRE_FM <- series(datos = Agregado_MatPreFM, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de estudiantes matriculados en pregrado por estrato socioeconómico - Etapa Movilidad", eje = "Número de estudiantes (k: miles)")
ESTRATO_ACTUAL_MAT_PRE_FM <- barra_vertical_ord(datos = Agregado_MatPreFM, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en pregrado por estrato - Etapa Movilidad -", eje = "Número de estudiantes (k: miles)")


# Tablas ----


TABLA_MUNICIPIOS_MAT_PRE_FM <- tabla_n(
  datos = Mat_Pre_FM_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de residencia del matriculado en pregrado - Etapa Movilidad -',
  mensaje = "Número de matriculados en pregrado por lugar de procedencia - Etapa Movilidad -",
  titulo = "Histórico total de matriculados en pregrado por departamento y municipio de procedencia - Etapa Movilidad -"  
)


TABLA_CARRERAS_MAT_PRE_FM <- tabla_n(
  datos = Mat_Pre_FM_Carreras,
  categorias = c("Año","Semestre","Sede Andina","Facultad","Programa", "Total"),
  variable = 'Sede Andina, Facultad y Programas Académicos del matriculado en pregrado -Etapa Inciial -',
  mensaje = "Número de matriculados en pregrado por Sede, Facultad y Programa - Etapa Movilidad -",
  titulo = "Histórico total de matriculados en pregrado por sede andina, facultad y programas - Etapa Movilidad -"
)

# # Matriculados en Postgrado ----
# 
# ano <- max(Agregado_MatPos %>% select(YEAR))
# semestre <- Agregado_MatPos[[nrow(Agregado_MatPos), "SEMESTRE"]]
# periodo_actual_titulo <- paste0(" ", ano, "-", semestre)
# 
# # Serie histórica ----
# 
# col <-   c("#f15a24") # naranja, Total
# 
# SERIE_MAT_POS <- series(datos = Agregado_MatPos, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de matriculados en postgrado", eje = "Número de matriculados")           
# 
# # Sexo ----
# 
# col <-   c( "#f15a24", # naranja, hombres
#             "#8cc63f") # verde, mujeres
# 
# 
# SEXO_SERIE_MAT_POS <- series(datos = Agregado_MatPos, categoria = "SEXO", colores = col, titulo = "Evolución del número de matriculados en postgrado por sexo", eje = "Número de matriculados")
# SEXO_ACTUAL_MAT_POS <- torta(datos = Agregado_MatPos, variable = "SEXO", colores = col, titulo = "Distribución de matriculados en postgrado por sexo", etiqueta = "Número de matriculados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)
# 
# 
# # Edad ----
# 
# col <-   c( "#6d6666", # gris, 
#             "#8cc63f", # verde, 
#             "#0071bc", # azul vivo, 
#             "#f15a24", # naranja,
#             "#fbb03b" ) # amarillo, sin información
# 
# EDAD_SERIE_MAT_POS <- series(datos = Agregado_MatPos, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de estudiantes matriculados en postgrado por grupos de edad", eje = "Número de estudiantes")
# EDAD_ACTUAL_MAT_POS <- barra_vertical_ord(datos = Agregado_MatPos, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes matriculados en postgrado por grupos de edad", eje = "Número de estudiantes")
# 
# 
# # Tablas ----
# 
# 
# TABLA_MUNICIPIOS_MAT_POS <- tabla_n(
#   datos = Mat_Pos_Municipios ,
#   categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
#   variable = 'Lugar de residencia del matriculado en postgrado',
#   mensaje = "Número de matriculados en postgrado por lugar de procedencia",
#   titulo = "Histórico total de matriculados en postgrado por departamentos y municipios de procedencia"  
# )
# 
# 
# TABLA_CARRERAS_MAT_POS <- tabla_n(
#   datos = Mat_Pos_Carreras,
#   categorias = c("Año", "Semestre", "Nivel de formación", "Programa", "Total"),
#   variable = 'Nivel de formación y Programas Académicos del matriculado en postgrado',
#   mensaje = "Número de matriculados en postgrado por nivel de formación y programa académico",
#   titulo = "Histórico total de matriculados en postgrado por niveles de formación y programas académicos"
# )


# GRADUADOS EN PREGRADO----

ano <- max(Agregado_GraPre %>% select(YEAR))
semestre <- Agregado_GraPre[[nrow(Agregado_GraPre), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total

SERIE_GRA_PRE <- series(
  datos = Agregado_GraPre,
  categoria = "TOTAL",
  colores = col,
  titulo = "Evolución histórica del total de graduados en pregrado",
  eje = "Número de graduados"
)           

# Sede Andina ----


col <-   c( "#8cc63f", # verde, Bogotá
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#93278f"  # Morado, Palmira
            
) 


SEDE_ANDINA_SERIE_GRA_PRE <- series(datos = Agregado_GraPre, categoria = "ADM_PEAMA_ANDINA", colores = col, titulo = "Evolución del número de estudiantes graduados en pregrado según sede andina de graduación", eje = "Número de graduados")
SEDE_ANDINA_ACTUAL_GRA_PRE <- barra_vertical(datos = Agregado_GraPre, categoria = "ADM_PEAMA_ANDINA",colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de estudiantes graduados en pregrado por sede andina de graduación", eje = "Número de graduados")


# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_SERIE_GRA_PRE <- series(datos = Agregado_GraPre, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo", eje = "Número de graduados")
SEXO_ACTUAL_GRA_PRE <- torta(datos = Agregado_GraPre, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)

# Edad ----

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


EDAD_SERIE_GRA_PRE <- series(datos = Agregado_GraPre, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad", eje = "Número de graduados")
EDAD_ACTUAL_GRA_PRE <- barra_vertical_ord(datos = Agregado_GraPre, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad", eje = "Número de graduados")


# Estrato ----

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)

ESTRATO_SERIE_GRA_PRE <- series(datos = Agregado_GraPre, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico", eje = "Número de graduados")
ESTRATO_ACTUAL_GRA_PRE <- barra_vertical_ord(datos = Agregado_GraPre, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico", eje = "Número de graduados")


# Tablas ----


TABLA_MUNICIPIOS_GRA_PRE <- tabla_n(
  datos = Gra_Pre_Municipios,
  categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
  variable = 'Lugar de nacimiento del graduados en pregrado',
  mensaje = "Número de graduados en pregrado por lugar de nacimiento",
  titulo = "Histórico total de graduados en pregrado por departamento y municipios de nacimiento"  
)

TABLA_CARRERAS_GRA_PRE <- tabla_n(
  datos = Gra_Pre_Carreras,
  categorias = c("Año","Semestre","Sede Andina","Facultad","Programa", "Total"),
  variable = 'Sede Andina, Facultad y Programas Académicos de los graduados en pregrado',
  mensaje = "Número de graduados en pregrado por Sede, Facultad y Programa",
  titulo = "Histórico total de graduados en pregrado por sede andina, facultad y programas"
)

# # GRADUADOS EN POSTGRADO----
# 
# ano <- max(Agregado_GraPos %>% select(YEAR))
# semestre <- Agregado_GraPos[[nrow(Agregado_GraPos), "SEMESTRE"]]
# periodo_actual_titulo <- paste0(" ", ano, "-", semestre)
# 
# 
# # Serie histórica ----
# 
# col <-    c("#f15a24") # naranja, Total
# 
# EVOLUCION_SERIE_GRA_POS <- series(datos = Agregado_GraPos, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado", eje = "Número de graduados")
# 
# 
# 
# # Sexo ----
# 
# col <-   c( "#f15a24", # naranja, hombres
#             "#8cc63f") # verde, mujeres
# 
# 
# SEXO_SERIE_GRA_POS <- series(datos = Agregado_GraPos, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo", eje = "Número de graduados")
# SEXO_ACTUAL_GRA_POS <- torta(datos = Agregado_GraPos, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)
# 
# 
# # Edad ----
# 
# col <-   c( "#8cc63f", # verde, 25 años o menos
#             "#0071bc", # azul vivo, 26 a 30 años
#             "#f15a24", # naranja, 31 a 35 años
#             "#fbb03b", # amarillo, 36 años o más
#             "#6d6666")  # gris, sin información 
# 
# 
# CAT_EDAD_SERIE_GRA_POS <- series(datos = Agregado_GraPos, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad", eje = "Número de graduados")
# CAT_EDAD_ACTUAL_GRA_POS <- barra_vertical_ord(datos = Agregado_GraPos, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad", eje = "Número de graduados")
# 
# # Tablas ----
# 
# 
# TABLA_MUNICIPIOS_GRA_POS <- tabla_n(
#   datos = Gra_Pos_Municipios,
#   categorias = c("Año","Semestre","Departamento", "Municipio", "Total"),
#   variable = 'Lugar de nacimiento del graduados en postgrado',
#   mensaje = "Número de graduados en postgrado por lugar de nacimiento",
#   titulo = "Histórico total de graduados en postgrado por departamento y municipios de nacimiento"  
# )
# 
# TABLA_CARRERAS_GRA_POS <- tabla_n(
#   datos = Gra_Pos_Carreras,
#   categorias = c("Año","Semestre","Modalidad de formación","Programa", "Total"),
#   variable = 'Modalidad de formación y programas académicos de los graduados en postgrado',
#   mensaje = "Número de graduados en postgrado por modalidad de formación y Programa",
#   titulo = "Histórico total de graduados en postgrado por modalidad de formación y programas académicos"
# )
# 

# DOCENTES DE CARRERA----

ano <- max(Agregado_Doc %>% select(YEAR))
semestre <- Agregado_Doc[[nrow(Agregado_Doc), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total
SERIE_DOC_CAR <- series(datos = Agregado_Doc, categoria = "TOTAL",  colores = col, titulo = "Evolución histórica del total de docentes de carrera",eje = "Número de docentes")           

# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_SERIE_DOC_CAR <- series(datos = Agregado_Doc, categoria = "SEXO", colores = col, titulo = "Evolución del número de docentes de carrera por sexo", eje = "Número de docentes")
SEXO_ACTUAL_DOC_CAR <- torta(datos = Agregado_Doc, variable = "SEXO", colores = col, titulo = "Distribución de docentes de carrera por sexo", etiqueta = "Número de docentes", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)


# Edad ----

col <-   c( "#93278f", # Morado, 29 años o menos
            "#6d6666", # gris, 50 a 59
            "#f15a24", # naranja,  40 a 49
            "#8cc63f", # verde, 50 a 59
            "#0071bc" # azul vivo, 60 o más años
) 

EDAD_SERIE_DOC_CAR <- series(datos = Agregado_Doc, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de docentes de carrera por grupos de edad", eje = "Número de docentes")
EDAD_ACTUAL_DOC_CAR <- barra_vertical_ord(datos = Agregado_Doc, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de docentes de carrera por grupos de edad", eje = "Número de docentes")

# Máxima Formación ----

col <-   c( "#8cc63f", # verde, Doctorado
            "#0071bc", # azul vivo, Especialidad Médica
            "#93278f", # Morado, Especialización
            "#f15a24", # naranja, Maestría
            "#6d6666" # gris, Pregrado
) 

FORMACION_SERIE_DOC_CAR <- series(datos = Agregado_Doc, categoria = "FORMACION", colores = col, titulo = "Evolución del número de docentes de carrera según máximo nivel de formación", eje = "Número de docentes")
FORMACION_ACTUAL_DOC_CAR <- barra_vertical_ord(datos = Agregado_Doc, categoria = "FORMACION", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de docentes de carrera según máximo nivel de formación", eje = "Número de docentes")



# ADMINISTRATIVOS DE CARRERA----

ano <- max(Agregado_Admi %>% select(YEAR))
semestre <- Agregado_Admi[[nrow(Agregado_Admi), "SEMESTRE"]]
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)

# Serie histórica ----

col <-   c( "#8cc63f") # verde, Total

SERIE_ADMI_CAR <- series(datos = Agregado_Admi, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de administrativos de carrera", eje = "Número de administrativos")           

# Sexo ----

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_SERIE_ADMI_CAR <- series(datos = Agregado_Admi, categoria = "SEXO", colores = col, titulo = "Evolución del número de administrativos de carrera por sexo", eje = "Número de administrativos")
SEXO_ACTUAL_ADMI_CAR <- torta(datos = Agregado_Admi, variable = "SEXO", colores = col, titulo = "Distribución de administrativos de carrera por sexo", etiqueta = "Número de administrativos", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo)

# Edad ----

col <-   c( "#93278f", # morado, 29 años o menos
            "#6d6666", # gris, 30 a 39 años
            "#f15a24", # naranja, 40 a 49 años
            "#8cc63f", # verde, 50 a 59 años
            "#0071bc") # azul vivo, 60 o más años


EDAD_SERIE_ADMI_CAR <- series(datos = Agregado_Admi, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de funcionarios administrativos por grupos de edad", eje = "Número de funcionarios")
EDAD_ACTUAL_ADMI_CAR <- barra_vertical_ord(datos = Agregado_Admi, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo,titulo = "Distribución de funcionarios administrativos por grupos de edad",eje = "Número de funcionarios")

# Máxima Formación ----

col <-   c( "#c1272d", # rojo, Doctorado
            "#ed1e79", # vinotinto, Tecnología
            "#6d6666", # gris, Especialización
            "#fbb03b", # amarillo, Maestría
            "#f15a24", # naranja, Pregrado
            "#8cc63f", # verde, Secundaria o menos
            "#29abe2", # azul claro, Sin información
            "#0071bc", # azul vivo, Técnico
            "#93278f") # morado, Especialidad médica 


FORMACION_SERIE_ADMI_CAR <- series(datos = Agregado_Admi, categoria = "FORMACION", colores = col, titulo = "Evolución del número de funcionarios administrativos por máximo nivel de formación", eje = "Número de funcionarios")
FORMACION_ACTUAL_ADMI_CAR <- barra_horizontal(datos = Agregado_Admi, categoria = "FORMACION", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de funcionarios administrativos por máximo nivel de formación", eje = "Número de funcionarios")


