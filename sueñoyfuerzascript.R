#  paquetes
library(haven)

# Crear carpetas
dir.create("datos/originales",
           recursive = TRUE,
           showWarnings = FALSE)

dir.create("datos/procesados",
           recursive = TRUE,
           showWarnings = FALSE)

# Archivos a convertir
archivos <- c(
  "MGX_H",
  "SLQ_H",
  "PAQ_H",
  "BMX_H",
  "DEMO_H"
)

# Convertir XPT a RDS
for (nombre in archivos) {
  
  ruta_xpt <- file.path(
    "datos", "originales",
    paste0(nombre, ".xpt")
  )
  
  ruta_rds <- file.path(
    "datos", "procesados",
    paste0(nombre, ".rds")
  )
  
  # Leer archivo original
  datos <- read_xpt(ruta_xpt)
  
  # Guardar en formato RDS
  saveRDS(datos, file = ruta_rds,
          compress = "xz")
  
  cat("Convertido:", nombre, "\n")
}
# cargar bases rds
fuerza <- readRDS("datos/procesados/MGX_H.rds")
sueno     <- readRDS("datos/procesados/SLQ_H.rds")
actividad <- readRDS("datos/procesados/PAQ_H.rds")
cuerpo    <- readRDS("datos/procesados/BMX_H.rds")
personas  <- readRDS("datos/procesados/DEMO_H.rds")


# =====================================================
# PROYECTO: SUEÑO, ACTIVIDAD FÍSICA Y FUERZA MUSCULAR
# NHANES
# Preparación de la base unificada
# =====================================================

library(dplyr)

# 1. CARGAR LAS CINCO BASES ----------------------------

ruta <- "datos/procesados"

fuerza <- readRDS(file.path(ruta, "MGX_H.rds"))
sueno <- readRDS(file.path(ruta, "SLQ_H.rds"))
actividad <- readRDS(file.path(ruta, "PAQ_H.rds"))
cuerpo <- readRDS(file.path(ruta, "BMX_H.rds"))
personas <- readRDS(file.path(ruta, "DEMO_H.rds"))


# 2. SELECCIONAR VARIABLES -----------------------------

fuerza <- fuerza %>%
  select(SEQN, MGDCGSZ, MGDEXSTS)

sueno <- sueno %>%
  select(SEQN, SLD010H)

actividad <- actividad %>%
  select(SEQN, PAQ650, PAQ665)

cuerpo <- cuerpo %>%
  select(SEQN, BMXBMI)

personas <- personas %>%
  select(
    SEQN, RIDAGEYR, RIAGENDR, DMDEDUC2,
    WTMEC2YR, SDMVSTRA, SDMVPSU
  )


# 3. COMPROBAR IDENTIFICADORES -------------------------

bases <- list(
  fuerza, sueno, actividad, cuerpo, personas
)

stopifnot(
  all(vapply(
    bases,
    function(x) anyDuplicated(x$SEQN) == 0,
    logical(1)
  ))
)


# 4. UNIFICAR LAS CINCO BASES --------------------------

base_unificada <- personas %>%
  left_join(fuerza, by = "SEQN") %>%
  left_join(sueno, by = "SEQN") %>%
  left_join(actividad, by = "SEQN") %>%
  left_join(cuerpo, by = "SEQN")

stopifnot(
  nrow(base_unificada) == nrow(personas)
)


# 5. LIMPIAR Y CREAR VARIABLES -------------------------

base_unificada <- base_unificada %>%
  transmute(
    
    SEQN,
    
    # Variable dependiente
    fuerza_kg = as.numeric(MGDCGSZ),
    
    # Horas de sueño
    sueno_horas = case_when(
      SLD010H >= 2 & SLD010H <= 11 ~
        as.numeric(SLD010H),
      TRUE ~ NA_real_
    ),
    
    # Actividad física recreativa
    actividad_recreativa = case_when(
      PAQ650 == 1 | PAQ665 == 1 ~ 1L,
      PAQ650 == 2 & PAQ665 == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Variables de control
    edad = as.numeric(RIDAGEYR),
    sexo = as.numeric(RIAGENDR),
    imc = as.numeric(BMXBMI),
    
    educacion = case_when(
      DMDEDUC2 %in% 1:5 ~ as.numeric(DMDEDUC2),
      TRUE ~ NA_real_
    ),
    
    # Estado de prueba muscular
    prueba_fuerza = as.numeric(MGDEXSTS),
    
    # Diseño muestral
    peso_mec = as.numeric(WTMEC2YR),
    estrato = as.numeric(SDMVSTRA),
    psu = as.numeric(SDMVPSU)
  )


# 6. IDENTIFICAR LA MUESTRA DE ESTUDIO -----------------

base_unificada <- base_unificada %>%
  mutate(
    muestra_estudio = coalesce(
      between(edad, 20, 39) &
        prueba_fuerza == 1 &
        !is.na(fuerza_kg) &
        !is.na(sueno_horas) &
        !is.na(actividad_recreativa) &
        !is.na(sexo) &
        !is.na(imc) &
        !is.na(educacion) &
        !is.na(peso_mec) &
        peso_mec > 0 &
        !is.na(estrato) &
        !is.na(psu),
      FALSE
    )
  )


# 7. GUARDAR BASE UNIFICADA ----------------------------

saveRDS(
  base_unificada,
  "datos/procesados/base_unificada.rds",
  compress = "xz"
)


# 8. VERIFICAR RESULTADOS ------------------------------

cat(
  "Participantes en base unificada:",
  nrow(base_unificada), "\n"
)

cat(
  "Participantes elegibles de 20 a 39 años:",
  sum(base_unificada$muestra_estudio), "\n"
)

cat(
  "Número de variables:",
  ncol(base_unificada), "\n"
)

# Muestra válida para exploración descriptiva
base_estudio <- base_unificada %>%
  filter(muestra_estudio)

summary(
  base_estudio %>%
    select(fuerza_kg, sueno_horas, edad, imc)
)

table(base_estudio$actividad_recreativa)

cat("Base unificada guardada correctamente.\n")

# Abrir base unificada:

base_unificada <- readRDS(
  "datos/procesados/base_unificada.rds"
)

View(base_unificada)