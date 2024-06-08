install.packages(c("pdftools", "tidyverse", "readxl", "xlsx", "mongolite", "rentrez", "shiny", "processx"))
install.packages("DT", repos = "http://cran.us.r-project.org")
install.packages("shinyFiles")
install.packages("DBI", "RSQLite")
install.packages("jsonlite")


library(shiny)

runApp("Shiny_App")

#update.packages("shinyFiles")

##library(tools)
library(pdftools)
library(tidyverse)
#library(devtools)
#library(stringr)
library(readxl)
library(xlsx)
#library(openxlsx)
library(DBI)
#library(RSQLite)
library(mongolite)
library(rentrez)

CarpetaEntrada <- "INPUT"
CarpetaDatos <- "DATOS"
CarpetaInformes <- "INFORMES"
CarpetaSalida <- "OUTPUT"
CarpetaResultados <- "RESULTADOS"
PathBase <- getwd()
#base_datos <- dbConnect(SQLite(), dbname = "TFG.sqlite")

LeerFicherosPDF <- function(ruta) {
  ficheros <- list.files(path = ruta, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  return(ficheros)
}

LeerDocumento <- function(nombreFichero) {
  doc <- pdf_text(nombreFichero)
  text <- paste(doc, collapse = "\n")
  return(strsplit(text, "\n")[[1]])
}


BuscarValor <- function(textoBuscar, lines) {
  Encontrados <- character()
  valores <- ifelse(grepl(textoBuscar, lines), 1, 0)
  valores <- which(valores == 1)
  array_buscar <- strsplit(textoBuscar, " ")[[1]]
  for (i in valores) {
    array_line <- strsplit(lines[i], " ")[[1]]
    for (j in 1:(length(array_line)-length(array_buscar))){
      cadena <- character()
      sub_array_line <- array_line[j:(j + length(array_buscar) - 1)]
      if (all(identical(sub_array_line, array_buscar))) {
        añadir <-j+length(array_buscar)
        while (!is.na(array_line[añadir]) && array_line[añadir] != ""){
          cadena <- c(cadena, array_line[añadir])
          añadir<-añadir+1
        }
        cadena <- paste(cadena, collapse = " ")
        Encontrados<- c(Encontrados, cadena)
      }
    }
  }
  return(Encontrados)
}

BuscarVariable <- function(lines, textoBuscar){
  posicionValor <- grep(textoBuscar, lines)
  if (length(posicionValor) != 0){
    lineaValor <- lines[posicionValor[1]]
    if (textoBuscar == patron_porcentaje_tumoral){
      lineaValor <- sub("\\s*/.*", "", lineaValor)
    }
    valor <- sub(textoBuscar, "", lineaValor)
    trimws(valor)
  } else {
    "Null"
  }
}

acotarTexto<-function(textoInicio, textoInicio2, linesTotal){
  lines <- character()
  indices_inicio <- grep(textoInicio, linesTotal)
  indices_inicio2 <- grep(textoInicio2, linesTotal)
  indice_limite <- grep(textoLimite, linesTotal)[1]
  indice_limite2 <- grep(textoLimite2, linesTotal)
  print(indices_inicio)
  print(indices_inicio2)
  print(indice_limite)
  print(indice_limite2)
  if (length(indices_inicio2) != 0 | length(indices_inicio)>1){
    if (length(indices_inicio2) != 0 && length(indices_inicio) != 0){
      lines <- linesTotal[(indices_inicio + 1):(indices_inicio2 - 1)]
    }else if (length(indice_limite2) != 0 && length(indices_inicio)!=0){
      lines <- linesTotal[(indices_inicio + 1):(indice_limite2[1] - 1)]
    }else if (length(indices_inicio2) != 0){
      lines <- linesTotal[(indices_inicio2 + 1):(indice_limite - 1)]
    }else{
      lines <- linesTotal[(indices_inicio + 1):(indice_limite - 1)]
    }
  }else if (length(indices_inicio2) == 0 && length(indices_inicio) == 0){
    lines <- c(lines, "Sin biomarcadores")
  }else {
    lines <- linesTotal[(indices_inicio + 1):(indice_limite - 1)]
  }
  return(lines)
}

ficheroDiagnostico <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx")
diagnostico <- read_excel(ficheroDiagnostico)
diagnosticos_dic <- setNames(diagnostico$`NÚMERO DIAGNÓSTICO`, diagnostico$DIAGNÓSTICO)

ficheroGenes <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx")
genes <- read_excel(ficheroGenes)
mutaciones <- unique(genes$GEN)
mutaciones_dic <- setNames(genes$`Número gen`, genes$GEN)


#variables generales

rutaEntrada <- file.path(PathBase, CarpetaEntrada, CarpetaInformes)
ficheros <- LeerFicherosPDF(rutaEntrada)

NHC_Data <- Nbiopsia_Data <- fecha_Data <- texto_Data <- genes_mut2 <- genes_mut_ordenados <- frecuencias_totales <- num_mutaciones <- numero_iden <- 
  añadir <- cambiosPato <- frecuenciasPato <- mutaciones_pato <- patogen <- numero_iden_pato <- num_mutacionesPato <- 
  diagnostico2 <- sexo <- porcentaje_tumoral <- calidad <- list()
textoDiag <- NHC <- biopsia <- fechas <- chip2 <- fusiones <- character()
numeroDiag <- lista_ensayos <- ensayos_finales <- lista_tratamientos <- tratamientos_finales <- numeric()

benigno <- resultado <- FALSE
max_mut <- 0

B <- "1"
C <- "3"
P <- "2"

patron <- "(\\d+)\\s* Ensayos clínicos"
patron2 <- "(\\d+)\\s* Tratamientos disponibles"
patron_frecuencia <- "\\d{2}\\.\\d{2}\\%"
patron_cambio <-"\\(.*?\\)"
patron_codificacion <- "c\\.[0-9]+[A-Za-z>_]+"

patron_diagnostico <- ".*Diagnóstico:\\s"
patron_sexo <- ".*Sexo:\\s*"
patron_porcentaje_tumoral <- ".*% células tumorales:\\s"
patron_calidad <- ".*CALIDAD DE LA MUESTRA /LIMITACIONES PARA SU ANÁLISIS:\\s"

textoInicio<- "Variantes de secuencia de ADN"
textoInicio2<-"   Variaciones del número de copias"
textoLimite <- "Genes analizados"
textoLimite2 <-"Comentarios adicionales sobre las variantes"

for (ficheroPDF in ficheros){
  lines <- LeerDocumento(ficheroPDF)
  diagnostico2 <- c(diagnostico2, BuscarVariable(lines, patron_diagnostico))
  sexo <- c(sexo, BuscarVariable(lines, patron_sexo))
  porcentaje_tumoral <- c(porcentaje_tumoral, BuscarVariable(lines, patron_porcentaje_tumoral))
  calidad <- c(calidad, BuscarVariable(lines, patron_calidad))
  
}

for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  NHC_Data[[length(NHC_Data) + 1]] <- BuscarValor("NHC:", lines)
  Nbiopsia_Data[[length(Nbiopsia_Data) + 1]] <- BuscarValor("biopsia:", lines)
  fecha_Data[[length(fecha_Data) + 1]] <- BuscarValor("Fecha:", lines)
  texto_Data[[length(texto_Data) + 1]] <- BuscarValor("de la muestra:", lines)
}

textoDiag <- sapply(texto_Data, function(x) unique(x)[1])

for (diagnostico in textoDiag) {
  if (diagnostico %in% names(diagnosticos_dic)){
    valor <- diagnosticos_dic[[diagnostico]]
    numeroDiag <- c(numeroDiag, valor)
  }
  else{
    numeroDiag <- c(numeroDiag, " ")
  }
}

NHC <- sapply(NHC_Data, function(x) unique(x)[1])

lista_resultante <- lapply(Nbiopsia_Data, function(sublist) {
  sublist_sin_duplicados <- sublist[1]
  sublist_sin_duplicados
})

NB_values <- unlist(lista_resultante)

biopsia<- sapply(lista_resultante, function(x) substr(x, 5,5))

Biopsia_solida <- character()
Biopsia_solida <- ifelse(biopsia == "B", B,
                         ifelse(biopsia == "P", P, C))

fechas <- sapply(fecha_Data, function(x) unique(x)[1])

lista_ensayos <- as.integer(sapply(ficheros, function(ficheroPDF) {
  lines <- LeerDocumento(ficheroPDF)
  ensayos <- sapply(lines, function(line) {
    resultado <- str_match(line, patron)
    if (!is.na(resultado[1])) {
      return(as.integer(resultado[1, 2]))
    } else {
      return(0)
    }
  })
  return(sum(ensayos))
}))

ensayos_finales <- ifelse(lista_ensayos %in% 0, 0, 1)

lista_tratamientos <- as.integer(sapply(ficheros, function(ficheroPDF) {
  lines <- LeerDocumento(ficheroPDF)
  tratamientos <- sapply(lines, function(line) {
    resultado <- str_match(line, patron2)
    if (!is.na(resultado[1])) {
      return(as.integer(resultado[1, 2]))
    } else {
      return(0)
    }
  })
  return(sum(tratamientos))
}))

tratamientos_finales <- as.integer(lista_tratamientos != 0)

numero_paciente <- gsub("^.*Sample_(\\d+)_.*\\.pdf$", "\\1", ficheros)

pdf_files <- ficheros[grep("\\.pdf$", ficheros)]
#chip_match <- str_match(pdf_files, "v(\\d+)_")
chip2 <- gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", pdf_files)
#chip2 <- as.integer(chip_match[, 2])

cod_totales <- frecuencias_totales <-patogenicidad_ordenadas<- genes_mut_ordenados <- frecuencias_totales2 <- genes_mut_ordenados2 <-list()

for (ficheroPDF in ficheros) {
  linesTotal <- LeerDocumento(ficheroPDF)
  lines <- acotarTexto(textoInicio, textoInicio2 ,linesTotal)
  posiciones <- mutaciones_patogenicas <- lista_frec <- mutaciones_pdf <- patogenicidad <- lista_cod <- c()
  lines_divididas <- strsplit(lines, "\\s+")
  print(lines)
  pos = 0
  for (line in lines_divididas){
    pos = pos+1
    if (line[1] %in% mutaciones){
      posiciones <- c(posiciones, pos)
      mutaciones_pdf <- c(mutaciones_pdf, line[1])
      for (i in strsplit(line, " ")) {
        resultado <- str_match(i, patron_frecuencia)
        resultado2 <- str_match(i, patron_codificacion)
        if (!is.na(resultado)) {
          frec <- resultado[1]
          lista_frec <- c(lista_frec, frec)
        }
        else if (!is.na(resultado2)) {
          cod <- resultado2[1]
          lista_cod <- c(lista_cod, cod)
        }
      }
    }
  }
  if (length(posiciones) !=0){
    for (pos in seq(1, length(posiciones))){
      if (pos == length(posiciones)){
        if (length(grep("pathogenicity", lines[posiciones[pos]:length(lines)])) == 1 || length(grep("Pathogenic", lines[posiciones[pos]:length(lines)])) == 1){
          patogenicidad <- c(patogenicidad, "Pathogenic")
        } else{
          patogenicidad <- c(patogenicidad, " ")
        }
      } else if(length(grep("pathogenicity", lines[posiciones[pos]:posiciones[pos+1]-1])) == 1 || length(grep("Pathogenic", lines[posiciones[pos]:posiciones[pos+1]-1])) == 1){
        patogenicidad <- c(patogenicidad, "Pathogenic")
      }else{
        patogenicidad <- c(patogenicidad, " ")
      }
    }
  }
  genes_mut_ordenados <- c(genes_mut_ordenados, list(mutaciones_pdf))
  patogenicidad_ordenadas <- c(patogenicidad_ordenadas, list(patogenicidad))
  frecuencias_totales <- c(frecuencias_totales, list(lista_frec))
  cod_totales <- c(cod_totales, list(lista_cod))
  #print(genes_mut_ordenados)
}
print(frecuencias_totales)
print(genes_mut_ordenados)

patogenicidad_buscadas <- c()
for (lista in seq_along(patogenicidad_ordenadas)){
  patogenicidad <- c()
  for (elemento in seq_along(patogenicidad_ordenadas[[lista]])){
    print(genes_mut_ordenados[[lista]][[elemento]])
    gen = paste(genes_mut_ordenados[[lista]][[elemento]], "[gene]", cod_totales[[lista]][[elemento]])
    res <- entrez_search(db = "clinvar", term = gen)
    if (length(res$ids)!=0){
      esums <- entrez_summary(db = "clinvar", id = res$ids[1])
      resumen <- extract_from_esummary(esums, "germline_classification")
      patogenicidad <- c(patogenicidad, resumen$description)
    }
    else{
      patogenicidad <- c(patogenicidad, "Sin resultados")
    }
  }
  patogenicidad_buscadas <- c(patogenicidad_buscadas, list(patogenicidad))
}

mutaciones_pato <-c()
for (lista in seq_along(patogenicidad_ordenadas)){
  patogenicidad <- c()
  genpato2 <- c()
  for (elemento in seq_along(patogenicidad_ordenadas[[lista]])){
    if (patogenicidad_ordenadas[[lista]][[elemento]] == "Pathogenic"){
    genpato2 <- c(genpato2, genes_mut_ordenados[[lista]][[elemento]])
    }
  }
  mutaciones_pato <- append(mutaciones_pato, list(genpato2))
}


for (lista in genes_mut_ordenados){
  num_mutaciones<- c(num_mutaciones, length(lista))
}

for (i in genes_mut_ordenados){
  for (gen in i){
    añadir <- c(añadir, mutaciones_dic[[gen]])
  }
  numero_iden <- c(numero_iden, list(unlist(añadir)))
  añadir <- list()
}

for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(file.path(ficheroPDF))
  variantes <- character()
  for (linea in lines) {
    for (mutacion in mutaciones) {
      patronGen <-paste0(mutacion, "\\.[A-Za-z0-9]+\\.[A-Za-z0-9]+")
      if (grepl(patronGen, linea)) {
        for (palabra in strsplit(linea, " ")[[1]]){
          if (grepl(patronGen, palabra)){
            variantes <- c(variantes, palabra)
          }
        }
      }
    }
  }
  fusiones <- append(fusiones, list(variantes))
}

frecuenciasPato <- c()
cambiosPato<-c()
for (ficheroPDF in ficheros) {
  lista_frec <- list()
  lista_cambio <- list()
  linesTotal <- LeerDocumento(ficheroPDF)
  lines <- acotarTexto(textoInicio, textoInicio2 ,linesTotal)
  for (mutaciones in mutaciones_pato){
    for (mutacion in mutaciones){
      print(mutacion)
      coincidencias <- character()
      if (!is.null(mutacion)){
        coincidencias <- grepl(mutacion, lines)
        for (coincidencia in 1:length(coincidencias)){
          if (coincidencias[coincidencia] == TRUE){
            posicion <- coincidencia
                
            for (i in strsplit(lines[posicion], " ")[[1]]) {
              resultado <- str_match(i, patron_frecuencia)
              resultado2 <- str_match(i, patron_cambio)
              if (!is.na(resultado)) {
                frec <- resultado[1]
                lista_frec <- c(lista_frec, frec)
              }
              if (!is.na(resultado2)) {
                cambio <- resultado2[1]
                lista_cambio <- c(lista_cambio, cambio)
              }
            }
          }
        }
      }
    }
  }
  frecuenciasPato <- append(frecuenciasPato, list(unlist(lista_frec)))
  cambiosPato<- append(cambiosPato, list(unlist(lista_cambio)))
}

#mutaciones_pato<-c()
#for (ficheroPDF in ficheros) {
#  nombreFichero <- file.path(ficheroPDF)
#  linesTotal <- LeerDocumento(nombreFichero)
#  genpato2 <- list()
#  inicio <- FALSE
#  lines <- character()
#  for (line in linesTotal){
#    if (grepl(textoInicio, line) | grepl(textoInicio2, line)){
#      inicio <- TRUE
#    }else if (grepl(textoLimite, line) ){
#      inicio <- FALSE
#    }
#    if (grepl(textoLimite, line)==FALSE && inicio == TRUE){
#      lines <- c(lines,line)
#    }
#  }
#  print(lines)
#  for (mutacion in mutaciones){
#    coincidencias <- character()
#    coincidencias <- grepl(mutacion, lines)
#    for (coincidencia in 1:length(coincidencias)){
#      if (coincidencias[coincidencia] == TRUE){
#        posicion <- coincidencia
#        
#        for (a in strsplit(lines[posicion], " ")[[1]]) {
#          if (grepl("Pathogeni", a)) {
#            genpato2 <- c(genpato2, mutacion)
#          }
#        }
#      }
#    }
#  }
#  
#  patogen[[ficheroPDF]] <- genpato2
#  mutaciones_pato <- append(mutaciones_pato, list(genpato2))
#}

for (i in mutaciones_pato) {
  valores <- sapply(i, function(gen) ifelse(is.null(mutaciones_dic[[gen]]), 0, mutaciones_dic[[gen]]))
  numero_iden_pato <- append(numero_iden_pato, list(valores))
}

for (lista in numero_iden_pato){
  num_mutacionesPato <- c(num_mutacionesPato, length(lista))
}
print(unlist(num_mutacionesPato))

print(chip2)

print(numero_paciente)

print(NHC)

print(NB_values)

print(Biopsia_solida)

print(fecha_Data)

print(fechas)

print(texto_Data)

print(textoDiag)

print(numeroDiag)

print(unlist(num_mutaciones))

print(unlist(num_mutacionesPato))

print(genes_mut_ordenados)

print(fusiones)

print(frecuencias_totales)

print(frecuenciasPato)

print(numero_iden)

print(lista_ensayos)

print(ensayos_finales)

print(lista_tratamientos)

print(tratamientos_finales)

genes_mut_ordenados <- lapply(genes_mut_ordenados, function(x) if(length(x) == 0) NA else x)
print(length(genes_mut_ordenados))
frecuencias_totales <- lapply(frecuencias_totales, function(x) if(length(x) == 0) NA else x)
print(length(frecuencias_totales))
fusiones <- lapply(fusiones, function(x) if(length(x) == 0) NA else x)
print(fusiones)
numero_iden <- lapply(numero_iden, function(x) if(length(x) == 0) NA else x)
mutaciones_pato <- lapply(mutaciones_pato, function(x) if(length(x) == 0) NA else x)
frecuenciasPato <- lapply(frecuenciasPato, function(x) if(length(x) == 0) NA else x)
numero_iden_pato <- lapply(numero_iden_pato, function(x) if(length(x) == 0) NA else x)
cambiosPato <- lapply(cambiosPato, function(x) if(length(x) == 0) NA else x)


T1 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'NHC' = NHC, 
                 'Biopsia sólida' = Biopsia_solida, 'Fecha de informe' = fechas,
                 'diagnostico'= diagnostico, 'Sexo'=unlist(sexo), 'Porcentaje_tumoral'=porcentaje_tumoral, 'Calidad'=unlist(calidad))

T2 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Diagnóstico' = textoDiag, 
                 'Número del diagnóstico' = numeroDiag)

T3 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Mutaciones detectadas' = I(genes_mut_ordenados), 
                 'Número de la mutación específica' = I(numero_iden), 'Total del número de mutaciones' = unlist(num_mutaciones), 
                 'Porcentaje de frecuencia alélica (ADN)' = I(frecuencias_totales), 'Fusiones ID' = I(fusiones), 'Patogenicidad'=I(patogenicidad_ordenadas))

T4 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Genes patogénicos' = I(mutaciones_pato), 
                 'Número de la mutación específica' = I(numero_iden_pato), '% frecuencia alélica' = I(frecuenciasPato),
                 'Cambios' = I(cambiosPato), 'Total de mutaciones patogénicas' = I(num_mutacionesPato))

T5 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Ensayos clínicos' = lista_ensayos, 
                 'SI/NO ensayo' = ensayos_finales, 'Fármaco aprobado' = lista_tratamientos, 'SI/NO fármacos' = tratamientos_finales)

tabla_unida <- merge(T1, T2, by = c("Número.de.chip", "Número.de.biopsia"), all=TRUE)
print(tabla_unida)

tabla_unida2 <- merge(tabla_unida, T3, by = c("Número.de.chip", "Número.de.biopsia"), all=TRUE)
print(tabla_unida2)
tabla_final <- merge(tabla_unida2, T5, by = c("Número.de.chip", "Número.de.biopsia"))
tabla_unida3 <- merge(tabla_unida, T4, by = c("Número.de.chip", "Número.de.biopsia"))
tabla_final_pato <- merge(tabla_unida3, T5, by = c("Número.de.chip", "Número.de.biopsia"))




####################################################

rutaSalida <- file.path(PathBase, CarpetaSalida)
if (!file.exists(rutaSalida)) {
  dir.create(rutaSalida)
}

rutaResultados <- file.path(rutaSalida, CarpetaResultados)
if (!file.exists(rutaResultados)) {
  dir.create(rutaResultados)
}

fichero <- file.path(rutaSalida, "TablaPato.xlsx")
write.xlsx(tabla_final_pato, fichero)

fichero <- file.path(rutaSalida, "TablaGeneral.xlsx")
write.xlsx(tabla_final, fichero)

fragmentos <- split(tabla_final, rep(1:(nrow(tabla_final) %/% 80 + 1), each = 80, length.out = nrow(tabla_final)))
for (i in seq_along(fragmentos)) {
  nombre_archivo <- paste0("tabla_final", i, ".xlsx")
  ruta_archivo <- file.path(rutaResultados, nombre_archivo)
  write.xlsx(fragmentos[[i]], ruta_archivo, row.names = FALSE)
  cat("Archivo", nombre_archivo, "guardado correctamente.\n")
}

fragmentos <- split(tabla_final_pato, rep(1:(nrow(tabla_final_pato) %/% 80 + 1), each = 80, length.out = nrow(tabla_final_pato)))
for (i in seq_along(fragmentos)) {
  nombre_archivo <- paste0("patogenicos_", i, ".xlsx")
  ruta_archivo <- file.path(rutaResultados, nombre_archivo)
  write.xlsx(fragmentos[[i]], ruta_archivo, row.names = FALSE)
  cat("Archivo", nombre_archivo, "guardado correctamente.\n")
}

#####################
library(jsonlite)

entrez_dbs()
entrez_db_searchable(db = "clinvar")
res <- entrez_search(db = "clinvar", term = "CTNNB1 [gene]   c.134C>T", retmax = 100)

res <- entrez_search(db = "clinvar", term = "BRAF [gene]  c.1789_1790delCTinsT", retmax = 100)
print(length(res$ids))
esums <- entrez_summary(db = "clinvar", id = res$ids)
resumen <- extract_from_esummary(esums, "germline_classification")
print(resumen$description)
extract_from_esummary(esums, "germline_classification")[1:3]
descriptions <- lapply(resumen, function(x) x[[1]])
print(descriptions)

resumen[5]
print(length(resumen[[1]]))

entrez_db_searchable(db = "Cbioportal")
##########################

library(mongolite)
path_to_mongod <- "C:\\Program Files\\MongoDB\\Server\\7.0\\bin\\mongod.exe"
db_path <- "C:\\data\\db"

mongo_process <- processx::process$new(path_to_mongod, c("--dbpath", db_path))

if (mongo_process$is_alive()) {
  cat("MongoDB server started successfully.\n")
} else {
  cat("Failed to start MongoDB server.\n")
}

mongo_url <- "mongodb://localhost:27017"  # URL de conexión a MongoDB
collection_name <- "TFG"  # Nombre de la colección en la que se almacenará el dataframe
mongo_conn <- mongo(collection_name, url = "mongodb://localhost:27017/")
cooml$insert(tabla_final)

# Cerrar la conexión
ml$disconnect()

