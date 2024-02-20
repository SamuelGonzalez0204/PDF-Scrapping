install.packages(c("pdftools", "tidyverse", "stringr", "readxl", "xlsx", "shinyFiles"))

runApp("Shiny_App")

library(tools)
library(pdftools)
library(tidyverse)
library(stringr)
library(readxl)
library(xlsx)

CarpetaEntrada <- "INPUT"
CarpetaDatos <- "DATOS"
CarpetaInformes <- "INFORMES"
CarpetaSalida <- "OUTPUT"
CarpetaResultados <- "RESULTADOS"
PathBase <- getwd()

LeerFicherosPDF <- function(ruta) {
  ficheros <- list.files(path = ruta, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  return(ficheros)
}

LeerDocumento <- function(nombreFichero) {
  #print("llegue")
  doc <- pdf_text(nombreFichero)
  #print(doc)
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

fichero <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx")
diagnostico <- read_excel(fichero)
diagnosticos_dic <- setNames(diagnostico$`NÚMERO DIAGNÓSTICO`, diagnostico$DIAGNÓSTICO)
for (diagnostico in names(diagnosticos_dic)) {
  valor <- diagnosticos_dic[[diagnostico]]
  print(paste(diagnostico, valor))
}

fichero <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx")
genes <- read_excel(fichero)
mutaciones <- unique(genes$GEN)
mutaciones_dic <- setNames(genes$`Número gen`, genes$GEN)
for (gen in names(mutaciones_dic)) {
  valor <- mutaciones_dic[[gen]]
  print(paste(gen, valor))
}

rutaEntrada <- file.path(PathBase, CarpetaEntrada, CarpetaInformes)
ficheros <- LeerFicherosPDF(rutaEntrada)
print(ficheros)
NHC_Data <- list()
Nbiopsia_Data <- list()
fecha_Data <- list()
texto_Data <- list()
for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  NHC_Data[[length(NHC_Data) + 1]] <- BuscarValor("NHC:", lines)
  Nbiopsia_Data[[length(Nbiopsia_Data) + 1]] <- BuscarValor("biopsia:", lines)
  fecha_Data[[length(fecha_Data) + 1]] <- BuscarValor("Fecha:", lines)
  texto_Data[[length(texto_Data) + 1]] <- BuscarValor("de la muestra:", lines)
}
print(NHC_Data)
print(Nbiopsia_Data)
print(fecha_Data)
print(texto_Data)

textoDiag <- character()
numeroDiag <- numeric()
for (i in texto_Data) {
  sinduplicados <- unique(i)
  textoDiag <- c(textoDiag, sinduplicados[1])
}
print(textoDiag)
for (diagnostico in textoDiag) {
  valor <- diagnosticos_dic[[diagnostico]]
  numeroDiag <- c(numeroDiag, valor)
}
print(numeroDiag)

NHC <- character()
for (i in NHC_Data) {
  sinduplicadosNHC <- unique(i)
  NHC <- c(NHC, sinduplicadosNHC[1])
}
print(NHC)

lista_resultante <- list()
elementos_vistos <- character()
for (sublist in Nbiopsia_Data) {
  sublist_sin_duplicados <- character()
  for (elemento in sublist) {
    if (!(elemento %in% elementos_vistos)) {
      sublist_sin_duplicados <- c(sublist_sin_duplicados, elemento)
    }
    elementos_vistos <- c(elementos_vistos, elemento)
  }
  lista_resultante <- c(lista_resultante, sublist_sin_duplicados)
}
print(lista_resultante)

NB_values <- unlist(lista_resultante)
print(NB_values)

biopsia<- character()
for (i in NB_values){
  caracter <- substr(i, 3, 3)
  biopsia <- c(biopsia, caracter)
}
  
print(biopsia)
B <- "1"
C <- "3"
P <- "2"
Biopsia_solida <- character()
for (i in biopsia) {
  if (i == "B") {
    Biopsia_solida <- c(Biopsia_solida, B)
    print("1")
  } else if (i == "P") {
    Biopsia_solida <- c(Biopsia_solida, P)
    print("2")
  } else {
    Biopsia_solida <- c(Biopsia_solida, C)
    print("3")
  }
}
print(Biopsia_solida)

fechas <- character()
for (i in fecha_Data) {
  sinduplicados <- unique(i)
  fechas <- c(fechas, sinduplicados[1])
}
print(fechas)

patron <- "(\\d+)\\s* Ensayos clínicos"
ficheros <- LeerFicherosPDF(rutaEntrada)
lista_ensayos <- numeric()
ensayos_finales <- numeric()

for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  ensayos <- 0
  for (line in lines) {
    resultado <- str_match(line, patron)
    if (!is.na(resultado[1])) {
      ensayos <- as.integer(resultado[1, 2]) 
    }
  }
  lista_ensayos <- c(lista_ensayos, ensayos)
}
print(lista_ensayos)

for (i in lista_ensayos) {
  if (i == 0) {
    ensayos_finales <- c(ensayos_finales, 0)
  } else {
    ensayos_finales <- c(ensayos_finales, 1)
  }
}
print(ensayos_finales)

patron2 <- "(\\d+)\\s* Tratamientos disponibles"
ficheros <- LeerFicherosPDF(rutaEntrada)
lista_tratamientos <- numeric()
tratamientos_finales <- numeric()

for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  tratamientos <- 0
  for (line in lines) {
    resultado <- str_match(line, patron2)
    if (!is.na(resultado[1])) {
      tratamientos <- as.integer(resultado[1, 2]) 
    }
  }
  lista_tratamientos <- c(lista_tratamientos, tratamientos)
}
print(lista_tratamientos)
for (i in lista_tratamientos) {
  if (i == 0) {
    tratamientos_finales <- c(tratamientos_finales, 0)
  } else {
    tratamientos_finales <- c(tratamientos_finales, 1)
  }
}
print(tratamientos_finales)


for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && grepl("\\.pdf$", ficheroPDF)) {
    print(ficheroPDF)
  }
}

numero_paciente <- character()
for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && grepl("\\.pdf$", ficheroPDF)) {
    fichero1 <- basename(ficheroPDF)
    paciente <- str_remove(fichero1, "\\.pdf$")
    pacientes <- substr(paciente, 8, 8)
    numero_paciente <- c(numero_paciente, pacientes)
  }
}
print(numero_paciente)

chip2 <- c()
for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && grepl("\\.pdf$", ficheroPDF)) {    
    patron <- "v(\\d+)_"
    resultado <- str_match(ficheroPDF, patron)
    
    if (!is.na(resultado[1])) {
      numero_chip <- resultado[1,2]
      chip2 <- c(chip2, numero_chip)
    }
  }
}
print(chip2)
benigno <- FALSE
resultado <- FALSE
ficheros <- LeerFicherosPDF(rutaEntrada)
max_mut <- 0
genes_mut2 <- list()
genes_mut_ordenados <- list()
frecuencias_totales <- list()
textoInicio<- "Detalles de la variante"
textoInicio2<-"   Variaciones del número de copias"
textoLimite <- "1 Basado en la versión ClinVar 20180225"
patron_frecuencia <- "\\d{2}\\.\\d{2}"
for (ficheroPDF in ficheros) {
  inicio <- FALSE
  nombreFichero <- ficheroPDF
  linesTotal <- LeerDocumento(nombreFichero)
  lines <- character()
  for (line in linesTotal){
    if (line == textoInicio | line == textoInicio2){
      inicio <- TRUE
    }
    if (line != textoLimite && inicio == TRUE){
      lines <- c(lines,line)
    }else if (line == textoLimite){
      inicio <- FALSE
    }
  }
  total_mut <- 0
  encontrados2 <- list()
  lista_frec <- character()
  for (mutacion in mutaciones) {
    coincidencias <- character()
    coincidencias <- grepl(mutacion, lines)
    for (coincidencia in 1:length(coincidencias)){
      if (coincidencias[coincidencia] == TRUE){
        posicion <- coincidencia
      
        if (mutacion == "FGFR4") {
          benigno2<-FALSE
          for (a in strsplit(lines[posicion], " ")[[1]]) {
            #print(a)
            if ("p.(P136L)"==a) {
              benigno2 <- TRUE
            }
          }
          #print(benigno2)
          if (benigno2) {
            next
          }else{
            total_mut <- total_mut + 1
            encontrados2 <- c(encontrados2, mutacion)
          }
      } else {
          benigno <- FALSE
          for (a in strsplit(lines[posicion], " ")[[1]]) {
            if (grepl("Benign", a)) {
              benigno <- TRUE
            }
          }
          if (!benigno) {
            total_mut <- total_mut + 1
            encontrados2 <- c(encontrados2, mutacion)
            genes_mut2 <- append(genes_mut2,mutacion)
            print(paste(nombreFichero, "- Existe:", mutacion))
            for (i in strsplit(lines[posicion], " ")[[1]]) {
              resultado <- str_match(i, patron_frecuencia)
              if (!is.na(resultado)) {
                frec <- resultado[1]
                lista_frec <- c(lista_frec, frec)
              }
            }
          }
        }
      }
    }
    
  }

  if (total_mut > max_mut) {
    max_mut <- total_mut
  }
  frecuencias_totales <- append(frecuencias_totales, list(lista_frec))
  genes_mut_ordenados <- append(genes_mut_ordenados, list(genes_mut2))
  genes_mut2 <- list()
}

print(frecuencias_totales)

print(genes_mut_ordenados)

num_mutaciones<- list()

for (lista in genes_mut_ordenados){
  num_mutaciones<- c(num_mutaciones, length(lista))
}
print(num_mutaciones)

numero_iden <- lapply(genes_mut_ordenados, function(x) {
  sapply(x, function(gen) {
    mutaciones_dic[[gen]]
  })
})
print(numero_iden)

fusiones <- character()
for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(file.path(ficheroPDF))
  variantes <- character()
  for (linea in lines) {
    for (mutacion in mutaciones) {
      patronGen <- paste0("[A-Z0-9]{1,}-", mutacion)
      if (grepl(patronGen, linea)) {
        print(strsplit(linea, " ")[[1]])
        for (palabra in strsplit(linea, " ")[[1]]){
          if (grepl(patronGen, palabra)){
            print(palabra)
            variantes <- c(variantes, palabra)
          }
        }
      }
    }
  }
  fusiones <- append(fusiones, list(variantes))
}
print(fusiones)




for (ficheroPDF in ficheros) {
  nombreFichero <- file.path(ficheroPDF)
  lines <- LeerDocumento(nombreFichero)
  inicio <- FALSE
  lines <- character()
  for (line in linesTotal){
    if (line == textoInicio | line == textoInicio2){
      inicio <- TRUE
    }
    if (line != textoLimite && inicio == TRUE){
      lines <- c(lines,line)
    }else if (line == textoLimite){
      inicio <- FALSE
    }
  }
  for (mutacion in mutaciones){
    coincidencias <- character()
    coincidencias <- grepl(mutacion, lines)
    for (coincidencia in 1:length(coincidencias)){
      if (coincidencias[coincidencia] == TRUE){
        posicion <- coincidencia
        
        for (a in strsplit(lines[posicion], " ")[[1]]) {
          if (grepl("Pathogeni", a)) {
            print(paste(nombreFichero, " - Existe: ", mutacion, " - Pathogenic"))
          }
        }
      }
    }
  }
}

patron_frecuencia <- "\\d{2}\\.\\d{2}"
frecuenciasPato <- list()

for (ficheroPDF in ficheros) {
  lista_frec <- list()
  nombreFichero <- file.path(ficheroPDF)
  lines <- LeerDocumento(nombreFichero)
  inicio <- FALSE
  nombreFichero <- ficheroPDF
  linesTotal <- LeerDocumento(nombreFichero)
  lines <- character()
  for (line in linesTotal){
    if (line == textoInicio | line == textoInicio2){
      inicio <- TRUE
    }
    if (line != textoLimite && inicio == TRUE){
      lines <- c(lines,line)
    }else if (line == textoLimite){
      inicio <- FALSE
    }
  }
  for (mutacion in mutaciones){
    coincidencias <- character()
    coincidencias <- grepl(mutacion, lines)
    for (coincidencia in 1:length(coincidencias)){
      if (coincidencias[coincidencia] == TRUE){
        posicion <- coincidencia
      
      for (a in strsplit(lines[posicion], " ")[[1]]) {
        if (grepl("Pathogeni", a)) {
          print(paste(nombreFichero, " - Existe: ", mutacion, " - Pathogenic"))

          for (i in strsplit(lines[posicion], " ")[[1]]) {
            resultado <- str_match(i, patron_frecuencia)
            if (!is.na(resultado)) {
              frec <- resultado[1]
              lista_frec <- c(lista_frec, frec)
              }
            }
          }
        }
      }
    }
  }
  frecuenciasPato <- append(frecuenciasPato, list(lista_frec))
}
print(frecuenciasPato)


patogen <- list()
mutaciones_pato <- list()

for (ficheroPDF in ficheros) {
  nombreFichero <- file.path(ficheroPDF)
  linesTotal <- LeerDocumento(nombreFichero)
  genpato2 <- list()
  inicio <- FALSE
  lines <- character()
  for (line in linesTotal){
    if (line == textoInicio | line == textoInicio2){
      inicio <- TRUE
    }
    if (line != textoLimite && inicio == TRUE){
      lines <- c(lines,line)
    }else if (line == textoLimite){
      inicio <- FALSE
    }
  }
  for (mutacion in mutaciones){
    coincidencias <- character()
    coincidencias <- grepl(mutacion, lines)
    for (coincidencia in 1:length(coincidencias)){
      if (coincidencias[coincidencia] == TRUE){
        posicion <- coincidencia
        
        for (a in strsplit(lines[posicion], " ")[[1]]) {
          if (grepl("Pathogeni", a)) {
            genpato2 <- c(genpato2, mutacion)
          }
        }
      }
    }
  }
  
  patogen[[ficheroPDF]] <- genpato2
  mutaciones_pato <- append(mutaciones_pato, list(genpato2))
}
print(patogen)

print(mutaciones_pato)

numero_iden_pato <- list()

for (i in mutaciones_pato) {
  valores <- sapply(i, function(gen) ifelse(is.null(mutaciones_dic[[gen]]), 0, mutaciones_dic[[gen]]))
  numero_iden_pato <- append(numero_iden_pato, list(valores))
}
print(numero_iden_pato)

num_mutacionesPato <- list()
for (lista in numero_iden_pato){
  num_mutacionesPato <- c(num_mutacionesPato, length(lista))
}
print(unlist(num_mutacionesPato))

print(length(chip2))

print(numero_paciente)

print(NHC)

print(length(NB_values))

print(Biopsia_solida)

print(fecha_Data)

print(fechas)

print(texto_Data)

print(textoDiag)

print(numeroDiag)

print(length(unlist(num_mutaciones)))

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




T1 <- data.frame('Número de chip' = chip2, 'Número de paciente' = numero_paciente, 'NHC' = NHC, 
                 'Número de biopsia' = NB_values, 'Biopsia sólida' = Biopsia_solida, 'Fecha de informe' = fechas)
print(T1)

T2 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Diagnóstico' = textoDiag, 
                 'Número del diagnóstico' = numeroDiag)
print(T2)

T3 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Mutaciones detectadas' = genes_mut_ordenados, 
                 'Número de la mutación específica' = numero_iden, 'Total del número de mutaciones' = unlist(num_mutaciones), 
                 'Porcentaje de frecuencia alélica (ADN)' = frecuencias_totales, stringsAsFactors = FALSE)
print(T3)

T4 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Genes patogénicos' = mutaciones_pato, 
                 'Número de la mutación específica' = numero_iden_pato, '% frecuencia alélica' = frecuenciasPato, 
                 'Total de mutaciones patogénicas' = num_mutacionesPato)
print(T4)
T5 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Ensayos clínicos' = lista_ensayos, 
                 'SI/NO ensayo' = ensayos_finales, 'Fármaco aprobado' = lista_tratamientos, 'SI/NO fármacos' = tratamientos_finales)


tabla_unida <- merge(T1, T2, by = c("Número.de.chip", "Número.de.biopsia"))
print(tabla_unida)

tabla_unida2 <- merge(tabla_unida, T3, by = c("Número.de.chip", "Número.de.biopsia"))
print(tabla_unida2)
tabla_final <- merge(tabla_unida2, T5, by = c("Número.de.chip", "Número.de.biopsia"))
tabla_unida3 <- merge(tabla_unida, T4, by = c("Número.de.chip", "Número.de.biopsia"))
tabla_final_pato <- merge(tabla_unida3, T5, by = c("Número.de.chip", "Número.de.biopsia"))

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


