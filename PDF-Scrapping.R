install.packages(c("pdftools", "tidyverse", "stringr", "readxl"))

runApp("Shiny_App")

library(tools)
library(pdftools)
library(tidyverse)
library(stringr)
library(readxl)

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

ficheros <- LeerFicherosPDF(rutaEntrada)
max_mut <- 0
genes_mut2 <- list()
frecuencias_totales <- list()
textoLimite <- "1 Basado en la versión ClinVar 20180225"
patron_frecuencia <- "\\d{2}\\.\\d{2}"
for (ficheroPDF in ficheros) {
  nombreFichero <- ficheroPDF
  linesTotal <- LeerDocumento(nombreFichero)
  lines <- character()
  for (line in linesTotal){
    if (line != textoLimite){
      lines <- c(lines,line)
    }else{
      break
    }
  }
  total_mut <- 0
  encontrados2 <- character()
  lista_frec <- character()
  for (mutacion in mutaciones) {
    coincidencias <- character()
    coincidencias <- grepl(mutacion, lines)
    for (coincidencia in 1:length(coincidencias)){
      if (coincidencias[coincidencia] == TRUE){
        posicion <- coincidencia
      
        if (mutacion == "FGFR4") {
          if (posicion < length(lines) && lines[posicion + 1] == "p.(P136L)") {
            next
          }else{
            total_mut <- total_mut + 1
            encontrados2 <- c(encontrados2, mutacion)
          }
      } else {
          benigno <- FALSE
          for (a in lines[posicion]) {
            if ("Benign"==a) {
              benigno <- TRUE
            }
          }
          if (!benigno) {
            total_mut <- total_mut + 1
            encontrados2 <- c(encontrados2, mutacion)
          }
        }
      }
    }
    print(paste(nombreFichero, "- Existe:", mutacion))
    if (!benigno) {
      for (i in lines[posicion]) {
        resultado <- str_match(i, patron_frecuencia)
        if (!is.na(resultado)) {
          frec <- resultado[1]
          lista_frec <- c(lista_frec, frec)
        }
      }
    }
  }
  genes_mut2[[gsub("\\\\", "_", ficheroPDF)]] <- encontrados2
  if (total_mut > max_mut) {
    max_mut <- total_mut
  }
  frecuencias_totales <- c(frecuencias_totales, lista_frec)
}
print(frecuencias_totales)

mut <- unlist(genes_mut2)
print(mut)

num_mutaciones <- sapply(mut, length)
print(num_mutaciones)

numero_iden <- lapply(mut, function(x) {
  sapply(x, function(gen) {
    mutaciones_dic[[gen]]
  })
})
print(numero_iden)

fusiones <- character()
for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(file.path(rutaEntrada, ficheroPDF))
  variantes <- character()
  for (linea in lines) {
    for (mutacion in mutaciones) {
      patronGen <- paste0("[A-Z0-9]{1,}-", mutacion)
      if (grepl(patronGen, linea)) {
        variantes <- c(variantes, linea)
      }
    }
  }
  fusiones <- c(fusiones, variantes)
}