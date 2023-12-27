install.packages("pdftools");
install.packages("tidyverse");
install.packages("stringr");
install.packages("readxl");

library(pdftools)
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
  doc <- pdf_text(nombreFichero)
  text <- paste(doc, collapse = "\n")
  return(strsplit(text, "\n")[[1]])
}

BuscarValor <- function(textoBuscar, lines) {
  Encontrados <- character()
  valores <- ifelse(grepl(textoBuscar, lines), 1, 0)
  valores <- which(valores == 1)
  posiciones <- nchar(textoBuscar)
  for (i in valores) {
    Encontrados <- c(Encontrados, trimws(substring(lines[i], nchar(textoBuscar) + 1)))
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

biopsia <- NB_values[seq(3, length(NB_values), 3)]
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

ficheros <- LeerFicherosPDF(rutaEntrada)
lista_ensayos <- numeric()
ensayos_finales <- numeric()
for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  ensayos <- 0
  for (line in lines) {
    resultado <- str_match(line, patron)
    if (!is.na(resultado)) {
      ensayos <- as.integer(resultado[2])
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

ficheros <- LeerFicherosPDF(rutaEntrada)
lista_tratamientos <- numeric()
tratamientos_finales <- numeric()
for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(ficheroPDF)
  tratamientos <- 0
  for (line in lines) {
    resultado <- str_match(line, patron2)
    if (!is.na(resultado)) {
      tratamientos <- as.integer(resultado[2])
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

ficheros <- LeerFicherosPDF(rutaEntrada)
for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && file_ext(ficheroPDF) == ".pdf") {
    print(ficheroPDF)
  }
}

numero_paciente <- character()
for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && file_ext(ficheroPDF) == ".pdf") {
    fichero1 <- basename(ficheroPDF)
    paciente <- str_remove(fichero1, "\\.pdf$")
    pacientes <- substr(paciente, 8, 8)
    numero_paciente <- c(numero_paciente, pacientes)
  }
}
print(numero_paciente)

chip2 <- character()
for (ficheroPDF in ficheros) {
  if (file.exists(ficheroPDF) && file_ext(ficheroPDF) == ".pdf") {
    resultado <- str_match(ficheroPDF, patron)
    if (!is.na(resultado)) {
      numero_chip <- resultado[2]
      chip2 <- c(chip2, numero_chip)
    }
  }
}
print(chip2)

ficheros <- LeerFicherosPDF(rutaEntrada)
max_mut <- 0
genes_mut2 <- list()
frecuencias_totales <- list()
patron_frecuencia <- "\\d{2}\\.\\d{2}"
for (ficheroPDF in ficheros) {
  nombreFichero <- file.path(rutaEntrada, ficheroPDF)
  lines <- LeerDocumento(nombreFichero)
  total_mut <- 0
  encontrados2 <- character()
  lista_frec <- character()
  for (mutacion in mutaciones) {
    if (mutacion %in% lines) {
      posicion <- which(lines == mutacion)
      if (mutacion == "FGFR4") {
        if (posicion < length(lines) && lines[posicion + 1] == "p.(P136L)") {
          next
        }
      }
      total_mut <- total_mut + 1
      encontrados2 <- c(encontrados2, mutacion)
    } else {
      benigno <- FALSE
      for (a in (posicion + 1):(posicion + 10)) {
        if ("Benign" %in% lines[a]) {
          benigno <- TRUE
        }
      }
      if (!benigno) {
        total_mut <- total_mut + 1
        encontrados2 <- c(encontrados2, mutacion)
      }
    }
    print(paste(nombreFichero, "- Existe:", mutacion))
    if (!benigno) {
      for (i in lines[(posicion):(posicion + 10)]) {
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


