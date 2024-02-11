#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel(
    img(src = "LogoSACYL.png", align = "left", height = 40, width = 300),
    h1("PDF-Scrapping: Shiny app", style = "font-weight: 300; text-align: center; padding: 20px")
  ),
  
  br(),
  br(),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", h3("File input")),
      h3("Buttons"),
      actionButton("action", "Aceptar"),
      br(),
      br(),
      submitButton("Leer"),

    ),
    
    mainPanel(
      verbatimTextOutput("pdf_content_output")
    )
  )
)
        

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$action, {
    
    
    
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
    file_path <- input$file$datapath
    print(file_path)
    
    LeerFicherosPDF <- function(file_path) {
      ficheros <- file_path
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
      cadena <- character()
      valores <- ifelse(grepl(textoBuscar, lines), 1, 0)
      valores <- which(valores == 1)
      array_buscar <- strsplit(textoBuscar, " ")[[1]]
      for (i in valores) {
        array_line <- strsplit(lines[i], " ")[[1]]
        for (j in 1:(length(array_line)-length(array_buscar))){
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
      print(Encontrados)
      return(Encontrados)
    }
    
    fichero <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx")
    diagnostico <- read_excel(fichero)
    diagnosticos_dic <- setNames(diagnostico$`NÚMERO DIAGNÓSTICO`, diagnostico$DIAGNÓSTICO)
    for (diagnostico in names(diagnosticos_dic)) {
      valor <- diagnosticos_dic[[diagnostico]]
      #print(paste(diagnostico, valor))
    }
    
    fichero <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx")
    genes <- read_excel(fichero)
    mutaciones <- unique(genes$GEN)
    mutaciones_dic <- setNames(genes$`Número gen`, genes$GEN)
    for (gen in names(mutaciones_dic)) {
      valor <- mutaciones_dic[[gen]]
      #print(paste(gen, valor))
    }
    
    rutaEntrada <- file.path(PathBase, CarpetaEntrada, CarpetaInformes)
    ficheros <- file_path
    NHC_Data <- list()
    Nbiopsia_Data <- list()
    fecha_Data <- list()
    texto_Data <- list()
    lines <- LeerDocumento(file_path)
    NHC_Data[[length(NHC_Data) + 1]] <- BuscarValor("NHC:", lines)
    Nbiopsia_Data[[length(Nbiopsia_Data) + 1]] <- BuscarValor("biopsia:", lines)
    fecha_Data[[length(fecha_Data) + 1]] <- BuscarValor("Fecha:", lines)
    texto_Data[[length(texto_Data) + 1]] <- BuscarValor("de la muestra:", lines)
  
    output$pdf_content_output <- renderPrint({
      dput(NHC_Data)
      dput(Nbiopsia_Data)
      dput(fecha_Data)
      dput(texto_Data)
    })
  
    #print(NHC_Data)
    textoDiag <- character()
    numeroDiag <- numeric()
    for (i in texto_Data) {
      sinduplicados <- unique(i)
      textoDiag <- c(textoDiag, sinduplicados[1])
    }
    #print(textoDiag)
    for (diagnostico in textoDiag) {
      valor <- diagnosticos_dic[[diagnostico]]
      numeroDiag <- c(numeroDiag, valor)
    }
    #print(numeroDiag)
  
    NHC <- character()
    for (i in NHC_Data) {
      sinduplicadosNHC <- unique(i)
      NHC <- c(NHC, sinduplicadosNHC[1])
    }
    #print(NHC)
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
    #print(lista_resultante)
  
    NB_values <- unlist(lista_resultante)
    #print(NB_values)
    
    biopsia <- NB_values[seq(3, length(NB_values), 3)]
    B <- "1"
    C <- "3"
    P <- "2"
    Biopsia_solida <- character()
    for (i in biopsia) {
      if (i == "B") {
        Biopsia_solida <- c(Biopsia_solida, B)
        #print("1")
      } else if (i == "P") {
        Biopsia_solida <- c(Biopsia_solida, P)
        #print("2")
      } else {
        Biopsia_solida <- c(Biopsia_solida, C)
        #print("3")
      }
    }
    #print(Biopsia_solida)
  
    fechas <- character()
    for (i in fecha_Data) {
      sinduplicados <- unique(i)
      fechas <- c(fechas, sinduplicados[1])
    }
    #print(fechas)
    
    patron <- "(\\d+)\\s* Ensayos clínicos"
    ficheros <- file_path
    lista_ensayos <- numeric()
    ensayos_finales <- numeric()
    lines <- LeerDocumento(ficheros)
    ensayos <- 0
    for (line in lines) {
      resultado <- str_match(line, patron)
      if (!is.na(resultado[1,1])) {
        ensayos <- as.integer(resultado[2])
      }
    }
    
    lista_ensayos <- c(lista_ensayos, ensayos)
    #print(lista_ensayos)
    for (i in lista_ensayos) {
      if (i == 0) {
        ensayos_finales <- c(ensayos_finales, 0)
      } else {
        ensayos_finales <- c(ensayos_finales, 1)
      }
    }
    #print(ensayos_finales)
    
    patron2 <- "(\\d+)\\s* Tratamientos disponibles"
    ficheros <- file_path
    lista_tratamientos <- numeric()
    tratamientos_finales <- numeric()
    lines <- LeerDocumento(ficheros)
    tratamientos <- 0
    for (line in lines) {
      resultado <- str_match(line, patron2)
      if (!is.na(resultado[1,1])) {
        tratamientos <- as.integer(resultado[2])
      }
    }
    lista_tratamientos <- c(lista_tratamientos, tratamientos)
    #print(lista_tratamientos)
    for (i in lista_tratamientos) {
      if (i == 0) {
        tratamientos_finales <- c(tratamientos_finales, 0)
      } else {
        tratamientos_finales <- c(tratamientos_finales, 1)
      }
    }
    #print(tratamientos_finales)
  
    ficheros <- file_path
  
    numero_paciente <- character()
    fichero1 <- basename(ficheros)
    paciente <- str_remove(fichero1, "\\.pdf$")
    pacientes <- substr(paciente, 8, 8)
    numero_paciente <- c(numero_paciente, pacientes)

    #print(numero_paciente)
  
    chip2 <- character()
    resultado <- str_match(ficheros, patron)
    if (!is.na(resultado[1,1])) {
      numero_chip <- resultado[2]
      chip2 <- c(chip2, numero_chip)
    }

    #print(chip2)
  
    ficheros <- file_path
    max_mut <- 0
    genes_mut2 <- list()
    frecuencias_totales <- list()
    patron_frecuencia <- "\\d{2}\\.\\d{2}"
    lines <- LeerDocumento(ficheros)
    total_mut <- 0
    encontrados2 <- character()
    lista_frec <- character()
    posicion <- NA
    for (mutacion in mutaciones) {
      if (mutacion %in% lines) {
        posicion <- which(lines == mutacion)
        if (mutacion == "FGFR4") {
          if (posicion < length(lines) && lines[posicion + 1] == "p.(P136L)") {
            next
          }
        }
        #print("hola3")
        total_mut <- total_mut + 1
        encontrados2 <- c(encontrados2, mutacion)
      } else {
        #print(2.2)
        benigno <- FALSE
        if (!is.na(posicion)){
          for (a in (posicion + 1):(posicion + 10)) {
            #print()
            if ("Benign" %in% lines[a]) {
              benigno <- TRUE
            }
          }
        }
        if (!benigno) {
          total_mut <- total_mut + 1
          encontrados2 <- c(encontrados2, mutacion)
        }
      }
      #print(paste(ficheros, "- Existe:", mutacion))
      
      if (!benigno) {
        if (!is.na(posicion)){
          for (i in lines[(posicion):(posicion + 10)]) {
            resultado <- str_match(i, patron_frecuencia)
            if (!is.na(resultado)) {
              frec <- resultado[1]
              lista_frec <- c(lista_frec, frec)
            }
          }
        }
      }
    }
    genes_mut2[[gsub("\\\\", "_", ficheros)]] <- encontrados2
    if (total_mut > max_mut) {
      max_mut <- total_mut
    }
    frecuencias_totales <- c(frecuencias_totales, lista_frec)
    ########
    
    #print(frecuencias_totales)
    
    mut <- unlist(genes_mut2)
    #print(mut)
    
    num_mutaciones <- sapply(mut, length)
    #print(num_mutaciones)
    
    numero_iden <- lapply(mut, function(x) {
      sapply(x, function(gen) {
        mutaciones_dic[[gen]]
      })
    })
    #print(numero_iden)
    
    fusiones <- character()
    lines <- LeerDocumento(ficheros)
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
  })

}

# Run the application
shinyApp(ui = ui, server = server)



#sidebarLayout(
#  sidebarPanel(
#    sliderInput("bins",
#                "Number of bins:",
#                min = 1,
#                max = 50,
#                value = 30),
    
#    tags$div(class="header", checked=NA,
#             tags$p("Hipervinculo"),
#             tags$a(href="https://www.ubu.es/", "Click Here!")
#    )
#  ),


#file_path <- input$file$datapath
#pdf_content <- pdftools::pdf_text(file_path)
#output$pdf_content_output <- renderPrint({
#  cat(pdf_content, sep = "\n")
#})
#})
#observeEvent(input$Analizar, {