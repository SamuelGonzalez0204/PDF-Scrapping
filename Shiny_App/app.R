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
library(DT)
#library(shinyFiles)

ui <- fluidPage(
  
  titlePanel(
    img(src = "LogoSACYL.png", align = "left", height = 40, width = 300),
    #img(src = "LogoUBU.png", align = "right", height = 50, width = 100)
    h1("PDF-Scrapping: Shiny app", style = "font-weight: 300; text-align: center; padding: 20px")
  ),
  
  br(),
  br(),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload","Seleccione un archivo PDF:", NULL, buttonLabel = "Upload...", multiple = TRUE),
      actionButton("action", "Analizar"),
      br(),
      br(),
      actionButton("overwrite_btn", "Sobreescribir"),
      #downloadButton("download_excel", "Descargar tabla final pato")
    ),
    
    mainPanel(
      textOutput("status"),
      tabsetPanel(
        id = "tabset",
        tabPanel("tabla final", DTOutput("pdf_content_output")),
        tabPanel("tabla final pato", DTOutput("pdf_content_output2"))
      )
    )
  )
)
        

server <- function(input, output) {
  
  observeEvent(input$action, {
    
    
    library(tools)
    library(pdftools)
    library(tidyverse)
    library(stringr)
    library(readxl)
    library(openxlsx)
    
    
    CarpetaEntrada <- "INPUT"
    CarpetaDatos <- "DATOS"
    CarpetaInformes <- "INFORMES"
    CarpetaSalida <- "OUTPUT"
    CarpetaResultados <- "RESULTADOS"
    PathBase <- getwd()
    
    req(input$upload)  
    
    
    LeerFicherosPDF <- function() {
      ficheros <- list()
      for (i in seq_along(input$upload$name)) {
        file <- input$upload
        ficheros <- c(ficheros, file$datapath[i])
      }
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
    
    acotarTexto<-function(textoInicio, textoInicio2, linesTotal){
      lines <- character()
      indices_inicio <- grep(textoInicio, linesTotal)
      indices_inicio2 <- grep(textoInicio2, linesTotal)
      indice_limite <- grep(textoLimite, linesTotal)
      indice_limite2 <- grep(textoLimite2, linesTotal)
      if (length(indices_inicio2) != 0){
        if (length(indice_limite2) != 0){
          lines <- linesTotal[(indices_inicio + 1):(indice_limite2[1] - 1)]
        }else{
          lines <- linesTotal[(indices_inicio + 1):(indice_limite[2] - 1)]
        }
      }else{
        lines <- linesTotal[(indices_inicio + 1):(indice_limite[1] - 1)]
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
    ficheros <- LeerFicherosPDF()
    
    NHC_Data <- Nbiopsia_Data <- fecha_Data <- texto_Data <- genes_mut2 <- genes_mut_ordenados <- frecuencias_totales <- num_mutaciones <- numero_iden <- 
      añadir <- cambiosPato <- frecuenciasPato <- mutaciones_pato <- patogen <- numero_iden_pato <- num_mutacionesPato <- list()
    textoDiag <- NHC <- biopsia <- fechas <- chip2 <- fusiones <- character()
    numeroDiag <- lista_ensayos <- ensayos_finales <- lista_tratamientos <- tratamientos_finales <- numeric()
    
    benigno <- resultado <- FALSE
    max_mut <- 0
    
    B <- "1"
    C <- "3"
    P <- "2"
    
    patron <- "(\\d+)\\s* Ensayos clínicos"
    patron2 <- "(\\d+)\\s* Tratamientos disponibles"
    patron_frecuencia <- "\\d{2}\\.\\d{2}"
    patron_cambio <-"\\(.*?\\)"
    
    textoInicio<- "Detalles de la variante"
    textoInicio2<-"   Variaciones del número de copias"
    textoLimite <- "1 Basado en la versión ClinVar"
    textoLimite2 <-"Comentarios adicionales sobre las variantes"
    
    
    for (ficheroPDF in ficheros) {
      lines <- LeerDocumento(ficheroPDF)
      NHC_Data[[length(NHC_Data) + 1]] <- BuscarValor("NHC:", lines)
      Nbiopsia_Data[[length(Nbiopsia_Data) + 1]] <- BuscarValor("biopsia:", lines)
      fecha_Data[[length(fecha_Data) + 1]] <- BuscarValor("Fecha:", lines)
      texto_Data[[length(texto_Data) + 1]] <- BuscarValor("de la muestra:", lines)
    }
    
    textoDiag <- sapply(texto_Data, function(x) unique(x)[1])
    
    for (diagnostico in textoDiag) {
      valor <- diagnosticos_dic[[diagnostico]]
      numeroDiag <- c(numeroDiag, valor)
    }
    
    NHC <- sapply(NHC_Data, function(x) unique(x)[1])
    
    lista_resultante <- lapply(Nbiopsia_Data, function(sublist) {
      sublist_sin_duplicados <- sublist[!duplicated(sublist)]
      sublist_sin_duplicados
    })
    
    NB_values <- unlist(lista_resultante)
    
    biopsia<- sapply(lista_resultante, function(x) substr(x, 3,3))
    
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
    chip_match <- str_match(pdf_files, "v(\\d+)_")
    chip2 <- as.integer(chip_match[, 2])
    
    for (ficheroPDF in ficheros) {
      linesTotal <- LeerDocumento(ficheroPDF)
      lines <- acotarTexto(textoInicio, textoInicio2 ,linesTotal)
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
                if ("p.(P136L)"==a) {
                  benigno2 <- TRUE
                }
              }
              if (benigno2) {
                next
              }else{
                total_mut <- total_mut + 1
                encontrados2 <- c(encontrados2, mutacion)
              }
            } else {
              benigno <- FALSE
              for (a in strsplit(lines[posicion], " ")[[1]]) {
                if (grepl("Benign", a) | grepl("benign", a)) {
                  benigno <- TRUE
                }
              }
              if (!benigno) {
                total_mut <- total_mut + 1
                encontrados2 <- c(encontrados2, mutacion)
                genes_mut2 <- c(genes_mut2,mutacion)
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
      frecuencias_totales <- c(frecuencias_totales, list(lista_frec))
      genes_mut_ordenados <- c(genes_mut_ordenados, list(unlist(genes_mut2)))
      genes_mut2 <- list()
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
    
    for (ficheroPDF in ficheros) {
      lista_frec <- list()
      lista_cambio <- list()
      linesTotal <- LeerDocumento(ficheroPDF)
      lines <- acotarTexto(textoInicio, textoInicio2 ,linesTotal)
      for (mutacion in mutaciones){
        coincidencias <- character()
        coincidencias <- grepl(mutacion, lines)
        for (coincidencia in 1:length(coincidencias)){
          if (coincidencias[coincidencia] == TRUE){
            posicion <- coincidencia
            
            for (a in strsplit(lines[posicion], " ")[[1]]) {
              if (grepl("Pathogeni", a)) {

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
    
    for (i in mutaciones_pato) {
      valores <- sapply(i, function(gen) ifelse(is.null(mutaciones_dic[[gen]]), 0, mutaciones_dic[[gen]]))
      numero_iden_pato <- append(numero_iden_pato, list(valores))
    }
    
    for (lista in numero_iden_pato){
      num_mutacionesPato <- c(num_mutacionesPato, length(lista))
    }
    
    genes_mut_ordenados <- lapply(genes_mut_ordenados, function(x) if(length(x) == 0) NA else x)
    frecuencias_totales <- lapply(frecuencias_totales, function(x) if(length(x) == 0) NA else x)
    fusiones <- lapply(fusiones, function(x) if(length(x) == 0) NA else x)
    numero_iden <- lapply(numero_iden, function(x) if(length(x) == 0) NA else x)
    mutaciones_pato <- lapply(mutaciones_pato, function(x) if(length(x) == 0) NA else x)
    frecuenciasPato <- lapply(frecuenciasPato, function(x) if(length(x) == 0) NA else x)
    numero_iden_pato <- lapply(numero_iden_pato, function(x) if(length(x) == 0) NA else x)
    cambiosPato <- lapply(cambiosPato, function(x) if(length(x) == 0) NA else x)
    
    
    T1 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'NHC' = NHC, 
                     'Número de biopsia' = NB_values, 'Biopsia sólida' = Biopsia_solida, 'Fecha de informe' = fechas)

    T2 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Diagnóstico' = textoDiag, 
                     'Número del diagnóstico' = numeroDiag)

    T3 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Mutaciones detectadas' = I(genes_mut_ordenados), 
                     'Número de la mutación específica' = I(numero_iden), 'Total del número de mutaciones' = unlist(num_mutaciones), 
                     'Porcentaje de frecuencia alélica (ADN)' = I(frecuencias_totales), 'Fusiones ID' = I(fusiones))

    T4 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Genes patogénicos' = I(mutaciones_pato), 
                     'Número de la mutación específica' = I(numero_iden_pato), '% frecuencia alélica' = I(frecuenciasPato),
                     'Cambios' = I(cambiosPato), 'Total de mutaciones patogénicas' = I(num_mutacionesPato))

    T5 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Ensayos clínicos' = lista_ensayos, 
                     'SI/NO ensayo' = ensayos_finales, 'Fármaco aprobado' = lista_tratamientos, 'SI/NO fármacos' = tratamientos_finales)
    
    
    tabla_unida <- merge(T1, T2, by = c("Número.de.chip", "Número.de.biopsia"), all=TRUE)
    tabla_unida2 <- merge(tabla_unida, T3, by = c("Número.de.chip", "Número.de.biopsia"), all=TRUE)
    tabla_final <- merge(tabla_unida2, T5, by = c("Número.de.chip", "Número.de.biopsia"))
    tabla_unida3 <- merge(tabla_unida, T4, by = c("Número.de.chip", "Número.de.biopsia"))
    tabla_final_pato <- merge(tabla_unida3, T5, by = c("Número.de.chip", "Número.de.biopsia"))
    
    output$pdf_content_output <- DT::renderDataTable({
      DT::datatable(tabla_final, filter = 'top',extensions = 'Buttons', 
                    options = list(
                      dom = 'Bfrtip',
                      buttons =  
                        list('copy', 'print', list(
                          extend = 'collection',
                          buttons = c('csv', 'excel', 'pdf'),
                          text = 'Download')
                      )
                    )
                  )
    })
    output$pdf_content_output2 <- DT::renderDataTable({
      DT::datatable(tabla_final_pato, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons =  
          list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'))))
    })
    

    
    output$panel <- renderText({paste("Panel actual: ", input$tabset)})
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0(input$file, ".csv", sep = "")
      },
      content = function(file) {
        vroom::vroom_write(tabla_final, file, col_names = TRUE, 
                           append=TRUE,quote = "needed", bom = TRUE)
      }
    )
    
    list_to_string <- function(x) {
      if (is.list(x)) {
        paste(x, collapse = ", ")
      } else {
        as.character(x)
      }
    }
    
    observeEvent(input$overwrite_btn, {
      req(input$upload)  # Verifica que se haya seleccionado un archivo
      
      # Lee el contenido del archivo seleccionado
      wb <- loadWorkbook(input$upload$datapath)
      
      tabla_final_texto <- lapply(tabla_final, function(col) {
        if (is.list(col)) {
          sapply(col, list_to_string)
        } else {
          col
        }
      })
      addWorksheet(wb, "Nueva Hoja")
      writeData(wb, "Nueva Hoja", tabla_final_texto, startCol = 1, startRow = 1)
      
      # Guarda el libro actualizado
      saveWorkbook(wb, input$upload$datapath, overwrite = TRUE)
      
      # Muestra un mensaje de éxito
      output$status <- renderText({
        paste("Se ha añadido contenido al archivo:", input$upload$name)
      })
    })

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