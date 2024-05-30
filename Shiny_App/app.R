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
    tags$head(
      tags$script(
        HTML("
        $(document).keypress(function(event) {
          if (event.which == 32) { // 32 es el código ASCII para la barra espaciadora
            $('#splash').fadeOut('slow'); // Oculta el splash
          }
        });
      ")
      )
    ),
    tags$style(
      "
    #splash {
      position: fixed;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      background-color:   #1a1a1a;
      display: flex;
      justify-content: center;
      align-items: center;
      z-index: 9999; /* Establece un z-index alto para asegurar que esté en primer plano */
    }
    #splash-content {
      text-align: center;
    }
    #splash img {
      max-width: 100%; /* Ajusta el ancho máximo del logo */
      max-height: 100%; /* Ajusta la altura máxima del logo */
    }
    "
    ),
    div(id = "splash",
        div(id = "splash-content",
            h1("¡Presiona espacio para comenzar!"),
            img(src = "logoInicio2.png", alt = "Logo")
        )
    ),
    conditionalPanel(
      condition = "!(document.getElementById('splash').style.display === 'flex')",
      fluidPage(
  
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
          actionButton("subir", "Almacenar datos"),
          #downloadButton("download_excel", "Descargar tabla final pato")
        ),
        
        mainPanel(
          div(id = "table_proxy_div", style = "display: none;", dataTableOutput("table_proxy")),
          div(id = "table_proxy_div", style = "display: none;", dataTableOutput("table_proxy2")),
          textOutput("status"),
          tabsetPanel(
            id = "tabset",
            tabPanel("Genes mutados", DTOutput("pdf_content_output")),
            tabPanel("Genes patogénicos", DTOutput("pdf_content_output2"))
          )
        )
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
    library(mongolite)
    library(processx)
    library(rentrez)
    
    
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
    
    NombreFicherosPDF <- function() {
      ficherosNombre <- list()
      for (i in seq_along(input$upload$name)) {
        file <- input$upload
        ficherosNombre <- c(ficherosNombre, input$upload$name[i])
      }
      return(ficherosNombre)
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
      indice_limite <- grep(textoLimite, linesTotal)
      indice_limite2 <- grep(textoLimite2, linesTotal)
      if (length(indices_inicio2) != 0 | length(indices_inicio)>1){
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
      añadir <- cambiosPato <- frecuenciasPato <- mutaciones_pato <- patogen <- numero_iden_pato <- num_mutacionesPato <-
      diagnostico2 <- sexo <- porcentaje_tumoral <- calidad <- patogenicidad_buscadas <- cod_totales <-list()
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
    patron_codificacion <- "c\\.[0-9]+[A-Za-z>_]+"
    
    patron_diagnostico <- ".*Diagnóstico:\\s"
    patron_sexo <- ".*Sexo:\\s*"
    patron_porcentaje_tumoral <- ".*% células tumorales:\\s"
    patron_calidad <- ".*CALIDAD DE LA MUESTRA /LIMITACIONES PARA SU ANÁLISIS:\\s"
    
    textoInicio<- "Variantes de secuencia de ADN"
    textoInicio2<-"   Variaciones del número de copias"
    textoLimite <- "1 Basado en la versión ClinVar"
    textoLimite2 <-"Comentarios adicionales sobre las variantes"
    
    ficheroDiagnostico <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Diagnostico.xlsx")
    diagnostico <- read_excel(ficheroDiagnostico)
    diagnosticos_dic <- setNames(diagnostico$`NÚMERO DIAGNÓSTICO`, diagnostico$DIAGNÓSTICO)
    
    ficheroGenes <- file.path(PathBase, CarpetaEntrada, CarpetaDatos, "Genes.xlsx")
    genes <- read_excel(ficheroGenes)
    mutaciones <- unique(genes$GEN)
    mutaciones_dic <- setNames(genes$`Número gen`, genes$GEN)
    
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
      valor <- diagnosticos_dic[[diagnostico]]
      numeroDiag <- c(numeroDiag, valor)
    }
    
    NHC <- sapply(NHC_Data, function(x) unique(x)[1])
    
    lista_resultante <- lapply(Nbiopsia_Data, function(sublist) {
      sublist_sin_duplicados <- sublist[!duplicated(sublist)]
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
    
    pdf_files <- NombreFicherosPDF()
    #chip_match <- str_match(pdf_files, "v(\\d+)_")
    chip2 <- gsub(".*?([0-9]+\\.[0-9]+).*", "\\1", pdf_files)
    #chip2 <- as.integer(chip_match[, 2])

    frecuencias_totales <-patogenicidad_ordenadas<- genes_mut_ordenados <- frecuencias_totales2 <- genes_mut_ordenados2 <-list()

    for (ficheroPDF in ficheros) {
      #print(ficheroPDF)
      linesTotal <- LeerDocumento(ficheroPDF)
      lines <- acotarTexto(textoInicio, textoInicio2 ,linesTotal)
      posiciones <- mutaciones_patogenicas <- lista_frec <- mutaciones_pdf <- patogenicidad <- lista_cod <- c()
      lines_divididas <- strsplit(lines, "\\s+")
      print(lines)
      for (line in lines_divididas){
        if (length(grep(line[1], mutaciones))==1){
          posiciones <- c(posiciones, grep(line[1], lines))
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
      genes_mut_ordenados <- c(genes_mut_ordenados, list(mutaciones_pdf))
      patogenicidad_ordenadas <- c(patogenicidad_ordenadas, list(patogenicidad))
      frecuencias_totales <- c(frecuencias_totales, list(lista_frec))
      cod_totales <- c(cod_totales, list(lista_cod))
      #print(genes_mut_ordenados)
    }
    
    for (lista in seq_along(patogenicidad_ordenadas)){
      patogenicidad <- c()
      for (elemento in seq_along(patogenicidad_ordenadas[[lista]])){
        gen = paste(genes_mut_ordenados[[lista]][[elemento]], "[gene]", cod_totales[[lista]][[elemento]])
        res <- entrez_search(db = "clinvar", term = gen)
        if (length(res$ids)!=0){
          esums <- entrez_summary(db = "clinvar", id = res$ids)
          resumen <- extract_from_esummary(esums, "germline_classification")
          print(genes_mut_ordenados[[lista]][[elemento]])
          patogenicidad <- c(patogenicidad, resumen$description)
        }
        else{
          patogenicidad <- c(patogenicidad, "Sin resultados")
        }
      }
      patogenicidad_buscadas <- c(patogenicidad_buscadas, list(patogenicidad))
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
    
    #
#    for (ficheroPDF in ficheros) {
#      nombreFichero <- file.path(ficheroPDF)
#      linesTotal <- LeerDocumento(nombreFichero)
#      genpato2 <- list()
#      inicio <- FALSE
#      lines <- character()
#      for (line in linesTotal){
#        if (grepl(textoInicio, line) | grepl(textoInicio2, line)){
#          inicio <- TRUE
#        }else if (grepl(textoLimite, line) ){
#          inicio <- FALSE
#        }
#        if (grepl(textoLimite, line)==FALSE && inicio == TRUE){
#          lines <- c(lines,line)
#        }
#      }
#      for (mutacion in mutaciones){
#        coincidencias <- character()
#        coincidencias <- grepl(mutacion, lines)
#        for (coincidencia in 1:length(coincidencias)){
#          if (coincidencias[coincidencia] == TRUE){
#            posicion <- coincidencia
#            
#            for (a in strsplit(lines[posicion], " ")[[1]]) {
#              if (grepl("Pathogeni", a)) {
#                genpato2 <- c(genpato2, mutacion)
#              }
#            }
#          }
#        }
#      }
#      
#      patogen[[ficheroPDF]] <- genpato2
#      mutaciones_pato <- append(mutaciones_pato, list(genpato2))
#    }
    
    
    
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
                    'Biopsia sólida' = Biopsia_solida, 'Fecha de informe' = fechas,
                     'diagnostico'= unlist(diagnostico2), 'Sexo'=unlist(sexo), 'Porcentaje_tumoral'=unlist(porcentaje_tumoral), 'Calidad'=unlist(calidad))
    
    T2 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Diagnóstico' = textoDiag, 
                     'Número del diagnóstico' = numeroDiag)
    
    T3 <- data.frame('Número de chip' = chip2, 'Número de biopsia' = NB_values, 'Mutaciones detectadas' = I(genes_mut_ordenados), 
                     'Número de la mutación específica' = I(numero_iden), 'Total del número de mutaciones' = unlist(num_mutaciones), 
                     'Porcentaje de frecuencia alélica (ADN)' = I(frecuencias_totales), 'Fusiones ID' = I(fusiones), 
                     'Patogenicidad'=I(patogenicidad_ordenadas), 'Patogenicidad Buscada' = I(patogenicidad_buscadas))
    
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
    
    #Tabla_final
    
    output$table_proxy <- renderDataTable({
      datatable(tabla_final, selection = 'none', editable = 'cell')
    })
    
    proxy <- dataTableProxy("table_proxy")
    
    output$pdf_content_output <- DT::renderDataTable({
      DT::datatable(tabla_final, selection = 'none', editable = 'cell', extensions = 'Buttons', 
                    options = list(
                      dom = 'Bfrtip',
                      buttons = list('copy', 'print', list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download')
                      )
                    )
      )
    })
    
    observeEvent(input$pdf_content_output_cell_edit, {
      info <- input$pdf_content_output_cell_edit
      str(info)
      tabla_final <<- editData(tabla_final, info)
      replaceData(proxy, tabla_final, resetPaging = FALSE)
    })
    
    # Tabla_final_pato
    
    output$table_proxy2 <- renderDataTable({
      datatable(tabla_final_pato, selection = 'none', editable = 'cell')
    })
    
    proxy2 <- dataTableProxy("table_proxy2")
    
    output$pdf_content_output2 <- DT::renderDataTable({
      DT::datatable(tabla_final_pato, selection = 'none', editable = 'cell', extensions = 'Buttons', 
                    options = list(
                      dom = 'Bfrtip',
                      buttons = list('copy', 'print', list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download')
                      )
                    )
      )
    })
    
    observeEvent(input$pdf_content_output2_cell_edit, {
      info <- input$pdf_content_output2_cell_edit
      str(info)
      tabla_final_pato <<- editData(tabla_final_pato, info)
      replaceData(proxy2, tabla_final_pato, resetPaging = FALSE)
    })

    
   
    
    observeEvent(input$subir, {
      path_to_mongod <- "C:\\Program Files\\MongoDB\\Server\\7.0\\bin\\mongod.exe"
      db_path <- "C:\\data\\db"
      mongo_process <- processx::process$new(path_to_mongod, c("--dbpath", db_path))
      Sys.sleep(5)
      
      mongo_url <- "mongodb://localhost:27017"
      collection_name <- "Totales"
      collection_name_pato <- "Patogenicas"
      ml <- mongo(collection_name, url = mongo_url)
      ml_pato <- mongo(collection_name_pato, url = mongo_url)
      
      for (paciente in 1:nrow(tabla_final)) {
        nhc <- tabla_final$NHC[paciente]
        query <- paste('{"NHC": "', nhc, '"}', sep="")
        if (ml$count(query) == 0) {
          ml$insert(tabla_final[paciente, ])
        } 
      }
      for (paciente in 1:nrow(tabla_final_pato)) {
        nhc <- tabla_final_pato$NHC[paciente]
        query <- paste('{"NHC": "', nhc, '"}', sep="")
        if (ml_pato$count(query) == 0) {
          ml_pato$insert(tabla_final_pato[paciente, ])
        } 
      }
      print("guardados")
      ml$disconnect()
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