# Server logic dealing with file upload ########################################

docUploadModule <- function(input, output, session, con) {
  
  # Create containers
  vectors <- reactiveValues()
  selectedVectors <- reactiveValues()
  
  # points, lines, polygons
  vectors$points <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type = 'ST_Point';")
  vectors$lines <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type = 'ST_LineString';")
  vectors$polygons <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type = 'ST_Polygon';")
  
  # Select points in pop-up and window
  observeEvent(input$associatePoints, {
    showModal(modalDialog(
      title = "Associate document with location/points",
      DT::DTOutput("pointsTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectPoints", "Select")
      )
    ))
    output$pointsTable <- DT::renderDT({
      DT::datatable(vectors$points, 
                    selection = "multiple",
                    rownames = FALSE
      )
    })
  })
  observeEvent(input$selectPoints, {
    selectedVectors$points <- vectors$points[input$pointsTable_rows_selected,]
    removeModal()
    output$associatedPoints <- renderUI({
      HTML(paste("Selected Points: <br>", 
                 paste(selectedVectors$points$layer_name, selectedVectors$points$feature_name, selectedVectors$points$description, sep = " - ", collapse = "<br>"),
                 collapse = "<br>")
      )
    })
  })
  # Select lines in pop-up window
  observeEvent(input$associateLines, {
    showModal(modalDialog(
      title = "Associate document with lines",
      DT::DTOutput("linesTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectLines", "Select")
      )
    ))
    output$linesTable <- DT::renderDT({
      DT::datatable(vectors$lines, 
                    selection = "multiple",
                    rownames = FALSE
      )
    })
  })
  observeEvent(input$selectLines, {
    selectedVectors$lines <- vectors$lines[input$linesTable_rows_selected,]
    removeModal()
    output$associatedLines <- renderUI({
      HTML(paste("Selected Lines: <br>", 
                 paste(selectedVectors$lines$layer_name, selectedVectors$lines$feature_name, selectedVectors$lines$description, sep = " - ", collapse = "<br>"),
                 collapse = "<br>")
      )
    })
  })
  # Select polygons in pop-up window
  observeEvent(input$associatePolygons, {
    showModal(modalDialog(
      title = "Associate document with polygons",
      DT::DTOutput("polygonsTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectPolygons", "Select")
      )
    ))
    output$polygonsTable <- DT::renderDT({
      DT::datatable(vectors$polygons, 
                    selection = "multiple",
                    rownames = FALSE
      )
    })
  })
  observeEvent(input$selectPolygons, {
    selectedVectors$polygons <- vectors$polygons[input$polygonsTable_rows_selected,]
    removeModal()
    
    output$associatedPolygons <- renderUI({
      HTML(paste("Selected Polygons: <br>", 
                 paste(selectedVectors$polygons$layer_name, selectedVectors$polygons$feature_name, selectedVectors$polygons$description, sep = " - ", collapse = "<br>"),
                 collapse = "<br>")
      )
    })
  })
  
  observeEvent(input$addDocumentBtn, {
    if (is.null(input$documentFile$datapath) | is.null(input$documentName) | is.null(input$documentType) | is.null(input$documentDesc) | input$documentAuthors == "") {
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "Please fill in all mandatory fields.",
        easyClose = TRUE
      ))
      return()
    }
    exist_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM documents WHERE name = '", input$documentName, "';"))
    if (nrow(exist_name) != 0) {
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "The document name already exists in the database.",
        easyClose = TRUE
      ))
      return()
    }
    if (nchar(input$documentDesc) < 5) {
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "Please fill in the description with at least 5 characters.",
        easyClose = TRUE
      ))
      return()
    }
    if (input$documentDate == Sys.Date()) {
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "Please fill in the date. It's still on today!",
        easyClose = TRUE
      ))
      return()
    }
    
    authors <- unlist(strsplit(input$documentAuthors, "\n"))
    geom_ids <- c(selectedVectors$points$geom_id, selectedVectors$lines$geom_id, selectedVectors$polygons$geom_id)
    print(geom_ids)
    
    if (length(geom_ids) == 0) {
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "There are no points, lines, or polygons specified.",
        footer = tagList(
          actionButton("okButton", "That's ok"),
          actionButton("oopsButton", "Oops!")
        )
      ))
      
      observeEvent(input$okButton, {
        removeModal()
        authors <- strsplit(input$documentAuthors, "\n")
        insertHydrometDocument(path = input$documentFile$datapath,
                               name = input$documentName,
                               type = input$documentType,
                               description = input$documentDesc,
                               authors = unlist(authors),
                               publish_date = input$documentDate,
                               url = if (input$documentURL == "") NULL else input$documentURL,
                               geoms = if (length(geom_ids) == 0) NULL else geom_ids
        )
        output$documentUploadStatus <- showModal(modalDialog(
          title = "Document Upload Status",
          "Document has been uploaded to the database.",
          easyClose = TRUE
        ))
      })
      observeEvent(input$oopsButton, { #exit out of the observevent so the user can try again
        removeModal()
        return()
      })
    } else {
      insertHydrometDocument(path = input$documentFile$datapath,
                             name = input$documentName,
                             type = input$documentType,
                             description = input$documentDesc,
                             authors = authors,
                             publish_date = input$documentDate,
                             url = if (input$documentURL == "") NULL else input$documentURL,
                             geoms = geom_ids
      )
      output$documentUploadStatus <- showModal(modalDialog(
        title = "Document Upload Status",
        "Document has been uploaded to the database.",
        easyClose = TRUE
      ))
    }
  })
}
