# This script is the server-side of a Shiny application that creates an easy-to-use interface for database administrators to update the database with new data. The UI side script is located in the inst/ folder. It will facilitate the use of functions addHydrometTimeseries, addHydrometImageSeries, addHydrometRasterSeries, insertHydrometRaster, insertHydrometVector, insertHydrometDocument, and insertHydrometImage. The application will be called with function hydrometApp.
# This script is the server-side of a Shiny application that creates an easy-to-use interface for database administrators to update the database with new data. The UI side script is located in the inst/ folder. It will facilitate the use of functions addHydrometTimeseries, addHydrometImageSeries, addHydrometRasterSeries, insertHydrometRaster, insertHydrometVector, insertHydrometDocument, and insertHydrometImage. The application will be called with function hydrometApp.


server <- function(input, output, session) {
  
  con <- HydroMetDB::hydrometConnect()
  
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Server logic dealing with document upload #####################################
  vectors <- reactiveValues()
  selectedVectors <- reactiveValues()
  document_types <- reactiveValues()
  # points, lines, polygons
  vectors$points <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type IN ('ST_Point', 'ST_MultiPoint');")
  vectors$lines <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type IN ('ST_LineString', 'ST_MultiLineString');")
  vectors$polygons <- DBI::dbGetQuery(con, "SELECT layer_name, feature_name, description, geom_id FROM vectors WHERE geom_type IN ('ST_Polygon', 'ST_MultiPolygon');")
  # document types
  document_types$types <- DBI::dbGetQuery(con, "SELECT document_type_id, document_type_en FROM document_types;")
  
  observeEvent(input$tabsetPanel1 == "addDocument", {
    types <- setNames(document_types$types$document_type_id, document_types$types$document_type_en)
    updateSelectizeInput(session, "documentType", choices = types)
  })
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
    
    if (input$documentAuthors == "") {
      authors <- NULL
    } else {
      authors <- unlist(strsplit(input$documentAuthors, "\n"))
    }
    
    geom_ids <- c(selectedVectors$points$geom_id, selectedVectors$lines$geom_id, selectedVectors$polygons$geom_id)

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
        
        insertHydrometDocument(path = input$documentFile$datapath,
                               name = input$documentName,
                               type = document_types$types[document_types$types$document_type_id == input$documentType, "document_type_en"],
                               description = input$documentDesc,
                               authors = authors,
                               publish_date = input$documentDate,
                               url = if (input$documentURL == "") NULL else input$documentURL,
                               geoms = if (length(geom_ids) == 0) NULL else geom_ids,
                               con = con
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
                             type = document_types$types[document_types$types$document_type_id == input$documentType, "document_type_en"],
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
  
  
  
  
  # Server logic dealing with image upload #####################################
  locations <- reactiveValues()
  selectedLocation <- reactiveValues()
  
  locations$locations <- DBI::dbGetQuery(con, "SELECT location, name, latitude, longitude, location_id FROM locations;")
  observeEvent(input$associateLocation, {
    showModal(modalDialog(
      title = "Associate image with a location",
      DT::DTOutput("locationsTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectLocation", "Select")
      )
    ))
    output$locationsTable <- DT::renderDT({
      DT::datatable(locations$locations[, c("location", "name", "latitude", "longitude")], 
                    selection = "single",
                    rownames = FALSE
      )
    })
  })
  observeEvent(input$selectLocation, {
    selectedLocation$location <- locations$locations[input$locationsTable_rows_selected , ]
    removeModal()
    
    output$associatedLocation <- renderUI({
      HTML(paste("Selected Location: <br>", 
                 paste(selectedLocation$location$location, selectedLocation$location$name, sep = " - ", collapse = "<br>"),
                 collapse = "<br>")
      )
    })
    selectedLocation$img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = ", selectedLocation$location$location_id, " AND img_type = 'manual';"))[1,1]
    if (is.na(selectedLocation$img_meta_id)) { # Create a new entry to images_index since none exists
      new_img_idx <- data.frame(location_id = selectedLocation$location$location_id,
                                public = TRUE,
                                description = "Image location index added via hydrometApp.",
                                img_type = "manual",
                                first_img = Sys.time(),
                                last_img = Sys.time()
      )
      DBI::dbAppendTable(con, "images_index", new_img_idx)
      selectedLocation$img_meta_id <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images_index WHERE location_id = ", selectedLocation$location$location_id, " AND img_type = 'manual';"))[1,1]
    }
  })
  observeEvent(input$addImageBtn, {
    if (is.null(input$imageFile$datapath) | is.null(input$imageDesc) | is.null(input$imageDatetime) | is.null(selectedLocation$location)) {
      output$imageUploadStatus <- showModal(modalDialog(
        title = "Image Upload Status",
        "Please fill in all mandatory fields.",
        easyClose = TRUE
      ))
      return()
    }
    if (nchar(input$imageDesc) < 5) {
      output$imageUploadStatus <- showModal(modalDialog(
        title = "Image Upload Status",
        "Please fill in the description with at least 5 characters.",
        easyClose = TRUE
      ))
      return()
    }
    # Check if the combination of image_meta_id and datetime exists in the images table. If it does, ask the user via  modal if it should be overwritten.
    img_exists <- DBI::dbGetQuery(con, paste0("SELECT img_meta_id FROM images WHERE img_meta_id = ", selectedLocation$img_meta_id, " AND datetime = '", lubridate::floor_date(input$imageDatetime, "minute"), "';"))
    if (nrow(img_exists) > 0) {
      output$imageUploadStatus <- showModal(modalDialog(
        title = "Image Upload Status",
        "An image with the same datetime and location already exists. Do you want to overwrite it?",
        footer = tagList(
          actionButton("yesButton", "Yes"),
          actionButton("noButton", "No")
        )
      ))
      observeEvent(input$yesButton, {
        removeModal()
        insertHydrometImage(object = input$imageFile$datapath,
                            img_meta_id = selectedLocation$img_meta_id,
                            datetime = input$imageDatetime,
                            description = input$imageDesc,
                            image_type = "manual",
                            con = con
        )
        output$imageUploadStatus <- showModal(modalDialog(
          title = "Image Upload Status",
          "Image has been uploaded to the database.",
          easyClose = TRUE
        ))
      })
      observeEvent(input$noButton, {
        removeModal()
        return()
      })
    } else {
      insertHydrometImage(object = input$imageFile$datapath,
                          img_meta_id = selectedLocation$img_meta_id,
                          datetime = input$imageDatetime,
                          description = input$imageDesc,
                          image_type = "manual",
                          con = con
      )
      output$imageUploadStatus <- showModal(modalDialog(
        title = "Image Upload Status",
        "Image has been uploaded to the database.",
        easyClose = TRUE
      ))
    }
  })
  
  # Server logic dealing with raster upload #####################################
  raster <- reactiveValues(path = NULL)
  shinyFiles::shinyFileChoose(input, "rasterFile", roots = shinyFiles::getVolumes(), hidden = TRUE, filetypes = c("tif", "tiff"))
  observeEvent(input$rasterFile, {
    raster$path <- shinyFiles::parseFilePaths(roots = shinyFiles::getVolumes(), input$rasterFile)$datapath
    output$rasterPath <- renderUI({
      HTML(paste("Selected Raster: <br>", 
                 raster$path,
                 collapse = "<br>")
      )
    })
  })
  observeEvent(input$addRasterBtn, {
    if (is.null(raster$path) | is.null(input$rasterDesc)) {
      output$rasterUploadStatus <- showModal(modalDialog(
        title = "Raster Upload Status",
        "Please fill in all mandatory fields.",
        easyClose = TRUE
      ))
      return()
    }
    if (nchar(input$rasterDesc) < 5) {
      output$rasterUploadStatus <- showModal(modalDialog(
        title = "Raster Upload Status",
        "Please fill in the description with at least 5 characters.",
        easyClose = TRUE
      ))
      return()
    }
    print(raster$path)
    print(input$rasterDesc)
    print(input$rasterUnits)
    print(input$rasterSource)
    print("done")
    output$rasterUploadStatus <- showModal(modalDialog(
      title = "Raster Upload Status",
      "Please be patient, this could take a while!",
      easyClose = TRUE
    ))
    insertHydrometRaster(con = con,
                         raster = raster$path,
                         description = input$rasterDesc,
                         units = if (input$rasterUnits == "") NULL else input$rasterUnits,
                         source = if (input$rasterSource == "") NULL else input$rasterSource
    )
    output$rasterUploadStatus <- showModal(modalDialog(
      title = "Raster Upload Status",
      "Raster has been uploaded to the database.",
      easyClose = TRUE
    ))
  })
  
  
  # Server logic dealing with vector upload #####################################
  vector <- reactiveValues(path = NULL, type = NULL)
  vector$selected_feature_desc_col <- ""
  vector$selected_feature_name_col <- ""
  
  shinyjs::hide("featureName")
  shinyjs::hide("featureNameCol")
  shinyjs::hide("featureDesc")
  shinyjs::hide("featureDescCol")
  
  shinyFiles::shinyFileChoose(input, "vectorFile", roots = shinyFiles::getVolumes(), hidden = TRUE, filetypes = c("shp", "gpkg", "geojson", "kml", "kmz", "gml"))
  
  observeEvent(input$vectorFile, {
    vector$path <- shinyFiles::parseFilePaths(roots = shinyFiles::getVolumes(), input$vectorFile)$datapath
    output$vectorPath <- renderUI({
      HTML(paste("Selected Vector: <br>", 
                 vector$path,
                 collapse = "<br>")
      )
    })
    try({
      vector$vect <- terra::vect(vector$path)
      vector$table <- as.data.frame(vector$vect)[1:5, ]
      vector$rows <- nrow(vector$vect)
      
      if (vector$rows > 1) {
        shinyjs::show("featureNameCol")
        shinyjs::show("featureDescCol")
        shinyjs::hide("featureName")
        shinyjs::hide("featureDesc")
      } else {
        shinyjs::show("featureName")
        shinyjs::show("featureDesc")
        shinyjs::hide("featureNameCol")
        shinyjs::hide("featureDescCol")
      }
    })
  })
  observeEvent(input$featureNameCol, {
    showModal(modalDialog(
      title = "Select column with feature names",
      DT::DTOutput("featureColTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectFeatureNameCol", "Select")
      )
    ))
    output$featureColTable <- DT::renderDT({
      DT::datatable(vector$table,
                    selection = list(target = "column", mode = "single"),
                    rownames = FALSE,
                    options = list(scrollX = TRUE)
      )
    })
  })
  observeEvent(input$selectFeatureNameCol, {
    vector$selected_feature_name_col <- colnames(vector$table)[input$featureColTable_columns_selected]
    removeModal()
    output$featureNames <- renderUI({
      HTML(paste("Selected feature name column: <br>", 
                 vector$selected_feature_name_col,
                 collapse = "<br>")
      )
    })
  })
  observeEvent(input$featureDescCol, {
    showModal(modalDialog(
      title = "Select column with feature descriptions",
      DT::DTOutput("featureColTable"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("selectFeatureDescCol", "Select")
      )
    ))
    output$featureColTable <- DT::renderDT({
      DT::datatable(vector$table,
                    selection = list(target = "column", mode = "single"),
                    rownames = FALSE,
                    options = list(scrollX = TRUE)
      )
    })
  })
  observeEvent(input$selectFeatureDescCol, {
    vector$selected_feature_desc_col <- colnames(vector$table)[input$featureColTable_columns_selected]
    removeModal()
    output$featureDescs <- renderUI({
      HTML(paste("Selected feature description column: <br>", 
                 vector$selected_feature_desc_col,
                 collapse = "<br>")
      )
    })
  })
  observeEvent(input$addVectorBtn, {
    if (vector$rows > 1) {
      if (!(vector$selected_feature_desc_col %in% colnames(vector$table))) {
        output$vectorUploadStatus <- showModal(modalDialog(
          title = "Vector Upload Status",
          "Please select a column with feature descriptions.",
          easyClose = TRUE
        ))
        return()
      }
      if (!(vector$selected_feature_name_col %in% colnames(vector$table))) {
        output$vectorUploadStatus <- showModal(modalDialog(
          title = "Vector Upload Status",
          "Please select a column with feature names.",
          easyClose = TRUE
        ))
        return()
      }
      output$vectorUploadStatus <- showModal(modalDialog(
        title = "Vector Upload Status",
        "Please be patient, this could take a while!",
        easyClose = TRUE
      ))
      insertHydrometVector(geom = vector$vect,
                           layer_name = input$layerName,
                           feature_name_col = vector$selected_feature_name_col,
                           description_col = vector$selected_feature_desc_col,
                           con = con)
      output$vectorUploadStatus <- showModal(modalDialog(
        title = "Vector Upload Status",
        "Vector has been uploaded to the database.",
        easyClose = TRUE
      ))
    } else {
      if (input$featureName == "" | input$featureDesc == "") {
        output$vectorUploadStatus <- showModal(modalDialog(
          title = "Vector Upload Status",
          "Please fill in all mandatory fields.",
          easyClose = TRUE
        ))
        return()
      }
      output$vectorUploadStatus <- showModal(modalDialog(
        title = "Vector Upload Status",
        "Please be patient, this could take a while!",
        easyClose = TRUE
      ))
      insertHydrometVector(geom = vector$vect,
                           layer_name = input$layerName,
                           feature_name = input$featureName,
                           description = input$featureDesc,
                           con = con)
      output$vectorUploadStatus <- showModal(modalDialog(
        title = "Vector Upload Status",
        "Vector has been uploaded to the database.",
        easyClose = TRUE
      ))
    }
  })
  
  
  # Server logic dealing with data time series upload #################################
  
  # Server logic dealing with raster timeseries upload #################################
  
  # Server logic dealing with image timeseries upload #################################
}
