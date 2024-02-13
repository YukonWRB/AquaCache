# This script is the server-side of a Shiny application that creates an easy-to-use interface for database administrators to update the database with new data. The UI side script is located in the inst/ folder. It will facilitate the use of functions addHydrometTimeseries, addHydrometImageSeries, addHydrometRasterSeries, insertHydrometRaster, insertHydrometVector, insertHydrometDocument, and insertHydrometImage. The application will be called with function hydrometApp.



server <- function(input, output, session) {
  
  con <- hydrometConnect()
  
  # Create containers
  locations <- reactiveValues()
  selectedLocation <- reactiveValues()
  
  # Server logic dealing with file upload ######
  # Load and call module
  source("module_documentUpload.R")
  docUploadModule(input, output, session, con)
  
  
  
  
  # Server logic dealing with image upload ######
  locations$locations <- DBI::dbGetQuery(con, "SELECT location, name, owner, operator, geom_id FROM locations;")
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
      DT::datatable(locations$locations[, c("location", "name", "owner", "operator")], 
                    selection = "single",
                    rownames = FALSE
      )
    })
  })
  observeEvent(input$selectLocation, {
    selectedLocation$location <- locations$locations[input$locationsTable_rows_selected,]
    removeModal()
    
    output$associatedLocation <- renderUI({
      HTML(paste("Selected Location: <br>", 
                 paste(selectedLocation$location$location, selectedLocation$location$name, sep = " - ", collapse = "<br>"),
                 collapse = "<br>")
      )
    })
  })
  
  # Server logic dealing with raster upload ######

  
  
  # Server logic dealing with vector upload #########

}
