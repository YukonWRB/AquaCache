# This script is the UI-side of a Shiny application that creates an easy-to-use interface for database administrators to update the database with new data. The server side script is located in the inst/ folder. It will facilitate the use of functions addACTimeseries, addACImageSeries, addACRasterSeries, insertACRaster, insertACVector, insertACDocument, and insertACImage. The application will be called with function AquaCacheApp


ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  titlePanel("aquacache Data Entry App"),
  tabsetPanel(id = "tabsetPanel1",
              # Add Document tab ############################################################################################################
              tabPanel("Add Document", value = "addDocument",
                       uiOutput("documentFile"),
                       textInput("documentName", "Enter a descriptive name for the document (must be unique in the database)"),
                       selectizeInput("documentType", "Select the type of document", choices = "placeholder"),
                       textInput("documentDesc", "Enter a detailled description of the document (think about key words future users will search)"),
                       textAreaInput("documentAuthors", "Enter the authors of the document (one per line)", cols = 1, placeholder = "Firstname Lastname"),
                       dateInput("documentDate", "Enter the publish date of the document"),
                       textInput("documentURL", "Enter the URL or DOI of the document", placeholder = "Optional but recommended!"),
                       checkboxInput("documentPublic", "Make the document public?", value = FALSE),
                       actionButton("associatePoints", "Associate with locations/points", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedPoints"),
                       actionButton("associateLines", "Associate with lines", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedLines"),
                       actionButton("associatePolygons", "Associate with polygons", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedPolygons"),
                       actionButton("addDocumentBtn", "Add Document to Database", style = c("margin-top: 10px;", "margin-bottom: 15px;"), class = "btn btn-primary")
              ),
              
              # Add Image tab ############################################################################################################
              tabPanel("Add Image", value = "addImage",
                       fileInput("imageFile", "Choose an image to upload", accept = "image/*"),
                       actionButton("associateLocation", "Associate with a location", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedLocation", style = c("margin-bottom: 10px;")),
                       shinyWidgets::airDatepickerInput("imageDatetime", "Enter the date and time the image was taken", timepicker = TRUE, placeholder = "YYYY-MM-DD HH:MM:SS", timepickerOpts = shinyWidgets::timepickerOptions(minutesStep = 15, timeFormat = "HH:mm"), maxDate = Sys.Date() + 1),
                       textInput("imageDesc", "Enter a detailled description of the image (think about key words future users will search)"),
                       actionButton("addImageBtn", "Add Image to Database", style = c("margin-top: 10px;", "margin-bottom: 15px;"), class = "btn btn-primary")
              ),
              
              # Add Raster tab ############################################################################################################
              tabPanel("Add Raster", value = "addRaster",
                       shinyFiles::shinyFilesButton("rasterFile", "Choose a raster file to upload", "Select", multiple = FALSE, style = c("margin-top: 15px;", "margin-bottom: 10px;")),
                       uiOutput("rasterPath"),
                       textInput("rasterDesc", "Enter a detailled description of the raster"),
                       textInput("rasterUnits", "Enter the units of the raster", placeholder = "Optional - can get from file if exists"),
                       textInput("rasterSource", "Enter the source of the raster", placeholder = "Optional but recommended"),
                       actionButton("addRasterBtn", "Add Raster to Database", style = c("margin-top: 10px;", "margin-bottom: 15px;"), class = "btn btn-primary")
              ),
              
              # Add Vector tab ############################################################################################################
              tabPanel("Add Vector", value = "addVector",
                       shinyFiles::shinyFilesButton("vectorFile", "Choose a vector file to upload", "Select", multiple = FALSE, style = c("margin-top: 15px;", "margin-bottom: 10px;")),
                       uiOutput("vectorPath"),
                       selectInput("layerName", "Enter a name for the vector layer", choices = "placeholder"),
                       textInput("newLayerName", "Enter a name for the new vector layer"),
                       textInput("featureName", "Enter a name for the feature"), # This will be hidden if the number of features for the layer is more than 1
                       textInput("featureDesc", "Enter a description for the feature"),
                       actionButton("featureNameCol", "Select the column for feature names", style = c("margin-bottom: 5px;")),
                       uiOutput("featureNames"),
                       actionButton("featureDescCol", "Select the column for feature descriptions", style = c("margin-bottom: 5px;")),
                       uiOutput("featureDescs"),
                       
                       actionButton("addVectorBtn", "Add Vector to Database", style = c("margin-top: 10px;", "margin-bottom: 15px;"), class = "btn btn-primary")
              ),
              
              
              
              
              # Add Timeseries tab ############################################################################################################
              tabPanel("Add Timeseries", value = "addTimeseries",
                       conditionalPanel(condition = "input.tabsetPanel1 == 'addTimeseries'",
                                        tabsetPanel(id = "addTimeseriesTabs",
                                                    
                                                    # # Add Data Timeseries sub-tab ############################################################################################################
                                                    # tabPanel("Add Data Timeseries", value = "addDataTimeseries"),
                                                    # shinyWidgets::materialSwitch("newDataTSLocation", "Add a new location"), # If the user toggles this the elements below will be shown
                                                    # uiOutput("existDataTSLocation"), # This will be populated with the location options from the DB if the user doesn't toggle the switch
                                                    # textInput("newLocCode", "Enter the code for the new location (used to pull in new data)"),
                                                    # textInput("NewLocName", "Enter the name for the new location"),
                                                    # numericInput("newLocLat", "Enter the latitude for the new location", value = 0, min = -90, max = 90, step = 0.0001),
                                                    # numericInput("newLocLon", "Enter the longitude for the new location", value = 0, min = -180, max = 180, step = 0.0001),
                                                    # numericInput("newLocElev", "Enter the elevation for the new location", value = 0, min = 0, max = 3000, step = 0.001),
                                                    # uiOutput("newLocDatum"), # This will be populated with the datum options from the DB
                                                    # 
                                                    # textInput("newLocNote", "Enter a note for the new location"),
                                                    # selectizeInput("newLocElev", "Enter the elevation for the new location", choices = c("elevation1", "elevation2", "elevation3"), multiple = FALSE, options = list(maxItems = 1)),
                                                    # 
                                                    # 
                                                    # # Add Raster Timeseries sub-tab ############################################################################################################
                                                    # tabPanel("Add Raster Timeseries", value = "addRasterTimeseries"),
                                                    # 
                                                    # # Add Image Timeseries sub-tab ############################################################################################################
                                                    # tabPanel("Add Image Timeseries", value = "addImageTimeseries")
                                        )
                       )
              )
              
  )
)
