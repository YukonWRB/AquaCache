# This script is the UI-side of a Shiny application that creates an easy-to-use interface for database administrators to update the database with new data. The server side script is located in the inst/ folder. It will facilitate the use of functions addHydrometTimeseries, addHydrometImageSeries, addHydrometRasterSeries, insertHydrometRaster, insertHydrometVector, insertHydrometDocument, and insertHydrometImage. The application will be called with function hydrometApp.


ui <- fluidPage(
  titlePanel("Hydromet Data Entry App"),
  tabsetPanel(id = "tabsetPanel1",
              tabPanel("Add Document", value = "addDocument",
                       fileInput("documentFile", "Choose a document to upload"),
                       textInput("documentName", "Enter a descriptive name for the document"),
                       selectizeInput("documentType", "Select the type of document", choices = c("thesis", "report", "well log", "conference paper", "poster", "journal article", "map", "graph", "protocol", "grading scheme", "metadata", "other"), multiple = TRUE, options = list(maxItems = 1)),
                       textInput("documentDesc", "Enter a detailled description of the document"),
                       textAreaInput("documentAuthors", "Enter the authors of the document (one per line)", cols = 1, placeholder = "Firstname Lastname"),
                       dateInput("documentDate", "Enter the publish date of the document"),
                       textInput("documentURL", "Enter the URL or DOI of the document", placeholder = "Optional but recommended!"),
                       actionButton("associatePoints", "Associate with locations/points", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedPoints"),
                       actionButton("associateLines", "Associate with lines", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedLines"),
                       actionButton("associatePolygons", "Associate with polygons", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedPolygons"),
                       actionButton("addDocumentBtn", "Add Document to Database", style = c("margin-top: 10px;", "margin-bottom: 15px;"), class = "btn btn-primary")
              ),
              
              tabPanel("Add Image", value = "addImage",
                       fileInput("imageFile", "Choose an image to upload"),
                       actionButton("associateLocation", "Associate with a location", style = c("margin-bottom: 5px;")),
                       uiOutput("associatedLocation", style = c("margin-bottom: 10px;")),
                       shinyWidgets::airDatepickerInput("imageDatetime", "Enter the date and time of the image", timepicker = TRUE, placeholder = "YYYY-MM-DD HH:MM:SS"),
                       textInput("imageDesc", "Enter a detailled description of the image"),
                       
              ),
              tabPanel("Add Raster", value = "addRaster"
                       
              ),
              tabPanel("Add Vector", value = "addVector"
                       
              ),
              tabPanel("Add Timeseries", value = "addTimeseries",
                       conditionalPanel(condition = "input.tabsetPanel1 == 'addTimeseries'",
                                        tabsetPanel(id = "addTimeseriesTabs",
                                                    tabPanel("Add Data Timeseries", value = "addDataTimeseries"),
                                                    tabPanel("Add Raster Timeseries", value = "addRasterTimeseries"),
                                                    tabPanel("Add Image Timeseries", value = "addImageTimeseries")
                                        )
                       )
              )
              
  )
)
