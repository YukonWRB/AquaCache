ui <- fluidPage(
  titlePanel("Hydromet Data Entry App"),
  tabsetPanel(
    tabPanel("Add Document", value = "addDocument",
             textInput("textInput", "Example text input"),
             textOutput("textOutput")
    ),
    tabPanel("Add Image", value = "addImage"),
    tabPanel("Add Raster", value = "addRaster"),
    tabPanel("Add Vector", value = "addVector"),
    tabPanel("Add Timeseries", value = "addTimeseries",
             conditionalPanel(condition = "input.tabsetPanel1 == 'addTimeseries'",
                              tabsetPanel(
                                tabPanel("Add Data Timeseries", value = "addDataTimeseries"),
                                tabPanel("Add Raster Timeseries", value = "addRasterTimeseries"),
                                tabPanel("Add Image Timeseries", value = "addImageTimeseries")
                              )
             )
    )

  )
)
