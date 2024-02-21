
timeseries_df <- openxlsx::read.xlsx("C:/Users/gtdelapl/Desktop/YOWN to AQ.xlsx", sheet = 1)
settings_df <- openxlsx::read.xlsx("C:/Users/gtdelapl/Desktop/YOWN to AQ.xlsx", sheet = 2)
locations_df <- openxlsx::read.xlsx("C:/Users/gtdelapl/Desktop/YOWN to AQ.xlsx", sheet = 3)

timeseries_df$start_datetime <- as.POSIXct("2000-01-01 00:00", tz = "UTC")
