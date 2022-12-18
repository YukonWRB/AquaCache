utils_level_data <- function(
    station_number,
    select_years,
    high_res = TRUE,
    filter = FALSE,
    recent_prctile = FALSE,
    rate = FALSE,
    rate_days = "all",
    force_CGVD28 = FALSE
){
  
  select_years <- as.numeric(select_years) #In case it somehow got fed through as a character vector
  
  if (max(select_years) <= lubridate::year(Sys.Date() - 577)) {high_res <- FALSE} #reset high_res if no high-res data is available but the user asked for it
  
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  level_historic <- (tidyhydat::hy_daily_levels(station_number = station_number)[,-c(3,5)])
  colnames(level_historic) <- c("STATION_NUMBER", "Date", "Level")
  record_yrs <- unique(substr(level_historic$Date, 1,4))
  record_yrs <- c(as.character(lubridate::year(Sys.Date())-2), record_yrs, as.character(lubridate::year(Sys.Date())-1), as.character(lubridate::year(Sys.Date())))
  
  if (!(TRUE %in% (select_years %in% record_yrs))){
    stop(paste0("There is no data for the years you have requested. Years of record for historical data at this station are ", paste0(record_yrs, collapse = ", "), ". In addition, later high-resolution data may be available if the station is currently active."))
  }
  #Truncate the Yukon at Whitehorse at 2014, since data before that is garbage (much predates the dam for level)
  if (station_number == "09AB001") {
    level_historic <- level_historic[level_historic$Date > "2014-01-01",]
    if (nrow(level_historic) == 0){
      warning(paste0("Reliable data for the Yukon River at Whitehorse begins in 2014. Please use caution if using data prior to this date."))
    }
  }
  
  #Truncate all to the last requested year. Only these years are used for calculating stats.
  level_historic <- level_historic[level_historic$Date < paste0(max(select_years), "-12-31"),]
  
  datum_na <- is.na(as.numeric(utils::tail(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1)))#Check if there is a datum on record - any datum
  if (datum_na == FALSE){
    if (force_CGVD28 == FALSE){
      datum <- as.numeric(utils::tail(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1))
    } else if (force_CGVD28 == TRUE) {
      datum <- as.numeric(utils::head(tidyhydat::hy_stn_datum_conv(station_number)[,4], n=1))
    }
  }
  
  
  level_historic$Level_masl <- level_historic$Level #create col here so we end up with two cols filled out
  if(datum_na == FALSE) {
    level_historic$Level_masl[level_historic$Level_masl < 50 & !is.na(level_historic$Level_masl)] <- level_historic$Level_masl[level_historic$Level_masl <50 & !is.na(level_historic$Level_masl)] + datum #This deals with instances where at least part of the historic data has the station datum already added to it, so long as the base level is <50. The if statement ensures that stations with no datum don't have anything applied to them so as to keep the data. Applies the last (most recent) datum.
  } else {
    level_historic$Level_masl <- as.numeric(NA)
  }
  
  recent_level <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant in class
  if (max(select_years) >= lubridate::year(Sys.Date() - 577)) {
  
    level_real_time <- aqLevel[[station_number]]$timeseries[,1:2]
    names(level_real_time) <- c("Date", "Value")
    level_real_time$STATION_NUMBER <- station_number

    
    #Filter the data here if requested (option exists in case user wants to see the outliers). Note that this step happens before creating recent_level!
    if (filter == TRUE) {
      IQR <- stats::IQR(level_real_time$Value, na.rm=TRUE)
      quantiles <- stats::quantile(level_real_time$Value, na.rm=TRUE, probs = c(.05, .95))
      
      level_real_time <- subset(level_real_time, level_real_time$Value > (quantiles[1] - 2*IQR) & level_real_time$Value < (quantiles[2] + 2*IQR))
    }
    
    if (high_res == TRUE){ #If requesting high-res data
      recent_level <- level_real_time %>% plyr::rename(c("Value"="Level"))
      recent_level$DateOnly <- lubridate::date(recent_level$Date)
      recent_level <- recent_level[,-c(3,5:10)]
    }
    
    level_real_time <- level_real_time %>%
      dplyr::group_by(.data$STATION_NUMBER, lubridate::year(.data$Date), lubridate::yday(.data$Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(.data$Date)), #retain single data (mean) point per day
                       Level = mean(.data$Value),
                       .groups = "drop")
    level_real_time <- level_real_time[,-c(2,3)]
    
    if (datum_na == FALSE){ #Generate new column to hold masl levels in level_real_time and recent_level. At this point level_real_time has a single point per day, recent_level has the data at max resolution.
      level_real_time$Level_masl <- level_real_time$Level + datum #adjusting to MASL if there is a datum
      if (high_res == TRUE) {recent_level$Level_masl <- recent_level$Level + datum}
    } else {
      level_real_time$Level_masl <- as.numeric(NA)
      if(high_res == TRUE) {recent_level$Level_masl <- as.numeric(NA)}
    }
    
    level_df <- dplyr::bind_rows(level_historic, level_real_time)
    
  } else {
    level_df <- level_historic
  } 
  
  # Add rows of missing dates
  level_df <- fasstr::fill_missing_dates(data = level_df, dates = "Date")
  
  # Remove Feb. 29 data as it would mess with the percentiles
  level_df <- level_df[!(format(level_df$Date,"%m") == "02" & format(level_df$Date, "%d") == "29"), , drop = FALSE]
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  if (datum_na==TRUE) {
    level_df <- level_df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
                                       ifelse(lubridate::month(.data$Date) <= 2,
                                              lubridate::yday(.data$Date),
                                              lubridate::yday(.data$Date) - 1),
                                       lubridate::yday(.data$Date)))
    
    current.year <- dplyr::filter(level_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside
    
    level_df <- dplyr::filter(level_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
      dplyr::filter(!is.na(.data$Level)) %>%  #remove na values in Level so that stats::ecdf can work below - they're added in after
      dplyr::group_by(.data$dayofyear) %>%
      dplyr::mutate(prctile = (stats::ecdf(.data$Level)(.data$Level)) * 100) %>%
      fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now - including Feb 29
      dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
                                       ifelse(lubridate::month(.data$Date) <= 2,
                                              lubridate::yday(.data$Date),
                                              lubridate::yday(.data$Date) - 1),
                                       lubridate::yday(.data$Date))) %>%
      dplyr::group_by(.data$dayofyear) %>%
      dplyr::mutate(Max = max(.data$Level, na.rm = TRUE),
                    Min = min(.data$Level, na.rm = TRUE),
                    QP90 = stats::quantile(.data$Level, 0.90, na.rm = TRUE),
                    QP75 = stats::quantile(.data$Level, 0.75, na.rm = TRUE),
                    QP50 = stats::quantile(.data$Level, 0.50, na.rm = TRUE),
                    QP25 = stats::quantile(.data$Level, 0.25, na.rm = TRUE),
                    QP10 = stats::quantile(.data$Level, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      hablar::rationalize() #rationalize replaces Inf values with NA
    
    current.year$Max <- as.numeric(NA)
    current.year$Min <- as.numeric(NA)
    current.year$QP90 <- as.numeric(NA)
    current.year$QP75 <- as.numeric(NA)
    current.year$QP50 <- as.numeric(NA)
    current.year$QP25 <- as.numeric(NA)
    current.year$QP10 <- as.numeric(NA)
    current.year$prctile<- as.numeric(NA)
    
    for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats
      current.year$Max[current.year$dayofyear==i] <- unique(level_df$Max[level_df$dayofyear==i])
      current.year$Min[current.year$dayofyear==i] <- unique(level_df$Min[level_df$dayofyear==i])
      current.year$QP90[current.year$dayofyear==i] <- unique(level_df$QP90[level_df$dayofyear==i])
      current.year$QP75[current.year$dayofyear==i] <- unique(level_df$QP75[level_df$dayofyear==i])
      current.year$QP50[current.year$dayofyear==i] <- unique(level_df$QP50[level_df$dayofyear==i])
      current.year$QP25[current.year$dayofyear==i] <- unique(level_df$QP25[level_df$dayofyear==i])
      current.year$QP10[current.year$dayofyear==i] <- unique(level_df$QP10[level_df$dayofyear==i])
      current.year$prctile[current.year$dayofyear==i] <- ((current.year$Level[current.year$dayofyear==i] - unique(level_df$Min[level_df$dayofyear==i])) / (unique(level_df$Max[level_df$dayofyear==i]) - unique(level_df$Min[level_df$dayofyear==i]))) * 100
    }
    level_df <- dplyr::bind_rows(level_df, current.year)#add in the current year
    
  } else { #datum_na == FALSE
    level_df <- level_df %>%
      dplyr::mutate(dayofyear = ifelse(lubridate::year(Date) %in% leap_list, 
                                       ifelse(lubridate::month(Date) <= 2,
                                              lubridate::yday(Date),
                                              lubridate::yday(Date) - 1),
                                       lubridate::yday(Date)))
    
    current.year <- dplyr::filter(level_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside
    
    level_df <- dplyr::filter(level_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
      dplyr::filter(!is.na(.data$Level_masl)) %>%  #remove na values in Level_masl so that stats::ecdf can work below - they're added in after
      dplyr::mutate(prctile = (stats::ecdf(.data$Level_masl)(.data$Level_masl)) * 100) %>%
      fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now
      dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
                                       ifelse(lubridate::month(.data$Date) <= 2,
                                              lubridate::yday(.data$Date),
                                              lubridate::yday(.data$Date) - 1),
                                       lubridate::yday(.data$Date))) %>%
      dplyr::group_by(.data$dayofyear) %>%
      dplyr::mutate(Max = max(.data$Level_masl, na.rm = TRUE),
                    Min = min(.data$Level_masl, na.rm = TRUE),
                    QP90 = stats::quantile(.data$Level_masl, 0.90, na.rm = TRUE),
                    QP75 = stats::quantile(.data$Level_masl, 0.75, na.rm = TRUE),
                    QP50 = stats::quantile(.data$Level_masl, 0.50, na.rm = TRUE),
                    QP25 = stats::quantile(.data$Level_masl, 0.25, na.rm = TRUE),
                    QP10 = stats::quantile(.data$Level_masl, 0.10, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      hablar::rationalize() #rationalize replaces Inf values with NA
    
    current.year$Max <- as.numeric(NA)
    current.year$Min <- as.numeric(NA)
    current.year$QP90 <- as.numeric(NA)
    current.year$QP75 <- as.numeric(NA)
    current.year$QP50 <- as.numeric(NA)
    current.year$QP25 <- as.numeric(NA)
    current.year$QP10 <- as.numeric(NA)
    current.year$prctile <- as.numeric(NA)
    
    for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats and calculate the percent of historic max for each day
      current.year$Max[current.year$dayofyear==i] <- unique(level_df$Max[level_df$dayofyear==i])
      current.year$Min[current.year$dayofyear==i] <- unique(level_df$Min[level_df$dayofyear==i])
      current.year$QP90[current.year$dayofyear==i] <- unique(level_df$QP90[level_df$dayofyear==i])
      current.year$QP75[current.year$dayofyear==i] <- unique(level_df$QP75[level_df$dayofyear==i])
      current.year$QP50[current.year$dayofyear==i] <- unique(level_df$QP50[level_df$dayofyear==i])
      current.year$QP25[current.year$dayofyear==i] <- unique(level_df$QP25[level_df$dayofyear==i])
      current.year$QP10[current.year$dayofyear==i] <- unique(level_df$QP10[level_df$dayofyear==i])
      current.year$prctile[current.year$dayofyear==i] <- ((current.year$Level_masl[current.year$dayofyear==i] - unique(level_df$Min[level_df$dayofyear==i])) / (unique(level_df$Max[level_df$dayofyear==i]) - unique(level_df$Min[level_df$dayofyear==i]))) * 100
    }
    level_df <- dplyr::bind_rows(level_df, current.year)#add in the current year
  }
  
  last_year <- lubridate::year(max(level_df$Date))
  
  # For loop to populate level_years with data from each year in select_years. last_year is used here so that everything gets the same year (so Feb 1 1989 shows up as Feb 1 2022 if the year is 2022) which allows for easy plotting of multiple years together. Column Year_Real holds the true year.
  level_years <- data.frame()
  for(i in select_years) {
    single_year <- level_df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = last_year,
                    Month = lubridate::month(.data$Date),
                    Day = lubridate::day(.data$Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(.data$Year, .data$Month, .data$Day, sep = "-"))) %>%
      dplyr::select(-.data$Date, -.data$Month, -.data$Day, -.data$Year) %>%
      dplyr::rename(Date = .data$Date_2)
    
    single_year <- single_year[,c(1, 14, 4:12, 2, 3, 13)]
    level_years <- dplyr::bind_rows(level_years, single_year)
  }
  
  
  #If loop below modifies recent_level if high_res is TRUE
  if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){ # Create a few columns here depending on other options
    recent_level <- recent_level %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list,
                                                                      ifelse(lubridate::month(.data$Date) <=2,
                                                                             lubridate::yday(.data$Date),
                                                                             lubridate::yday(.data$Date) - 1),
                                                                      lubridate::yday(.data$Date))) %>%
      dplyr::mutate(prct_max_hist = as.numeric(NA))
    
    if (recent_prctile == TRUE){ #Calculate a percent historic for the 5 minute data 
      for (i in 1:nrow(recent_level)){
        if (datum_na==TRUE){
          recent_level$prct_max_hist[i] <- ((recent_level$Level[i] - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]])) / (unique(level_df$Max[level_df$dayofyear == recent_level$dayofyear[i]]) - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]]))) * 100
        } else {
          recent_level$prct_max_hist[i] <- ((recent_level$Level_masl[i] - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]])) / (unique(level_df$Max[level_df$dayofyear == recent_level$dayofyear[i]]) - unique(level_df$Min[level_df$dayofyear == recent_level$dayofyear[i]]))) * 100
        }
      }
    }
    
    #Fill missing data points in recent_level: first figure out the recording rate, then fill with NAs
    diff <- vector()
    for (i in 1:nrow(recent_level)){
      diff[i] <- as.numeric(difftime(recent_level$Date[i+1], recent_level$Date[i]))
    }
    diff <- as.numeric(names(sort(table(diff),decreasing=TRUE)[1])) #Take the tightest difference between data points in minutes
    recent_level <- tidyr::complete(recent_level, Date = seq.POSIXt(min(.data$Date), max(.data$Date), by=paste0(diff, " min"))) %>%
      dplyr::arrange(dplyr::desc(.data$Date))
  }
  
  level_years <- level_years[with(level_years, order(Year_Real, dayofyear, decreasing = TRUE)),]
  
  if (rate == TRUE) {
    level_years$rate <- as.numeric(NA)
    for (i in 1:(nrow(level_years)-1)){
      try(level_years$rate[i] <- level_years$Level[i] - level_years$Level[i+1], silent=TRUE)
    }
    
    if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){
      recent_level <- dplyr::mutate(recent_level, rate = as.numeric(NA))
      if (rate_days == "all"){
        for (i in 1:(nrow(recent_level)-1)){
          try(recent_level$rate[i] <- recent_level$Level[i] - recent_level$Level[i+1], silent=TRUE)
        }
      } else {
        last_row <- which(recent_level$Date== (recent_level$Date[1] - rate_days*60*60*24))
        for (i in 1:last_row){
          try(recent_level$rate[i] <- recent_level$Level[i] - recent_level$Level[i+1], silent=TRUE)
        }
      }
    }
  }
  
  #Warning messages
  if (nrow(recent_level) <= 1 & nrow(level_years) >= 1){
    warning("There is no high-resolution data for the data range you selected. Note that high-resolution data is only kept by the WSC for 18 months. All of the available data for the date range you requested is in the requested_years data.frame.")
  }
  
  if (nrow(recent_level) <= 1 & nrow(level_years) <= 1){
    range <- dplyr::filter(tidyhydat::hy_stn_data_range(station_number), .data$DATA_TYPE == "H")
    range <- seq(as.numeric(range$Year_from), as.numeric(range$Year_to))
    warning(paste0("No data exists for the years you requested. Only historical data was returned. Note that the historical data range is from ", range[1], " to ", range[length(range)], " and that high-resolution data is only kept for 18 months."))
  }
  if (length(select_years) > 1 & nrow(level_years > 1)){
    for (i in select_years){
      if (!(i %in% unique(level_years$Year_Real))){
        warning(paste0("No data was available (historical daily means or recent, high-resolution data) for year ", i, ". All other years of data have been returned."))
      } 
    }
  }
  
  levelData <- list(all_historical = level_df, requested_years = level_years, recent_level = recent_level)
  return(levelData)
}




utils_flow_data <- function(
    station_number,
    select_years,
    high_res = TRUE,
    filter = TRUE,
    recent_prctile = FALSE,
    rate = FALSE,
    rate_days = "all"
){
  
  select_years <- as.numeric(select_years) #In case it somehow got fed through as a character vector
  
  if (max(select_years) <= lubridate::year(Sys.Date()-577)) {high_res <- FALSE} #reset high_res if no high_res data is available
  
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  
  flow_historic <- (tidyhydat::hy_daily_flows(station_number = station_number)[,-c(3,5)])
  colnames(flow_historic) <- c("STATION_NUMBER", "Date", "Flow")
  record_yrs <- unique(substr(flow_historic$Date, 1,4))
  record_yrs <- c(as.character(lubridate::year(Sys.Date())-2), record_yrs, as.character(lubridate::year(Sys.Date())-1), as.character(lubridate::year(Sys.Date())))
  
  if (!(TRUE %in% (select_years %in% record_yrs))){
    stop(paste0("There is no data for the years you have requested. Years of record for historical data at this station are ", paste0(record_yrs, collapse = ", "), ". In addition, later high-resolution data may be available if the station is currently active."))
  }
  
  #Truncate all to the last requested year. Only these years are used for calculating stats.
  flow_historic <- flow_historic[flow_historic$Date < paste0(max(select_years), "-12-31"),]
  
  recent_flow <- data.frame() #creates it in case the if statement below does not run so that the output of the function is constant in class
  if (max(select_years) >= lubridate::year(Sys.Date() - 577)) {
    
    flow_real_time <- aqFlow[[station_number]]$timeseries[,1:2]
    names(flow_real_time) <- c("Date", "Value")
    flow_real_time$STATION_NUMBER <- station_number
    
    #Filter the data here if requested (option exists in case user wants to see the outliers)
    if (filter == TRUE) {
      IQR <- stats::IQR(flow_real_time$Value, na.rm=TRUE)
      quantiles <- stats::quantile(flow_real_time$Value, na.rm=TRUE, probs = c(.05, .95))
      
      flow_real_time <- subset(flow_real_time, flow_real_time$Value > (quantiles[1] - 2*IQR) & flow_real_time$Value < (quantiles[2] + 2*IQR))
    }
    
    
    if (high_res == TRUE){ #If requesting high-res data
      recent_flow <- flow_real_time %>% plyr::rename(c("Value"="Flow"))
      recent_flow$DateOnly <- lubridate::date(recent_flow$Date)
      recent_flow <- recent_flow[,-c(3,5:10)]
    }
    
    flow_real_time <- flow_real_time %>%
      dplyr::group_by(.data$STATION_NUMBER, lubridate::year(.data$Date), lubridate::yday(.data$Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(.data$Date)), #retain single data (mean) point per day
                       Flow = mean(.data$Value),
                       .groups = "drop")
    flow_real_time <- flow_real_time[,-c(2,3)]
    
    flow_df <- dplyr::bind_rows(flow_historic, flow_real_time)
    
  } else {
    flow_df <- flow_historic
  } 
  
  
  # Add rows of missing dates
  flow_df <- fasstr::fill_missing_dates(data = flow_df, dates = "Date")
  
  # Remove Feb. 29 data
  flow_df <- flow_df[!(format(flow_df$Date,"%m") == "02" & format(flow_df$Date, "%d") == "29"), , drop = FALSE]
  
  # Create dayofyear column with seq(1:365) so that leap years and non leap years are equal
  # Calculate percentiles (IQR, max/min)
  flow_df <- flow_df %>%
    dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
                                     ifelse(lubridate::month(.data$Date) <= 2,
                                            lubridate::yday(.data$Date),
                                            lubridate::yday(.data$Date) - 1),
                                     lubridate::yday(.data$Date)))
  
  current.year <- dplyr::filter(flow_df, Date == seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) #set the current year aside
  
  flow_df <- dplyr::filter(flow_df, Date!=seq.Date(from=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")), to=as.Date(paste0(lubridate::year(Sys.Date()), "-12-31")), by="day")) %>% #remove current year so it doesn't mess with the stats
    dplyr::filter(!is.na(.data$Flow)) %>% #remove na values in Flow so that stats::ecdf can work below - they're added in after
    dplyr::group_by(.data$dayofyear) %>%
    dplyr::mutate(prctile = (stats::ecdf(.data$Flow)(.data$Flow)) * 100) %>%
    fasstr::fill_missing_dates(dates = "Date") %>% #add the missing dates back in now - including Feb 29
    dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list, 
                                     ifelse(lubridate::month(.data$Date) <= 2,
                                            lubridate::yday(.data$Date),
                                            lubridate::yday(.data$Date) - 1),
                                     lubridate::yday(.data$Date))) %>%
    dplyr::group_by(.data$dayofyear) %>%
    dplyr::mutate(Max = max(.data$Flow, na.rm = TRUE),
                  Min = min(.data$Flow, na.rm = TRUE),
                  QP90 = stats::quantile(.data$Flow, 0.90, na.rm = TRUE),
                  QP75 = stats::quantile(.data$Flow, 0.75, na.rm = TRUE),
                  QP50 = stats::quantile(.data$Flow, 0.50, na.rm = TRUE),
                  QP25 = stats::quantile(.data$Flow, 0.25, na.rm = TRUE),
                  QP10 = stats::quantile(.data$Flow, 0.10, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    hablar::rationalize() #rationalize replaces Inf values with NA
  
  current.year$Max <- as.numeric(NA)
  current.year$Min <- as.numeric(NA)
  current.year$QP90 <- as.numeric(NA)
  current.year$QP75 <- as.numeric(NA)
  current.year$QP50 <- as.numeric(NA)
  current.year$QP25 <- as.numeric(NA)
  current.year$QP10 <- as.numeric(NA)
  current.year$prctile <- as.numeric(NA)
  
  for (i in unique(current.year$dayofyear)){ #populate rows with the necessary stats
    current.year$Max[current.year$dayofyear==i] <- unique(flow_df$Max[flow_df$dayofyear==i])
    current.year$Min[current.year$dayofyear==i] <- unique(flow_df$Min[flow_df$dayofyear==i])
    current.year$QP90[current.year$dayofyear==i] <- unique(flow_df$QP90[flow_df$dayofyear==i])
    current.year$QP75[current.year$dayofyear==i] <- unique(flow_df$QP75[flow_df$dayofyear==i])
    current.year$QP50[current.year$dayofyear==i] <- unique(flow_df$QP50[flow_df$dayofyear==i])
    current.year$QP25[current.year$dayofyear==i] <- unique(flow_df$QP25[flow_df$dayofyear==i])
    current.year$QP10[current.year$dayofyear==i] <- unique(flow_df$QP10[flow_df$dayofyear==i])
    current.year$prctile[current.year$dayofyear==i] <- ((current.year$Flow[current.year$dayofyear==i] - unique(flow_df$Min[flow_df$dayofyear==i])) / (unique(flow_df$Max[flow_df$dayofyear==i]) - unique(flow_df$Min[flow_df$dayofyear==i]))) * 100
  }
  flow_df <- dplyr::bind_rows(flow_df, current.year)#add in the current year
  
  last_year <- lubridate::year(max(flow_df$Date))
  
  # For loop to populate flow_years with data from each year in select_years. last_year is used here so that everything gets the same year (so Feb 1 1989 shows up as Feb 1 2022 if the year is 2022) which allows for easy plotting of multiple years together. Column Year_Real holds the true year.
  flow_years <- data.frame()
  for(i in select_years) {
    single_year <- flow_df %>%
      subset(lubridate::year(Date) == i) %>%
      dplyr::mutate(Year = last_year,
                    Month = lubridate::month(.data$Date),
                    Day = lubridate::day(.data$Date), 
                    Year_Real = i) %>%
      dplyr::mutate(Date_2 = as.Date(paste(.data$Year, .data$Month,.data$ Day, sep = "-"))) %>%
      dplyr::select(-.data$Date, -.data$Month, -.data$Day, -.data$Year) %>%
      dplyr::rename(Date = .data$Date_2)
    
    single_year <- single_year[,c(1, 13, 3:11, 2, 12)]
    flow_years <- dplyr::bind_rows(flow_years, single_year)
  }
  
  
  
  
  
  
  
  #If loop below modifies recent_flow if high_res is TRUE
  if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){ # Create a few columns here depending on other options
    recent_flow <- recent_flow %>% dplyr::mutate(dayofyear = ifelse(lubridate::year(.data$Date) %in% leap_list,
                                                                    ifelse(lubridate::month(.data$Date) <=2,
                                                                           lubridate::yday(.data$Date),
                                                                           lubridate::yday(.data$Date) - 1),
                                                                    lubridate::yday(.data$Date))) %>%
      dplyr::mutate(prct_max_hist = as.numeric(NA))
    
    if (recent_prctile == TRUE){ #Calculate a percent historic for the 5 minute data 
      for (i in 1:nrow(recent_flow)){
        recent_flow$prct_max_hist[i] <- ((recent_flow$Flow[i] - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]])) / (unique(flow_df$Max[flow_df$dayofyear == recent_flow$dayofyear[i]]) - unique(flow_df$Min[flow_df$dayofyear == recent_flow$dayofyear[i]]))) * 100
      }
    }
    
    #Fill missing data points in recent_flow: first figure out the recording rate, then fill with NAs
    diff <- vector()
    for (i in 1:nrow(recent_flow)){
      diff[i] <- as.numeric(difftime(recent_flow$Date[i+1], recent_flow$Date[i]))
    }
    diff <- as.numeric(names(sort(table(diff),decreasing=TRUE)[1])) #Take the tightest difference between data points in minutes
    recent_flow <- tidyr::complete(recent_flow, Date = seq.POSIXt(min(.data$Date), max(.data$Date), by=paste0(diff, " min"))) %>%
      dplyr::arrange(dplyr::desc(.data$Date))
  }
  
  flow_years <- flow_years[with(flow_years, order(Year_Real, dayofyear, decreasing = TRUE)),]
  
  if (rate == TRUE) {
    flow_years$rate <- as.numeric(NA)
    for (i in 1:(nrow(flow_years)-1)){
      try(flow_years$rate[i] <- flow_years$Flow[i] - flow_years$Flow[i+1], silent=TRUE)
    }
    
    if (high_res == TRUE & max(select_years) >= lubridate::year(Sys.Date() - 577)){
      recent_flow <- dplyr::mutate(recent_flow, rate = as.numeric(NA))
      if (rate_days == "all"){
        for (i in 1:(nrow(recent_flow)-1)){
          try(recent_flow$rate[i] <- recent_flow$Flow[i] - recent_flow$Flow[i+1], silent=TRUE)
        }
      } else {
        last_row <- which(recent_flow$Date== (recent_flow$Date[1] - rate_days*60*60*24))
        for (i in 1:last_row){
          try(recent_flow$rate[i] <- recent_flow$Flow[i] - recent_flow$Flow[i+1], silent=TRUE)
        }
      }
    }
  }
  
  #Warning messages
  if (nrow(recent_flow) <= 1 & nrow(flow_years) >= 1){
    warning("There is no high-resolution data for the data range you selected. Note that high-resolution data is only kept by the WSC for 18 months. All of the available data for the date range you requested is in the requested_years data.frame.")
  }
  
  if (nrow(recent_flow) <= 1 & nrow(flow_years) <= 1){
    range <- dplyr::filter(tidyhydat::hy_stn_data_range(station_number), .data$DATA_TYPE == "Q")
    range <- seq(as.numeric(range$Year_from), as.numeric(range$Year_to))
    warning(paste0("No data exists for the years you requested. Only historical data was returned. Note that the historical data range is from ", range[1], " to ", range[length(range)], " and that high-resolution data is only kept for 18 months."))
  }
  if (length(select_years) > 1 & nrow(flow_years > 1)){
    for (i in select_years){
      if (!(i %in% unique(flow_years$Year_Real))){
        warning(paste0("No data was available (historical daily means or recent, high-resolution data) for year ", i, ". All other years of data have been returned."))
      } 
    }
  }
  
  
  flowData <- list(all_historical = flow_df, requested_years = flow_years, recent_flow = recent_flow)
  return(flowData)
  
}
