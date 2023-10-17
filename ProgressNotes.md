Progress tracking for postgres migration/updates


completed: #########
hydrometInit
initial_WSC
synchronizeContinuous
vacuum
add_timeseries
getNewContinuous
   getRealtimeAquarius
   getRealtimeWSC
calculate_stats
hydrometConnect
update_hydat
hydat_check


to do: #############
dailyUpdate   # works on continuous timeseries, 
getSnowCourse  # new snow DB needs to be created before working on this function  !!! This function is in dev folder
synchronizeDiscrete
getWatersheds   - and also add a feature to calculate watersheds for locs without a WSC polygon
getNewDiscrete
update_hydat_datums
create thresholds table
Create code to pull all data in the existing sqlite DB, cast to new DB, and use to populate the new DB
Create function to add polygons using terra
Create function to add rasters using terra


# Roadmap to getting discrete data in the DB:
1. Create the source_fx function that will actually fetch the data.
2. Finish the relevant section of addHydrometTimeseries.R (see notes withint that script).
3. Add the new data by passing relevant arguments to addHydrometTimeseries.R.
4. Populate script getNewDiscrete.R and test it out. It should call your source_fx functions(s) and look for new data. See example of getNewContinuous which forms an argument list and calls do.call() to trigger the function. You might need to delete some data points from the end of the timeseries and modify timeseries.end_datetime so that your function pulls in new data.
5. Function dailyUpdate is already set to call getNewDiscrete whenever it is run (see line 54 and on)
6. If you want/need to get data in the DB more frequently than dailyUpdate will do, schedule getNewDiscrete on the VM to run more often.
