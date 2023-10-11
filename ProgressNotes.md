Progress tracking for postgres migration/updates


completed: #########
initial_create
initial_WSC
synchronizeContinuous
vacuum
getRealtimeAquarius
getRealtimeWSC
add_timeseries
getNewRealtime
calculate_stats
hydrometConnect  #mirrored in YGwater, so update there as well. Consider if function should instead only live in YGwater.
update_hydat
hydat_check


to do: #############
daily   # Getting there... big tasks are being broken up into individual functions
getSnowCourse  # new snow DB needs to be created before working on this function
synchronizeDiscrete
getWatersheds   - and also add a feature to calculate watersheds for locs without a WSC polygon
getNewDiscrete
update_hydat_datums
create thresholds table
remove all YGWater dependencies, if exist
Create code to pull all data in the existing sqlite DB, cast to new DB, and use to populate the new DB

other ##############
What about filling missing temperature and precip data using ML? 
The HRDPA is an obvious tool for precip, while nearby locations could be used for temp. Unless there's a temperature equivalent to the HRDPA...
