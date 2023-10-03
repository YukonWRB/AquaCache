Progress tracking for postgres migration/updates


List of functions updated/left to update

completed: #########
initial_create
initial_WSC
weekly
vacuum
getRealtimeAquarius
getRealtimeWSC
add_timeseries
update_hydat

getNewRealtime
calculate_stats

to do: #############
hydrometConnect   # Update required to keep credentials in .Renviron file instead of direct in function.  Also, this one needs to be mirrored in WRBtools once mature.
daily   # Getting there... big tasks are being broken up into individual functions
getSnowCourse
getWatersheds   -and also add a feature to calculate watersheds for locs without a WSC polygon
getNewDiscrete
update_hydat_datums
create thresholds table

other ##############
What about filling missing temperature and precip data using ML? 
The HRDPA is an obvious tool for precip, while nearby locations could be used for temp. Unless there's a temperature equivalent to the HRDPA...
