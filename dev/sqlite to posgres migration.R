# Data migration from old SQLite to new postgres DB

# 1. deal with the timeseries, locations, and datum tables.


# 2. Add continuous data to the DB
# 2.1 Pull apart each timeseries into a list element.
# 2.2 Drop location, parameter and add timeseries_id and imputed column.
# 2.3 Convert datetime_UTC column to datetime, as POSIXct
# 2.4 Add to the table, trigger calculate_stats to populate the daily table.

# 3. Run ancillary functions:
# 3.1 update_hydat
# 3.2 getWatersheds
# 3.3
