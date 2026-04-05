# Patch framework to update AquaCache database so that it can compute and serve out composite or calculated timeseries.
# Use case examples:
# - Met station data where the ECCC station_id and sometimes the monitoring location itself has changed over time. Dawson is a prime example: measurements used to be in town then switched to the airport, and move there twice after. It's super handy to see and use a single timeseries for Dawson, but stitching the records together requires adjustments to the older timeseries to align with the new. A composite can allow for adjustments to be explicit to certain sections, without losing the existing original data or losing the ability to search by the old location.
# - Specific conductance, calculated from raw conductivity and temperature. Two timeseries with an equation resulting in a third, calculated, parameter

# To perserve the existing functionality of the 'timeseries' table and the 'measurements_continuous_corrected' and 'measurements_calculated_daily_corrected' views, this patch aims to be a 'bolt on' update that should require no changes to application logic except to use new functionality.

# Current setup:
# metatadata -> raw measurements -> corrections, if any -> raw and corrected data to end user
# 'timeseries' -> 'measurements_continuous' -> 'corrections' -> 'measurements_continuous_corrected' (view)
# !This setup is kept for 'basic' timeseries.

# New setup for derived timeseries:
# metadata (incl derived ts) -> raw measurements (basic ts) -> corrections (basic ts) -> view table (basic ts) -> composite calculations (SQL?) -> view table (composite ts)
# 'timeseries' -> 'measurements_continuous' -> 'corrections' -> view -> composte calculations -> view 
# Exact routing TBD... but corrections need to apply to the basic TS before being used for the composite TS, which itself should have corrections applied!

# Add new column to 'timeseries' and modify unique key so that's it's different between the two timeseries types
DBI::dbExecute(con, "ALTER TABLE timeseries ADD COLUMN type TEXT CHECK type IN ('basic', 'derived')")
# Modify unique key
# Remove blanket NOT NULLs, make conditional

DBI::dbExecute(con, "CREATE TABLE continuous.timeseries_derivations 
  derivation_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
  timeseries_id INTEGER REFERENCES continuous.timeseries, -- the product timeseries
  created TIMESTAMP WITH TIME ZONE DEFAULT now(),
  created_by TEXT NOT NULL DEFAULT current_user(),
  modfied TIMESTAMP WITH TIME ZONE,
  modified_by TEXT
")
# Ensure that 'timeseres_id' can only reference a type 'derived'
# Attach triggers for modified, modified_by

# Questions below:
# Should equation be NULLABLE?
# Should this take advantage of temporal constraints by using a datetimerange?
DBI::dbExecute(con, "CREATE TABLE continous.timeseries_derivation_periods
  derivation_period_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  derivation_id INTEGER NOT NULL REFERENCES continuous.timeseries_derivations.derivation_id
  start TIMESTAMP WITH TIME ZONE NOT NULL, -- start of the calculation period
  end TIMESTAMP WITH TIME ZONE, -- end of the calculation period
  components INTEGER NOT NULL, -- kept in synch by trigger on table 'timeseries_derivation_components'
  equation TEXT, -- parsable as SQL
  created TIMESTAMP WITH TIME ZONE DEFAULT now(),
  created_by TEXT NOT NULL DEFAULT current_user(),
  modfied TIMESTAMP WITH TIME ZONE,
  modified_by TEXT
")
# Attach triggers for modified, modified_by


DBI::dbExecute(con, "CREATE TABLE continuous.timeseries_derivation_components
  component_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  derivation_id INTEGER NOT NULL REFERENCES continuous.timeseries_derivations
  timeseries_id INTEGER REFEERENCES continuous.timeseries, -- can reference either basic or derived timeseries
  created TIMESTAMP WITH TIME ZONE DEFAULT now(),
  created_by TEXT NOT NULL DEFAULT current_user(),
  modfied TIMESTAMP WITH TIME ZONE,
  modified_by TEXT
")
# Attach triggers for modified, modified_by



# Other option: bypass table timeseries_derivations entirely
DBI::dbExecute(con, "CREATE TABLE continous.timeseries_derivations
  derivation_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  timeseries_id INTEGER NOT NULL REFERENCES continuous.timeseries -- but limited to only reference timeseries of type 'derived'
  start TIMESTAMP WITH TIME ZONE NOT NULL, -- start of the calculation period
  end TIMESTAMP WITH TIME ZONE, -- end of the calculation period
  components INTEGER NOT NULL, -- kept in synch by trigger on table 'timeseries_derivation_components'
  equation TEXT, -- parsable as SQL
  created TIMESTAMP WITH TIME ZONE DEFAULT now(),
  created_by TEXT NOT NULL DEFAULT current_user(),
  modfied TIMESTAMP WITH TIME ZONE,
  modified_by TEXT
")
# Attach triggers for modified, modified_by

DBI::dbExecute(con, "CREATE TABLE continuous.timeseries_derivation_components
  component_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  derivation_id INTEGER NOT NULL REFERENCES continuous.timeseries_derivations
  timeseries_id INTEGER REFEERENCES continuous.timeseries, -- can reference either basic or derived timeseries
  created TIMESTAMP WITH TIME ZONE DEFAULT now(),
  created_by TEXT NOT NULL DEFAULT current_user(),
  modfied TIMESTAMP WITH TIME ZONE,
  modified_by TEXT
")
# Attach triggers for modified, modified_by
