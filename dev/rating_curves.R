# New tables to hold rating curves

DBI::dbExecute(con, "CREATE TABLE rating_curves_reference (
  curve_reference_id SERIAL PRIMARY KEY,
  location_id INTEGER NOT NULL REFERENCES locations(id),
  input_parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id),
  output_parameter_id INTEGER NOT NULL REFERENCES parameters(parameter_id),
  last_modified TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  description TEXT,
  notes TEXT,
  UNIQUE(location_id, input_timeseries_id, output_timeseries_id)
)")

DBI::dbExecute(con, "CREATE TABLE rating_curves_curves (
  curve_id SERIAL PRIMARY KEY,
  curve_reference_id INTEGER REFERENCES rating_curves_reference(curve_reference_id),
  curve_type TEXT NOT NULL,
  notes TEXT,
  valid_from TIMESTAMP WITH TIME ZONE,
  valid_to TIMESTAMP WITH TIME ZONE,
  inputValues numeric[] NOT NULL,
  outputValues numeric[] NOT NULL,
  offset numeric
  UNIQUE(curve_reference_id, valid_from, valid_to)
)")

DBI::dbExecute(con, "CREATE TABLE rating_curves_curves_shifts (
  curve_shift_id SERIAL PRIMARY KEY,
  curve_id INTEGER NOT NULL REFERENCES rating_curves_curves(curve_id),
  shift_start TIMESTAMP WITH TIME ZONE NOT NULL,
  shift_end TIMESTAMP WITH TIME ZONE NOT NULL,
  shift_value numeric NOT NULL
  UNIQUE(curve_id, shift_start, shift_end)
)")

# Add constraint or check that inputValues and outputValues are the same length
DBI::dbExecute(con, "ALTER TABLE rating_curves_curves ADD CONSTRAINT input_output_length CHECK (array_length(inputValues, 1) = array_length(outputValues, 1))")

# New table to hold more extensive location metadata


