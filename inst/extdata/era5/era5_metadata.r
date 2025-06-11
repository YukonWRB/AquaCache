library(readr)
library(dplyr)

load_era5_metadata <- function(metadata_dir = "metadata") {
    params_invariant <- read_csv(file.path(metadata_dir, "table1.csv"))
    params_instantaneous <- read_csv(file.path(metadata_dir, "table2.csv"))
    params_accumulations <- read_csv(file.path(metadata_dir, "table3.csv"))
    mean_rates_and_fluxes <- read_csv(file.path(metadata_dir, "table4.csv"))
    params_vertical_instantaneous <- read_csv(file.path(metadata_dir, "table6.csv"))

    # Add a column with the table_name names as the column values
    params_invariant$table_name <- "params_invariant"
    params_instantaneous$table_name <- "params_instantaneous"
    params_accumulations$table_name <- "params_accumulations"
    mean_rates_and_fluxes$table_name <- "mean_rates_and_fluxes"
    params_vertical_instantaneous$table_name <- "params_vertical_instantaneous"

    param_md <- bind_rows(
        params_invariant,
        params_instantaneous,
        params_accumulations,
        mean_rates_and_fluxes,
        params_vertical_instantaneous
    )
    return(param_md)
}