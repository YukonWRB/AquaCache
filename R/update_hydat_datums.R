# Without this function, only function which adds new datums from HYDAT is initial_create.

# this function could be triggered if there is a new hydat (see fx update_hydat) or on demand. Should NOT delete entries that are not in the new hydat, unless they are not in use in the DB anywhere, since these may have been added by a user for a specific purpose.

# Needs to check if a WSC location has a new current datum and update the datums table


# Consider whether it makes sense to incorporate the datum conversion tool from ECCC in case the user requests a datum not in the DB. This would likely have to be a separate function.
