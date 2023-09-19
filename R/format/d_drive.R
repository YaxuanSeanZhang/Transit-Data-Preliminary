svy_2022$drive = svy_2022$DO_YOU_DRIVE

svy_2022[, drive := fcase(
  DO_YOU_DRIVE == "Yes",
  "Drive, but could not have made this trip by car",
  DO_YOU_DRIVE == "No",
  "Don't drive, have household vehicle",
  is.na(DO_YOU_DRIVE),
  "Don't drive, don't have household vehicle"
)]

svy_2022[, drive := fifelse(
  (drive == "Drive, but could not have made this trip by car" & 
     USED_VEH_TRIP == "Yes"),
  "Drive, and could have made this trip by car",
  drive
)]

svy_2022[is.na(drive),"drive"] = "Drive, but could not have made this trip by car"

#do you have household vehicle
svy_2022[, vehicle := fifelse(
  COUNT_VH_HH == 'None (0)', 'No', 'Yes'
)]