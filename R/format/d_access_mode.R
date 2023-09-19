# select variables ------------
var_2016 <- "ACCESS_MODE"
var_2022 <- "ORIGIN_TRANSPORT"
new_name <- "access_mode"

# copy the old column to the new column --------
svy_2016[, eval(new_name) := svy_2016[, get(var_2016)]]
svy_2022[, eval(new_name) := svy_2022[, get(var_2022)]]

# make factor--------
svy_2016[, eval(new_name) := as.factor(get(new_name))]
svy_2022[, eval(new_name) := as.factor(get(new_name))]


# update levels ----------
lev_2016 <- levels(svy_2016[, get(new_name)])
lev_2022 <- levels(svy_2022[, get(new_name)])


svy_2022[, eval(new_name) := fcase(
  get(new_name) == "Drove alone and parked",
  "Drove alone and parked",
  get(new_name) == "Drove or rode with others and parked",
  "Drove or rode with others and parked",
  get(new_name) == "My bike / e-bike / scooter / skateboard",
  "Bike/ Scooter/ Skateboard",
  get(new_name) == "Shared bike / e-bike / scooter (e.g. Nice Ride, Bird, Lime)",
  "Bike/ Scooter/ Skateboard",
  get(new_name) == "Uber, Lyft, Taxi (smartphone ride hailing)",
  "Uber, Lyft, Taxi (smartphone ride hailing)",
  get(new_name) == "Walk, jog, or roll using a mobility device (e.g. wheelchair)",
  "Walk, jog, or roll using a mobility device (e.g. wheelchair)",
  get(new_name) == "Was dropped off by someone",
  "Was dropped off by someone"
)]


svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Walk",
  "Walk, jog, or roll using a mobility device (e.g. wheelchair)",
  get(new_name) == "Bike",
  "Bike/ Scooter/ Skateboard",
  get(new_name) == "Shuttle Bus",
  "Other",
  get(new_name) == "Uber, Lyft, etc.",
  "Uber, Lyft, Taxi (smartphone ride hailing)",
  get(new_name) == "Wheelchair, walker, motorized cart",
  "Walk, jog, or roll using a mobility device (e.g. wheelchair)",
  get(new_name) == "Drove alone and parked",
  "Drove alone and parked",
  get(new_name) == "Drove or rode with others and parked",
  "Drove or rode with others and parked",
  get(new_name) == "Car share",
  "Other",
  get(new_name) == "Taxi",
  "Uber, Lyft, Taxi (smartphone ride hailing)",
  get(new_name) == "Was dropped off by someone",
  "Was dropped off by someone",
  get(new_name) == "Skateboard",
  "Bike/ Scooter/ Skateboard",
  get(new_name) == "Dial-a-Ride",
  "Other"
)]


# set factor levels -------------
new_levels <- c(
  "Walk, jog, or roll using a mobility device (e.g. wheelchair)",
  "Bike/ Scooter/ Skateboard",
  "Drove alone and parked",
  "Drove or rode with others and parked",
  "Was dropped off by someone",
  "Uber, Lyft, Taxi (smartphone ride hailing)",
  "Other"
)
svy_2016[, eval(new_name) := factor(get(new_name),
  levels = new_levels
)]
svy_2022[, eval(new_name) := factor(get(new_name),
  levels = new_levels
)]

# check for missing levels--------------
lev_2016 <- levels(svy_2016[, get(new_name)])
lev_2022 <- levels(svy_2022[, get(new_name)])

if (length(setdiff(lev_2016, lev_2022)) > 0 |
  length(setdiff(lev_2022, lev_2016)) > 0
) {
  message("Some factor levels missing from one data set or another! Go check.")
}



# cleanup -------------------------
message(paste0(
  "New column in svy2016p and svy2022p: ", new_name, "\n", "With factor levels:\n",
  paste0(lev_2016, collapse = "\n")
))

rm(lev_2016, lev_2022, new_name, var_2016, var_2022, new_levels)
