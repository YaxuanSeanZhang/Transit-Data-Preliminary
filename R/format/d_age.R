# select variables ------------
var_2016 <- "AGE"
var_2022 <- "YOUR_AGE"
new_name <- "age_v2"

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
  get(new_name) == "45 - 54",
  "45-54",
  get(new_name) == "55 - 64",
  "55-64",
  get(new_name) == "25 - 34",
  "25-34",
  get(new_name) == "18 - 24",
  "18-24",
  get(new_name) == "35 - 44",
  "35-44",
  get(new_name) == "16 - 17",
  "Under 18",
  get(new_name) == "65 - 74",
  "Over 65",
  get(new_name) == "15-May",
  "Under 18",
  get(new_name) == "Under 5",
  "Under 18",
  get(new_name) == "75 and over",
  "Over 65",
  get(new_name) == "Prefer not to answer",
  "Prefer not to answer"
)]


svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Under 12",
  "Under 18",
  get(new_name) == "13-15",
  "Under 18",
  get(new_name) == "16-17",
  "Under 18",
  get(new_name) == "18-24",
  "18-24",
  get(new_name) == "25-34",
  "25-34",
  get(new_name) == "35-44",
  "35-44",
  get(new_name) == "45-54",
  "45-54",
  get(new_name) == "55-64",
  "55-64",
  get(new_name) == "65-74",
  "Over 65",
  get(new_name) == "75-84",
  "Over 65",
  get(new_name) == "85 and Over",
  "Over 65"
)]


# set factor levels -------------
new_levels <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64",
                "Over 65","Prefer not to answer")
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
