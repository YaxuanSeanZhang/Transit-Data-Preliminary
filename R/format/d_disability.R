# select variables ------------
var_2016 <- "DISABILITY"
var_2022 <- "HAVE_DISABILITY"
new_name <- "disability_v2"

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
  get(new_name) == "No",
  "No",
  get(new_name) == "Prefer not to say",
  "Prefer not to say",
  get(new_name) == "Yes",
  "Yes",
  is.na(get(new_name)),
  "Prefer not to say"
)]


svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Yes",
  "Yes",
  get(new_name) == "No",
  "No",
  get(new_name) == "Don't Know / Refuse",
  "Prefer not to say",
  is.na(get(new_name)),
  "Prefer not to say"
)]


# set factor levels -------------
new_levels <- c("Yes", "No", "Prefer not to say")
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
