#define gender variable for 2022--------
var_2022 <- "YOUR_GENDER"

gender_2022 <- c('YOUR_GENDER_MC..SQ001.','YOUR_GENDER_MC..SQ002.',
               'YOUR_GENDER_MC..SQ003.','YOUR_GENDER_MC..SQ004.',
               'YOUR_GENDER_MC..SQ005.')

svy_2022[, eval(var_2022) := fcase(
  `YOUR_GENDER_MC..SQ001.` == "Yes",
  "Male",
  `YOUR_GENDER_MC..SQ002.` == "Yes",
  "Female",
  `YOUR_GENDER_MC..SQ003.` == "Yes",
  "Transgender",
  `YOUR_GENDER_MC..SQ004.` == "Yes",
  "Non-binary / third gender",
  `YOUR_GENDER_MC..SQ005.` == "Yes",
  "Other / Prefer to self-describe",
  `YOUR_GENDER_MC..SQ006.` == "Yes",
  "Prefer not to say"
)]

svy_2022[, eval(var_2022) := fcase(
  apply(svy_2022[,gender_2022,with = F], 1, function(x) sum(x == "Yes"))>1,
  "Multiple Gender", 
  rep_len(TRUE, nrow(svy_2022)),
  get(var_2022)
)]

# select variables ------------
var_2016 <- "GENDER"
var_2022 <- "YOUR_GENDER"
new_name <- "Gender"

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
  get(new_name) == "Female",
  "Female",
  get(new_name) == "Male",
  "Male",
  get(new_name) == "Non-binary / third gender",
  "Other Gender",
  get(new_name) == "Other / Prefer to self-describe",
  "Other Gender",
  get(new_name) == "Prefer not to say",
  "Prefer not to say",
  get(new_name) == "Transgender",
  "Other Gender",
  get(new_name) == "Multiple Gender",
  "Other Gender"
)]


svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Female",
  "Female",
  get(new_name) == "Male",
  "Male"
)]


# set factor levels -------------
new_levels <- c("Female", "Male", "Other Gender", "Prefer not to say")
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
  "New column in svy2016p and svy2022p: ", new_name, "\n", "With factor levels: ",
  paste0(lev_2016, collapse = ", ")
))

rm(lev_2016, lev_2022, new_name, var_2016, var_2022, new_levels)
