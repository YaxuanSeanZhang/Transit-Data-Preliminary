#define gender variable for 2016--------
var_2016 <- "race_ethnicity"
race_2016 <- c('AMERICAN_INDIAN_ALASKAN_NATIVE','ASIAN',
               'BLACK_AFRICAN_AMERICAN','HISPANIC_LATINO',
               'NATIVE_HAWAIIAN_PACIFIC_ISLANDER','WHITE')

svy_2016[, eval(var_2016) := fcase(
  AMERICAN_INDIAN_ALASKAN_NATIVE == "Yes",
  "American Indian/Alaskan Native",
  ASIAN == "Yes",
  "Asian",
  BLACK_AFRICAN_AMERICAN == "Yes",
  "Black/African American",
  HISPANIC_LATINO == "Yes",
  "Hispanic/Latino",
  NATIVE_HAWAIIAN_PACIFIC_ISLANDER == "Yes",
  "Native Hawaiian/Pacific Islander",
  WHITE == "Yes",
  "White"
)]

svy_2016[, eval(var_2016) := fcase(
  apply(svy_2016[,race_2016,with = F], 1, function(x) sum(x == "Yes"))>1,
  "Mixed", 
  rep_len(TRUE, nrow(svy_2016)),
  get(var_2016)
  )]

svy_2016[, eval(var_2016) := fcase(
  is.na(svy_2016[,eval(var_2016),with=F]),
  "Prefer not to answer", 
  rep_len(TRUE, nrow(svy_2016)),
  get(var_2016)
)]

#define gender variable for 2022--------
var_2022 <- "race_ethnicity"
race_2022 <- c('RACE..1.','RACE..2.',
               'RACE..3.','RACE..4.',
               'RACE..5.','RACE..6.','RACE..7.')

svy_2022[, eval(var_2022) := fcase(
  RACE..1. == "Yes",
  "American Indian / Alaska Native",
  RACE..3. == "Yes",
  "Asian",
  RACE..2. == "Yes",
  "Black / African / African American",
  RACE..6. == "Yes",
  "Hispanic / Latino / Spanish origin",
  RACE..7. == "Yes",
  "Middle Eastern or North African",
  RACE..5. == "Yes",
  "Native Hawaiian / Pacific Islander",
  RACE..4. == "Yes",
  "White"
)]

svy_2022[, eval(var_2022) := fcase(
  apply(svy_2022[,race_2022,with = F], 1, function(x) sum(x == "Yes"))>1,
  "multiple identified", 
  rep_len(TRUE, nrow(svy_2022)),
  get(var_2022)
)]

svy_2022[, eval(var_2022) := fcase(
  is.na(svy_2022[,eval(var_2022),with=F]),
  "Prefer not to answer", 
  rep_len(TRUE, nrow(svy_2022)),
  get(var_2022)
)]

# select variables ------------
var_2016 <- "race_ethnicity"
var_2022 <- "race_ethnicity"
new_name <- "race_ethnicity"

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
  get(new_name) == "American Indian / Alaska Native",
  "American Indian",
  get(new_name) == "Asian",
  "Asian",
  get(new_name) == "Black / African / African American",
  "African American",
  get(new_name) == "Hispanic / Latino / Spanish origin",
  "Hispanic",
  get(new_name) == "Middle Eastern or North African",
  "Other",
  get(new_name) == "multiple identified",
  "Multiple Race",
  get(new_name) == "Native Hawaiian / Pacific Islander",
  "Pacific Islander",
  get(new_name) == "Prefer not to answer",
  "Prefer not to answer",
  get(new_name) == "White",
  "White"
)]

svy_2016[, eval(new_name) := fcase(
  get(new_name) == "White",
  "White",
  get(new_name) == "Black/African American",
  "African American",
  get(new_name) == "Mixed",
  "Multiple Race",
  get(new_name) == "American Indian/Alaskan Native",
  "American Indian",
  get(new_name) == "Native Hawaiian/Pacific Islander",
  "Pacific Islander",
  get(new_name) == "Hispanic/Latino",
  "Hispanic",
  get(new_name) == "Asian",
  "Asian",
  get(new_name) == "Prefer not to answer",
  "Prefer not to answer"
)]

# set factor levels -------------
new_levels <- c(
  "Prefer not to answer",
  "Multiple Race",
  "White",
  "African American",
  "Asian",
  "Hispanic",
  "American Indian",
  "Other",
  "Pacific Islander"
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
  "New column in svy2016p and svy2022p: ", new_name, "\n", "With factor levels: \n",
  paste0(lev_2016, collapse = "\n")
))

rm(lev_2016, lev_2022, new_name, var_2016, var_2022, new_levels, race_2016, race_2022)
