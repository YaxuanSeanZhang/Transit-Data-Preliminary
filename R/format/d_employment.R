# select variables ------------
var_2016 <- "STATUS_EMPLOYMENT"
var_2022 <- "EMPLOYMENT_STATUS"
new_name <- "employment_v2"

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
  get(new_name) == "Employed full-time (paid, working 30 or more hours per week)",
  "Employed full-time",
  get(new_name) == "Employed part-time (paid, working less than 30 hours per week)",
  "Employed part-time",
  get(new_name) == "Employed, but not currently working (e.g. waiting for workplace to reopen, furloughed)",
  "Unemployed, seeking work",
  get(new_name) == "Not currently employed, and looking for work",
  "Unemployed, seeking work",
  get(new_name) == "Primarily self-employed",
  "Employed full-time",
  get(new_name) == "Not currently employed, and not looking for work",
  "Unemployed, not seeking work",
  get(new_name) == "Retired",
  "Retired",
  get(new_name) == "Stay at home parent or caregiver",
  "Stay at home parent or caregiver",
  get(new_name) == "Unpaid volunteer or intern",
  "Employed part-time"
)]

svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Employed Full-time",
  "Employed full-time",
  get(new_name) == "Employed Part-time",
  "Employed part-time",
  get(new_name) == "Retired",
  "Retired",
  get(new_name) == "Not currently employed - not seeking work",
  "Unemployed, not seeking work",
  get(new_name) == "Not currently employed - seeking work",
  "Unemployed, seeking work",
  get(new_name) == "Stay at home parent or caregiver",
  "Stay at home parent or caregiver",
  get(new_name) == "",
  "Prefer not to answer"
)]

#combine unemployed-----
svy_2016[,employment_v3:= fifelse(
  employment_v2=="Unemployed, not seeking work"| 
    employment_v2=="Unemployed, seeking work",
  'Unemployed',employment_v2)
]

svy_2022[,employment_v3:= fifelse(
  employment_v2=="Unemployed, not seeking work"| 
    employment_v2=="Unemployed, seeking work",
  'Unemployed',employment_v2)
]

# set factor levels -------------
new_levels <- c(
  "Employed full-time", 
  "Employed part-time",
  "Unemployed, not seeking work",
  "Unemployed, seeking work",
  "Retired",
  "Stay at home parent or caregiver",
  "Prefer not to answer"
)

svy_2016[, eval(new_name) := factor(get(new_name),
  levels = new_levels
)]
svy_2022[, eval(new_name) := factor(get(new_name),
  levels = new_levels
)]

new_levels <- c(
  "Employed full-time", 
  "Employed part-time",
  "Unemployed",
  "Retired",
  "Stay at home parent or caregiver",
  "Prefer not to answer"
)

svy_2016[, employment_v3 := factor(employment_v3,
                                    levels = new_levels
)]
svy_2022[, employment_v3 := factor(employment_v3,
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
