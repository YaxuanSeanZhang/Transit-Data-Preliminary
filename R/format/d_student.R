# select variables ------------
var_2016 <- "STUDENT_STATUS"
var_2022 <- "STUDENT_STATUS"
new_name <- "student_v2"

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
  get(new_name) == "Not a student",
  "Not a student",
  get(new_name) == "Other",
  "Student",
  get(new_name) == "Refused/No Answer",
  "Refused/No Answer",
  get(new_name) == "Yes - Full-time College / University / Trade School student",
  "Student",
  get(new_name) == "Yes - High School (9th-12th grade)",
  "Student",
  get(new_name) == "Yes - K-8th grade",
  "Student",
  get(new_name) == "Yes - Part-time College / University / Trade School student",
  "Student"
)]


svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Yes - College / University / Community College",
  "Student",
  get(new_name) == "Yes - K - 12th grade",
  "Student",
  get(new_name) == "Yes - Vocational / Technical / Trade school",
  "Student",
  get(new_name) == "Not a student",
  "Not a student",
  get(new_name) == "Other",
  "Student",
  get(new_name) == "" | get(new_name) == "0",
  "Refused/No Answer"
)]


# set factor levels -------------
new_levels <- c(
  "Student",
  "Not a student",
  "Refused/No Answer"
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
  "New column in svy2016 and svy2022: ", new_name, "\n", "With factor levels: ",
  paste0(lev_2016, collapse = "\n")
))

rm(lev_2016, lev_2022, new_name, var_2016, var_2022, new_levels)
