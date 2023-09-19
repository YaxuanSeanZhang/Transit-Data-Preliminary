# select variables ------------
var_2016 <- "INCOME"
var_2022 <- "INCOME"
new_name <- "income_v2"

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
  get(new_name) == "$100,000 - $149,999",
  "$100 - $150K",
  get(new_name) == "$15,000 - $24,999",
  "$15 - $35K",
  get(new_name) == "$150,000 - $199,999",
  "$150K or more",
  get(new_name) == "$200,000 or more",
  "$150K or more",
  get(new_name) == "$25,000 - $34,999",
  "$15 - $35K",
  get(new_name) == "$35,000 - $59,999",
  "$35 - $60K",
  get(new_name) == "$60,000 - $79,999",
  "$60 - $100K",
  get(new_name) == "$80,000 - $99,999",
  "$60 - $100K",
  get(new_name) == "Less than $15,000",
  "Less than $15K",
  get(new_name) == "Refused/No Answer",
  "Refused/No Answer"
)]

svy_2016[, eval(new_name) := fcase(
  get(new_name) == "Less than $15,000",
  "Less than $15K",
  get(new_name) == "$15,000 - $24,999",
  "$15 - $35K",
  get(new_name) == "$25,000 - $34,999",
  "$15 - $35K",
  get(new_name) == "$35,000 -  $59,999",
  "$35 - $60K",
  get(new_name) == "$60,000  - $99,999",
  "$60 - $100K",
  get(new_name) == "$100,000 - $149,999",
  "$100 - $150K",
  get(new_name) == "$150,000 - $199,999",
  "$150K or more",
  get(new_name) == "$200,000 or more",
  "$150K or more",
  get(new_name) == "Don’t Know / Refuse",
  "Refused/No Answer"
)]


# set factor levels -------------
new_levels <- c("Less than $15K", "$15 - $35K", "$35 - $60K", "$60 - $100K", 
                "$100 - $150K", "$150K or more", "Refused/No Answer")
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
