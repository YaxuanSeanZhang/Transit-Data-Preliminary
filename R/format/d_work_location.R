new_name = 'WORK_LOCATION'

svy_2022[, eval(new_name) := fcase(
  get(new_name) == "Work only from home (self-employed / telework)",
  "Work only from home",
  get(new_name) == "Only one work location (outside of home)",
  "On-site",
  get(new_name) == "Work location is outside of home and regularly varies (different offices/jobsites)",
  "On-site",
  get(new_name) == "Telework some days and travel to a work for remainder",
  "Hybrid",
  get(new_name) == "Drive/bike/travel for work (driver, sales, deliveries)",
  "Driver, sales, deliveries",
  get(new_name) == "Refused/No Answer" | is.na(get(new_name)),
  "Refused/No Answer"
)]

svy_2022[, eval(new_name) := factor(get(new_name),
                                    levels = c("Work only from home",
                                               "Hybrid",
                                               "On-site",
                                               "Driver, sales, deliveries",
                                               "Refused/No Answer")
)]