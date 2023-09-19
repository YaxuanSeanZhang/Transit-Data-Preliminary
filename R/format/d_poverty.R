var_2022 <- "HH_SIZE"
new_name <- "poverty"

svy_2022[, eval(new_name) := svy_2022[, get(var_2022)]]
svy_2022[, eval(new_name) := as.factor(get(new_name))]
lev_2022 <- levels(svy_2022[, get(new_name)])

svy_2022[, eval(new_name) := fcase(
  get(new_name) == "One (1)",
  "1",
  get(new_name) == "Two (2)",
  "2",
  get(new_name) == "Three (3)",
  "3",
  get(new_name) == "Four (4)",
  "4",
  get(new_name) == "Five (5)",
  "5",
  get(new_name) == "Six (6)",
  "6",
  get(new_name) == "Seven (7)",
  "7",
  get(new_name) == "Eight (8)",
  "8",
  get(new_name) == "Nine (9)",
  "9",
  get(new_name) == "Ten or More (10+)",
  "9"
)]

svy_2022[, poverty := fifelse(
  (poverty == '1' & INCOME == 'Less than $15,000')|
    (poverty == '2' & INCOME == 'Less than $15,000')|
    (poverty == '3' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'))|
    (poverty == '4' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'))|
    (poverty == '5' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '6' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '7' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '8' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'|INCOME == '$35,000 - $59,999'))|
    (poverty == '9' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'|INCOME == '$35,000 - $59,999')),
  'Below Poverty Thresholds', "Above Poverty Thresholds"
)]

svy_2022[,poverty := fifelse(INCOME =='Refused/No Answer',
                             'Refused/No Answer',poverty)]


var_2016 <- "COUNT_MEMBER_HH"
new_name <- "poverty"

svy_2016[, eval(new_name) := svy_2016[, get(var_2016)]]
svy_2016[, eval(new_name) := as.factor(get(new_name))]
lev_2016 <- levels(svy_2016[, get(new_name)])

svy_2016[, eval(new_name) := fcase(
  get(new_name) == "One (1)",
  "1",
  get(new_name) == "Two (2)",
  "2",
  get(new_name) == "Three (3)",
  "3",
  get(new_name) == "Four (4)",
  "4",
  get(new_name) == "Five (5)",
  "5",
  get(new_name) == "Six (6)",
  "6",
  get(new_name) == "Seven (7)",
  "7",
  get(new_name) == "Eight (8)",
  "8",
  get(new_name) == "Nine (9)",
  "9",
  get(new_name) == "Ten or More (10+)",
  "9",
  get(new_name) == "",
  "1"
)]

svy_2016[, poverty := fifelse(
  (poverty == '1' & INCOME == 'Less than $15,000')|
    (poverty == '2' & INCOME == 'Less than $15,000')|
    (poverty == '3' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'))|
    (poverty == '4' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'))|
    (poverty == '5' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '6' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '7' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'))|
    (poverty == '8' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'|INCOME == '$35,000 - $59,999'))|
    (poverty == '9' & (INCOME == 'Less than $15,000' |INCOME == '$15,000 - $24,999'|
                         INCOME == '$25,000 - $34,999'|INCOME == '$35,000 - $59,999')),
  'Below Poverty Thresholds', "Above Poverty Thresholds"
)]

svy_2016[,poverty := fifelse(INCOME =='Don’t Know / Refuse',
                             'Refused/No Answer',poverty)]