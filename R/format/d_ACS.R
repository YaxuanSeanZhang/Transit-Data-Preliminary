# draft version
acs = read_xlsx('data/CensusACSMCD.xlsx') %>% 
  data.table()

#metro area
acs = acs[COUNTY %in% c('053','003','037','123','139','019','163')]

#gender---------
female = c('F_0_4','F_5_9','F_10_14','F_15_19','F_20_24','F_25_29','F_30_34',
           'F_35_39','F_40_44','F_45_49','F_50_54','F_55_59','F_60_64','F_65_69',
           'F_70_74','F_75_79','F_80_84','F_OVER85')

male = c('M_0_4','M_5_9','M_10_14','M_15_19','M_20_24','M_25_29','M_30_34',
         'M_35_39','M_40_44','M_45_49','M_50_54','M_55_59','M_60_64','M_65_69',
         'M_70_74','M_75_79','M_80_84','M_OVER85')

acs[, female := rowSums(.SD, na.rm = T), .SDcols = female][
  , male := rowSums(.SD, na.rm = T), .SDcols = male]

gender = data.table(gender = c('Female','Male'),
                    pop = c(sum(acs$female),sum(acs$male)))
gender[,frac_pop := pop/sum(pop)]

#disability-----
disability = data.table(disability = c('Yes','No'),
                        pop = c(sum(acs$ANYDIS),sum(acs$POPTOTAL)-sum(acs$ANYDIS)))
disability[,frac_pop := pop/sum(pop)]


#age-----
age_18 = c('F_0_4','F_5_9','F_10_14','F_15_19',
           'M_0_4','M_5_9','M_10_14','M_15_19')
age_24 = c('F_20_24','M_20_24')
age_34 = c('F_25_29','F_30_34','M_25_29','M_30_34')
age_44 = c('F_35_39','F_40_44','M_35_39','M_40_44')
age_54 = c('F_45_49','F_50_54','M_45_49','M_50_54')
age_64 = c('F_55_59','F_60_64','M_55_59','M_60_64')
age_65 = c('F_65_69','F_70_74','F_75_79','F_80_84','F_OVER85',
           'M_65_69','M_70_74','M_75_79','M_80_84','M_OVER85')

acs[, age_18 := rowSums(.SD, na.rm = T), .SDcols = age_18][
  , age_24 := rowSums(.SD, na.rm = T), .SDcols = age_24][
    , age_34 := rowSums(.SD, na.rm = T), .SDcols = age_34][
      , age_44 := rowSums(.SD, na.rm = T), .SDcols = age_44][
        , age_54 := rowSums(.SD, na.rm = T), .SDcols = age_54][
          , age_64 := rowSums(.SD, na.rm = T), .SDcols = age_64][
            , age_65 := rowSums(.SD, na.rm = T), .SDcols = age_65]


age = data.table(age = c('Under 18','18-24','25-34','35-44','45-54','55-64','Over 65'),
                 pop = c(sum(acs$AGEUNDER18),
                         sum(acs$age_24) + sum(acs$age_18) - sum(acs$AGEUNDER18),
                         sum(acs$age_34),sum(acs$age_44),
                         sum(acs$age_54),sum(acs$age_64),
                         sum(acs$age_65)))

age[,frac_pop := pop/sum(pop)]
#poverty------
poverty = data.table(poverty = c('Below Poverty Thresholds','Above Poverty Thresholds'),
                     pop = c(sum(acs$POVERTYN),sum(acs$POPTOTAL)-sum(acs$POVERTYN)))
poverty[,frac_pop := pop/sum(pop)]

#race-----
race = data.table(race = c('Pacific Islander','American Indian','Hispanic','Asian',
                           'African American','White','Multiple Race'),
                  pop = c(sum(acs$PACIFICNH), sum(acs$AMINDNH),
                          sum(acs$HISPPOP),sum(acs$ASIANNH),
                          sum(acs$BLACKNH),sum(acs$WHITENH,na.rm = T),
                          sum(acs$MULTRACENH)))

race[,frac_pop := pop/sum(pop)]
#vehicle ownership----
vehicle = data.table(vehicle = c('No','Yes'),
                     pop = c(sum(acs$HH_NOVEH),sum(acs$POPTOTAL)-sum(acs$HH_NOVEH)))
vehicle[,frac_pop := pop/sum(pop)]
