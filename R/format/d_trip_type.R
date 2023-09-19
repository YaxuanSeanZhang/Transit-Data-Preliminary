## trip ends
place_lut21 <-
  svy_2022[, .N, .(ORIGIN_PLACE_TYPE, ocode = ORIGIN_PLACE_TYPE.Code.)]
place_lut21[, place := fcase(
  ocode == 2,
  "home",
  ocode == 1,
  "work",
  ocode %in% c(5, 6),
  "school",
  ocode %in% c(8, 7, 98, 9, 13, 11, 12),
  "other"
)]

dplace_lut21 <-
  svy_2022[, .N, .(DESTIN_PLACE_TYPE, dcode = DESTIN_PLACE_TYPE.Code.)]
dplace_lut21[, place := fcase(
  dcode == 2,
  "home",
  dcode == 1,
  "work",
  dcode %in% c(5, 6),
  "school",
  dcode %in% c(8, 7, 9, 13, 11, 12,98),
  "other"
)]

svy_2022[place_lut21, on = .(ORIGIN_PLACE_TYPE.Code. = ocode), o_place := place]
svy_2022[dplace_lut21, on = .(DESTIN_PLACE_TYPE.Code. = dcode), d_place := place]

place_lut16 <-
  svy_2016[, .N, .(ORIGIN_PLACE_TYPE, ocode = ORIGIN_PLACE_TYPE_CODE)][order(-N)]
place_lut16[, place := fcase(
  ocode %in% c(2, 11),
  "work",
  ocode == 1,
  "home",
  ocode %in% c(3, 4),
  "school",
  ocode %in% c(8, 6, 7, 5, 99, 9, 10, 11, 12),
  "other"
)]

dplace_lut16 <-
  svy_2016[, .N, .(DESTIN_PLACE_TYPE, dcode = DESTIN_PLACE_TYPE_CODE)][order(-N)]
dplace_lut16[, place := fcase(
  dcode %in% c(2, 11),
  "work",
  dcode == 1,
  "home",
  dcode %in% c(3, 4),
  "school",
  dcode %in% c(8, 6, 7, 5, 99, 9, 10, 11, 12),
  "other"
)]

svy_2016[place_lut16, on = .(ORIGIN_PLACE_TYPE_CODE = ocode), o_place := place]
svy_2016[dplace_lut16, on = .(DESTIN_PLACE_TYPE_CODE = dcode), d_place := place]

# classification
svy_2022[, trip_type := fcase(
  (o_place == "home" &
     d_place == "other"),
  "HBO",
  (d_place == "home" & 
     o_place == "other"),
  "OBH",
  (o_place == "work" &
     d_place == "other"),
  "WBO",
  (d_place == "work" & 
     o_place == "other"),
  "OBW",
  (o_place == "school" &
     d_place == "other"),
  "SBO",
  (d_place == "school" & 
     o_place == "other"),
  "OBS",
  o_place == "work" &
    d_place == "home",
  "W2H",
  o_place == "home" &
    d_place == "work",
  "H2W",
  o_place == "school" &
    d_place == "home",
  "S2H",
  o_place == "home" &
    d_place == "school",
  "H2S",
  o_place == "school" &
    d_place == "work",
  "S2W",
  o_place == "work" &
    d_place == "school",
  "W2S",
  o_place == "other" &
    d_place == "other",
  "OaA",
  o_place == "school" &
    d_place == "school",
  "S2S",
  o_place == "work" &
    d_place == "work",
  "W2W"
)]

svy_2016[, trip_type := fcase(
  (o_place == "home" &
    d_place == "other"),
  "HBO",
  (d_place == "home" & 
     o_place == "other"),
  "OBH",
  (o_place == "work" &
    d_place == "other"),
  "WBO",
  (d_place == "work" & 
     o_place == "other"),
  "OBW",
  (o_place == "school" &
    d_place == "other"),
  "SBO",
  (d_place == "school" & 
     o_place == "other"),
  "OBS",
  o_place == "work" &
    d_place == "home",
  "W2H",
  o_place == "home" &
    d_place == "work",
  "H2W",
  o_place == "school" &
    d_place == "home",
  "S2H",
  o_place == "home" &
    d_place == "school",
  "H2S",
  o_place == "school" &
    d_place == "work",
  "S2W",
  o_place == "work" &
    d_place == "school",
  "W2S",
  o_place == "other" &
    d_place == "other",
  "OaA",
  o_place == "school" &
    d_place == "school",
  "S2S",
  o_place == "work" &
    d_place == "work",
  "W2W"
)]


## EL trip types (custom) ####
svy_2022[, trip_type_custom := fcase(
  trip_type %in% c("H2W", "W2H","W2W","S2W", "W2S"),
  "Work commute",
  trip_type %in% c("H2S", "S2H","S2S"),
  "School commute",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Shopping / errand / other appointment (e.g. haircut)",
  "Errands / shopping",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Friend's house/Social visit/religious/community activity",
  "Social / Community",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Medical appointment (doctor, clinic, hospital) non-work",
  "Medical",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Dining out / getting coffee, or take-out",
  "Dining / take-out",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Airport (passengers only)",
  "Airport",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") & 
    DESTIN_PLACE_TYPE %in% c(
      "Sporting or Special Event",
      "Other" ),
  "Special event",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Shopping / errand / other appointment (e.g. haircut)",
  "Errands / shopping",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Friend's house/Social visit/religious/community activity",
  "Social / Community",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Medical appointment (doctor, clinic, hospital) non-work",
  "Medical",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Dining out / getting coffee, or take-out",
  "Dining / take-out",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Airport (passengers only)",
  "Airport",
  trip_type %in% c("OBH", "OBW", "OBS") & 
    ORIGIN_PLACE_TYPE %in% c(
      "Sporting or Special Event",
      "Other" ),
  "Special event"
)]

svy_2022[is.na(trip_type_custom), .N, .(trip_type)][order(-N)]
svy_2022[, .N, trip_type_custom][order(-N), .(trip_type_custom, N / sum(N))]
svy_2022[is.na(trip_type_custom), .N, .(ORIGIN_PLACE_TYPE, DESTIN_PLACE_TYPE, trip_type)][order(-N)]
#svy_2022[is.na(trip_type_custom), trip_type_custom := "Social / Community"]

## 2016
svy_2016[, .N, .(ORIGIN_PLACE_TYPE)][order(-N)]
svy_2016[, .N, TIME_PERIOD]
svy_2016[, trip_type_custom := fcase(
  trip_type %in% c("H2W", "W2H","W2W","S2W", "W2S"),
  "Work commute",
  trip_type %in% c("H2S", "S2H","S2S"),
  "School commute",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Shopping",
  "Errands / shopping",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE ==
      "Social Visit / Community / Religious / Personal",
  "Social / Community",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Doctor / Clinic / Hospital (non-work)",
  "Medical",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Recreation / Sightseeing / Restaurant",
  "Dining / take-out",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") &
    DESTIN_PLACE_TYPE == "Airport (passengers only)",
  "Airport",
  trip_type %in% c("OaA", "HBO", "WBO", "SBO") & 
    DESTIN_PLACE_TYPE %in% c( "Sporting or Special Event",
                              "Hotel",
                              "Other"),
  "Special event",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Shopping",
  "Errands / shopping",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE ==
    "Social Visit / Community / Religious / Personal",
  "Social / Community",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Doctor / Clinic / Hospital (non-work)",
  "Medical",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Recreation / Sightseeing / Restaurant",
  "Dining / take-out",
  trip_type %in% c("OBH", "OBW", "OBS") &
    ORIGIN_PLACE_TYPE == "Airport (passengers only)",
  "Airport",
  trip_type %in% c("OBH", "OBW", "OBS") & 
    ORIGIN_PLACE_TYPE %in% c( "Sporting or Special Event",
                              "Hotel",
                              "Other"),
  "Special event"
)]
svy_2016[is.na(trip_type_custom), .N, .(trip_type)][order(-N)]
svy_2016[, .N, trip_type_custom][order(-N), .(trip_type_custom, N / sum(N))]
svy_2016[is.na(trip_type_custom), .N, .(ORIGIN_PLACE_TYPE, DESTIN_PLACE_TYPE, trip_type)][order(-N)]
svy_2016[is.na(trip_type_custom), trip_type_custom := "Special event"]

svy_2016[,trip_type_custom:= factor(trip_type_custom, levels = c(
  "Work commute",
  "School commute", "Errands / shopping", "Social / Community",
  "Medical", "Dining / take-out", "Airport", "Special event"
))]

svy_2022[,trip_type_custom:= factor(trip_type_custom, levels = c(
  "Work commute",
  "School commute", "Errands / shopping", "Social / Community",
  "Medical", "Dining / take-out", "Airport", "Special event"
))]

rm(dplace_lut16); rm(dplace_lut21); rm(place_lut16); rm(place_lut21)

message(paste0("New column: trip_type_custom in svy_2016 and svy_2022 with levels: ", "\n",
               paste0(levels(svy_2016$trip_type_custom), collapse = "\n")))
