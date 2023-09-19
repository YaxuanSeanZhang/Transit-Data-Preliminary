# By Time of day --------------
## tally and plot
svy_2022[, TIME2 := fcase(
  TIME_ON == "6:00 am - 7:00 am", "6a",
  TIME_ON == "8:00 am - 9:00 am", "8a",
  TIME_ON == "7:00 am - 8:00 am", "7a",
  TIME_ON == "9:01 am - 10:00 am", "9a",
  TIME_ON == "2:00 pm - 3:00 pm", "2p",
  TIME_ON == "10:00 am - 11:00 am", "10a",
  TIME_ON == "11:00 am - 12:00 pm", "11a",
  TIME_ON == "1:00 pm - 2:00 pm", "1p",
  TIME_ON == "12:00 pm - 1:00 pm", "12p",
  TIME_ON == "5:00 pm - 6:00 pm", "5p",
  TIME_ON == "3:01 pm - 4:00 pm", "3p",
  TIME_ON == "4:00 pm - 5:00 pm", "4p",
  TIME_ON == "6:00 pm - 6:30 pm", "6p",
  TIME_ON == "6:31 pm - 7:00 pm", "6p",
  TIME_ON == "7:00 pm - 8:00 pm", "7p",
  TIME_ON == "8:00 pm - 9:00 pm", "8p",
  TIME_ON == "After 9:00 pm", "After 9p",
  TIME_ON == "Before 6:00 am", "Before 6a"
)]

svy_2016[, TIME2 := fcase(
  TIME_ON == "12:00 pm - 1:00 pm", "12p",
  TIME_ON == "3:00 - 4:00 pm", "3p",
  TIME_ON == "6:00 - 7:00 am", "6a",
  TIME_ON == "9:00 - 10:00 am", "9a",
  TIME_ON == "10:00 - 11:00 am", "10a",
  TIME_ON == "7:00 - 8:00 am", "7a",
  TIME_ON == "6:30 - 7:00 pm", "6p",
  TIME_ON == "11:00 am - 12:00 pm", "11a",
  TIME_ON == "Before 6:00 am", "Before 6a",
  TIME_ON == "4:00 - 5:00 pm", "4p",
  TIME_ON == "11:00 - 12:00 pm", "11a",
  TIME_ON == "7:00 - 8:00 pm", "7p",
  TIME_ON == "5:00 - 6:00 pm", "5p",
  TIME_ON == "8:00 - 9:00 am", "8a",
  TIME_ON == "8:00 - 9:00 pm", "8p",
  TIME_ON == "1:00 - 2:00 pm", "1p",
  TIME_ON == "2:00 - 3:00 pm", "2p",
  TIME_ON == "6:00 - 6:30 pm", "6p",
  TIME_ON == "After 9:00 pm", "After 9p"
)]

svy_2016[, timefac := factor(
  TIME2,
  levels = c(
    "Before 6a", "6a", "7a", "8a", "9a", "10a", "11a", "12p",
    "1p", "2p", "3p", "4p",
    "5p", "6p", "7p", "8p",
    "After 9p"
  ),
  ordered = TRUE
)]

svy_2022[, timefac := factor(
  TIME2,
  levels = c(
    "Before 6a", "6a", "7a", "8a", "9a", "10a", "11a", "12p",
    "1p", "2p", "3p", "4p",
    "5p", "6p", "7p", "8p",
    "After 9p"
  ),
  ordered = TRUE
)]

