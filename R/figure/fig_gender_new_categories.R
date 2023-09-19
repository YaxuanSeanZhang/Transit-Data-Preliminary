toplot <- svy_2022[, .(trips = length(ID)), 
                   keyby = .(age = age_v2, gender = YOUR_GENDER)]

toplot <- toplot[!(gender %in% c("Male", "Female")) &
                   gender != "Prefer not to say" &
                   age != 'Prefer not to answer' &
                   !is.na(age)]

new_row <-  data.table("age" = c('55-64','Over 65','Over 65'),
                       "gender" = c('Other / Prefer to self-describe',
                                    'Non-binary / third gender',
                                    'Transgender'), 
                       "trips" = c(0,0,0))
toplot <- rbindlist(list(toplot, new_row)) 

toplot <- dcast(toplot, age ~ gender, value.var = "trips")

fig <- plot_ly(data = toplot, 
               x = ~age, y = ~`Non-binary / third gender`,
               name = 'Non-binary/ third gender',
               type = "bar",
               marker = list(color = "#A14771")
) %>%
  add_trace(y = ~`Multiple Gender`, name = 'Multiple Gender',
            marker = list(color = "#378B9F")) %>%
  add_trace(y = ~`Other / Prefer to self-describe`,
            name = 'Other/ Prefer to self-describe',
            marker = list(color = "#6C297D")) %>%
  add_trace(y = ~`Transgender`, name = 'Transgender',
            marker = list(color = "#49621C")) %>%
  layout(
    xaxis = list(
      title=list(text='Age', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(title = list(
      text = "Number of Responses", 
      font = list(size = 16)), 
      rangemode = "tozero",
      tickfont = list(size = 14)),
    legend = list(title=list(text='New Gender Category', 
                             font = list(size = 15))),
    font = list(family = "Arial", size = 14),
    bargap = 0.2,
    margin = list(l = 10, r = 10, t = 30, b = 60),
    plot_bgcolor = "white",
    width = 1000, height = 500
  )