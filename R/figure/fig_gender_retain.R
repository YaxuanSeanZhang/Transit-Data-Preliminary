toplot <- get_onewaytab("Gender")
toplot_w <- dcast(toplot, value ~ survey, value.var = "trips")
toplot_w[, frac := (`2022`) / `2016`]

fig <- plot_ly(data = toplot_w[!value == "Prefer not to say" & 
                                 !value == "Other Gender"], 
               x = ~value, y = ~`2016`, 
               type = "bar", 
               name = "2016", 
               marker = list(color = "#d2e9ff")
) %>%
  add_trace(y = ~`2022`, name = "2022", 
            marker = list(color = "#0054A4"),
            text = ~paste0(scales::percent(frac, accuracy = 1L),'\nretain'),
            textposition = "outside") %>%
  layout(
    xaxis = list(
      title=list(text='Gender', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(title = list(
      text = "Total trips per day", 
      font = list(size = 16)), 
      rangemode = "tozero",
      range = c(0,170000),
      tickfont = list(size = 14)),
    legend = list(title=list(text='Year', 
                             font = list(size = 15))),
    #barmode = 'overlay',
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 60),
    plot_bgcolor = "white",
    width = 500, height = 500
  )
