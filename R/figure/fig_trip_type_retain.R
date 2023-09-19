toplot <- get_onewaytab("trip_type_custom")
toplot[, frac := trips / sum(trips), survey]

# calculate retained:
toplot_w <- dcast(toplot, value ~ survey, value.var = "trips")
toplot_w[, frac := (`2022`) / `2016`]

# get rid of some rare trip types:
toplot_w <- toplot_w[!toplot_w$value == "Special event"]

toplot_w$value = stringr::str_wrap(as.character(toplot_w$value),10)

toplot_w$value = factor(toplot_w$value, levels=unique(toplot_w$value))

fig <- plot_ly(data = toplot_w[!value == "Refused/No Answer"], 
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
      title=list(text='Trip Type', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(title = list(
      text = "Total trips per day", 
      font = list(size = 16)), 
      rangemode = "tozero",
      range = c(0,150000),
      tickfont = list(size = 14)),
    legend = list(title=list(text='Year', 
                             font = list(size = 15))),
    font = list(family = "Arial", size = 14),
    uniformtext=list(minsize=13, mode='show'),
    margin = list(l = 10, r = 10, t = 30, b = 120),
    plot_bgcolor = "white",
    width = 1000, height = 500
  )
