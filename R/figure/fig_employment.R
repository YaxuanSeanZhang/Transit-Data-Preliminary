toplot_w <- svy_2022[, .(`2022` = sum(Final_unlinked_weight_fctr)), 
                     keyby = .(value = employment_v2)]
toplot_w[, frac := `2022` / sum(`2022`)]

toplot_w$value = stringr::str_wrap(as.character(toplot_w$value),15)

toplot_w$value = factor(toplot_w$value, levels=unique(toplot_w$value))

fig <- plot_ly(data = toplot_w, 
               x = ~value, y = ~`2022`, 
               type = "bar", 
               marker = list(color = "#0054A4"),
               text = ~scales::percent(frac, accuracy = 1L),
               textposition = "outside") %>%
  layout(
    xaxis = list(
      title=list(text='Employment Status', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(title = list(
      text = "Total trips per day", 
      font = list(size = 16)), 
      rangemode = "tozero",
      range = c(0,75000),
      tickfont = list(size = 14)),
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 120),
    plot_bgcolor = "white",
    width = 800, height = 500
  )