toplot_w <- svy_2022[, .(`2022` = sum(Final_unlinked_weight_fctr)), 
                     keyby = .(value = race_ethnicity)]
toplot_w <- toplot_w[!value == 'Prefer not to answer' & !value == 'Other']
toplot_w[, frac := `2022` / sum(`2022`)]
toplot_w <- toplot_w[race,on=c(value = "race")]

toplot_w$value = stringr::str_wrap(as.character(toplot_w$value),10)

toplot_w$value = factor(toplot_w$value, levels=unique(toplot_w$value))

fig <- plot_ly(data = toplot_w) %>%
  add_trace(x = ~frac, y = ~value, 
            type = "bar", name = 'Transit User',
            marker = list(color = "#0054A4"),
            text = ~scales::percent(frac, accuracy = 1L),
            textposition = "outside") %>%
  add_trace(x = ~frac_pop, y = ~value, 
            type = "bar", name = 'General Population',
            marker = list(color = "#d9e4e4"),
            text = ~scales::percent(frac_pop, accuracy = 1L),
            textposition = "outside") %>%
  layout(
    yaxis = list(
      title=list(text='Race', 
                 font = list(size = 16), standoff = 10)),
    xaxis = list(
      title = "Percentage",
      tickformat = ".0%",
      tickvals = seq(0, 0.72, by = 0.1),
      tickmode = "array"
    ),
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 60),
    plot_bgcolor = "white",
    width = 1000, height = 650
  )
