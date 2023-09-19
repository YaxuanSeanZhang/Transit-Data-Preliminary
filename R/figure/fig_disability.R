toplot_w <- svy_2022[, .(`2022` = sum(Final_unlinked_weight_fctr)), 
                     keyby = .(value = disability_v2)]
toplot_w <- toplot_w[value %in% c('Yes','No')]
toplot_w[, frac := `2022` / sum(`2022`)]
toplot_w <- toplot_w[disability,on=c(value = "disability")]

fig <- plot_ly(data = toplot_w) %>%
  add_trace(x = ~value, y = ~frac_pop, 
            type = "bar", name = 'General Population',
            marker = list(color = "#d9e4e4"),
            text = ~scales::percent(frac_pop, accuracy = 1L),
            textposition = "outside") %>%
  add_trace(x = ~value, y = ~frac, 
            type = "bar", name = 'Transit User',
            marker = list(color = "#0054A4"),
            text = ~scales::percent(frac, accuracy = 1L),
            textposition = "outside") %>%
  layout(
    xaxis = list(
      title=list(text='Do you have disbility?', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(
      title = "Percentage",
      tickformat = ".0%",
      tickvals = seq(0, 0.92, by = 0.1),
      tickmode = "array"
    ),
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 60),
    annotations = list(
      x = ~value,
      y = ~frac/2,
      xref = "x",
      xshift = 35,
      font = list(color = 'white',orientation = 'h'),
      text = ~paste(prettyNum(round(`2022`,0),big.mark=","),'\ntrips'),
      showarrow = F,
      textangle = 270
    ),
    plot_bgcolor = "white",
    width = 600, height = 500
  )
