toplot <- svy_2022[, .(trips = sum(Final_unlinked_weight_fctr)), 
                   keyby = .(gender = Gender, timefac = timefac)]

toplot <- toplot[!is.na(gender) & !gender == "Prefer not to say" & 
                   !timefac %in% c("Before 6a", "After 9p"), ]

toplot[, frac := trips / sum(trips), .(gender)]

toplot <- dcast(toplot, timefac ~ gender, value.var = "frac")

fig =
  plot_ly(toplot,
          x = ~timefac,
          y = ~Female,
          name = 'Female',
          type = 'scatter',
          mode = 'bar',
          line = list(color = "#49621C"),
          marker = list(color = "#49621C")) %>%
  add_trace(y = ~Male, name = 'Male', mode = 'lines+markers',
            line = list(color = "#6C297D"),
            marker = list(color = "#6C297D"))%>%
  add_trace(y = ~`Other Gender`, name = 'Other Gender', mode = 'lines+markers',
            line = list(color = "#2C88D4"),
            marker = list(color = "#2C88D4"))%>%
  layout(
    xaxis = list(
      title = "Time of the day"
    ),
    yaxis = list(
      title = "Percent of trips",
      tickformat = ".00%",
      tickvals = seq(0, 0.1, by = 0.01),
      tickmode = "array"
    ),
    legend = list(title=list(text='Gender')),
    margin = list(l = 10, r = 10, t = 30, b = 60),
    font = list(family = "Arial", size = 14),
    width = 1000, height = 500
  )
