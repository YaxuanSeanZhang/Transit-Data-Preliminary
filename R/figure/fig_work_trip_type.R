toplot <- svy_2022[, .(trips = sum(Final_unlinked_weight_fctr)), 
                   keyby = .(work = WORK_LOCATION, triptype = trip_type_custom)]

# get rid of some rare trip values:
toplot <- toplot[!toplot$triptype == "Special event"]

# arrange factor levels of trip type: 
toplot[, triptype := factor(
  triptype,
  levels = c(
    "Work commute",
    "School commute", "Errands / shopping", "Social / Community",
    "Medical", "Dining / take-out", "Airport"
  ),
  ordered = TRUE
)]

toplot[, frac := trips / sum(trips), .(work)]

toplot <- dcast(toplot,  work ~ triptype, value.var = "frac")

toplot <- toplot[!work=='Refused/No Answer']

toplot$work = stringr::str_wrap(as.character(toplot$work),20)

toplot$work = factor(toplot$work, levels=unique(toplot$work))

fig <- plot_ly(data = toplot, 
               x = ~work, y = ~`Work commute`,
               type = "bar", 
               name = "Work commute", 
               marker = list(color = "#78A22F")
) %>%
  add_trace(y = ~`School commute`, 
            name = "School commute", 
            marker = list(color = "#378B9F")) %>%
  add_trace(y = ~`Errands / shopping`, 
            name = "Errands / shopping", 
            marker = list(color = "#ED6F65")) %>%
  add_trace(y = ~`Social / Community`, 
            name = "Social / Community", 
            marker = list(color = "#2C88D4")) %>%
  add_trace(y = ~`Medical`, 
            name = "Medical", 
            marker = list(color = "#003E77")) %>%
  add_trace(y = ~`Dining / take-out`, 
            name = "Dining / take-out", 
            marker = list(color = "#BB7FC9")) %>%
  add_trace(y = ~`Airport`, 
            name = "Airport", 
            marker = list(color = "#6C297D")) %>%
  layout(
    xaxis = list(
      title=list(text='Work Type', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(
      title = "Percent of trips",
      tickformat = ".00%",
      tickvals = seq(0, 0.65, by = 0.1),
      tickmode = "array"),
    legend = list(title=list(text='Trip Type', 
                             font = list(size = 15))),
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 120),
    plot_bgcolor = "white",
    width = 1000, height = 500
  )
