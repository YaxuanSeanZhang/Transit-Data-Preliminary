toplot <- svy_2022[, .(trips = sum(Final_unlinked_weight_fctr)), 
                   keyby = .(drive = drive, triptype = trip_type_custom)]

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

toplot[, frac := trips / sum(trips), .(triptype)]

toplot <- dcast(toplot, triptype ~ drive, value.var = "frac")

toplot$triptype = stringr::str_wrap(as.character(toplot$triptype),10)

toplot$triptype = factor(toplot$triptype, levels=unique(toplot$triptype))

fig <- plot_ly(data = toplot, 
               x = ~triptype, y = ~`Don't drive, don't have household vehicle`,
               type = "bar", 
               name = "Don't drive, don't have household vehicle", 
               marker = list(color = "#6C297D")
) %>%
  add_trace(y = ~`Don't drive, have household vehicle`, 
            name = "Don't drive, have household vehicle", 
            marker = list(color = "#BB7FC9")) %>%
  add_trace(y = ~`Drive, but could not have made this trip by car`, 
            name = "Drive, but could not have made this trip by car", 
            marker = list(color = "#378B9F")) %>%
  add_trace(y = ~`Drive, and could have made this trip by car`, 
            name = "Drive, and could have made this trip by car", 
            marker = list(color = "#49621C")) %>%
  layout(
    xaxis = list(
      title=list(text='Trip Type', 
                 font = list(size = 16), standoff = 10)),
    yaxis = list(
      title = "Percent of trips",
      tickformat = ".00%",
      tickvals = seq(0, 0.85, by = 0.1),
      tickmode = "array"),
    legend = list(title=list(text='Can you Drive?', 
                             font = list(size = 15)),
                  xanchor = "left",x = 0,
                  yanchor = "bottom",y = -0.5),
    font = list(family = "Arial", size = 14),
    margin = list(l = 10, r = 10, t = 30, b = 120),
    plot_bgcolor = "white",
    width = 1000, height = 500
  )