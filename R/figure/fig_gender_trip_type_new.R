toplot <- get_twowaytab("Gender", "trip_type_custom")

# Filter prefer not to answer:
toplot <- toplot[value %in% c("Male", "Female", 'Other Gender'), ]

# get rid of some rare trip values:
toplot <- toplot[!toplot$value2 == "Special event"]

# arrange factor levels of trip type: 
toplot[, value2 := factor(
  value2,
  levels = c(
    "Work commute",
    "School commute", "Errands / shopping", "Social / Community",
    "Medical", "Dining / take-out", "Airport"
  ),
  ordered = TRUE
)]

toplot[, frac := trips / sum(trips), .(survey, value)]

colors = c("#78A22F", "#378B9F", "#ED6F65",
           "#2C88D4","#003E77", "#BB7FC9", "#6C297D")

p1 = toplot[survey == '2022' & value =='Female'] %>%
  plot_ly(.,
          x = ~value,
          y = ~frac,
          color = ~value2,
          type = "bar",
          width = 0.8,
          legendgroup = ~value2,
          colors = colors)

p2 = toplot[survey == '2022' & value =='Male'] %>%
  plot_ly(.,
          x = ~value,
          y = ~frac,
          color = ~value2,
          type = "bar",
          width = 0.8,
          legendgroup = ~value2,
          showlegend = F,
          colors = colors)

p3 = toplot[survey == '2022' & value =='Other Gender'] %>%
  plot_ly(.,
          x = ~value,
          y = ~frac,
          color = ~value2,
          type = "bar",
          width = 0.8,
          legendgroup = ~value2,
          showlegend = F,
          colors = colors)

fig <- subplot(p1,p2,p3, nrows = 1, shareX = T,shareY = T,titleY = F, titleX = FALSE) 

fig <- fig %>%
  layout(
    yaxis = list(
      title = "Percent of trips",
      tickformat = ".00%",
      tickvals = seq(0, 0.4, by = 0.05),
      tickmode = "array"
    ),
    legend = list(title=list(text='Trip Type')),
    margin = list(l = 10, r = 10, t = 30, b = 60),
    showlegend = TRUE,
    font = list(family = "Arial", size = 14),
    width = 1000, height = 500
  )