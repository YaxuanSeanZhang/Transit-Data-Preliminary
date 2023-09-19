toplot <- get_twowaytab("Gender", "employment_v3")

toplot <- toplot[!is.na(value) & 
                   value %in% c("Female", "Male") &
                   value2 != 'Prefer not to answer']

toplot_w <- dcast(toplot, value + value2 ~ survey, value.var = "trips")
toplot_w[, frac := (`2022`) / `2016`]

toplot_w$value2 = stringr::str_wrap(as.character(toplot_w$value2),12)

toplot_w$value2 = factor(toplot_w$value2, levels=unique(toplot_w$value2))

p1 = plot_ly(data = toplot_w[value == "Female"], 
             x = ~value2, y = ~`2016`, 
             type = "bar", 
             name = "2016", 
             marker = list(color = "#e0c9e1")) %>%
  add_trace(y = ~`2022`, name = "2022", 
            marker = list(color = "#643967"),
            text = ~paste0(scales::percent(frac, accuracy = 1L),'\nretain'), 
            textposition = "outside") %>%
  layout(
    xaxis = list(title = list(text = "Employment", standoff = 10)),
    yaxis = list(title = "Total trips per day", showgrid = FALSE, zeroline = FALSE, 
                 rangemode = "tozero",
                 range = c(0,87000)),
    legend = list(orientation = "h", x = 0.3, y = -0.22),
    showlegend = TRUE,
    font = list(family = "Arial",size =14),
    margin = list(l = 60, r = 10, t = 60, b = 120),
    plot_bgcolor = "white"
  )

p2 = plot_ly(data = toplot_w[value == "Male"], 
             x = ~value2, y = ~`2016`, 
             type = "bar", 
             name = "2016", 
             marker = list(color = "#d2e9ff")) %>%
  add_trace(y = ~`2022`, name = "2022", 
            marker = list(color = "#0054A4"),
            text = ~paste0(scales::percent(frac, accuracy = 1L),'\nretain'), 
            textposition = "outside") %>%
  layout(
    xaxis = list(title = list(text = "Employment", standoff = 10)),
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, 
                 rangemode = "tozero",
                 range = c(0,87000)),
    legend = list(orientation = "h", x = 0.3, y = -0.22),
    showlegend = TRUE,
    font = list(family = "Arial"),
    margin = list(l = 10, r = 10, t = 30, b = 120),
    plot_bgcolor = "white"
  )

fig <- subplot(p1,p2, nrows = 1, shareX=T, shareY=T,titleY = TRUE, titleX = T)

annotations = list( 
  list(
    x = 0.2,  
    y = 1.0,  
    text = "Female", 
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size =16)
  ),  
  list(
    x = 0.8,  
    y = 1,
    text = "Male",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size =16)
  ))

fig <- fig %>%layout(uniformtext=list(minsize=13, mode='show'),
                     annotations = annotations,
                     width = 1000, height = 500) 