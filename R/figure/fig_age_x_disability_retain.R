toplot <- get_twowaytab("disability_v2", "age_v2")

toplot <- toplot[!value == "Prefer not to say" & 
                   !is.na(value2) &
                   !value2 == 'Prefer not to answer']

toplot_w <- dcast(toplot, value + value2 ~ survey, value.var = "trips")
toplot_w[, frac := (`2022`) / `2016`]

p1 = plot_ly(data = toplot_w[value == "Yes"], 
             x = ~value2, y = ~`2016`, 
             type = "bar", 
             name = "2016", 
             marker = list(color = "#e0c9e1")) %>%
  add_trace(y = ~`2022`, name = "2022", 
            marker = list(color = "#643967")) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = ~value2, y = ~`2022` + 400, xshift =15,
                  text =  ~paste0(scales::percent(frac, accuracy = 1L),'\nretain'), 
                  showarrow = FALSE) %>%
  layout(
    xaxis = list(title = list(text = "Age", standoff = 10)),
    yaxis = list(title = "Total trips per day", showgrid = FALSE, zeroline = FALSE, 
                 rangemode = "tozero",
                 range = c(0,8000)),
    legend = list(orientation = "h", x = 0.3, y = -0.18),
    showlegend = TRUE,
    font = list(family = "Arial", size =14),
    plot_bgcolor = "white"
  )

p2 = plot_ly(data = toplot_w[value == "No"], 
             x = ~value2, y = ~`2016`, 
             type = "bar", 
             name = "2016", 
             marker = list(color = "#d2e9ff")) %>%
  add_trace(y = ~`2022`, name = "2022", 
            marker = list(color = "#0054A4")) %>%
  add_annotations(xref = 'x', yref = 'y',
                  x = ~value2, y = ~`2022`+4000,xshift =15,
                  text =  ~paste0(scales::percent(frac, accuracy = 1L),'\nretain'), 
                  showarrow = FALSE) %>%
  layout(
    xaxis = list(title = list(text = "Age", standoff = 10)),
    yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, 
                 rangemode = "tozero",
                 range = c(0,80000)),
    legend = list(orientation = "h", x = 0.3, y = -0.18),
    showlegend = TRUE,
    font = list(family = "Arial", size = 14),
    plot_bgcolor = "white"
  )

fig <- subplot(p1,p2, nrows = 1, shareX=T,titleY = TRUE, titleX = T)

annotations = list( 
  list(
    x = 0.2,  
    y = 1.0,  
    text = "With Disability", 
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
    text = "Without Disability",
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size =16)
  ))

fig <- fig %>%layout(uniformtext=list(minsize=13, mode='show'),
                     annotations = annotations,
                     margin = list(l = 10, r = 10, t = 30, b = 120),
                     width = 1000, height = 500) 
