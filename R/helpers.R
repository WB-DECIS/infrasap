empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers                                                                                                                                                                        ") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = list(
        text = title,
        yref = "paper",
        y = 0.5
      )
    )
  return(p)
} 