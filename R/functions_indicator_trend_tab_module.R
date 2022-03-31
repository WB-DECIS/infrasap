## Buttons for downloading the data

download_bttns <- function(id, lab){
  shinyWidgets::downloadBttn(
    outputId = id,
    label = lab,
    style = "gradient",
    color = "primary",
    # size = "md",
    block = FALSE,
    no_outline = TRUE
  )  
}


## Function for displaying a table and graph when no comparison is needed.

no_comparison_output <- function(dat, sector, selection, min_year_reactive, max_year_reactive){
  
  if(!is.null(dat)){
 
  ## Create a dataset that only contains data for the country selected
  if(sector != "ports"){
  df1 <- dat %>% 
    dplyr::select(`Country Name`, `Indicator Name`, Year, Value)
  }else{
  df1 <- dat %>% 
    dplyr::select(`Country Name`, `Indicator Name`, `Sub-national Unit Name`,  Year, Value)
  } 
  
  ## The years to be displayed will depend on the option chosen in the "Type of selection" tab
  if(selection == "Select a range of years"){
    df2 <- df1 %>% dplyr::filter(Year >= min_year_reactive & Year <= max_year_reactive)
  }else{
    df2 <- df1 %>% dplyr::filter(Year == max(Year, na.rm = TRUE))
  }

  tooltip_text <-  paste('Country: ', df2$`Country Name`, "\n",
                         'Year: ', as.character(df2$Year),"\n",
                         'Value: ', round(df2$Value, 6))
  
  ## Generate the plot
  ### If the years displayed are less than 4, we will display bar charts otherwise line charts
  bar_width <- ifelse(length(unique(df2$Year)) == 1 , 0.1,
                      ifelse( dplyr::between(length(unique(df2$Year)), 2, 3) , 0.2, 0.6))
  

  if(length(unique(df2$Year)) <= 6){
    plot1 <- ggplot2::ggplot(data = df2, ggplot2::aes(x = as.factor(Year), y = Value, text = tooltip_text))+
      ggplot2::geom_bar(stat = "identity", position = "dodge", fill = "#4682B4", width = bar_width)

  }else{
    plot1 <- ggplot2::ggplot(data = df2, ggplot2::aes(x = Year, y = Value, group = 1, text = tooltip_text))+
      ggplot2::geom_point(color = "#000000", size = 2) + 
      ggplot2::geom_line(color = "#4682B4")
  }
  
  plot2 <- plot1 +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, colour = "#28313d"),
                   axis.title.y = ggplot2::element_text(size = 10, colour = "#28313d"),
                   axis.title.x = ggplot2::element_text(size = 10, colour = "#28313d"),
                   axis.text = ggplot2::element_text(size = 11, colour = "#28313d"),
                   plot.title = ggplot2::element_text(colour = "#28313d"),
                   axis.ticks = ggplot2::element_line(colour = "#ebebeb"))
                   
  if(sector != "ports"){
    plot2 <- plot2 +
      labs(x = "", y = "" , title = unique(df2$`Indicator Name`), 
           subtitle = paste0("( ",unique(df2$`Country Name`) , " )"))
  }else{
    plot2 <- plot2 +
      labs(x = "", y = "" , title = unique(df2$`Indicator Name`), 
           subtitle = paste0("( ",unique(df2$`Country Name`), " : " , unique(df2$`Sub-national Unit Name`) , " )"))
  }
  
  ## Plotly version of the output 
  if(sector != "ports"){
    fig <- plotly::ggplotly(plot2, tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(title = list(text = paste0(unique(df2$`Indicator Name`) ,
                                                '<br>',
                                                '<sup >',
                                                paste0("( ", unique(df2$`Country Name`), " )"),
                                                '</sup>')
      )
      )
  }else{
    fig <- plotly::ggplotly(plot2, tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(title = list(text = paste0(unique(df2$`Indicator Name`) ,
                                                '<br>',
                                                '<sup >',
                                          paste0("( ",unique(df2$`Country Name`), " : " , unique(df2$`Sub-national Unit Name`) , " )"),
                                                '</sup>')
      )
      )
  }

  ## Table
  tab <- df2 %>% 
    tidyr::spread(Year, Value)
  
  colnames(tab) <- trimws(gsub("Name", "", colnames(tab)))
  
  }else{
    plot2 <- NULL 
    fig <- NULL
    tab <- NULL
  }
  output <- list(plot2, fig, tab)
  
  return(output)
  
}
