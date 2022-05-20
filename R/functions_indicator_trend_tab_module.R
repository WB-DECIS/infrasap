
cols_tt <- c( "#002244", "#009FDA","#8B3A3A", "#4682B4", "#B88C1D",  "#277ec3",  "#98252B", "#B88C1D", "#006450", "#174463" ,"#34b3d9")

# tt_theme <- ggplot2::theme_classic() +
#   ggplot2::theme(
#     axis.title = ggplot2::element_text(color = "grey40", size = 16, margin = ggplot2::margin(t = 0, r = 25, b = 0, l = 0)),
#     axis.text = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 5))
#     )
# tt_theme <- ggplot2::theme_classic() +
#   ggplot2::theme(
#     axis.title.y = ggplot2::element_text(color = "grey40", size = 16, margin = ggplot2::margin(t = 0, r = 25, b = 0, l = 0)),
#     axis.text = ggplot2::element_text(color = "grey40"),
#     axis.text.x = ggplot2::element_text(size = 14, margin = ggplot2::margin(t = 5)),
#     axis.text.y = ggplot2::element_text(size = 14, margin = ggplot2::margin(r = 5)),
#     axis.ticks = ggplot2::element_line(color = "grey91"),
#     axis.line.x = ggplot2::element_line(size = 1.5, color = "grey10"),
#     axis.line.y = ggplot2::element_blank(),
#     panel.grid = ggplot2::element_blank(),
#     panel.grid.major.y = ggplot2::element_line(color = "grey91",size = 0.5, linetype = 2),
#     plot.margin = ggplot2::margin(20, 40, 20, 40),
#     plot.background = ggplot2::element_rect(fill = "white", color = NA),
#     panel.background = ggplot2::element_rect(fill = NA, color = NA),
#     plot.title = ggplot2::element_text(color = "grey10", size = 24, face = "bold",
#                                        margin = ggplot2::margin(t = 15)),
#     plot.subtitle = ggplot2::element_text(color = "grey30", size = 18, 
#                                           lineheight = 1.35,
#                                           margin = ggplot2::margin(t = 15, b = 40)),
#     plot.title.position = "plot",
#     plot.caption.position = "plot",
#     plot.caption = ggplot2::element_text(color = "grey30", size = 15,
#                                          lineheight = 1.2, hjust = 0, 
#                                          margin = ggplot2::margin(t = 40)),
#     legend.position = "bottom",
#     legend.text= ggplot2::element_text(size = 12, color = "grey40"),
#     legend.title = ggplot2::element_blank()
#   )

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
## Function for generating the dataset
data_out <- function(cname, sect, ind, l1, l2, l3){
  
  if(!grepl('Transport Port', sect)) {
    if(l1 %in% "None"){ ## 1
      out <- dat_appended %>% 
        dplyr::filter(`Country Name` %in% cname & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>% 
        select(`Country Name`, `Indicator Name`, Year, Value)
    }else{
      
      if(grepl('countries', l1)){ ## 2
        out <- dat_appended %>% 
          dplyr::filter(`Country Name` %in% c(cname, l2) & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>% 
          dplyr::filter(Year %in% Year[`Country Name` %in% cname]) %>% 
          dplyr::select(`Country Name`, `Indicator Name`, Year, Value) %>% 
          dplyr::rename(Grouping = `Country Name`) %>% 
          dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
          dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, cname))
      }else{
        
        if(grepl('indicators', l1)){ ## 3
          out <- dat_appended %>% 
            dplyr::filter(`Country Name` %in% cname & `Indicator Sector` %in% sect & `Indicator Name` %in% c(ind, l2)) %>% 
            dplyr::filter(Year %in% Year[`Indicator Name` %in% ind]) %>% 
            select(`Country Name`, `Indicator Name`, Year, Value) %>% 
            rename(Grouping = `Indicator Name`) %>% 
            dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
            dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, ind))
        }else{
          
          if(grepl('benchmark', l1)){ 
            if(grepl('Region', l2)){ ## 4
              out <- dat_appended %>% 
                dplyr::filter((`Country Name` %in% cname | Grouping %in% l3)  & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>% 
                dplyr::filter(Year %in% Year[`Country Name` %in% cname]) %>% 
                dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Country Name`, Grouping))%>% 
                dplyr::select(Grouping, `Indicator Name`, Year, Value) %>% 
                dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
                dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, cname))
            }else{
              if(grepl('Income', l2)){ ## 5
                out <- dat_appended %>% 
                  dplyr::filter((`Country Name` %in% cname | Grouping %in% l3)  & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>% 
                  dplyr::filter(Year %in% Year[`Country Name` %in% cname]) %>% 
                  dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Country Name`, Grouping))%>% 
                  dplyr::select(Grouping, `Indicator Name`, Year, Value) %>% 
                  dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
                  dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, cname))
              }
            }
          }
        }
      }
    }
  }else{
    
    if(grepl('Transport Port', sect)) {
      if(l2 %in% "None"){ ## 6
        out <- dat_ports_appended %>%
          dplyr::filter(`Country Name` %in% cname & `Indicator Name` %in% ind) %>%
          dplyr::filter(`Sub-national Unit Name` %in% l1) %>% 
          dplyr::select(`Country Name`, `Indicator Name`, `Sub-national Unit Name`, Year, Value)
      }else{
        
        if(grepl('Other ports', l2)){ ## 7
          out <- dat_ports_appended %>%
            dplyr::filter(`Country Name` %in% cname & `Indicator Name` %in% ind) %>%
            dplyr::filter(`Sub-national Unit Name` %in% c(l1, l3)) %>%
            dplyr::filter(Year %in% Year[`Sub-national Unit Name` %in% l1])%>% 
            dplyr::select(`Country Name`, `Indicator Name`, `Sub-national Unit Name`, Year, Value) %>% 
            dplyr::rename(Grouping = `Sub-national Unit Name`)%>% 
            dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
            dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, l1))
        }else{
          if(grepl('Regional', l2)){ ## 8
            out <- dat_ports_appended %>%
              dplyr::filter((`Country Name` %in% cname | Grouping %in% l3) & `Indicator Name` %in% ind) %>%
              dplyr::filter(`Sub-national Unit Name` %in% l1 | Grouping %in% l3) %>%
              dplyr::filter(Year %in% Year[`Sub-national Unit Name` %in% l1]) %>% 
              dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Sub-national Unit Name`, Grouping)) %>% 
              dplyr::select(Grouping, Year, Value)%>% 
              dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
              dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, l1))
          }else{
            if(grepl('Volume', l2)){ ## 9
              out <- dat_ports_appended %>%
                dplyr::filter((`Country Name` %in% cname | Grouping %in% l3) & `Indicator Name` %in% ind) %>%
                dplyr::filter(`Sub-national Unit Name` %in% l1 | Grouping %in% l3) %>%
                dplyr::filter(Year %in% Year[`Sub-national Unit Name` %in% l1])%>% 
                dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Sub-national Unit Name`, Grouping)) %>% 
                dplyr::select(Grouping, Year, Value)%>% 
                dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
                dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, l1))
            }
          }
        }
      }
    }else{
      out <- NULL
    }
  }
  
  return(out)
}

## Function for displaying a table and graph when no comparison is needed.

no_comparison_output <- function(dat, sector, selection, min_year_reactive, max_year_reactive){
  

  df1 <- dat
  
  if(!is.null(df1) & nrow(df1) > 0){
    
  ## The years to be displayed will depend on the option chosen in the "Type of selection" tab
  if(selection %in% "Select a range of years"){
    df2 <- df1 %>% dplyr::filter(Year >= min_year_reactive & Year <= max_year_reactive)
  }else{
    df2 <- df1 %>% dplyr::filter(Year %in% max(Year, na.rm = TRUE))
  }

  tooltip_text <-  paste('Country: ', df2$`Country Name`, "\n",
                         'Year: ', as.character(df2$Year),"\n",
                         'Value: ', round(df2$Value, 6))
  
  ## Generate the plot
  ### If the years displayed are less than 4, we will display bar charts otherwise line charts
  bar_width <- ifelse(length(unique(df2$Year)) %in% 1 , 0.1,
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
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 11, colour = "#28313d"),
                   plot.title = ggplot2::element_text(colour = "#28313d"),
                   axis.ticks = ggplot2::element_line(colour = "#ebebeb"))
                   
  if(!grepl('Transport Port', sector)){
    plot2 <- plot2 +
      labs(x = "", y = "" , title = unique(df2$`Indicator Name`), 
           subtitle = paste0("( ",unique(df2$`Country Name`) , " )"))
  }else{
    plot2 <- plot2 +
      labs(x = "", y = "" , title = unique(df2$`Indicator Name`), 
           subtitle = paste0("( ",unique(df2$`Country Name`), " : " , unique(df2$`Sub-national Unit Name`) , " )"))
  }
  
  ## Plotly version of the output 
  if(!grepl('Transport Port', sector)){
    fig <- plotly::ggplotly(plot2, tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(title = list(text = paste0(unique(df2$`Indicator Name`) ,
                                                '<br>',
                                                '<sup >',
                                                paste0("( ", unique(df2$`Country Name`), " )"),
                                                '</sup>')),
                    legend = list( orientation = "h", xanchor = "center",  x = 0.5, y = -0.2),
                    yaxis =  list(title = list(text = "" , standoff = 40L))
                   )
  }else{
    fig <- plotly::ggplotly(plot2, tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(title = list(text = paste0(unique(df2$`Indicator Name`) ,
                                                '<br>',
                                                '<sup >',
                                          paste0("( ",unique(df2$`Country Name`), " : " , unique(df2$`Sub-national Unit Name`) , " )"),
                                                '</sup>')),
                     legend = list( orientation = "h", xanchor = "center",  x = 0.5, y = -0.2),
                     yaxis =  list(title = list(text = "" , standoff = 40L))
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


comparison_output <- function(dat, selection, min_year_reactive, max_year_reactive){
  
  
  ## Create a dataset that only contains data for the country selected
  df1 <- dat
  
  if(!is.null(df1) & nrow(df1) > 0){

  ## Subset the data to have the years selected on the slider
  if(selection %in% "Select a range of years"){
    df2 <- df1 %>% dplyr::filter(Year >= min_year_reactive & Year <= max_year_reactive)
  }else{
    df2 <- df1 %>% dplyr::filter(Year %in% max(Year, na.rm = TRUE))
  }

  ## Create the text for the tooltip to be displayed on the plot
    tooltip_text <-  paste(df2$Grouping,"\n",
                           'Year: ', as.character(df2$Year),"\n",
                           'Value: ', round(df2$Value, 6))
    
    
  ## Generate the plot
  #### If the years displayed are less than 4, we will display bar charts otherwise line charts
    # col_pal <- RColorBrewer::brewer.pal(n = length(unique(df2$Grouping)), name = 'Set1')
    
    bar_width <- ifelse(length(unique(df2$Year)) == 1 , 0.2,
                        ifelse( dplyr::between(length(unique(df2$Year)), 2, 3) , 0.4, 0.7))
    
    if(length(unique(df2$Year)) <= 6){
      
      plot1 <- ggplot2::ggplot(df2, ggplot2::aes(as.factor(Year), Value, fill = Grouping, text = tooltip_text))+
             ggplot2::geom_bar(stat= 'identity', position = 'dodge', width = bar_width) +
              ggplot2::scale_fill_manual(values = cols_tt) 
    }else{
      plot1 <- ggplot2::ggplot(df2, ggplot2::aes(Year, Value, group = Grouping, color = Grouping, text = tooltip_text))+
        ggplot2::geom_point() + 
        ggplot2::geom_line()+
        ggplot2::scale_color_manual(values = cols_tt)
    }
    
    plot2 <- plot1 + 
      ggplot2::guides(fill=ggplot2::guide_legend(title=""))+
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.5, colour = "#28313d"),
                     axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text = ggplot2::element_text(size = 11, colour = "#28313d"),
                     plot.title = ggplot2::element_text(colour = "#28313d"),
                     axis.ticks = ggplot2::element_line(colour = "#ebebeb"),
                     legend.position = "bottom")
    
    ## Display a plotly version of the graph
    fig <- plotly::ggplotly(plot2, tooltip = c("text")) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::layout(title = list(text = paste0(shiny::span("", style = "font-size:26px; font-weight:bold; padding-bottom:100px;" ),
                                              '<br style = "line-height: 300px;">',
                                              '<sup style = "font-size: 1px; color: grey30; font-style: italic">',
                                              '</sup>')),
                   # legend = list( orientation = "h", x = 0.2, y = -0.2),
                   legend = list(text = "", orientation = "h", xanchor = "center",  x = 0.5, y = -0.2),
                   yaxis =  list(title = list(text = "" , standoff = 40L)))
  
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

# ## Scenario 1
# cname <- "Jordan"
# sect <- "Cross-cutting"
# ind <- "Bank nonperforming loans to total gross loans (%)"
# l1 <- "None"
# l2 <- ""
# l3 <- ""
# 
# dat1 <- data_out(cname, sect, ind, l1, l2, l3)
# 
## Scenario 2
# cname <- "Jordan"
# sect <- "Cross-cutting"
# ind <- "Bank nonperforming loans to total gross loans (%)"
# l1 <- "Other countries"
# l2 <- "Afghanistan"
# l3 <- ""
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]

## Scenario 3
# cname <- "Jordan"
# sect <- "Cross-cutting"
# ind <- "Bank nonperforming loans to total gross loans (%)"
# l1 <- "Other indicators"
# l2 <- "Tax revenue (% of GDP)"
# l3 <- ""
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]

# dat <- data_out(cname, sect, ind, l1, l2, l3)
# 
## Scenario 4
# cname <- "Jordan"
# sect <- "Cross-cutting"
# ind <- "Bank nonperforming loans to total gross loans (%)"
# l1 <- "Other benchmarks"
# l2 <- "Regions"
# l3 <- "East Asia & Pacific"
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]
# 
## Scenario 5
# cname <- "Jordan"
# sect <- "Cross-cutting"
# ind <- "Bank nonperforming loans to total gross loans (%)"
# l1 <- "Other benchmarks"
# l2 <- "Income groups"
# l3 <- "High income"
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]] 
# 
## Scenario 6
# cname <- "Jordan"
# sect <- "Transport Port"
# ind <- "Port Liner Shipping Connectivity Index"
# l1 <- "Aqaba"
# l2 <- "None"
# l3 <- ""
# dat <- data_out(cname, sect, ind, l1, l2, l3)


# ## Scenario 7
# cname <- "Jordan"
# sect <- "Transport Port"
# ind <- "Annual Deployed Capacity per Port"
# l1 <- "Aqaba"
# l2 <- "Other ports"
# l3 <- "Aqaba Industrial"
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]
# 
## Scenario 8
# cname <- "Jordan"
# sect <- "Transport Port"
# ind <- "Annual Deployed Capacity per Port"
# l1 <- "Aqaba"
# l2 <- "Regional benchmarks"
# l3 <- "Caribbean small states+"
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]

## Scenario 9
# cname <- "Jordan"
# sect <- "Transport Port"
# ind <- "Annual Deployed Capacity per Port"
# l1 <- "Aqaba"
# l2 <- "Volume benchmarks"
# l3 <- "Small"
# dat <- data_out(cname, sect, ind, l1, l2, l3)
# selection = "Select a range of years"
# min_year_reactive = min(dat$Year, na.rm = TRUE)
# max_year_reactive = max(dat$Year, na.rm = TRUE)
# output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
# output[[1]]
# output[[2]]
# output[[3]]

