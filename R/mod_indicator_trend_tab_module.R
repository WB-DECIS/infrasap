#' indicator_trend_tab_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_indicator_trend_tab_module_ui <- function(id){
  
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::div(class = "controlSection",
               
               ## First row consists of country, sectors and indicator input tabs
               shiny::fluidRow(
                               ## 1. Select Country
                               shiny::column(4,
                                    shiny::selectInput(inputId = ns('data_country'), 
                                                      label = '1. Select country',
                                                       choices = sort(unique(infrasap::dat$`Country Name`)),
                                                       selected = 'Jordan'
                                                      )),
                               ## 2. Select Sector
                               shiny::column(4,shiny::uiOutput(ns('data_sector_ui'))),
                               
                               ## 3. Select Indicator
                               shiny::column(4, shiny::uiOutput(ns('data_indicator_ui')))),
               
               ## Second row consists of comparison inputs, Item 2 in this row doesn't appear when comparison is not needed
               ## ... refer to the note in the server section.
                 shiny::fluidRow(
                                ## Compare to / Choose a port
                               shiny::column(4,shiny::uiOutput(ns('row2_1'))),
                               shiny::column(4,shiny::uiOutput(ns('row2_2'))),
                               shiny::column(4,shiny::uiOutput(ns('row2_3')))
                               ),
               
              ## Third row ... refer to the note in the server section.
                shiny::fluidRow(
                                shiny::column(4,shiny::uiOutput(ns('row3_1'))),
                                shiny::column(8,shiny::uiOutput(ns('row3_2')))
                           ),
              
              ## Graph and table output
                  shiny::fluidRow(
                   shiny::uiOutput(ns('graph_output')),
                    shiny::uiOutput(ns('table_output'))
                    ),
              
              ## Download buttons
               shiny::fluidRow(
                      shiny::column(4, uiOutput(ns("download_chart"))),
                      shiny::column(4, offset = -2, uiOutput(ns("download_table"))),
                      shiny::column(4, offset = -1,  uiOutput(ns("download_data")))
                      )
  )
  )

}
    
#' indicator_trend_tab_module Server Functions
#'
#' @noRd 
mod_indicator_trend_tab_module_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

#----------------------------------------------- Dynamic input controls --------------------------------------------

## Row 1 --------------------------------------------------------------------------------------------------------

     ## Countries (since country list isn't dependent on any other input, it is explicitly defined in the UI
    
     ##  Sectors (depend on country)
          output$data_sector_ui <- shiny::renderUI({
          cn <- input$data_country
          sc_choices <- sort(dat_appended %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Indicator Sector`) %>% pull())

          shiny::selectInput(inputId = ns('data_sector'),
                             label = '2. Select sector',
                             choices = sc_choices,
                             selected = sc_choices[1]
                             ) 
         })
    
     ## Indicators (depend on country and sector)
          output$data_indicator_ui <- renderUI({
            cn <- input$data_country
            sc <- input$data_sector

            if(!grepl('Port', sc)) {
            indicator_choices <- sort(dat_appended %>% dplyr::filter(`Country Name` %in% cn & `Indicator Sector` %in% sc) %>% distinct(`Indicator Name`) %>% pull())
            }else
            indicator_choices <- sort(dat_ports_appended %>% dplyr::filter(`Country Name` %in% cn) %>% distinct(`Indicator Name`) %>% pull())
            
            shiny::selectInput(inputId = ns('data_indicator'),
                                               label = '3. Select an indicator',
                                               choices = indicator_choices,
                                               selected = indicator_choices[1]
                               )
          })

## Row 2 ---------------------------------------------------------------------------------------------------

     ## row2_1 (the label and choices here depend on whether the sector == "Transport Port" or other)
     
     output$row2_1 <- renderUI({
       cn <- input$data_country
       sc <- input$data_sector
       ic <- input$data_indicator
       
       if(!grepl('Port', sc)) {
         
         label <- 'Compare to: '
         choices <- c('None', 'Other countries', 'Other benchmarks', 'Other indicators')
       }else{
         label = paste0('Choose a port from ', cn )
         choices = c(sort(dat_ports_appended %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Sub-national Unit Name`) %>% pull()))
       }
       
         shiny::selectInput(ns("level1_dropdown"),
                                  label = label,
                                  choices = choices,
                                  selected = choices[1])
      })
          
      ## row2_2 (the labels and choices here depend on whether the sector == "Transport Port" or other and if comparison is needed. 
      ## If comparison is not needed, the input button doesn't appear)
      
      output$row2_2 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        level1 <-input$level1_dropdown
        
      if(!grepl('Port', sc)) {
        if(level1 != "None"){
          if(level1 == 'Other countries'){
            choices <- sort(dat_appended %>% dplyr::filter(`Country Name` != cn) %>% distinct(`Country Name`) %>% pull())
            label <- "Select comparison country"
          }else{
            if(level1 == 'Other benchmarks'){
              choices <- c("Regions", "Income groups")
              label <- "Select benchmark"
            }else{
              if(level1 == 'Other indicators'){
                choices <- sort(dat_appended %>% dplyr::filter(`Indicator Sector` == sc & `Indicator Name` != ic) %>% distinct(`Indicator Name`) %>% pull())
                label <- "Select comparison indicator"
              }
            } 
          }
          shiny::selectInput(ns('level2_dropdown'),
                             label = label,
                             choices = choices,
                             selected = choices[1])
        }else
          NULL
        }else
        {
            choices <- c("None", paste0("Other ports in ", cn), "Regional benchmarks", "Volume benchmarks")
            label <- "Compare to"
            
            shiny::selectInput(ns('level2_dropdown'),
                               label = label,
                               choices = choices,
                               selected = choices[1]
            )
        
 
        }
      })
      
      ## row2_3
      
      output$row2_3 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        level1 <-input$level1_dropdown
        level2 <- input$level2_dropdown
        
        if((grepl('Port', sc) & level2 != "None") | level1 == "Other benchmarks"){
          uiOutput(ns("level3_comparisons"))
        }else{
          NULL
          # shiny::selectInput(ns('data_selection_type_year'), 
          #                     label = 'Type of selection: ',
          #                     choices = c('Select latest year available', 'Select a range of years'),
          #                     selected = 'Select a range of years')
        }
      })
      
      output$level3_comparisons <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        level1 <-input$level1_dropdown
        level2 <- input$level2_dropdown
       
     if(!grepl('Port', sc)) { 
       if(level1 == "Other benchmarks"){
  
         if(level2 == "Regions"){
          choices <- sort(unique(dat_appended$Region))
          label <- "Select region"
        }else{
          if(level2 == "Income groups"){
            choices <- sort(unique(dat_appended$IncomeGroup))
            label <- "Select income group"
          }
        }
         
         shiny::selectInput(ns('level3_dropdown'),
                            label = label,
                            choices = choices,
                            selected = choices[1])
        }else
          NULL
       }else{
         
         if(level2 %in% "None"){
           NULL
         }else{
            if(level2 %in% grep("Other ports in ", level2, value = TRUE)){
              choices <- sort(dat_ports_appended %>% dplyr::filter(`Country Name` == cn & `Sub-national Unit Name` != level1) %>% distinct(`Sub-national Unit Name`) %>% pull())
              label <- "Select port"
            }else{
              if(level2 == "Regional benchmarks"){
                choices <- sort(unique(dat_ports_appended$Region))
                label <- "Select regional benchmark"
              }else{
                if(level2 == "Volume benchmarks"){
                  label = "Select volume"
                  choices = c('Small', 'Medium', 'Large', 'Upper 25 Percentile')
                }else{
                choices <- NULL
                label <- NULL
              }
              }
            }
         shiny::selectInput(ns('level3_dropdown'),
                            label = label,
                            choices = choices,
                            selected = choices[1]
         )
         }
         
       }

      })

## Row 3 ----------------------------------------------------------------------------------------------------------------------

    ## row 3_1 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and 
    ## the comparison is by other benchmarks, we have the 'Type of Selection' as the first input tab on row 3
    ##  otherwise the year slider occupies the whole row .)
      
      output$row3_1 <- renderUI({
        shiny::selectInput(ns('data_selection_type_year'), 
                          label = 'Type of selection: ',
                          choices = c('Select latest year available', 'Select a range of years'),
                          selected = 'Select a range of years')
      })
 
    ## Reactive object that will determine the years to be displayed on the year slider (pending) ------------------------------------------------------------------
    
    output_data <- reactive({
      cn <- input$data_country
      sc <- input$data_sector
      ic <- input$data_indicator
      level1 <-input$level1_dropdown
      level2 <- input$level2_dropdown
      level3 <- input$level3_dropdown
      
      # cn <- "Jordan"
      # sc <- "Transport Port"
      # ic <- "Annual Deployed Capacity per Port"
      # level1 <- "Aqaba"
      # level2 <- "None"
      # level3 <- ""

      if(!grepl('Port', sc)) { 
        if(level1 == "None"){
          out <- dat_appended %>% 
                  filter(`Country Name` %in% cn & `Indicator Sector` %in% sc & `Indicator Name` %in% ic) 
        }else{
          
        if(grepl('countries', level1)){
          out <- dat_appended %>% 
            filter(`Country Name` %in% c(cn, level2) & `Indicator Sector` %in% sc & `Indicator Name` %in% ic) %>% 
            filter(Year %in% Year[`Country Name` %in% cn])
        }else{
          
          if(grepl('indicators', level1)){
            out <- dat_appended %>% 
              filter(`Country Name` %in% cn & `Indicator Sector` %in% sc & `Indicator Name` %in% c(ic, level2)) %>% 
              filter(Year %in% Year[`Indicator Sector` %in% sc])
          }
          else{
            
            if(grepl('benchmark', level1)){
              if(grepl('Region', level2)){
                out <- dat_appended %>% 
                        filter((`Country Name` %in% cn | Grouping %in% level3)  & `Indicator Sector` %in% sc & `Indicator Name` %in% ic) %>% 
                        filter(Year %in% Year[`Country Name` %in% cn])
              }else{
                if(grepl('Income', level2)){
                  out <- dat_appended %>% 
                    filter((`Country Name` %in% cn | Grouping %in% level3)  & `Indicator Sector` %in% sc & `Indicator Name` %in% ic) %>% 
                    filter(Year %in% Year[`Country Name` %in% cn])
                 }
                }
               }else
                 out <- data.frame()
              }
             }
            }
           }else
           {
           
            if(grepl('Port', sc)) { 
             if(level2 == "None"){
               out <- dat_ports_appended %>% 
                 filter(`Country Name` %in% cn & `Indicator Name` %in% ic) %>% 
                 filter(`Sub-national Unit Name` %in% level1) 
             }else{
               
               if(grepl('Other ports', level2)){
                 out <- dat_ports_appended %>% 
                   filter(`Country Name` %in% cn & `Indicator Name` %in% ic) %>% 
                   filter(`Sub-national Unit Name` %in% c(level1, level3)) %>% 
                   filter(Year %in% Year[`Sub-national Unit Name` %in% level1])
               }else{
                 if(grepl('Regional', level2)){
                   out <- dat_ports_appended %>% 
                     filter((`Country Name` %in% cn | Grouping %in% level3) & `Indicator Name` %in% ic) %>% 
                     filter(`Sub-national Unit Name` %in% level1) %>% 
                     filter(Year %in% Year[`Sub-national Unit Name` %in% level1])
                 }else{ 
                   if(grepl('Volume', level2)){
                     out <- dat_ports_appended %>% 
                       filter((`Country Name` %in% cn | Grouping %in% level3) & `Indicator Name` %in% ic) %>% 
                       filter(`Sub-national Unit Name` %in% level1) %>% 
                       filter(Year %in% Year[`Sub-national Unit Name` %in% level1])
                   }
                 }
               }
             }
            }
           }
    })
      
    ## row 3_2 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and 
    ## the comparison is by other other benchmarks, the second element on row 3 is the year slider)
    
      output$row3_2 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        level1 <-input$level1_dropdown
        level2 <- input$level2_dropdown
        level3 <- input$level3_dropdown
        
        if(nrow(output_data()) > 0){
        
        years_vec <- output_data() %>%
          dplyr::filter(!is.na(Value))  %>%
          dplyr::distinct(Year) %>%
          dplyr::arrange(Year) %>%
          dplyr::pull() 
        
        if(length(years_vec) == 1){
          shinyWidgets::sliderTextInput(
            inputId = ns("tselection"),
            label = "Select Years",
            grid = TRUE,
            force_edges = TRUE,
            choices = c(years_vec, years_vec),
            selected = years_vec[1]
          )
        }else{
          shinyWidgets::sliderTextInput(
            inputId = ns("tselection"),
            label = "Select Years",
            grid = TRUE,
            force_edges = TRUE,
            choices = years_vec,
            selected = years_vec[c(1,2)]
          )
        }
        
        }else
          NULL
      })

#----------------------------------------------- Data output ----------------------------------------------------------
      
## Reactive object that will hold the data (pending) ------------------------------------------------------------------

## Plot output (pending) -----------------------------------------------------------------------------------------------

## Table output (pending) ----------------------------------------------------------------------------------------------   

## Download buttons ----------------------------------------------------------------------------------------------------

      # output$download_table <- renderUI({
      #   download_bttns(ns("dchart"),"Download Chart")
      # }) 
      # 
      # output$download_chart <- renderUI({
      #   download_bttns(ns("dtable"),"Download Table")
      # }) 
      # 
      # output$download_data <- renderUI({
      #   download_bttns(ns("ddata"),"Download Indicator Data")
      # }) 
      
  })
}
    
## To be copied in the UI
# mod_indicator_trend_tab_module_ui("indicator_trend_tab_module_ui_1")
    
## To be copied in the server
# mod_indicator_trend_tab_module_server("indicator_trend_tab_module_ui_1")
