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
                           shiny::uiOutput(ns('row3_1')),
                           shiny::uiOutput(ns('row3_2'))
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
          sc_choices <- sort(dat_modified %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Indicator Sector`) %>% pull())

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

            indicator_choices <- sort(dat_modified %>% dplyr::filter(`Country Name` %in% cn & `Indicator Sector` %in% sc) %>% distinct(`Indicator Name`) %>% pull())
            
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
         choices = c("None", sort(dat_ports_modified %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Sub-national Unit Name`) %>% pull()))
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
        bm <-input$level1_dropdown
        
      if(!grepl('Port', sc)) {
        if(bm != "None"){
          if(bm == 'Other countries'){
            choices <- sort(dat_modified %>% dplyr::filter(`Country Name` != cn) %>% distinct(`Country Name`) %>% pull())
            label <- "Select comparison country"
          }else{
            if(bm == 'Other benchmarks'){
              choices <- c("Regions", "Income groups")
              label <- "Select benchmark"
            }else{
              if(bm == 'Other indicators'){
                choices <- sort(dat_modified %>% dplyr::filter(`Indicator Sector` == sc & `Indicator Name` != ic) %>% distinct(`Indicator Name`) %>% pull())
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
          if(bm != "None"){
            choices <- c(paste0("Other ports in ", cn), "Regional benchmarks", "Volume benchmarks")
            label <- "Compare to"
            
            shiny::selectInput(ns('level2_dropdown'),
                               label = label,
                               choices = choices,
                               selected = choices[1]
            )
          }
 
        }
      })
      
      ## row2_3
      
      output$row2_3 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        bm <-input$level1_dropdown
        
        comp <- input$level2_dropdown
        if((grepl('Port', sc) & bm != "None") | bm == "Other benchmarks"){
          uiOutput(ns("level3_comparisons"))
        }else{
          shiny::selectInput(ns('data_selection_type_year'), 
                              label = 'Type of selection: ',
                              choices = c('Select latest year available', 'Select a range of years'),
                              selected = 'Select a range of years')
        }
      })
      
      output$level3_comparisons <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        bm <-input$level1_dropdown
        comp <- input$level2_dropdown
       
     if(!grepl('Port', sc)) { 
       if(bm == "Other benchmarks"){
  
         if(comp == "Regions"){
          choices <- sort(unique(dat_modified$Region))
          label <- "Select region"
        }else{
          if(comp == "Income groups"){
            choices <- sort(unique(dat_modified$IncomeGroup))
            label <- "Select income group"
          }
        }
         
         shiny::selectInput(ns('level3_dropdown'),
                            label = label,
                            choices = choices,
                            selected = choices[1])
        }

       }else{
            if(comp %in% grep("Other ports in ", comp, value = TRUE)){
              choices <- sort(dat_ports_modified %>% dplyr::filter(`Country Name` == cn & `Sub-national Unit Name` != bm) %>% distinct(`Sub-national Unit Name`) %>% pull())
              label <- "Select port"
            }else{
              if(comp == "Regional benchmarks"){
                choices <- sort(unique(dat_modified$Region))
                label <- "Select regional benchmark"
              }else{
                if(comp == "Volume benchmarks"){
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

      })

## Reactive object that will determine the years to be displayed on the year slider (pending) ------------------------------------------------------------------
      
## Row 3 ----------------------------------------------------------------------------------------------------------------------

    ## row 3_1 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and 
    ## the comparison is by other benchmarks, we have the 'Type of Selection' as the first input tab on row 3
    ##  otherwise the year slider occupies the whole row .)
      
      output$row3_1 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        bm <-input$level1_dropdown
        comp <- input$level2_dropdown
        
      if((grepl('Port', sc) & bm != "None") | bm == "Other benchmarks"){
        shiny::column(4, shiny::selectInput(ns('data_selection_type_year'), 
                           label = 'Type of selection: ',
                           choices = c('Select latest year available', 'Select a range of years'),
                           selected = 'Select a range of years'))
      }else{
        shiny::column(8, offset = 2, 
                      shinyWidgets::sliderTextInput(
                        inputId = ns("tselection"),
                        label = span("Select Years"),
                        grid = TRUE,
                        force_edges = TRUE,
                        choices = 1990:2022, ##temp
                        selected = 1990, ##temp
                        width = "190%"
                      ))
            }
         })
      
    ## row 3_2 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and 
    ## the comparison is by other other benchmarks, the second element on row 3 is the year slider)
    
      output$row3_2 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        bm <-input$level1_dropdown
        comp <- input$level2_dropdown
        
        if((grepl('Port', sc) & bm != "None") | bm == "Other benchmarks"){
          shiny::column(8, 
                        shinyWidgets::sliderTextInput(
                          inputId = ns("tselection"),
                          label = span("Select Years"),
                          grid = TRUE,
                          force_edges = TRUE,
                          choices = 1990:2022, ##temp
                          selected = 1990, ##temp
                          width = "190%"
                        ))
        }else{
          NULL
        }
        
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
