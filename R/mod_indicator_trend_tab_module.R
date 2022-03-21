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
                               shiny::column(4, shiny::uiOutput(ns('data_indicator_ui')))
                              
      ),
      
                 shiny::fluidRow(
                                ## Compare to / Choose a port
                               shiny::column(4,shiny::uiOutput(ns('row2_1'))),
                               shiny::column(4,shiny::uiOutput(ns('row2_2'))),
                               shiny::column(4,shiny::uiOutput(ns('row2_3')))
                 ),
      
                    shiny::fluidRow(
                       shiny::column(4,shiny::selectInput(ns('data_selection_type_year'), 
                                         label = 'Type of selection: ',
                                         choices = c('Select latest year available', 'Select a range of years'),
                                         selected = 'Select a range of years'
                      )),
                              shiny::column(8,shiny::uiOutput(ns('row3_2')))
      
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
   
    ## row 1 ---------------------------------------------------------------------------------------------
     ##  Sectors
          output$data_sector_ui <- shiny::renderUI({
          cn <- input$data_country
          sc_choices <- sort(dat_modified %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Indicator Sector`) %>% pull())

          shiny::selectInput(inputId = ns('data_sector'),
                             label = '2. Select sector',
                             choices = sc_choices,
                             selected = sc_choices[1]
      )
    })
    
    ## Indicators
          output$data_indicator_ui <- renderUI({
            cn <- input$data_country
            sc <- input$data_sector
            
            if(grepl('Port', sc)) {
              sector_choices <- sort(unique(dat_modified$`Indicator Name`))
            }else{
              sector_choices <- sort(unique(dat_ports_modified$`Indicator Name`))
            }
            
            shiny::selectInput(inputId = ns('data_indicator'),
                                               label = '3. Select an indicator',
                                               choices = sector_choices,
                                               selected = sector_choices[1])
            
          })

    ## row 2 ---------------------------------------------------------------------------------------------
     ## row2_1
     output$row2_1 <- renderUI({
       cn <- input$data_country
       sc <- input$data_sector
       ic <- input$data_indicator
       
       if(!grepl('Port', sc)) {
         
         label <- 'Compare to: '
         choices <- c('Other countries', 'Other benchmarks', 'Other indicators')
       }else{
         label = paste0('Choose a port from ', cn )
         choices = sort(dat_ports %>% dplyr::filter(`Country Name` == cn) %>% distinct(`Sub-national Unit Name`) %>% pull())
       }
       
         shiny::selectInput(ns("data_compare_to"),
                                  label = label,
                                  choices = choices,
                                  selected = choices[1])
     })
          
      
      ## row2_2
      output$row2_2 <- renderUI({
        cn <- input$data_country
        sc <- input$data_sector
        ic <- input$data_indicator
        bm <-input$data_compare_to
        
        if(!grepl('Port', sc)) {
          if(bm == 'Other countries'){
            choices <- sort(dat_modified %>% dplyr::filter(`Country Name` != cn) %>% distinct(`Country Name`) %>% pull())
            label <- "Select other country"
          }else{
            if(bm == 'Other benchmarks'){
              choices <- c("Regions", "Income groups")
              label <- "Select benchmark"
            }else{
              if(bm == 'Other indicators'){
                choices <- sort(dat_modified %>% dplyr::filter(`Indicator Name` != ic) %>% distinct(`Indicator Name`) %>% pull())
                label <- "Select indicator"
              }
            } 
          }
          shiny::selectInput(ns('comparison_value2'),
                             label = label,
                             choices = choices,
                             selected = choices[1])
        }else
        {
          choices <- c(paste0("Other ports in ", cn), "Regional benchmarks", "Volume benchmarks")
           label <- "Compare to"
           
           shiny::selectInput(ns('comparison_value'),
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
        bm <-input$data_compare_to
        comp <- input$comparison_value
       
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
         
         shiny::selectInput(ns('comparison_value2'),
                            label = label,
                            choices = choices,
                            selected = choices[1])
        }

       }else{
            if(comp %in% grep("Other ports in ", comp, value = TRUE)){
              choices <- sort(dat_ports %>% dplyr::filter(`Country Name` == cn & `Sub-national Unit Name` != bm) %>% distinct(`Sub-national Unit Name`) %>% pull())
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
         shiny::selectInput(ns('comparison_value2'),
                            label = label,
                            choices = choices,
                            selected = choices[1]
         ) 
         
       }

      })
  })
}
    
## To be copied in the UI
# mod_indicator_trend_tab_module_ui("indicator_trend_tab_module_ui_1")
    
## To be copied in the server
# mod_indicator_trend_tab_module_server("indicator_trend_tab_module_ui_1")
