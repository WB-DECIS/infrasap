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
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
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
                                shiny::column(4, shiny::uiOutput(ns('row3_1'))),
                                shiny::column(8, shiny::uiOutput(ns('row3_2')))
                           ),
              
              ## Graph and table output
                  shiny::fluidRow(
                    shiny::column(8, offset = 2, shiny::uiOutput(ns('graph_output'))),
                    shiny::column(8, offset = 2, shiny::uiOutput(ns('table_output')))
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
      
      shiny::req(input$data_country)
      
      cn <- input$data_country
      sc_choices <- sort(dat_appended %>% dplyr::filter(`Country Name` %in% cn) %>% dplyr::distinct(`Indicator Sector`) %>% dplyr::pull())
      
      shiny::selectInput(inputId = ns('data_sector'),
                         label = '2. Select sector',
                         choices = sc_choices,
                         selected = sc_choices[1]
      ) 
    })
    
    ## Indicators (depend on country and sector)
    output$data_indicator_ui <- renderUI({
      
      shiny::req(input$data_country)
      shiny::req(input$data_sector)
      
      cn <- input$data_country
      sc <- input$data_sector
      
      if(!grepl('Transport Port', sc)) {
        indicator_choices <- sort(dat_appended %>% dplyr::filter(`Country Name` %in% cn & `Indicator Sector` %in% sc) %>% dplyr::distinct(`Indicator Name`) %>% dplyr::pull())
      }else
        indicator_choices <- sort(dat_ports_appended %>% dplyr::filter(`Country Name` %in% cn) %>% dplyr::distinct(`Indicator Name`) %>% dplyr::pull())
      
      shiny::selectInput(inputId = ns('data_indicator'),
                         label = '3. Select an indicator',
                         choices = indicator_choices,
                         selected = indicator_choices[1]
      )
    })
    
    ## Row 2 ---------------------------------------------------------------------------------------------------
    
    ## row2_1 (the label and choices here depend on whether the sector %in% "Transport Port" or other)
    
    output$row2_1 <- renderUI({
      
      shiny::req(input$data_country)
      shiny::req(input$data_sector)
      shiny::req(input$data_indicator)
      
      cn <- input$data_country
      sc <- input$data_sector
      ic <- input$data_indicator
      
      if(!grepl('Transport Port', sc)) {
        
        label <- 'Compare to: '
        choices <- c('None', 'Other countries', 'Other benchmarks', 'Other indicators')
      }else{
        label = paste0('Choose a port from ', cn )
        choices = c(sort(dat_ports_appended %>% dplyr::filter(`Country Name` %in% cn) %>% dplyr::distinct(`Sub-national Unit Name`) %>% dplyr::pull()))
      }
      
      shiny::selectInput(ns("level1_dropdown"),
                         label = label,
                         choices = choices,
                         selected = choices[1])
    })
    
    ## row2_2 (the labels and choices here depend on whether the sector %in% "Transport Port" or other and if comparison is needed. 
    ## If comparison is not needed, the input button doesn't appear)
    
    output$row2_2 <- renderUI({
      
      shiny::req(input$data_country)
      shiny::req(input$data_sector)
      shiny::req(input$data_indicator)
      shiny::req(input$level1_dropdown)


      shiny::conditionalPanel(condition = paste0("input['", ns("level1_dropdown"), "'] != 'None' "),
                              shiny::uiOutput(ns("row2_2b")))
      
    })
    
    output$row2_2b <- renderUI({ 
      shiny::req(input$data_country)
      shiny::req(input$data_sector)
      shiny::req(input$data_indicator)

    
      cn <- input$data_country
      sc <- input$data_sector
      ic <- input$data_indicator
     
   

    if(!grepl('Transport Port', sc)) {
      
      shiny::req(input$level1_dropdown)
      level1 <-input$level1_dropdown
      
        if(level1 %in% 'Other countries'){
          choices0 <- sort(dat_appended %>% dplyr::filter(`Indicator Name`%in% ic) %>% dplyr::filter(!`Country Name`  %in% cn) %>% dplyr::distinct(`Country Name`) %>% dplyr::pull())
          label0 <- "Select comparison country"
          selected_choice <- choices0[1]
          shiny::conditionalPanel(condition = paste0("input['", ns("level1_dropdown"), "'] != 'None' "),
                                  shiny::selectInput(ns('level2_dropdown'),
                                                     label = label0,
                                                     choices = choices0,
                                                     selected = selected_choice,
                                                     multiple = TRUE
                                  )
          )
        }else{
          if(level1 %in% 'Other benchmarks'){
            choices0 <- c("Regions", "Income groups")
            label0 <- "Select benchmark"
            selected_choice <- choices0[1]
            shiny::conditionalPanel(condition = paste0("input['", ns("level1_dropdown"), "'] != 'None' "),
                                    shiny::selectInput(ns('level2_dropdown'),
                                                       label = label0,
                                                       choices = choices0,
                                                       selected = selected_choice
                                    )
            )
          }else{
            if(level1 %in% 'Other indicators'){
              choices0 <- sort(dat_appended %>% dplyr::filter(`Indicator Sector` %in% sc & !`Indicator Name`  %in% ic) %>% dplyr::distinct(`Indicator Name`) %>% dplyr::pull())
              label0 <- "Select comparison indicator"
              selected_choice <- choices0[1]
              shiny::conditionalPanel(condition = paste0("input['", ns("level1_dropdown"), "'] != 'None' "),
                                      shiny::selectInput(ns('level2_dropdown'),
                                                         label = label0,
                                                         choices = choices0,
                                                         selected = selected_choice,
                                                         multiple = TRUE
                                      )
              )
            }
        }
      }
    }else{
      if(grepl('Transport Port', sc)) {
        choices0 <- c("None", paste0("Other ports in ", cn), "Regional benchmarks", "Volume benchmarks")
        label0 <- "Compare to"
        selected_choice <- choices0[1]
        shiny::conditionalPanel(condition = paste0("input['", ns("level1_dropdown"), "'] != 'None' "),
                                shiny::selectInput(ns('level2_dropdown'),
                                                   label = label0,
                                                   choices = choices0,
                                                   selected = selected_choice
                                )
        )

      }
    }
  

      
    })

## -------------------------------------------------------
    output$row2_3 <- renderUI({

      shiny::req(input$data_country)
      shiny::req(input$data_sector)
      shiny::req(input$level1_dropdown)
      shiny::req(input$level2_dropdown)
      sc <- input$data_sector


      shiny::conditionalPanel(condition = paste0("input['", ns("level2_dropdown"), "'] != 'None'", "&", 
                                                 "input['", ns("level1_dropdown"), "'] != 'Other countries'", "&",
                                                 "input['", ns("level1_dropdown"), "'] != 'Other indicators'"),
                              shiny::uiOutput(ns("row2_3b")))

    })
# 
    output$row2_3b <- renderUI({
      shiny::req(input$data_sector)
      shiny::req(input$data_indicator)
      shiny::req(input$level1_dropdown)
      shiny::req(input$level2_dropdown)

            cn <- input$data_country
            sc <- input$data_sector
            ic <- input$data_indicator
            level1 <-input$level1_dropdown
            level2 <- input$level2_dropdown

            if(!grepl('Transport Port', sc)) {
              if(level1 %in% "Other countries" | level1 %in% "Other indicators"){
                choices <- "None"
                label <- ""
                selected_choice <- choices[1]
                shiny::selectInput(ns('level3_dropdown'),
                                   label = label,
                                   choices = choices,
                                   selected = selected_choice,
                                   multiple = TRUE
                )
              }else{

              if(level1 %in% "Other benchmarks"){

                if(level2 %in% "Regions"){
                  choices <- sort(unique(dat_appended$Region))
                  label <- "Select region"
                  selected_choice <- choices[1]
                  shiny::selectInput(ns('level3_dropdown'),
                                     label = label,
                                     choices = choices,
                                     selected = selected_choice,
                                     multiple = TRUE
                  )
                }else{
                  if(level2 %in% "Income groups"){
                    choices <- sort(unique(dat_appended$IncomeGroup))
                    label <- "Select income group"
                    selected_choice <- choices[1]
                    shiny::selectInput(ns('level3_dropdown'),
                                       label = label,
                                       choices = choices,
                                       selected = selected_choice,
                                       multiple = TRUE
                    )
                  }
                }
              }

              }
            }else{
                          if(grepl("Other ports", level2)){
                            choices <- sort(dat_ports_appended %>% dplyr::filter(`Country Name` %in% cn & !`Sub-national Unit Name`  %in% level1) %>% dplyr::distinct(`Sub-national Unit Name`) %>% dplyr::pull())
                            label <- "Select port"
                            selected_choice <- choices[1]
                            shiny::selectInput(ns('level3_dropdown'),
                                               label = label,
                                               choices = choices,
                                               selected = selected_choice,
                                               multiple = TRUE
                            )
                          }else{
                            if(level2 %in% "Regional benchmarks"){
                              choices <- sort(unique(dat_ports_appended$Region))
                              label <- "Select regional benchmark"
                              selected_choice <- choices[1]
                              shiny::selectInput(ns('level3_dropdown'),
                                                 label = label,
                                                 choices = choices,
                                                 selected = selected_choice,
                                                 multiple = TRUE
                              )
                            }else{
                              if(level2 %in% "Volume benchmarks"){
                                choices = c('Small', 'Medium', 'Large', 'Upper 25 Percentile')
                                label = "Select volume"
                                selected_choice <- choices[1]
                                shiny::selectInput(ns('level3_dropdown'),
                                                   label = label,
                                                   choices = choices,
                                                   selected = selected_choice,
                                                   multiple = TRUE
                                )
                                
                              }
                             
                            }
                          }
                      
                }

    })


## Row 3 ----------------------------------------------------------------------------------------------------------------------

    ## row 3_1 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and
    ## the comparison is by other benchmarks, we have the 'Type of Selection' as the first input tab on row 3
    ##  otherwise the year slider occupies the whole row .)

      output$row3_1 <- renderUI({
        shiny::selectInput(ns('data_selection_type'),
                          label = 'Type of selection: ',
                          choices = c('Select latest year available', 'Select a range of years'),
                          selected = 'Select a range of years')
      })

## Reactive object that will determine the years to be displayed on the year slider (and luckily the
## data to be displayed.) ------------------------------------------------------------------

output_data <- reactive({

  shiny::req(input$data_country)
  shiny::req(input$data_sector)
  shiny::req(input$data_indicator)
  shiny::req(input$level1_dropdown)


  cname <- input$data_country
  sect <- input$data_sector
  ind <- input$data_indicator
  # l1 <-input$level1_dropdown
  # l2 <- input$level2_dropdown
  # l3 <- input$level3_dropdown


  if(!grepl('Transport Port', sect)) {

    shiny::req(input$level1_dropdown)
    l1 <-input$level1_dropdown

    if(l1 %in% "None"){ ## 1
      out <- dat_appended %>%
       dplyr::filter(`Country Name` %in% cname & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>%
        dplyr::select(`Country Name`, `Indicator Name`, Year, Value)
    }else{

      if(grepl('countries', l1)){ ## 2
        shiny::req(input$level2_dropdown)
        l2 <- input$level2_dropdown
        out <- dat_appended %>%
          dplyr::filter(`Country Name` %in% c(cname, l2) & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>%
          dplyr::filter(Year %in% Year[`Country Name` %in% cname]) %>%
          dplyr::select(`Country Name`, `Indicator Name`, Year, Value) %>%
          dplyr::rename(Grouping = `Country Name`) %>% 
          dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
          dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, cname))
      }else{

        if(grepl('indicators', l1)){ ## 3

          shiny::req(input$level2_dropdown)
          l2 <- input$level2_dropdown
          out <- dat_appended %>%
            dplyr::filter(`Country Name` %in% cname & `Indicator Sector` %in% sect & `Indicator Name` %in% c(ind, l2)) %>%
            dplyr::filter(Year %in% Year[`Indicator Name` %in% ind]) %>%
            dplyr::select(`Country Name`, `Indicator Name`, Year, Value) %>%
            dplyr::rename(Grouping = `Indicator Name`) %>% 
            dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
            dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, ind))
        }else{

          if(grepl('benchmark', l1)){

            shiny::req(input$level2_dropdown)
            l2 <- input$level2_dropdown

            if(grepl('Region', l2)){ ## 4
              shiny::req(input$level3_dropdown)
              l3 <- input$level3_dropdown

              out <- dat_appended %>%
                dplyr::filter((`Country Name` %in% cname | Grouping %in% l3)  & `Indicator Sector` %in% sect & `Indicator Name` %in% ind) %>%
                dplyr::filter(Year %in% Year[`Country Name` %in% cname]) %>%
                dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Country Name`, Grouping))%>%
                dplyr::select(Grouping, `Indicator Name`, Year, Value) %>% 
                dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
                dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, cname))

            }else{
              if(grepl('Income', l2)){ ## 5

                shiny::req(input$level3_dropdown)
                l3 <- input$level3_dropdown

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

      shiny::req(input$level1_dropdown)
      l1 <- input$level1_dropdown

      shiny::req(input$level2_dropdown)
      l2 <- input$level2_dropdown

      if(l2 %in% "None"){ ## 6
        out <- dat_ports_appended %>%
          dplyr::filter(`Country Name` %in% cname & `Indicator Name` %in% ind) %>%
          dplyr::filter(`Sub-national Unit Name` %in% l1) %>%
          dplyr::select(`Country Name`, `Indicator Name`, `Sub-national Unit Name`, Year, Value)
      }else{

        if(grepl('Other ports', l2)){ ## 7
          shiny::req(input$level3_dropdown)
          l3 <- input$level3_dropdown

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
            shiny::req(input$level3_dropdown)
            l3 <- input$level3_dropdown

            out <- dat_ports_appended %>%
              dplyr::filter((`Country Name` %in% cname | Grouping %in% l3) & `Indicator Name` %in% ind) %>%
              dplyr::filter(`Sub-national Unit Name` %in% l1 | Grouping %in% l3) %>%
              dplyr::filter(Year %in% Year[`Sub-national Unit Name` %in% l1]) %>%
              dplyr::mutate(Grouping = ifelse(is.na(Grouping), `Sub-national Unit Name`, Grouping)) %>%
              dplyr::select(Grouping, Year, Value) %>% 
              dplyr::mutate(Grouping = as.factor(Grouping)) %>% 
              dplyr::mutate(Grouping = forcats::fct_relevel(Grouping, l1))

          }else{
            if(grepl('Volume', l2)){ ## 9
              shiny::req(input$level3_dropdown)
              l3 <- input$level3_dropdown

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
    }

  }

  return(out)
  })

    ## row 3_2 : (if sector is 'Transport Port' and no comparison is needed, or sector is anything else, comparison is needed and
    ## the comparison is by other other benchmarks, the second element on row 3 is the year slider)

      output$row3_2 <- renderUI({
        # cn <- input$data_country
        # sc <- input$data_sector
        # ic <- input$data_indicator
        # level1 <-input$level1_dropdown
        # level2 <- input$level2_dropdown
        # level3 <- input$level3_dropdown

        if(nrow(output_data()) > 0){

        years_vec <- output_data() %>%
          dplyr::filter(!is.na(Value))  %>%
          dplyr::distinct(Year) %>%
          dplyr::arrange(Year) %>%
          dplyr::pull()

        if(length(years_vec) %in% 1){
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

## Reactive object that will hold the output ------------------------------------------------------------------

infrasap_output <- reactive({

  req(input$data_country)
  req(input$data_sector)
  req(input$data_indicator)
 req(input$level1_dropdown)
 # req(input$level2_dropdown)
 # req(input$level3_dropdown)

  dat <- output_data()
  cn <- input$data_country
  sc <- input$data_sector
  ic <- input$data_indicator
  level1 <-input$level1_dropdown
  level2 <- input$level2_dropdown
  level3 <- input$level3_dropdown
  selection <- input$data_selection_type
  tselection <- input$tselection
  # sector <- sc


  min_year <- shiny::reactive({
    as.numeric(input$tselection[1])
  })


  max_year <- shiny::reactive({
    if(length(input$tselection)>1){
      as.numeric(input$tselection[2])
    }else{
      as.numeric(input$tselection[1])
    }

  })

  # min_year_reactive <- min_year()
  # max_year_reactive <- max_year()


    if(all(!grepl('Transport Port', sc) & level1 %in% "None")) {
      dat <- output_data()
      sector <- input$data_sector
      selection <- input$data_selection_type
      min_year_reactive <- min_year()
      max_year_reactive <- max_year()
      output <- no_comparison_output(dat, sector, selection, min_year_reactive, max_year_reactive)
    }else{
      req(input$level2_dropdown)
      if(all(grepl('Transport Port', sc) & level2 %in% "None")){
        dat <- output_data()
        sector <- input$data_sector
        selection <- input$data_selection_type
        min_year_reactive <- min_year()
        max_year_reactive <- max_year()
        output <- no_comparison_output(dat, sector, selection, min_year_reactive, max_year_reactive)
      }else{
        req(input$level2_dropdown)
        if(all((!grepl('Transport Port', sc) & !level1 %in% "None") |
            (grepl('Transport Port', sc) & !level2 %in% "None"))){
      dat <- output_data()
      selection <- input$data_selection_type
      min_year_reactive <- min_year()
      max_year_reactive <- max_year()
        output <- comparison_output(dat, selection, min_year_reactive, max_year_reactive)
      }
      }
    }


  return(output)
})

#
## Plot output (pending) -----------------------------------------------------------------------------------------------
output$graph_output <- renderUI({
  plotly::plotlyOutput(ns("graph_output2")) %>%
    shinycssloaders::withSpinner(type = 7,color = "#002244")
})
output$graph_output2 <- plotly::renderPlotly({

  req(input$data_country)
  req(input$data_sector)
  req(input$data_indicator)
  req(input$level1_dropdown)
  infrasap_output()[[2]]
})


## Table output (pending) ----------------------------------------------------------------------------------------------

output$table_output <- renderUI({
  shiny::fluidRow(
       DT::DTOutput(ns('table_output2'))
    )
})


output$table_output2 <- DT::renderDT({

  req(input$data_country)
  req(input$data_sector)
  req(input$data_indicator)
  req(input$level1_dropdown)

  DT::datatable(infrasap_output()[[3]],
                options =
                  list(pageLength=10,
                       scrollX='400px'))
})


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

  #})
  })
}

## To be copied in the UI
# mod_indicator_trend_tab_module_ui("indicator_trend_tab_module_ui_1")
    
## To be copied in the server
# mod_indicator_trend_tab_module_server("indicator_trend_tab_module_ui_1")
