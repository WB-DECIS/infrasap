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
                               shiny::column(8, class = "column-eight-width",
                                             shiny::column(6,
                                                           shiny::selectInput(inputId = ns('data_country'), 
                                                                              label = '1. Select country',
                                                                              choices = sort(unique(infrasap::dat$`Country Name`)),
                                                                              selected = 'Jordan'
                                                           ),
                                                           shiny::uiOutput(ns('data_indicator_ui'))
                   
                                             ),
                                             shiny::column(6,
                                                           shiny::uiOutput(ns('data_sector_ui')),
                                                           shiny::selectInput(ns('data_selection_type_year'), 
                                                                              label = 'Type of selection: ',
                                                                              choices = c('Select latest year available', 'Select a range of years'),
                                                                              selected = 'Select a range of years'
                                                           )
                                             ),
                               shiny::uiOutput(ns('data_year_ui'))
                               ),
                               shiny::column(4,
                                             shiny::uiOutput(ns('country_ports_ui')),
                                             shiny::uiOutput(ns('ports_compare_to_indicator_type_ui')),
                                             shiny::uiOutput(ns('data_compare_to_ui')),
                                             shiny::uiOutput(ns('data_benchmark_ui'))
                               )
              )
    ),
    shiny::div(
      shiny::fluidRow(
                      shiny::column(12,
                                    align = 'center',
                                    plotly::plotlyOutput(ns('data_chart'),
                                                         width = '900px', 
                                                         height = "auto"
                                    ) %>% shinycssloaders::withSpinner(type = 7,color = "#154164")
                      )
      ),
      shiny::br(), shiny::br(),
      shiny::fluidRow(shiny::column(12,shiny::uiOutput(ns("btn_data_access")))),
      shiny::br(), shiny::br(),
      shiny::fluidRow(shiny::column(12,align = 'center',
                                    shiny::uiOutput(ns('data_table_access'), width = '900px')))       
    )
  )
}
    
#' indicator_trend_tab_module Server Functions
#'
#' @noRd 
mod_indicator_trend_tab_module_server <- function(id) {
  # Rename sector name
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "Transport"] <- "Transport cross-cutting"
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  # Rename sector name
  infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "Transport"] <- "Transport cross-cutting"
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"
  
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #Not sure what irf_indicators is referring to here, to avoid R CMD check errors initialising it as NULL
    irf_indicators <- NULL
    # Module Body
    
    #------- Initialize the Memory ----------
    selected_vals = shiny::reactiveValues(
                                          db_countries_name = NULL,
                                          data_benchmarks_name = NULL,
                                          data_ports_name = NULL,
                                          data_indicator = NULL
                    )
    
    shiny::observe({
                    shiny::req(
                               input$data_countries, 
                               input$data_benchmarks, 
                               input$data_country, 
                               input$data_compare_to, 
                               input$data_sector, 
                               input$data_indicator, 
                               input$data_year,
                               input$country_ports
                    )
      
                  input$data_country
                  input$data_sector
                  input$data_compare_to
             
                  selected_vals$db_countries_name <- input$data_countries
                  selected_vals$data_benchmarks_name <- input$data_benchmarks
                  selected_vals$data_indicator <- input$data_indicator
    })
    
    shiny::observeEvent(c(input$data_country,
                          input$data_compare_to,
                          input$data_sector,
                          input$data_indicator,
                          input$data_year,
                          input$country_ports
                         ), 
    {
     selected_vals$data_benchmarks_name <- input$data_benchmarks
     selected_vals$data_ports_name <- input$country_ports
     selected_vals$data_indicator <- input$data_indicator
    })
    
    shiny::observeEvent(input$data_country, {
                        bn_selected_cir <- infrasap::dat_country_income_region %>%
                                              dplyr::filter(.data$`Country Name` == input$data_country)
                        
                        shiny::updateSelectInput(session, 
                                                 "data_benchmarks",
                                                 selected = c(bn_selected_cir$Region, bn_selected_cir$IncomeGroup)
                        )
    })
    
    # Compare to UI
    output$data_compare_to_ui <- shiny::renderUI({
      if(!is.null(input$ports_compare_to_indicator_type) && (input$ports_compare_to_indicator_type == "to_regional_bench" && input$data_sector == 'Transport Port')) {
         shiny::selectizeInput(ns('data_compare_to'),
                               label = NULL,
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE,
                               options = list(
                                              # maxItems = 3,
                                              'plugins' = list('remove_button'),
                                              'create' = TRUE,
                                              'persist' = FALSE
                                         )

         )
      } else {
          shiny::selectInput(ns('data_compare_to'),
                             label = NULL,
                             choices = NULL,
                             selected = NULL
          )
      } 
    })
    
    # get sectors based on country input
    output$data_sector_ui <- shiny::renderUI({
                        req(input$data_country)
                        cn <- input$data_country
                        # subset data by country selected
                        df <- infrasap_dat_mod_modified %>% dplyr::filter(.data$`Country Name` == cn)
                        sc_choices <- sort(unique(df$`Indicator Sector`))
                        # sc_choices <- sc_choices[sc_choices != 'National']
                        shiny::selectInput(inputId = ns('data_sector'),
                                           label = '2. Select sector',
                                           choices = sc_choices,
                                           selected = sc_choices[2])
    })
    
    output$country_ports_ui <- shiny::renderUI({
                                                shiny::req(input$data_country, input$data_sector)
                                                cn <- input$data_country
                                                sc <- input$data_sector
                                                
                                                if(is.null(sc)) {
                                                  NULL
                                                } else {
                                                  if(!has_port(sc)) {
                                                    NULL
                                                  } else {
                                                    df <- infrasap::dat_ports
                                                    df <- df %>% dplyr::filter(.data$`Country Name` == cn)
                                                    # port_choices <- sort(unique(df$`Sub-national Unit Name`))
                                                    if(!is.null(input$data_compare_to)) {
                                                      port_choices <- sort(unique(df$`Sub-national Unit Name`[df$`Sub-national Unit Name` != input$data_compare_to]))
                                                    } else {
                                                      port_choices <- sort(unique(df$`Sub-national Unit Name`))
                                                    }
                                                    
                                                    if(selected_vals$data_ports_name %in% port_choices && !is.null(selected_vals$data_ports_name)) {
                                                      port_choice_selected <- selected_vals$data_ports_name
                                                    } else {
                                                      port_choice_selected <- port_choices[1]
                                                    }
                                                    
                                                    shiny::selectInput(ns('country_ports'),
                                                                       label = paste0('Choose a port from ',cn ),
                                                                       choices = port_choices,
                                                                       selected = port_choice_selected
                                                    )
                                                  }
                                                }
    })
    
    output$ports_compare_to_indicator_type_ui <- shiny::renderUI({
                                                                  shiny::req(input$data_country)
                                                                  shiny::req(input$data_sector)
                                                                  
                                                                  cn <- input$data_country
                                                                  sc <- input$data_sector
                                                                  if(is.null(sc)){
                                                                    NULL
                                                                  } else {
                                                                    if(!has_port(sc)){
                                                                      NULL
                                                                    } else {
                                                                      
                                                                      shiny::selectInput(ns('ports_compare_to_indicator_type'),
                                                                                         label = 'Compare to:',
                                                                                         choices = NULL,
                                                                                         selected = NULL
                                                                      )
                                                                    }
                                                                  }
    })
    
    
    
    shiny::observeEvent(c(input$data_sector), {
                                               shiny::req(input$data_sector)
                                              if(has_port(input$data_sector)) {
                                                
                                                 sc <- input$data_sector
                                                 cn <- input$data_country

                                                 if(is.null(input$country_ports)) {
                                                    df <- infrasap::dat_ports
                                                    df <- df %>% dplyr::filter(.data$`Country Name` == cn)
                                                    port_choices <- sort(unique(df$`Sub-national Unit Name`))
                                                    cp <- port_choices[1]
                                                 } else {
                                                    cp <- input$country_ports
                                                 }

                                                 country_choice <- as.character(stringr::str_glue('Compare to other ports in country {cn}'))
                                                 choices_ports_compare_to_indicator_type <- c('to_country', 'to_regional_bench', 'to_volume_bench')
                                                 names(choices_ports_compare_to_indicator_type) <- c(country_choice,
                                                                                                    'Compare to regional benchmarks',
                                                                                                    'Compare to Volume benchmarks'
                                                                                                    )
                                                shiny::updateSelectInput(session, 
                                                                         "ports_compare_to_indicator_type",
                                                                         choices = choices_ports_compare_to_indicator_type,
                                                                         selected = 'to_country'
                                                )
                                                
                                                shiny::observeEvent(input$ports_compare_to_indicator_type, {
                                                                    shiny::req(input$ports_compare_to_indicator_type)
                                                                    if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_country") {
                                                  
                    
                                                                        df <- infrasap::dat_ports %>% 
                                                                                  dplyr::filter(.data$`Country Name` == cn) %>% 
                                                                                  dplyr::filter(.data$`Sub-national Unit Name` != cp)
                                                            
                                                                        compare_ports <- sort(unique(df$`Sub-national Unit Name`[df$`Sub-national Unit Name` != cp]))
                                                                        shiny::updateSelectInput(session, 
                                                                                                 "data_compare_to",
                                                                                                 label = paste0('Compare to other ports from ',cn, ':'),
                                                                                                 choices = compare_ports,
                                                                                                 selected = compare_ports[1]
                                                                        )
                                                  
                                                                    } else {
                                                                      if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_regional_bench") {
                                                                        
                                                                        shiny::updateSelectizeInput(session, 
                                                                                                    "data_compare_to",
                                                                                                    label = 'Compare to regional benchmarks',
                                                                                                    choices = c('East Asia & Pacific', 
                                                                                                                'Europe & Central Asia',
                                                                                                                'Latin America & Caribbean',
                                                                                                                'Middle East & North Africa',
                                                                                                                'North America',
                                                                                                                'South Asia',
                                                                                                                'Sub-Saharan Africa'
                                                                                                                ),
                                                                                                    selected = 'East Asia & Pacific'
                                                                                          
                                                                        )
                                                                      } else {
                                                                        
                                                                        if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_volume_bench") {
                                                                          
                                                                           shiny::updateSelectInput(session, 
                                                                                                    "data_compare_to",
                                                                                                    label = "Compare ports by volume",
                                                                                                    choices = c('Small', 'Medium', 'Large', 'Upper 25 Percentile'),
                                                                                                    selected = 'Small'
                                                                           )
                                                                        } else {
                                                                        }
                                                                        
                                                                      }
                                                                      
                                                                    }
                                                })
                                              } else {
                                                shiny::updateSelectInput(session, 
                                                                         "data_compare_to",
                                                                         label = 'Compare to: ',
                                                                         choices = c('Other countries', 'Other benchmarks', 'Other indicators'),
                                                                         selected = 'Other benchmarks'
                                                )
                                              }
    })

    # UI for indicators, based on sector and year selection
    output$data_indicator_ui <- shiny::renderUI({
      req(input$data_sector)
      # get sector and year
      sc <- input$data_sector
      sc <- c(sc, 'Cross-cutting')
      cn <- input$data_country
      cp <- input$data_compare_to
      ct <- input$country_ports
      if(is.null(sc)) {
        NULL
      } else {
        if(has_port(sc)) {
           df <- infrasap::dat_ports %>%
                      dplyr::filter(.data$`Country Name` == cn) %>%
                      dplyr::filter(.data$`Sub-national Unit Name` %in% c(cp, ct)) %>%
                      dplyr::select(Grouping = .data$`Indicator Name`,.data$`Sub-national Unit Name`,.data$`1990`:.data$`2020`) %>%
                      tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`, -.data$`Sub-national Unit Name`) %>%
                      tidyr::drop_na()
           # get a unique list of indicators
           ic <- sort(unique(df$Grouping))
           
           shiny::fluidRow(
                           shiny::column(12,
                                         shiny::selectInput(inputId = ns('data_indicator'),
                                                            label = '3. Select an indicator',
                                                            choices = ic,
                                                            selected = ic[1]
                                         )
                           )
          )
        } else {
          # subset data by sector and year and remove NAs
          df <- infrasap_dat_mod_modified %>%
                    # dplyr::filter(`Country Name` == "Jordan") %>%
                    dplyr::filter(.data$`Country Name` == cn) %>%
                    # dplyr::filter(`Indicator Sector` %in% c('Digital Development')) %>%
                    dplyr::filter(.data$`Indicator Sector` %in% input$data_sector) %>%
                    dplyr::select(Grouping = .data$`Indicator Name`,.data$`1990`:.data$`2020`) %>%
                    tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`, na.rm = TRUE)
          
          ic <- sort(unique(df$Grouping))

          if((!is.null(selected_vals$data_indicator)) && (selected_vals$data_indicator %in% ic)) {
             selected_ic <- selected_vals$data_indicator
          } else {
             selected_ic <- ic[1]
          }
          shiny::fluidRow(
                          shiny::column(12,
                                        shiny::selectInput(inputId = ns('data_indicator'),
                                                           label = '3. Select an indicator',
                                                           choices = ic,
                                                           selected = selected_ic
                                        )
                          )
          )
        }
      }
    })
  
  
    
    # UI for benchmarks or countries depending on "data_compare_to" input
    output$data_benchmark_ui <- shiny::renderUI({
                                                 sc <- input$data_sector
                                                 sc <- c(sc, 'Cross-cutting')
                                                 cn <- input$data_country
                                                 ic <- input$data_indicator
                                                 yr <- input$data_year
                                                 ct <- input$data_compare_to
                                                # save(ct, file = 'temp_ct.rda')
                                                 if(is.null(yr)) {
                                                    NULL
                                                 } else {
                                                   if(has_port(sc)) {
                                                      selected_vals$data_benchmarks_name <<- NULL
                                                      NULL
                                                  } else {
                                                    if(ct == 'Other benchmarks') {
                                                      
                                                      df <- infrsap_dat_bm_mod_modfied %>%
                                                        dplyr::filter(.data$Indicator == ic) %>%
                                                        dplyr::filter(.data$Sector %in% sc) %>%
                                                        dplyr::select(.data$`Grouping`,.data$`1990`:.data$`2020`) %>%
                                                        tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
                                                        tidyr::drop_na() %>%
                                                        dplyr::filter(.data$key >= yr[1], .data$key<=yr[2])
                                                      # get unique list of benchmarks
                                                      bn <- sort(unique(df$Grouping))
                                                      
                                                      bn_selected_cir <- infrasap::dat_country_income_region %>%
                                                                            dplyr::filter(.data$`Country Name` == input$data_country)
                                                      
                                                      all_bn <- c("East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East & North Africa",
                                                                  "North America","South Asia","Sub-Saharan Africa","High income","Low income","Lower middle income",
                                                                  "Upper middle income","Fragile","Isolated","Low Human Capital","Low Population Density","Mountainous","OECD members","Oil Exporter")
                                                      bn <- bn[order(match(bn, all_bn))]
                                                      
                                                      
                                                      bn <- list(
                                                                'Region' = all_bn[stringr::str_detect(all_bn,'Asia|America|Africa')],
                                                                'Income group' = all_bn[stringr::str_detect(all_bn,'income')],
                                                                'Exogenous' = all_bn[!stringr::str_detect(all_bn,'Asia|America|Africa|income')]
                                                      )
                                                      
                                                      
                                                      # bn_selected <- c(bn$Region[1], bn$`Income group`[1])
                                                      bn_selected <- c(bn_selected_cir$Region, bn_selected_cir$IncomeGroup)
                                                      
                                                      shiny::fluidRow(
                                                                      shiny::column(12,
                                                                                    shiny::selectizeInput(inputId = ns('data_benchmarks'),
                                                                                                          label = 'Select benchmark',
                                                                                                          choices = bn,
                                                                                                          selected = bn_selected,
                                                                                                          multiple = TRUE,
                                                                                                          options = list(
                                                                                                                         # maxItems = 3,
                                                                                                                         'plugins' = list('remove_button'),
                                                                                                                         'create' = TRUE,
                                                                                                                         'persist' = FALSE
                                                                                                          )
                                                                                    )
                                                               
                                                                     )
                                                      )
                                                      } else {
                                                      
                                                        if(ct == 'Other countries') {
                                                            if(cn!='') {
                                                              # subset data by indicator, sector, and year, and remove NAs
                                                              df <- infrasap_dat_mod_modified %>%
                                                                dplyr::filter(.data$`Indicator Name` == ic) %>%
                                                                dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
                                                                dplyr::select(Grouping = .data$`Country Name`,.data$Region,.data$`1990`:.data$`2020`) %>%
                                                                tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`,-.data$Region) %>%
                                                                tidyr::drop_na() %>%
                                                                dplyr::filter(.data$key >= yr[1], .data$key<=yr[2])
                                                              # get unique countries that meet the criteria
                                                              cn_choices <- sort(unique(df$Grouping))
                                                              cn_choices <- cn_choices[cn_choices != input$data_country]
                                                              
                                                              # if countries exist that meet the criteria above, then get the region
                                                              if(length(cn_choices) > 0) {
                                                                rn <- infrasap_dat_mod_modified %>%
                                                                        dplyr::filter(.data$`Country Name` == cn) %>%
                                                                        dplyr::pull(.data$Region) %>% unique()
                                                                
                                                                df <- df %>% dplyr::filter(.data$Region == rn)
                                                                cs = sort(unique(df$Grouping))
                                                                # sample one country as default
                                                                if(is.null(selected_vals$db_countries_name)) {
                                                                  cs <- sample(cs, 1)
                                                                } else {
                                                                  cs <- selected_vals$db_countries_name
                                                                }
                                                                
                                                                shiny::fluidRow(
                                                                                shiny::column(12,
                                                                                              shiny::selectizeInput(inputId = ns('data_countries'),
                                                                                                                    label = 'Select other countries',
                                                                                                                    choices = cn_choices,
                                                                                                                    selected = cs,
                                                                                                                    multiple = TRUE,
                                                                                                                    options = list(
                                                                                                                                   'plugins' = list('remove_button'),
                                                                                                                                   'create' = TRUE,
                                                                                                                                   'persist' = FALSE
                                                                                                                    )
                                                                                              )
                                                                                )
                                                                  
                                                                )
                                                              }
                                                             }
                                                           } else {
                                                              
                                                              # Other indicator
                                                              
                                                              if(is.null(sc)){
                                                                NULL
                                                              } else {
                                                                # subset data by sector and year and remove NAs
                                                                df <- infrasap_dat_mod_modified %>%
                                                                          dplyr::filter(.data$`Country Name` == cn) %>%
                                                                          dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
                                                                          dplyr::select(Grouping = .data$`Indicator Name`,.data$`1990`:.data$`2020`) %>%
                                                                          tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
                                                                          tidyr::drop_na()
                                                                
                                                                # get a unique list of indicators
                                                                ic <- sort(unique(df$Grouping))
                                                                measure_brackets <- regmatches(input$data_indicator, gregexpr("(?=\\().*?(?<=\\))", input$data_indicator, perl=T))[[1]]
                                                                
                                                                
                                                                if(length(measure_brackets) == 0){
                                                                  
                                                                  ic <- setdiff(ic, ic[stringr::str_detect(ic, pattern = '[\\(\\)]')])
                                                                  ic <- ic[ic != input$data_indicator]
                                                                  
                                                                } else {
                                                                  ic <- ic[grep(pattern = as.character(stringr::str_glue('\\({measure_brackets}\\)')), x = ic)]
                                                                  ic <- ic[ic != input$data_indicator]
                                                                }

                                                                shiny::fluidRow(
                                                                                shiny::column(12,
                                                                                              if(length(ic) > 0) {
                                                                                         
                                                                                                 shiny::selectizeInput(inputId = ns('other_indicator'),
                                                                                                                       label = 'Select an indicator',
                                                                                                                       choices = ic,
                                                                                                                       selected = ic[1],
                                                                                                                       multiple = TRUE,
                                                                                                                       options = list(
                                                                                                                                      'plugins' = list('remove_button'),
                                                                                                                                      'create' = TRUE,
                                                                                                                                      'persist' = FALSE
                                                                                                                       )
                                                                                                 )
                                                                                               } else { 
                                                                                                 shiny::div(id = "idicatorsAvailabilty", 'No indicators available...')
                                                                                               }
                                                                                       
                                                                                )
                                                              )
                                                            }
                                                          }
                                                    }
                                                  }
                                                }
    })
    
    
    # Reactive data set that compiles data based on data inputs
    data_tab <- shiny::reactive({
      shiny::req(input$data_sector, input$data_indicator, input$data_year, input$data_benchmarks)
      # shiny::req(input$data_compare_to)
      # get sector and year
      sc <- input$data_sector
      sc <- c(sc, 'Cross-cutting')
      cn <- input$data_country
      ic <- input$data_indicator
      yr <- input$data_year
      ct <- input$data_compare_to
      cp <- input$country_ports
      bn <- input$data_benchmarks
      cc <- input$data_countries
      oi <- input$other_indicator
      
      if(input$data_sector == 'Transport Port') {
        if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_country") {
          # get country data
          df <- infrasap::dat_ports %>%
            dplyr::filter(.data$`Country Name`== cn) %>%
            # filter(`Country Name`== "Jordan") %>%
            dplyr::filter(.data$`Indicator Name`== ic) %>%
            # filter(`Indicator Name`== 'Annual Deployed Capacity per Port') %>%
            dplyr::filter(.data$`Sub-national Unit Name` %in% c(ct, cp)) %>%
            # filter(`Sub-national Unit Name` %in% c('Aqaba Industrial', 'Aqaba')) %>%
            dplyr::select(Grouping = .data$`Country Name`,.data$`Sub-national Unit Name`,.data$`1990`:.data$`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`, -.data$`Sub-national Unit Name`) %>%
            tidyr::drop_na() %>%
            # filter(key >= yr[1], key<=yr[2])
            dplyr::filter(.data$key >= 2016, .data$key<=2020)
          
          return(df)
        } else {
          
          df_port <- infrasap::dat_ports %>%
            dplyr::filter(.data$`Country Name`== cn) %>%
            # filter(`Country Name`== "Jordan") %>%
            dplyr::filter(.data$`Indicator Name`== ic) %>%
            # filter(`Indicator Name`== 'Annual Deployed Capacity per Port') %>%
            dplyr::filter(.data$`Sub-national Unit Name` %in% c(cp)) %>%
            # filter(`Sub-national Unit Name` %in% c('Aqaba')) %>%
            dplyr:: select(Grouping = .data$`Country Name`,.data$`Sub-national Unit Name`,.data$`1990`:.data$`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`, -.data$`Sub-national Unit Name`) %>%
            tidyr::drop_na() %>%
            dplyr::filter(.data$key >= yr[1], .data$key<=yr[2])
            # filter(key >= 2016, key<=2020)
          #commenting the below code because when you select Port transport the below df_bench returns no rows 
          #resulting in an error and the dashboard still works correctly after that so commenting this code. 
          # if(!is.null(ct)) {
          #   df_bench <- infrasap::dat_ports_bm %>%
          #     # filter(`Indicator`== "Annual Deployed Capacity per Port") %>%
          #     dplyr::filter(.data$`Indicator`== ic) %>%
          #     # filter(Grouping %in% c("Europe & Central Asia", "North America")) %>%
          #     dplyr::filter(.data$Grouping %in% ct) %>%
          #     dplyr::select(.data$`Grouping`,.data$`1990`:.data$`2020`) %>%
          #     tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
          #     tidyr::drop_na() %>%
          # 
          #     # filter(key >= 2016, key<=2020) %>%
          #     dplyr::filter(.data$key >= yr[1], .data$key<=yr[2]) %>%
          #     dplyr::rename(`Sub-national Unit Name` = .data$Grouping) %>%
          #     dplyr:: mutate(Grouping = cn) %>%
          #     # mutate(Grouping = "Jordan") %>%
          #     dplyr::select(.data$Grouping, .data$`Sub-national Unit Name`, dplyr::everything())
          #   
          #   df <- rbind(df_port, df_bench)
          # } else {
            df <- df_port
          #}
          return(df)
        }
      } else {
        
        if(ct == 'Other benchmarks'){
          if(is.null(bn)){
            # get country data
            df <- infrasap_dat_mod_modified %>%
              dplyr::filter(.data$`Country Name`== cn) %>%
              indicator_trend_data_manipulation(ic, sc, yr)
            return(df)
            
          } else {
            # get benchmark data
            df_bm <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(.data$Indicator == ic) %>%
              # filter(Indicator == "Annual Deployed Capacity per Port") %>%
              dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
              # filter(Sector == 'Transport Port') %>%
              dplyr::filter(.data$Grouping %in% bn) %>%
              # filter(Grouping %in% c("East Asia & Pacific", "Europe & Central Asia")) %>%
              dplyr::select(.data$`Grouping`,.data$`1990`:.data$`2020`) %>%
              tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
              tidyr::drop_na() %>%
              dplyr::filter(.data$key >= yr[1], .data$key<=yr[2])
              
            # get country data
            df <- infrasap_dat_mod_modified %>%
              dplyr::filter(.data$`Country Name`== cn) %>%
              indicator_trend_data_manipulation(ic, sc, yr)
            
            # combine with benchmark data
            df <- rbind(df, df_bm)
            
            return(df)
          }
        } else {
          
          if(ct == 'Other countries') {
            if(is.null(cn)){
              NULL
            } else {
              # combine country_names with country_name
              country_names <- c(cn, cc)
              
              # get country data
              df <- infrasap_dat_mod_modified %>%
                dplyr::filter(.data$`Country Name`%in% country_names) %>%
                indicator_trend_data_manipulation(ic, sc, yr)
              return(df)
            }
          } else { 
            
            if(ct == 'Other indicators') {
              if(is.null(oi)){
                NULL
              } else {
                # combine country_names with country_name
                country_names <- c(cn, cc)
                
                # get country data
                df <- infrasap_dat_mod_modified %>%
                  dplyr::filter(.data$`Country Name`%in% cn) %>%
                  indicator_trend_data_manipulation(ic, sc, yr, col = "Indicator Name")
                
                dfother <- infrasap_dat_mod_modified %>%
                  dplyr::filter(.data$`Country Name`%in% cn) %>%
                  indicator_trend_data_manipulation(oi, sc, yr, col = "Indicator Name")
                df <- rbind(df, dfother)
                return(df)
              } 
            } else {NULL}
          }
        }
      }
    })
    
    
    # Return fields based on data selection type year (range of years or specific year)
    shiny::observe({
      
      if(input$data_selection_type_year == "Select a range of years"){  
        # get available years for the inputs selected
        output$data_year_ui <- shiny::renderUI({
          shiny::req(input$data_sector)
          # get sector and year
          sc <- input$data_sector
          sc <- c(sc, 'Cross-cutting')
          
          cn <- input$data_country
          ic <- input$data_indicator
          cp <- input$country_ports
          ct <- input$data_compare_to
          if(is.null(ic)){
            NULL
          } else {
            if(has_port(ic)){
              # subset data by sector and year and remove NAs
              df <- infrasap::dat_ports %>%
                dplyr::filter(.data$`Country Name` == cn) %>%
                dplyr::filter(.data$`Indicator Name`==ic) %>%
                dplyr::filter(.data$`Sub-national Unit Name` %in% c(cp, ct)) %>%
                dplyr::select(Grouping = .data$`Indicator Name`,.data$`Sub-national Unit Name`,.data$`1990`:.data$`2020`) %>%
                tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`, -.data$`Sub-national Unit Name`) %>%
                tidyr::drop_na()
              
              # get a unique list of indicators
              yr <- as.numeric(sort(unique(df$key)))
              shiny::fluidRow(
                shiny::column(12, id = "DataTabSliderWidth", 
                              shiny::sliderInput(ns('data_year'),
                                                 label = 'Select years',
                                                 min = min(yr),
                                                 max = max(yr),
                                                 value = c(min(yr), max(yr)),
                                                 step = 1,
                                                 sep = ""
                              )
                )
              )
            } else {
              # subset data by sector and year and remove NAs
              df <- infrasap_dat_mod_modified %>%
                dplyr::filter(.data$`Country Name` == cn) %>%
                dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
                dplyr::filter(.data$`Indicator Name`==ic) %>%
                dplyr::select(Grouping = .data$`Indicator Name`,.data$`1990`:.data$`2020`) %>%
                tidyr::gather(key = 'key', value = 'value',-.data$`Grouping`) %>%
                tidyr::drop_na()
              # get a unique list of indicators
              yr <- as.numeric(sort(unique(df$key)))
              if(length(yr)==0){
                NULL
              } else {
                shiny::fluidRow(
                  shiny::column(12, id = "DataTabSliderWidth", 
                                shiny::sliderInput(ns('data_year'),
                                                   label = 'Select years',
                                                   min = min(yr),
                                                   max = max(yr),
                                                   value = c(min(yr), max(yr)),
                                                   step = 1,
                                                   sep = "")
                  )
                )
              }
            }
            
          }
        })
        
        
        
        # plot country comparison or country/benchmark comparison
        output$data_chart <- plotly::renderPlotly({
          req(data_tab())
          ic <- input$data_indicator
          sc <- input$data_sector
          sc <- c(sc, 'Cross-cutting')
          
          cn <- input$data_country
          df <- data_tab()
          
          # HERE is where you need to condition the chart to do ports (and then make sure do the same for latest year avaiable) And make sure benchmark doesnt show up.
          
          if(is.null(df)){
            NULL
          } else if(nrow(df)==0){ 
                    empty_plot(title = 'No data for selected inputs')
          } else {
            
            if(has_port(sc)) {
              df$value <- round(as.numeric(df$value), 2)
              plot_title <- paste0(cn, ' : ',ic)
              if(open_bracket(ic)){
                y_axis <- unlist(lapply(strsplit(ic, '(', fixed = TRUE), function(x) x[length(x)]))
                y_axis <- paste0('(', y_axis)
              } else {
                y_axis <- 'Value'
              }
              
              if(input$ports_compare_to_indicator_type == "to_country") {
                  # text for plot
                  mytext <- paste(
                    "Value: ",round(df$value,2),"<br>",
                    "Year: ", as.character(df$key),"<br>",
                    "Port: ", as.character(df$`Sub-national Unit Name`),"<br>",
                    sep="") %>%
                    lapply(htmltools::HTML)
                  
                  col_pal <- get_colors(df$`Sub-national Unit Name`)
              } else {
                  mytext <- paste(
                    "Value: ",round(df$value,2),"<br>",
                    "Year: ", as.character(df$key),"<br>",
                    "Data: ", as.character(df$Grouping),"<br>",
                    sep="") %>%
                    lapply(htmltools::HTML)
                  
                  col_pal <- get_colors(df$Grouping) 
              }
              

              if(length(unique(df$key))<=4){
                
                if("Sub-national Unit Name" %in% colnames(df)) {
                  p <- indicator_trend_plot1(df, 'Sub-national Unit Name', col_pal, y_axis, plot_title, mytext, 'theme2')
                } else {
                  p <- indicator_trend_plot1(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme2')
                }
              } else {
                
                if("Sub-national Unit Name" %in% colnames(df)) {
                  p <- indicator_trend_plot2(df, 'Sub-national Unit Name', col_pal, y_axis, plot_title, mytext, 'theme2')
                } else {
                  p <- indicator_trend_plot2(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme2')
                }
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                # condition for showing hover over
                if(ic %in% irf_indicators){
                  fig <- plotly::ggplotly(p, tooltip = NULL) %>%
                    plotly::config(displayModeBar = F)
                  fig
                } else {
                  fig <- plotly::ggplotly(p, tooltip ='text') %>%
                    plotly::config(displayModeBar = F)
                  fig
                }
              }
            # Not Transport port graph
            } else {
              # make value numeric
              df$value <- round(as.numeric(df$value), 2)
              
              # get title and subtitle
              
              if(input$data_compare_to == 'Other indicators'){
                plot_title <- paste0(input$data_country)
              } else {
                plot_title <- paste0(ic)
              }
              if(open_bracket(ic)) {
                y_axis <- unlist(lapply(strsplit(ic, '(', fixed = TRUE), function(x) x[length(x)]))
                y_axis <- paste0('(', y_axis)
              } else {
                y_axis <- 'Value'
              }
              
              # text for plot
              mytext <- paste(
                "Value: ",round(df$value,2),"<br>",
                "Year: ", as.character(df$key),"<br>",
                "Data: ", as.character(df$Grouping),"<br>",
                sep="") %>%
                lapply(htmltools::HTML)
              col_pal <- get_colors(df$Grouping)
              if(length(unique(df$key))<=4){
                p <- indicator_trend_plot1(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme2')
              } else {
                p <- indicator_trend_plot2(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme2')
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                # condition for showing hover over
                if(ic %in% irf_indicators){
                  fig <- plotly::ggplotly(p, tooltip = NULL) %>%
                    plotly::config(displayModeBar = FALSE)
                  fig
                } else {
                  fig <- plotly::ggplotly(p, tooltip ='text') %>%
                    plotly::config(displayModeBar = FALSE)
                  fig
                }
              }
            }
          }
          
          
        })
        
      } else {
        output$data_year_ui <- shiny::renderUI({
          NULL
        })
        
        
        # plot country comparison or country/benchmark comparison
        output$data_chart <- plotly::renderPlotly({
          ic <- input$data_indicator
          sc <- input$data_sector
          cn <- input$data_country
          
          df <- data_tab()
          # save(df, sc, ic, file = 'temp_ports_2.rda')
          if(is.null(df)){
            NULL
          } else if(nrow(df)==0){
            empty_plot(title = 'No data for selected inputs')
          } else {
            
            if(has_port(sc)){
              # make value numeric
              df$value <- round(as.numeric(df$value), 2)
              
              df <- df %>%
                dplyr::mutate(check_missing = dplyr::case_when(
                  is.na(.data$value) ~ 1,
                  TRUE ~ 0
                )) %>%
                dplyr::group_by(.data$key) %>%
                dplyr::filter(.data$check_missing == 0) %>%
                dplyr::ungroup() %>%
                dplyr::select(-.data$check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% dplyr::filter(.data$key == lastyearavailable)
              
              # get title and subtitle
              plot_title <- paste0(cn, ' : ', ic)
              if(open_bracket(ic)){
                y_axis <- unlist(lapply(strsplit(ic, '(', fixed = TRUE), function(x) x[length(x)]))
                y_axis <- paste0('(', y_axis)
              } else {
                y_axis <- 'Value'
              }
              
              # text for plot
              mytext <- paste(
                "Value: ",round(df$value,2),"<br>",
                "Year: ", as.character(df$key),"<br>",
                "Data: ", as.character(df$`Sub-national Unit Name`),"<br>",
                sep="") %>%
                lapply(htmltools::HTML)
              col_pal <- get_colors(df$`Sub-national Unit Name`)
              if(length(unique(df$key))<=4){
                p <- indicator_trend_plot1(df, 'Sub-national Unit Name', col_pal, y_axis, plot_title, mytext, 'theme1')
              } else {
                p <- indicator_trend_plot2(df, 'Sub-national Unit Name', col_pal, y_axis, plot_title, mytext, 'theme1')
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- plotly::ggplotly(p, tooltip = 'text') %>%
                  plotly::config(displayModeBar = F)
                fig
              }
            } else{
              # make value numeric
              df$value <- round(as.numeric(df$value), 2)

              df <- df %>%
                dplyr::mutate(check_missing = dplyr::case_when(
                  is.na(.data$value) ~ 1,
                  TRUE ~ 0
                )) %>%
                dplyr::group_by(.data$key) %>%
                dplyr::filter(.data$check_missing == 0) %>%
                dplyr::ungroup() %>%
                dplyr::select(-.data$check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% dplyr::filter(.data$key == lastyearavailable)
              
              # get title and subtitle
              if(input$data_compare_to == 'Other indicators'){
                plot_title <- paste0(input$data_country)
              } else {
                plot_title <- paste0("Indicator: ", ic)
              }
              if(open_bracket(ic)){
                y_axis <- unlist(lapply(strsplit(ic, '(', fixed = TRUE), function(x) x[length(x)]))
                y_axis <- paste0('(', y_axis)
              } else {
                y_axis <- 'Value'
              }
              
              # text for plot
              mytext <- paste(
                "Value: ",round(df$value,2),"<br>",
                "Year: ", as.character(df$key),"<br>",
                "Data: ", as.character(df$Grouping),"<br>",
                sep="") %>%
                lapply(htmltools::HTML)
              col_pal <- get_colors(df$Grouping)
              if(length(unique(df$key))<=4){
                p <- indicator_trend_plot1(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme1')
              } else {
                p <- indicator_trend_plot2(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme1')
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- plotly::ggplotly(p, tooltip = 'text') %>%
                  plotly::config(displayModeBar = F)

                fig
              }
            }
            
          }
          
        })
        
      }
      
    })
    
    
    # button group
    output$btn_data_access <- shiny::renderUI({
      if(!is.null(input$data_sector) && input$data_sector != "Transport Road") {
        div(id = "btn_groups",
            shiny::downloadButton(session$ns('downloadDataFilteredCSV'), 'Download Table'),
            shiny::uiOutput(session$ns('buttonByIndicator')),
            shiny::downloadButton(session$ns('downloadPlot'), 'Download Chart')
        ) 
      } else {
        div(id = "btn_groups",
            shiny::downloadButton(session$ns('downloadPlot'), 'Download Chart')
        ) 
      }
    })
    
    # Table with download
    output$data_table_access <- shiny::renderUI({
      shiny::req(input$data_indicator)
      ind_name <- input$data_indicator
      # message('should be ' ,ind_name %in% irf_indicators)
      if(!ind_name %in% irf_indicators){
        output$data_table <- DT::renderDataTable({
          sector_name <- input$data_sector
          df <- data_tab()
          if(is.null(df)){
            NULL
          } else if(nrow(df)==0){
            NULL
          } else {
            num_cols <- unique(df$key)
            # spread data
            df <- df %>% tidyr::spread(key = 'key', value = 'value')
            DT::datatable(df,
                          extensions = 'Buttons',
                          escape=FALSE,
                          options = list(scrollX = TRUE,
                                         pageLength = nrow(df),
                                         info = FALSE,
                                         dom='frtip',
                                         buttons = list('csv')
                          ),
                          selection = 'none'
            ) %>%
            DT::formatRound(columns=num_cols, digits=2) %>%
            #Thousand number separator for columns to compare along with benchmark
            DT::formatCurrency(columns = num_cols, currency = "", interval = 3, mark = ",")
          }
        })
        
        DT::dataTableOutput(session$ns('data_table'))
      } else {
        shiny::h2(style = 'color:#28323d', 'This data is available on request. Please contact the Library Network for access')
      }
      
    })
    
    
    
    # Reactive data frame prepared to download data by all indicators
    data_tab_all_indc <- shiny::reactive({
      ic <- input$data_indicator
      # get data
      df <- infrasap_dat_mod_modified %>% dplyr::filter(.data$`Indicator Name` == ic)
      return(df)
    })
    
    
    # Reactive data frame to download data by specific indicator
    data_tab_filtered_csv <- shiny::reactive({
      df <- data_tab()
      # make value numeric
      df$value <- round(as.numeric(df$value), 2)
      # spread data
      df <- df %>% tidyr::spread(key = 'key', value = 'value')
      return(df)
    })
    
    
    # Download button fucntionality for all indicators
    output$downloadDataAllIndicator <- shiny::downloadHandler(
      filename = function() {
        paste0(stringr::str_glue('{input$data_indicator}'), ".csv")
      },
      content = function(file) {
        utils::write.csv(data_tab_all_indc(), file)
      }
    )
    
    # Download button functionality for specific indicator
    output$downloadDataFilteredCSV <- shiny::downloadHandler(
      filename = function() {
        paste0(stringr::str_glue('{input$data_indicator}'), ".csv")
      },
      content = function(file) {
        utils::write.csv(data_tab_filtered_csv(), file)
      }
    )
    
    
    # Reactive data with prepared chart
    data_chart_download <- shiny::reactive({
      ic <- input$data_indicator
      sc <- input$data_sector
      df <- data_tab()
      
      if(is.null(df)){
        NULL
      } else if(nrow(df)==0){
        empty_plot(title = 'No data for selected inputs')
      } else {
        # make value numeric
        df$value <- round(as.numeric(df$value), 2)
        
        # get title and subtitle
        if(input$data_compare_to == 'Other indicators'){
          plot_title <- paste0(input$data_country)
        } else {
          plot_title <- paste0("Indicator: ", ic)
        }
        
        # text for plot
        mytext <- paste(
          "Value: ",round(df$value,2),"<br>",
          "Year: ", as.character(df$key),"<br>",
          "Data: ", as.character(df$Grouping),"<br>",
          sep="") %>%
          lapply(htmltools::HTML)
        col_pal <- get_colors(df$Grouping)
        if(length(unique(df$key))<=4){
          p <- indicator_trend_plot1(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme1')
        } else {
          p <- indicator_trend_plot2(df, 'Grouping', col_pal, y_axis, plot_title, mytext, 'theme1')
        }
        
        if(nrow(p$data)==0){
          NULL
        } else {
          fig <- p
          fig
        }
      }
      
    })
    
    # Plot download button functionality 
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() { paste0(input$data_indicator, '.png', sep='') },
      content = function(file) {
        ggplot2::ggsave(file, plot = data_chart_download(), device = "png", height=12, width=15)
      }
    )
    
    # Download button by indicator functionality
    output$buttonByIndicator <- shiny::renderUI({
      shiny::downloadButton(ns('downloadDataAllIndicator'), stringr::str_glue("Download Indicator Data"))
    })
  })
}
    
## To be copied in the UI
# mod_indicator_trend_tab_module_ui("indicator_trend_tab_module_ui_1")
    
## To be copied in the server
# mod_indicator_trend_tab_module_server("indicator_trend_tab_module_ui_1")
