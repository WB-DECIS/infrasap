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
  
  ns <- NS(id)
  tagList(
    div(class = "controlSection",
        fluidRow(
          column(8, class = "column-eight-width",
            column(6,
                   selectInput(inputId = ns('data_country'), 
                               label = '1. Select country',
                               choices = sort(unique(dat$`Country Name`)),
                               selected = 'Jordan'
                               ),
                   uiOutput(ns('data_indicator_ui'))
                   
            ),
            column(6,
                   uiOutput(ns('data_sector_ui')),
                   selectInput(ns('data_selection_type_year'), 
                               label = 'Type of selection: ',
                               choices = c('Select latest year available', 'Select a range of years'),
                               selected = 'Select a range of years'
                   )
            ),
            uiOutput(ns('data_year_ui'))
            
          ),
          column(4,
                 uiOutput(ns('country_ports_ui')),
                 # uiOutput(ns('data_compare_to_ui')),
                 uiOutput(ns('ports_compare_to_indicator_type_ui')),
                 # selectInput(ns('data_compare_to'),
                 #             label = NULL,
                 #             choices = NULL,
                 #             selected = NULL
                 # ),
                 uiOutput(ns('data_compare_to_ui')),
                 uiOutput(ns('data_benchmark_ui')),
          )
          
          
        ),
    
        
    ),
    div(
      fluidRow(
        column(12,
               align = 'center',
               plotly::plotlyOutput(ns('data_chart'),
                            width = '900px', 
                            height = "auto"
                            ) %>% shinycssloaders::withSpinner(type = 7,color = "#154164")
        )
      ),
      
      br(), 
      br(),
      
      fluidRow(
        column(12,
               uiOutput(ns("btn_data_access"))
        )
      ),
      
      br(), 
      br(),
      
      fluidRow(
        column(12,
               align = 'center',
               uiOutput(ns('data_table_access'),
                                   width = '900px')
               )
      )       
    )
  )
}
    
#' indicator_trend_tab_module Server Functions
#'
#' @noRd 
mod_indicator_trend_tab_module_server <- function(id){
  
  
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "Transport"] <- "Transport cross-cutting"
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "Transport"] <- "Transport cross-cutting"
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"
  
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    # Module Body
    
    #------- Initialize the Memory ----------
    selected_vals = reactiveValues(
      db_countries_name = NULL,
      data_benchmarks_name = NULL,
      data_ports_name = NULL
      
    )
    
    observe({
      req(input$data_countries, 
          input$data_benchmarks, 
          input$data_country, 
          input$data_compare_to, 
          input$data_sector, 
          input$data_indicator, 
          input$data_year,
          input$country_ports)
      
      input$data_country
      input$data_sector
      # input$data_compare_to
 
      selected_vals$db_countries_name <- input$data_countries
      selected_vals$data_benchmarks_name <- input$data_benchmarks
      selected_vals$data_ports_name <- input$country_ports
      
    })
    
    observeEvent(c(input$data_country, 
                   input$data_compare_to, 
                   input$data_sector, 
                   input$data_indicator, 
                   input$data_year,
                   input$country_ports
                   # ,
                   # input$data_benchmarks
                   # input$data_sector
                   # input$data_country
                   # input$data_indicator
                   # input$data_year
                   # input$data_compare_to
                   # input$country_ports
                   # input$data_benchmarks
                   # input$data_countries
                   # input$other_indicator
                   ),{
      selected_vals$data_benchmarks_name <- input$data_benchmarks
      selected_vals$data_ports_name <- input$country_ports
    })
    
    observeEvent(input$data_country, {
      
      
      bn_selected_cir <- infrasap::dat_country_income_region %>%
        filter(`Country Name` == input$data_country)
      
      updateSelectInput(session, "data_benchmarks",
                        selected = c(bn_selected_cir$Region, bn_selected_cir$IncomeGroup)
      )
    })
    
    
    # Compare to UI
    output$data_compare_to_ui <- renderUI({

      if(input$ports_compare_to_indicator_type == "to_regional_bench" && input$data_sector == 'Transport Port') {
        selectizeInput(ns('data_compare_to'),
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
        if(!is.null(input$data_sector)){
          selectInput(ns('data_compare_to'),
                      label = NULL,
                      choices = NULL,
                      selected = NULL
          )
        } else {
          selectInput(ns('data_compare_to'),
                      label = NULL,
                      choices = NULL,
                      selected = NULL
          )
        }

      } 
    })
    
    # get sectors based on country input
    output$data_sector_ui <- renderUI({
      cn <- input$data_country
      # save(cn, file = 'test_cn.rda')
      # subset data by country selected
      df <- infrasap_dat_mod_modified %>% dplyr::filter(`Country Name` == cn)
      sc_choices <- sort(unique(df$`Indicator Sector`))
      # sc_choices <- sc_choices[sc_choices != 'National']
      
      selectInput(inputId = ns('data_sector'),
                  label = '2. Select sector',
                  choices = sc_choices,
                  selected = sc_choices[2])
    })
    
    output$country_ports_ui <- renderUI({
      req(input$data_country)
      req(input$data_sector)
      # req(input$country_ports)
      
      cn <- input$data_country
      sc <- input$data_sector
      # message('in country ports ui')
      # message('Sector is ', sc)
      # save(sc, file = 'sc.rda')
      if(is.null(sc)){
        NULL
      } else {
        if(!grepl('Port', sc)){
          NULL
        } else {
          df <- infrasap::dat_ports
          df <- df %>% dplyr::filter(`Country Name` == cn)
          # port_choices <- sort(unique(df$`Sub-national Unit Name`))
          port_choices <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != input$data_compare_to ]
          # port_choices <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != input$country_ports ]
          if(selected_vals$data_ports_name %in% port_choices && !is.null(selected_vals$data_ports_name)) {
            port_choice_selected <- selected_vals$data_ports_name
          } else {
            port_choice_selected <- port_choices[1]
          }
          selectInput(ns('country_ports'),
                      label = paste0('Choose a port from ',cn ),
                      choices = port_choices,
                      selected = port_choice_selected
          )
        }
      }
    })
    
    output$ports_compare_to_indicator_type_ui <- renderUI({
      req(input$data_country)
      req(input$data_sector)
      
      cn <- input$data_country
      sc <- input$data_sector
      # message('in country ports ui')
      # message('Sector is ', sc)
      # save(sc, file = 'sc.rda')
      if(is.null(sc)){
        NULL
      } else {
        if(!grepl('Port', sc)){
          NULL
        } else {
          
          selectInput(ns('ports_compare_to_indicator_type'),
                      label = 'Compare to:',
                      choices = NULL,
                      selected = NULL
          )
        }
      }
    })
    
    
    
    observeEvent(c(input$data_sector), {
      req(input$data_sector)
      # req(input$data_country)
      
      
      if(grepl('Port', input$data_sector)){
        
        sc <- input$data_sector
        cn <- input$data_country
        
        
        if(is.null(input$country_ports)) {
          df <- infrasap::dat_ports
          df <- df %>% dplyr::filter(`Country Name` == cn)
          port_choices <- sort(unique(df$`Sub-national Unit Name`))
          cp <- port_choices[1]
        } else {
          cp <- input$country_ports
        }
        
          

        country_choice <- as.character(stringr::str_glue('Compare to other ports in country {cn}'))
        choices_ports_compare_to_indicator_type <- c('to_country', 'to_regional_bench', 'to_volume_bench')
        names(choices_ports_compare_to_indicator_type) <- c(country_choice,
                                                            'Compare to regional benchmarks',
                                                            'Compare to Volume benchmarks')
        updateSelectInput(session, "ports_compare_to_indicator_type",
                          choices = choices_ports_compare_to_indicator_type,
                          selected = 'to_country'
        )
        
        observeEvent(input$ports_compare_to_indicator_type, {
          req(input$ports_compare_to_indicator_type)
          if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_country") {
          
          print("Yes, to country")
              
            df <- infrasap::dat_ports %>% 
              dplyr::filter(`Country Name` == cn) %>% 
              dplyr::filter(`Sub-national Unit Name` != cp)

            compare_ports <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != cp ]
            
            
            updateSelectInput(session, "data_compare_to",
                              label = paste0('Compare to other ports from ',cn, ':'),
                              choices = compare_ports,
                              selected = compare_ports[1]
            )
          
          } else {
            if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_regional_bench") {
              
              updateSelectizeInput(session, "data_compare_to",
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
              
              print('Yes, to_regional_bench')
              
            } else {
              
              if(!is.null(input$ports_compare_to_indicator_type) && input$ports_compare_to_indicator_type == "to_volume_bench") {
                
                updateSelectInput(session, "data_compare_to",
                                  label = "Compare ports by volume",
                                  choices = c('Small', 'Medium', 'Large', 'Upper 25 Percentile'),
                                  selected = 'Small'
                )
                
                print("Yes, to_volume_bench")
                
              } else {
                print("No")
              }
              
            }
            
          }
          
          
        })
        
        
      } else {
        updateSelectInput(session, "data_compare_to",
                          label = 'Compare to: ',
                          choices = c('Other countries', 'Other benchmarks', 'Other indicators'),
                          selected = 'Other benchmarks'
        )
      }
      
      
    })
    
    
    # observeEvent(input$country_ports, {
    # 
    #   print(1)
    #   cn <- input$data_country
    #   cp <- input$country_ports
    #   df <- infrasap::dat_ports %>% dplyr::filter(`Country Name` == cn) %>% dplyr::filter(`Sub-national Unit Name` != cp)
    #   compare_ports <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != cp ]
    #   print("Country Ports:...231")
    #   print(compare_ports)
    #   # save(compare_ports, file = 'temp1.rda')
    #   
    #   updateSelectInput(session, "data_compare_to",
    #                     choices = compare_ports
    #                     # ,
    #                     # selected = compare_ports[1]
    #                     )
    #   # cn <- input$data_country
    #   # df <- infrasap::dat_ports
    #   # df <- df %>% dplyr::filter(`Country Name` == cn)
    #   # port_choices <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != input$data_compare_to ]
    #   # print(port_choices)
    #   # 
    #   # updateSelectInput(session, "country_ports",
    #   #                   choices = port_choices
    #   # )
    # })
    
    # output$data_compare_ports_ui <- renderUI({
    #   req(input$data_sector)
    #   
    #   cp <- input$country_ports
    #   sc <- input$data_sector
    #   if(!grepl('Port', sc)){
    #     NULL
    #   } else {
    #     cn <- input$data_country
    #     save(cp, cn, file = 'temp.rda')
    #     
    #     # remove port selected above for comparison
    #     df <- infrasap::dat_ports %>% dplyr::filter(`Country Name` == cn)%>% dplyr::filter(`Sub-national Unit Name` != cp)
    #     compare_ports <- sort(unique(df$`Sub-national Unit Name`))
    #     save(compare_ports, file = 'temp1.rda')
    #     
    #     selectInput(ns('data_compare_to'), 
    #                 label = paste0('Compare to other ports from ',cn, ':'),
    #                 choices = compare_ports,
    #                 selected = compare_ports[1]
    #     )
    #   }
    # })
    
    # UI for indicators, based on sector and year selection
    output$data_indicator_ui <- renderUI({
      # get sector and year
      sc <- input$data_sector
      sc <- c(sc, 'Cross-cutting')
      cn <- input$data_country
      cp <- input$data_compare_to
      ct <- input$country_ports
      if(is.null(sc)){
        NULL
      } else {
        if(grepl('Port', sc)){
          df <- infrasap::dat_ports %>%
            dplyr::filter(`Country Name` == cn) %>%
            dplyr::filter(`Sub-national Unit Name` %in% c(cp, ct)) %>%
            dplyr::select(Grouping = `Indicator Name`,`Sub-national Unit Name`,`1990`:`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
            tidyr::drop_na()
          # get a unique list of indicators
          ic <- sort(unique(df$Grouping))
          fluidRow(
            column(12,
                   
                   selectInput(inputId = ns('data_indicator'),
                               label = '3. Select an indicator',
                               choices = ic,
                               selected = ic[1]))
          )
        } else {
          # subset data by sector and year and remove NAs
          df <- infrasap_dat_mod_modified %>%
            # dplyr::filter(`Country Name` == "Jordan") %>%
            dplyr::filter(`Country Name` == cn) %>%
            # dplyr::filter(`Indicator Sector` %in% c('Digital Development')) %>%
            dplyr::filter(`Indicator Sector` %in% input$data_sector) %>%
            dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
            tidyr::drop_na()
          
          # df_national <- infrasap_dat_mod_modified %>%
          #   # dplyr::filter(`Country Name` == "Jordan") %>%
          #   dplyr::filter(`Country Name` == cn) %>%
          #   dplyr::filter(`Indicator Sector` %in% c('National')) %>%
          #   # dplyr::filter(`Indicator Sector` %in% sc) %>%
          #   dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
          #   tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
          #   tidyr::drop_na()
          
          
          # get a unique list of indicators
          # ic_sc <- sort(unique(df$Grouping))
          ic <- sort(unique(df$Grouping))
          # ic_national <- sort(unique(df_national$Grouping))
          
          # ic <- list(
          #   'Selected sector name' = ic_sc,
          #   'National' = ic_national
          # )
          # 
          # names(ic)[1] <- input$data_sector
          
          fluidRow(
            column(12,
                   
                   selectInput(inputId = ns('data_indicator'),
                               label = '3. Select an indicator',
                               choices = ic,
                               selected = ic[1]))
          )
        }
       
      }
    })
  
  
    
    # UI for benchmarks or countries depending on "data_compare_to" input
    output$data_benchmark_ui <- renderUI({
      sc <- input$data_sector
      sc <- c(sc, 'Cross-cutting')
      cn <- input$data_country
      ic <- input$data_indicator
      yr <- input$data_year
      ct <- input$data_compare_to
      # save(ct, file = 'temp_ct.rda')
      if(is.null(yr)){
        NULL
      } else {
        if(grepl('Port', sc) ){
          selected_vals$data_benchmarks_name <<- NULL
          NULL
        } else {
          if(ct == 'Other benchmarks'){

            df <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Indicator == ic) %>%
              dplyr::filter(Sector %in% sc) %>%
              dplyr::select(`Grouping`,`1990`:`2020`) %>%
              tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
              tidyr::drop_na() %>%
              dplyr::filter(key >= yr[1], key<=yr[2])
            
            # get unique list of benchmarks
            bn <- sort(unique(df$Grouping))
            
            bn_selected_cir <- infrasap::dat_country_income_region %>%
              filter(`Country Name` == input$data_country)
            
            # if(is.null(selected_vals$data_benchmarks_name)) {
            #   bn_selected <- bn[c(7,8)]
            # } else {
            #   bn_selected <- selected_vals$data_benchmarks_name
            # }
            
            
            all_bn <- c("East Asia & Pacific","Europe & Central Asia","Latin America & Caribbean","Middle East & North Africa","North America","South Asia","Sub-Saharan Africa","High income","Low income","Lower middle income","Upper middle income","Fragile","Isolated","Low Human Capital","Low Population Density","Mountainous","OECD members","Oil Exporter")
            bn <- bn[order(match(bn, all_bn))]
            
            
            bn <- list(
              'Region' = all_bn[str_detect(all_bn,'Asia|America|Africa')],
              'Income group' = all_bn[str_detect(all_bn,'income')],
              'Exogenous' = all_bn[!str_detect(all_bn,'Asia|America|Africa|income')]
            )
            
            
            # bn_selected <- c(bn$Region[1], bn$`Income group`[1])
            bn_selected <- c(bn_selected_cir$Region, bn_selected_cir$IncomeGroup)
            
            fluidRow(
              column(12,
                     
                     
                     selectizeInput(inputId = ns('data_benchmarks'),
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
              if(cn!=''){
                # subset data by indicator, sector, and year, and remove NAs
                df <- infrasap_dat_mod_modified %>%
                  dplyr::filter(`Indicator Name` == ic) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::select(Grouping = `Country Name`,Region,`1990`:`2020`) %>%
                  tidyr::gather(key = 'key', value = 'value',-`Grouping`,-Region) %>%
                  tidyr::drop_na() %>%
                  dplyr::filter(key >= yr[1], key<=yr[2])
                
                # get unique countries that meet the criteria
                cn_choices <- sort(unique(df$Grouping))
                cn_choices <- cn_choices[cn_choices != input$data_country]
                
                # if countries exist that meet the criteria above, then get the region
                if(length(cn_choices)> 0){
                  rn <- infrasap_dat_mod_modified %>%
                    dplyr::filter(`Country Name` == cn) %>%
                    .$Region
                  
                  rn <- unique(rn)
                  df <- df %>% dplyr::filter(Region == rn)
                  cs = sort(unique(df$Grouping))
                  # sample one country as default
                  if(is.null(selected_vals$db_countries_name)) {
                    cs <- sample(cs, 1)
                  } else {
                    cs <- selected_vals$db_countries_name
                  }
                  
                  fluidRow(
                    column(12,
                           selectizeInput(inputId = ns('data_countries'),
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
                  dplyr::filter(`Country Name` == cn) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
                  tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
                  tidyr::drop_na()
                
                # get a unique list of indicators
                ic <- sort(unique(df$Grouping))
                measure_brackets <- regmatches(input$data_indicator, gregexpr("(?=\\().*?(?<=\\))", input$data_indicator, perl=T))[[1]]
                
                
                if(length(measure_brackets) == 0){
                  
                  ic <- setdiff(ic, ic[dplyr::str_detect(ic, pattern = '[\\(\\)]')])
                  ic <- ic[ic != input$data_indicator]
                  
                } else {
                  ic <- ic[grep(pattern = as.character(stringr::str_glue('\\({measure_brackets}\\)')), x = ic)]
                  ic <- ic[ic != input$data_indicator]
                }
                
                
                
                fluidRow(
                  column(12,
                         
                         if(length(ic) > 0) {
                           
                           selectizeInput(inputId = ns('other_indicator'),
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
                           tags$div(id = "idicatorsAvailabilty", 'No indicators available...')
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
    data_tab <- reactive({
      req(input$data_sector)
      # req(input$data_compare_to)
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
      
      # print(input$data_sector)
      # print(input$country_ports)
      
      if(input$data_sector == 'Transport Port') {
        # print('Trans')
        
        # print(input$data_compare_to)
        # print(input$ports_compare_to_indicator_type)
        # print(cn)
        # print(ic)
        # print(ct)
        # print(cp)
        # 
        # [1] "Aqaba Industrial"
        # [1] "to_country"
        # [1] "Jordan"
        # [1] "Annual Deployed Capacity per Port"
        # [1] "Aqaba Industrial"
        # [1] "Aqaba"
        
        if(input$ports_compare_to_indicator_type == "to_country") {
          # get country data
          df <- infrasap::dat_ports %>%
            dplyr::filter(`Country Name`== cn) %>%
            # filter(`Country Name`== "Jordan") %>%
            dplyr::filter(`Indicator Name`== ic) %>%
            # filter(`Indicator Name`== 'Annual Deployed Capacity per Port') %>%
            dplyr::filter(`Sub-national Unit Name` %in% c(ct, cp)) %>%
            # filter(`Sub-national Unit Name` %in% c('Aqaba Industrial', 'Aqaba')) %>%
            dplyr::select(Grouping = `Country Name`,`Sub-national Unit Name`,`1990`:`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
            tidyr::drop_na() %>%
            # filter(key >= yr[1], key<=yr[2])
            dplyr::filter(key >= 2016, key<=2020)
          # print(df)
          
          return(df)
        } else {
          

          df_port <- infrasap::dat_ports %>%
            dplyr::filter(`Country Name`== cn) %>%
            # filter(`Country Name`== "Jordan") %>%
            dplyr::filter(`Indicator Name`== ic) %>%
            # filter(`Indicator Name`== 'Annual Deployed Capacity per Port') %>%
            dplyr::filter(`Sub-national Unit Name` %in% c(cp)) %>%
            # filter(`Sub-national Unit Name` %in% c('Aqaba')) %>%
            dplyr:: select(Grouping = `Country Name`,`Sub-national Unit Name`,`1990`:`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
            tidyr::drop_na() %>%
            dplyr::filter(key >= yr[1], key<=yr[2])
            # filter(key >= 2016, key<=2020)
          
          df_bench <- infrasap::dat_ports_bm %>%
            # filter(`Indicator`== "Annual Deployed Capacity per Port") %>%
            dplyr::filter(`Indicator`== ic) %>%
            # filter(Grouping %in% c("Europe & Central Asia", "North America")) %>%
            dplyr::filter(Grouping %in% ct) %>%
            dplyr::select(`Grouping`,`1990`:`2020`) %>%
            tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
            tidyr::drop_na() %>%

            # filter(key >= 2016, key<=2020) %>%
            dplyr::filter(key >= yr[1], key<=yr[2]) %>%
            dplyr::rename(`Sub-national Unit Name` = Grouping) %>%
            dplyr:: mutate(Grouping = cn) %>%
            # mutate(Grouping = "Jordan") %>%
            dplyr::select(Grouping, `Sub-national Unit Name`, everything())
          
          df <- rbind(df_port, df_bench)
          
          # print(df)  
          
          return(df)
          
        }

        
      } else {
        # print('Not Trans')
        # print(ct)
        
        if(ct == 'Other benchmarks'){
          if(is.null(bn)){
            NULL
          } else {
            
            # get benchmark data
            df_bm <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Indicator == ic) %>%
              # filter(Indicator == "Annual Deployed Capacity per Port") %>%
              dplyr::filter(Sector %in% sc) %>%
              # filter(Sector == 'Transport Port') %>%
              dplyr::filter(Grouping %in% bn) %>%
              # filter(Grouping %in% c("East Asia & Pacific", "Europe & Central Asia")) %>%
              dplyr::select(`Grouping`,`1990`:`2020`) %>%
              tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
              tidyr::drop_na() %>%
              filter(key >= yr[1], key<=yr[2])
              
            
            # get country data
            df <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Country Name`== cn) %>%
              # filter(`Country Name`== "Jordan") %>%
              dplyr::filter(`Indicator Name` == ic) %>%
              # filter(`Indicator Name` == "Annual Deployed Capacity per Port") %>%
              dplyr::filter(`Indicator Sector` %in% sc) %>%
              # filter(`Indicator Sector` %in% "Transport Port") %>%
              dplyr::select(Grouping = `Country Name`,`1990`:`2020`) %>%
              tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
              tidyr::drop_na() %>%
              dplyr::filter(key >= yr[1], key<=yr[2])
              # filter(key >= 2016, key<=2020)
           
            
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
                dplyr::filter(`Country Name`%in% country_names) %>%
                dplyr::filter(`Indicator Name` == ic) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::select(Grouping = `Country Name`,`1990`:`2020`) %>%
                tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
                tidyr::drop_na() %>%
                dplyr::filter(key >= yr[1], key<=yr[2])
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
                  dplyr::filter(`Country Name`%in% cn) %>%
                  dplyr::filter(`Indicator Name` == ic) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
                  tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
                  tidyr::drop_na() %>%
                  dplyr::filter(key >= yr[1], key<=yr[2])
                
                dfother <- infrasap_dat_mod_modified %>%
                  dplyr::filter(`Country Name`%in% cn) %>%
                  dplyr::filter(`Indicator Name` %in% oi) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
                  tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
                  tidyr::drop_na() %>%
                  dplyr::filter(key >= yr[1], key<=yr[2])
                
                
                df <- rbind(df, dfother)
                
                return(df)
                
                
              } 
              
            } else {NULL}
            
          }
          
          
        }
        
      }
      # if(!ct %in% c('Other benchmarks', 'Other countries', 'Other indicators') ){
      # 
      #   # get country data
      #   df <- infrasap::dat_ports %>%
      #     dplyr::filter(`Country Name`== cn) %>%
      #     dplyr::filter(`Indicator Name`== ic) %>%
      #     dplyr::filter(`Sub-national Unit Name` %in% c(ct, cp)) %>%
      #     dplyr::select(Grouping = `Country Name`,`Sub-national Unit Name`,`1990`:`2021`) %>%
      #     tidyr::gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
      #     tidyr::drop_na() %>%
      #     dplyr::filter(key >= yr[1], key<=yr[2])
      #   
      #   
      #   # combine with benchmark data
      #   # df <- rbind(df, df_bm)
      #   print(df)
      #   
      #   return(df)
      # } else {
      #   
      # }
      
      
    })
    
    
    # Return fields based on data selection type year (range of years or specific year)
    observe({
      
      if(input$data_selection_type_year == "Select a range of years"){  
        # get available years for the inputs selected
        output$data_year_ui <- renderUI({
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
            if(grepl('Port', ic)){
              # subset data by sector and year and remove NAs
              df <- infrasap::dat_ports %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Name`==ic) %>%
                dplyr::filter(`Sub-national Unit Name` %in% c(cp, ct)) %>%
                dplyr::select(Grouping = `Indicator Name`,`Sub-national Unit Name`,`1990`:`2020`) %>%
                tidyr::gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
                tidyr::drop_na()
              
              # get a unique list of indicators
              yr <- as.numeric(sort(unique(df$key)))
              fluidRow(
                column(12, id = "DataTabSliderWidth", 
                       sliderInput(ns('data_year'),
                                   label = 'Select years',
                                   min = min(yr),
                                   max = max(yr),
                                   value = c(min(yr), max(yr)),
                                   step = 1,
                                   sep = "")
                )
              )
            } else {
              # subset data by sector and year and remove NAs
              df <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::filter(`Indicator Name`==ic) %>%
                dplyr::select(Grouping = `Indicator Name`,`1990`:`2020`) %>%
                tidyr::gather(key = 'key', value = 'value',-`Grouping`) %>%
                tidyr::drop_na()
              
              # get a unique list of indicators
              yr <- as.numeric(sort(unique(df$key)))
              if(length(yr)==0){
                NULL
              } else {
                fluidRow(
                  column(12, id = "DataTabSliderWidth", 
                         sliderInput(ns('data_year'),
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
          ic <- input$data_indicator
          sc <- input$data_sector
          sc <- c(sc, 'Cross-cutting')
          
          cn <- input$data_country
          df <- data_tab()
          # save(df, file = 'temp_ports.rda')
          
          # print("Plot part ...")
          # print(df)
          
          # HERE is where you need to condition the chart to do ports (and then make sure do the same for latest year avaiable) And make sure benchmark doesnt show up.
          
          if(is.null(df)){
            NULL
          } else if(nrow(df)==0){
            empty_plot(title = 'No data for selected inputs')
          } else {
            
            if(grepl('Port', sc)){
              df$value <- round(as.numeric(df$value), 2)
              plot_title <- paste0(cn, ' : ',ic)
              if(grepl('(', ic, fixed = TRUE)){
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
                  
                  col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$`Sub-national Unit Name`)), name = 'Set1')
              } else {
                  mytext <- paste(
                    "Value: ",round(df$value,2),"<br>",
                    "Year: ", as.character(df$key),"<br>",
                    "Data: ", as.character(df$Grouping),"<br>",
                    sep="") %>%
                    lapply(htmltools::HTML)
                  
                  col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
              }
              

              if(length(unique(df$key))<=4){
                
                if("Sub-national Unit Name" %in% colnames(df)) {
                  p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = `Sub-national Unit Name`, text = mytext))
                } else {
                  p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = Grouping, text = mytext))

                }
                
                p <- p +
                  ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
                  ggplot2::scale_fill_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.title.x = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.text = ggplot2::element_text(colour = "#28313d"),
                        plot.title = ggplot2::element_text(colour = "#28313d"),
                        axis.ticks = ggplot2::element_line(colour = "#ebebeb")

                  )
              } else {
                
                if("Sub-national Unit Name" %in% colnames(df)) {
                  p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = `Sub-national Unit Name`, color = `Sub-national Unit Name`, text = mytext))
                } else {
                  p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = Grouping, color = Grouping, text = mytext))

                }
                
                p <- p +
                  ggplot2::geom_point() +
                  ggplot2::geom_line() +
                  ggplot2::scale_color_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.title.x = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.text = ggplot2::element_text(colour = "#28313d"),
                        plot.title = ggplot2::element_text(colour = "#28313d"),
                        axis.ticks = ggplot2::element_line(colour = "#ebebeb")

                  )
                
                
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
              if(grepl('(', ic, fixed = TRUE)){
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
              col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = Grouping, text = mytext)) +

                  ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
                  ggplot2::scale_fill_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.title.x = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.text = ggplot2::element_text(colour = "#28313d"),
                        plot.title = ggplot2::element_text(colour = "#28313d"),
                        axis.ticks = ggplot2::element_line(colour = "#ebebeb")
                  )
              } else {
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = Grouping, color = Grouping, text = mytext)) +
                  ggplot2::geom_point() +
                  ggplot2::geom_line() +
                  ggplot2::scale_color_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.title.x = ggplot2::element_text(size = 8, colour = "#28313d"),
                        axis.text = ggplot2::element_text(colour = "#28313d"),
                        plot.title = ggplot2::element_text(colour = "#28313d"),
                        axis.ticks = ggplot2::element_line(colour = "#ebebeb")

                  )
                
                
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
            }
          }
          
          
        })
        
      } else {
        
        
        output$data_year_ui <- renderUI({
          
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
            
            if(grepl('Port', sc)){
              # make value numeric
              df$value <- round(as.numeric(df$value), 2)
              
              df <- df %>%
                dplyr::mutate(check_missing = dplyr::case_when(

                  is.na(value) ~ 1,
                  TRUE ~ 0
                )) %>%
                dplyr::group_by(key) %>%
                dplyr::filter(check_missing == 0) %>%
                dplyr::ungroup() %>%
                dplyr::select(-check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% dplyr::filter(key == lastyearavailable)
              
              # get title and subtitle
              plot_title <- paste0(cn, ' : ', ic)
              if(grepl('(', ic, fixed = TRUE)){
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
              col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$`Sub-national Unit Name`)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = `Sub-national Unit Name`, text = mytext)) +

                  ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
                  ggplot2::scale_fill_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                        axis.title.y = ggplot2::element_text(size = 8))
              } else {
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = `Sub-national Unit Name`, color = `Sub-national Unit Name`, text = mytext)) +
                  ggplot2::geom_point() +
                  ggplot2::geom_line() +
                  ggplot2::scale_color_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                        axis.title.y = ggplot2::element_text(size = 8))

                
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
                  is.na(value) ~ 1,
                  TRUE ~ 0
                )) %>%
                dplyr::group_by(key) %>%
                dplyr::filter(check_missing == 0) %>%
                dplyr::ungroup() %>%
                dplyr::select(-check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% dplyr::filter(key == lastyearavailable)
              
              # get title and subtitle
              if(input$data_compare_to == 'Other indicators'){
                plot_title <- paste0(input$data_country)
              } else {
                plot_title <- paste0("Indicator: ", ic)
              }
              if(grepl('(', ic, fixed = TRUE)){
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
              col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = Grouping, text = mytext)) +

                  ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
                  ggplot2::scale_fill_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                        axis.title.y = ggplot2::element_text(size = 8))
              } else {
                p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = Grouping, color = Grouping, text = mytext)) +

                  ggplot2::geom_point() +
                  ggplot2::geom_line() +
                  ggplot2::scale_color_manual(name = '', values = col_pal)+
                  ggplot2::labs(x = 'Year', y = y_axis, title = plot_title) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                        axis.title.y = ggplot2::element_text(size = 8))

                
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
    output$btn_data_access <- renderUI({
      if(input$data_sector != "Transport Road") {
        div(id = "btn_groups",
            downloadButton(session$ns('downloadDataFilteredCSV'), 'Download Table'),
            uiOutput(session$ns('buttonByIndicator')),
            downloadButton(session$ns('downloadPlot'), 'Download Chart')
        ) 
      } else {
        div(id = "btn_groups",
            downloadButton(session$ns('downloadPlot'), 'Download Chart')
        ) 
      }
    })
    
    # Table with download
    output$data_table_access <- renderUI({
      req(input$data_indicator)
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
            # make value numeric
            df$value <- round(as.numeric(df$value), 2)
            
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
            )
          }
        })
        
        DT::dataTableOutput(session$ns('data_table'))
      } else {
        tags$h2(style = 'color:#28323d', 'This data is available on request. Please contact the Library Network for access')
        
        # h3("This data is available on request. Please contact the Library Network for access")
      }
      
    })
    
    
    
    # Reactive data frame prepared to download data by all indicators
    data_tab_all_indc <- reactive({
      ic <- input$data_indicator
      
      # get data
      df <- infrasap_dat_mod_modified %>%
        dplyr::filter(`Indicator Name` == ic)
      
      return(df)
      
    })
    
    
    # Reactive data frame to download data by specific indicator
    data_tab_filtered_csv <- reactive({
      df <- data_tab()
      # make value numeric
      df$value <- round(as.numeric(df$value), 2)
      
      # spread data
      df <- df %>% tidyr::spread(key = 'key', value = 'value')

      return(df)
    })
    
    
    # Download button fucntionality for all indicators
    output$downloadDataAllIndicator <- downloadHandler(
      filename = function() {
        paste0(stringr::str_glue('{input$data_indicator}'), ".csv")
      },
      content = function(file) {
        write.csv(data_tab_all_indc(), file)
      }
    )
    
    # Download button functionality for specific indicator
    output$downloadDataFilteredCSV <- downloadHandler(
      filename = function() {
        paste0(stringr::str_glue('{input$data_indicator}'), ".csv")
      },
      content = function(file) {
        write.csv(data_tab_filtered_csv(), file)
      }
    )
    
    
    # Reactive data with prepared chart
    data_chart_download <- reactive({
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
        col_pal <- RColorBrewer::brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
        if(length(unique(df$key))<=4){
          p <- ggplot2::ggplot(df, ggplot2::aes(key, value, fill = Grouping, text = mytext)) +

            ggplot2::geom_bar(stat= 'identity', position = 'dodge') +
            ggplot2::scale_fill_manual(name = '', values = col_pal)+
            ggplot2::labs(x = 'Year', y = ic, title = plot_title) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                  axis.title.y = ggplot2::element_text(size = 8))
        } else {
          p <- ggplot2::ggplot(df, ggplot2::aes(key, value, group = Grouping, color = Grouping, text = mytext)) +

            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::scale_color_manual(name = '', values = col_pal)+
            ggplot2::labs(x = 'Year', y = ic, title = plot_title) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90, vjust = 0.5),
                  axis.title.y = ggplot2::element_text(size = 8))

          
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
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0(input$data_indicator, '.png', sep='') },
      content = function(file) {
        ggplot2::ggsave(file, plot = data_chart_download(), device = "png", height=12, width=15)
      }
    )
    
    # Download button by indicator functionality
    output$buttonByIndicator <- renderUI({
      downloadButton(ns('downloadDataAllIndicator'), str_glue("Download Indicator Data"))
    })
    
    
  })
}
    
## To be copied in the UI
# mod_indicator_trend_tab_module_ui("indicator_trend_tab_module_ui_1")
    
## To be copied in the server
# mod_indicator_trend_tab_module_server("indicator_trend_tab_module_ui_1")
