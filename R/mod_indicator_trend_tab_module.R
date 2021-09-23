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
          column(4,
                 selectInput(inputId = ns('data_country'), 
                             label = 'Select country',
                             choices = sort(unique(dat$`Country Name`)),
                             selected = 'Jordan'
                             ),
                 uiOutput(ns('data_indicator_ui'))
                 
          ),
          column(4,
                 uiOutput(ns('data_sector_ui')),
                 selectInput(ns('data_selection_type_year'), 
                             label = 'Type of selection: ',
                             choices = c('Select latest year available', 'Select a range of years'),
                             selected = 'Select a range of years'
                 )
          ),
          column(4,
                 uiOutput(ns('country_ports_ui')),
                 # uiOutput(ns('data_compare_to_ui')),
                 selectInput(ns('data_compare_to'), 
                             label = NULL,
                             choices = NULL,
                             selected = NULL
                 ),
                 uiOutput(ns('data_benchmark_ui')),
          )
        ),
    
        # fluidRow(
        #   column(4,
        #          uiOutput(ns('data_indicator_ui'))
        #   ),
        #   column(4,
        #          selectInput(ns('data_selection_type_year'), 
        #                      label = 'Type of selection: ',
        #                      choices = c('Select latest year available', 'Select a range of years'),
        #                      selected = 'Select a range of years'
        #                      )
        #   ),
        #   column(4,
        #          uiOutput(ns('data_benchmark_ui')),
        #          # uiOutput(ns('data_compare_ports_ui'))
        #   )
        # ),
        uiOutput(ns('data_year_ui'))
    ),
    div(
      fluidRow(
        column(12,
               align = 'center',
               plotlyOutput(ns('data_chart'),
                            width = '900px', 
                            height = "auto"
                            ) %>% withSpinner(type = 7,color = "#28323d")
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
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    # Module Body
    
    #------- Initialize the Memory ----------
    selected_vals = reactiveValues(
      db_countries_name = NULL,
      data_benchmarks_name = NULL
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
    })
    
    
    
    
    # get sectors based on country input
    output$data_sector_ui <- renderUI({
      cn <- input$data_country
      # save(cn, file = 'test_cn.rda')
      # subset data by country selected
      df <- infrasap::dat %>% filter(`Country Name` == cn)
      sc_choices <- sort(unique(df$`Indicator Sector`))
      sc_choices <- sc_choices[sc_choices != 'National']
      
      selectInput(inputId = ns('data_sector'),
                  label = 'Select sector',
                  choices = sc_choices,
                  selected = NULL)
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
          df <- df %>% filter(`Country Name` == cn)
          # port_choices <- sort(unique(df$`Sub-national Unit Name`))
          port_choices <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != input$data_compare_to ]
          # port_choices <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != input$country_ports ]
          selectInput(ns('country_ports'),
                      label = paste0('Choose a port from ',cn ),
                      choices = port_choices,
                      selected = port_choices[1]
          )
        }
      }
    })
    
    # output$data_compare_to_ui <- renderUI({
    #   req(input$data_sector)
    #   # sc <- input$data_sector
    #   # save(sc, file = 'temp_sc.rda')
    #   # if(is.null(sc)){
    #   #   NULL
    #   # } else {
    #   #   if(grepl('Port', sc)){
    #   #     NULL
    #   # 
    #   #   } else {
    #       selectInput(ns('data_compare_to'), 
    #                   label = 'Compare to: ',
    #                   choices = c('Other countries', 'Other benchmarks', 'Other indicators'),
    #                   selected = 'Other benchmarks'
    #       )
    #   #   }
    #   #   
    #   # }
    #  
    # })
    
    observeEvent(input$data_sector, {
      req(input$data_sector)
      # req(input$country_ports)
      
      # print("Country Ports:...12")
      # print(input$country_ports)
      # if(is.null(input$country_ports)) {
      #   print("yeah null")
      # } else {
      #   # cp <- input$country_ports
      #   print("not null yeah")
      # }
      
      
      if(grepl('Port', input$data_sector)){
        
        sc <- input$data_sector
        cn <- input$data_country
        
        if(is.null(input$country_ports)) {
          df <- infrasap::dat_ports
          df <- df %>% filter(`Country Name` == cn)
          port_choices <- sort(unique(df$`Sub-national Unit Name`))
          cp <- port_choices[1]
        } else {
          cp <- input$country_ports
        }
        
        # req(input$country_ports)
          
          # cp <- input$country_ports
          
          # print(input$country_ports)
          
        df <- infrasap::dat_ports %>% filter(`Country Name` == cn) %>% filter(`Sub-national Unit Name` != cp)
        compare_ports <- sort(unique(df$`Sub-national Unit Name`))[sort(unique(df$`Sub-national Unit Name`)) != cp ]

              # save(compare_ports, file = 'temp1.rda')
        
        updateSelectInput(session, "data_compare_to",
                          label = paste0('Compare to other ports from ',cn, ':'),
                          choices = compare_ports,
                          selected = compare_ports[1]
        )
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
    #   df <- infrasap::dat_ports %>% filter(`Country Name` == cn) %>% filter(`Sub-national Unit Name` != cp)
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
    #   # df <- df %>% filter(`Country Name` == cn)
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
    #     df <- infrasap::dat_ports %>% filter(`Country Name` == cn)%>% filter(`Sub-national Unit Name` != cp)
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
      sc <- c(sc, 'National')
      cn <- input$data_country
      cp <- input$data_compare_to
      ct <- input$country_ports
      if(is.null(sc)){
        NULL
      } else {
        if(grepl('Port', sc)){
          df <- infrasap::dat_ports %>%
            filter(`Country Name` == cn) %>%
            filter(`Sub-national Unit Name` %in% c(cp, ct)) %>%
            select(Grouping = `Indicator Name`,`Sub-national Unit Name`,`1990`:`2021`) %>%
            gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
            drop_na()
          # get a unique list of indicators
          ic <- sort(unique(df$Grouping))
          fluidRow(
            column(12,
                   
                   selectInput(inputId = ns('data_indicator'),
                               label = 'Select an indicator',
                               choices = ic,
                               selected = ic[1]))
          )
        } else {
          # subset data by sector and year and remove NAs
          df <- infrasap::dat %>%
            filter(`Country Name` == cn) %>%
            filter(`Indicator Sector` %in% sc) %>%
            select(Grouping = `Indicator Name`,`1990`:`2021`) %>%
            gather(key = 'key', value = 'value',-`Grouping`) %>%
            drop_na()
          
          # get a unique list of indicators
          ic <- sort(unique(df$Grouping))
          fluidRow(
            column(12,
                   
                   selectInput(inputId = ns('data_indicator'),
                               label = 'Select an indicator',
                               choices = ic,
                               selected = ic[1]))
          )
        }
       
      }
    })
  
  
    
    # UI for benchmarks or countries depending on "data_compare_to" input
    output$data_benchmark_ui <- renderUI({
      sc <- input$data_sector
      sc <- c(sc, 'National')
      cn <- input$data_country
      ic <- input$data_indicator
      yr <- input$data_year
      ct <- input$data_compare_to
      save(ct, file = 'temp_ct.rda')
      if(is.null(yr)){
        NULL
      } else {
        if(grepl('Port', sc) ){
          selected_vals$data_benchmarks_name <<- NULL
          NULL
        } else {
          if(ct == 'Other benchmarks'){
            df <- infrasap::dat_bm %>%
              filter(Indicator == ic) %>%
              filter(Sector == sc) %>%
              select(`Grouping`,`1990`:`2021`) %>%
              gather(key = 'key', value = 'value',-`Grouping`) %>%
              drop_na() %>%
              filter(key >= yr[1], key<=yr[2])
            
            # get unique list of benchmarks
            bn <- sort(unique(df$Grouping))
            
            if(is.null(selected_vals$data_benchmarks_name)) {
              bn_selected <- bn[c(7,8)]
            } else {
              bn_selected <- selected_vals$data_benchmarks_name
            }
            
            
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
                df <- infrasap::dat %>%
                  filter(`Indicator Name` == ic) %>%
                  filter(`Indicator Sector` %in% sc) %>%
                  select(Grouping = `Country Name`,Region,`1990`:`2021`) %>%
                  gather(key = 'key', value = 'value',-`Grouping`,-Region) %>%
                  drop_na() %>%
                  filter(key >= yr[1], key<=yr[2])
                
                # get unique countries that meet the criteria
                cn_choices <- sort(unique(df$Grouping))
                cn_choices <- cn_choices[cn_choices != input$data_country]
                
                # if countries exist that meet the criteria above, then get the region
                if(length(cn_choices)> 0){
                  rn <- infrasap::dat %>%
                    filter(`Country Name` == cn) %>%
                    .$Region
                  
                  rn <- unique(rn)
                  df <- df %>% filter(Region == rn)
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
                df <- infrasap::dat %>%
                  filter(`Country Name` == cn) %>%
                  filter(`Indicator Sector` %in% sc) %>%
                  select(Grouping = `Indicator Name`,`1990`:`2021`) %>%
                  gather(key = 'key', value = 'value',-`Grouping`) %>%
                  drop_na()
                
                # get a unique list of indicators
                ic <- sort(unique(df$Grouping))
                measure_brackets <- regmatches(input$data_indicator, gregexpr("(?=\\().*?(?<=\\))", input$data_indicator, perl=T))[[1]]
                
                
                if(length(measure_brackets) == 0){
                  
                  ic <- setdiff(ic, ic[str_detect(ic, pattern = '[\\(\\)]')])
                  ic <- ic[ic != input$data_indicator]
                  
                } else {
                  ic <- ic[grep(pattern = as.character(str_glue('\\({measure_brackets}\\)')), x = ic)]
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
      req(input$data_compare_to)
      # get sector and year
      sc <- input$data_sector
      sc <- c(sc, 'National')
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
        
        # get country data
        df <- infrasap::dat_ports %>%
          filter(`Country Name`== cn) %>%
          filter(`Indicator Name`== ic) %>%
          filter(`Sub-national Unit Name` %in% c(ct, cp)) %>%
          select(Grouping = `Country Name`,`Sub-national Unit Name`,`1990`:`2021`) %>%
          gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
          drop_na() %>%
          filter(key >= yr[1], key<=yr[2])
        
        return(df)
        
      } else {
        # print('Not Trans')
        # print(ct)
        
        if(ct == 'Other benchmarks'){
          if(is.null(bn)){
            NULL
          } else {
            
            # get benchmark data
            df_bm <- infrasap::dat_bm %>%
              filter(Indicator == ic) %>%
              filter(Sector == sc) %>%
              filter(Grouping %in% bn) %>%
              select(`Grouping`,`1990`:`2021`) %>%
              gather(key = 'key', value = 'value',-`Grouping`) %>%
              drop_na() %>%
              filter(key >= yr[1], key<=yr[2])
            
            
            # get country data
            df <- infrasap::dat %>%
              filter(`Country Name`== cn) %>%
              filter(`Indicator Name` == ic) %>%
              filter(`Indicator Sector` %in% sc) %>%
              select(Grouping = `Country Name`,`1990`:`2021`) %>%
              gather(key = 'key', value = 'value',-`Grouping`) %>%
              drop_na() %>%
              filter(key >= yr[1], key<=yr[2])
            
            
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
              df <- infrasap::dat %>%
                filter(`Country Name`%in% country_names) %>%
                filter(`Indicator Name` == ic) %>%
                filter(`Indicator Sector` %in% sc) %>%
                select(Grouping = `Country Name`,`1990`:`2021`) %>%
                gather(key = 'key', value = 'value',-`Grouping`) %>%
                drop_na() %>%
                filter(key >= yr[1], key<=yr[2])
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
                df <- infrasap::dat %>%
                  filter(`Country Name`%in% cn) %>%
                  filter(`Indicator Name` == ic) %>%
                  filter(`Indicator Sector` %in% sc) %>%
                  select(Grouping = `Indicator Name`,`1990`:`2021`) %>%
                  gather(key = 'key', value = 'value',-`Grouping`) %>%
                  drop_na() %>%
                  filter(key >= yr[1], key<=yr[2])
                
                dfother <- infrasap::dat %>%
                  filter(`Country Name`%in% cn) %>%
                  filter(`Indicator Name` %in% oi) %>%
                  filter(`Indicator Sector` %in% sc) %>%
                  select(Grouping = `Indicator Name`,`1990`:`2021`) %>%
                  gather(key = 'key', value = 'value',-`Grouping`) %>%
                  drop_na() %>%
                  filter(key >= yr[1], key<=yr[2])
                
                
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
      #     filter(`Country Name`== cn) %>%
      #     filter(`Indicator Name`== ic) %>%
      #     filter(`Sub-national Unit Name` %in% c(ct, cp)) %>%
      #     select(Grouping = `Country Name`,`Sub-national Unit Name`,`1990`:`2021`) %>%
      #     gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
      #     drop_na() %>%
      #     filter(key >= yr[1], key<=yr[2])
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
          # sc <- c(sc, 'National')
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
                filter(`Country Name` == cn) %>%
                filter(`Indicator Name`==ic) %>%
                filter(`Sub-national Unit Name` %in% c(cp, ct)) %>%
                select(Grouping = `Indicator Name`,`Sub-national Unit Name`,`1990`:`2021`) %>%
                gather(key = 'key', value = 'value',-`Grouping`, -`Sub-national Unit Name`) %>%
                drop_na()
              
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
              df <- infrasap::dat %>%
                filter(`Country Name` == cn) %>%
                filter(`Indicator Sector` %in% sc) %>%
                filter(`Indicator Name`==ic) %>%
                select(Grouping = `Indicator Name`,`1990`:`2021`) %>%
                gather(key = 'key', value = 'value',-`Grouping`) %>%
                drop_na()
              
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
            }
            
          }
        })
        
        
        
        # plot country comparison or country/benchmark comparison
        output$data_chart <- renderPlotly({
          ic <- input$data_indicator
          sc <- input$data_sector
          cn <- input$data_country
          # sc <- c(sc, 'National')
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
              # text for plot
              mytext <- paste(
                "Value: ",round(df$value,2),"<br>",
                "Year: ", as.character(df$key),"<br>",
                "Port: ", as.character(df$`Sub-national Unit Name`),"<br>",
                sep="") %>%
                lapply(htmltools::HTML)
              col_pal <- brewer.pal(n = length(unique(df$`Sub-national Unit Name`)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot(df, aes(key, value, fill = `Sub-national Unit Name`, text = mytext)) +
                  geom_bar(stat= 'identity', position = 'dodge') +
                  scale_fill_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = element_text(size = 8, colour = "#28313d"),
                        axis.title.x = element_text(size = 8, colour = "#28313d"),
                        axis.text = element_text(colour = "#28313d"),
                        plot.title = element_text(colour = "#28313d"),
                        axis.ticks = element_line(colour = "#ebebeb")
                  )
              } else {
                p <- ggplot(df, aes(key, value, group = `Sub-national Unit Name`, color = `Sub-national Unit Name`, text = mytext)) +
                  geom_point() +
                  geom_line() +
                  scale_color_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = element_text(size = 8, colour = "#28313d"),
                        axis.title.x = element_text(size = 8, colour = "#28313d"),
                        axis.text = element_text(colour = "#28313d"),
                        plot.title = element_text(colour = "#28313d"),
                        axis.ticks = element_line(colour = "#ebebeb")
                  )
                
                
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- ggplotly(p, tooltip = 'text') %>%
                  config(displayModeBar = F)
                fig
              }
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
              col_pal <- brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot(df, aes(key, value, fill = Grouping, text = mytext)) +
                  geom_bar(stat= 'identity', position = 'dodge') +
                  scale_fill_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = element_text(size = 8, colour = "#28313d"),
                        axis.title.x = element_text(size = 8, colour = "#28313d"),
                        axis.text = element_text(colour = "#28313d"),
                        plot.title = element_text(colour = "#28313d"),
                        axis.ticks = element_line(colour = "#ebebeb")
                  )
              } else {
                p <- ggplot(df, aes(key, value, group = Grouping, color = Grouping, text = mytext)) +
                  geom_point() +
                  geom_line() +
                  scale_color_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5, colour = "#28313d"),
                        axis.title.y = element_text(size = 8, colour = "#28313d"),
                        axis.title.x = element_text(size = 8, colour = "#28313d"),
                        axis.text = element_text(colour = "#28313d"),
                        plot.title = element_text(colour = "#28313d"),
                        axis.ticks = element_line(colour = "#ebebeb")
                  )
                
                
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- ggplotly(p, tooltip = 'text') %>%
                  config(displayModeBar = F)
                fig
              }
            }
          }
          
          
        })
        
      } else {
        
        
        output$data_year_ui <- renderUI({
          
          NULL
          
        })
        
        
        # plot country comparison or country/benchmark comparison
        output$data_chart <- renderPlotly({
          ic <- input$data_indicator
          sc <- input$data_sector
          cn <- input$data_country
          
          # sc <- c(sc, 'National')
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
                mutate(check_missing = case_when(
                  is.na(value) ~ 1,
                  TRUE ~ 0
                )) %>%
                group_by(key) %>%
                filter(check_missing == 0) %>%
                ungroup() %>%
                select(-check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% filter(key == lastyearavailable)
              
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
              col_pal <- brewer.pal(n = length(unique(df$`Sub-national Unit Name`)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot(df, aes(key, value, fill = `Sub-national Unit Name`, text = mytext)) +
                  geom_bar(stat= 'identity', position = 'dodge') +
                  scale_fill_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                        axis.title.y = element_text(size = 8))
              } else {
                p <- ggplot(df, aes(key, value, group = `Sub-national Unit Name`, color = `Sub-national Unit Name`, text = mytext)) +
                  geom_point() +
                  geom_line() +
                  scale_color_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                        axis.title.y = element_text(size = 8))
                
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- ggplotly(p, tooltip = 'text') %>%
                  config(displayModeBar = F)
                fig
              }
            } else{
              # make value numeric
              df$value <- round(as.numeric(df$value), 2)
              
              
              
              df <- df %>%
                mutate(check_missing = case_when(
                  is.na(value) ~ 1,
                  TRUE ~ 0
                )) %>%
                group_by(key) %>%
                filter(check_missing == 0) %>%
                ungroup() %>%
                select(-check_missing)
              
              lastyearavailable <- max(unique(df$key), na.rm = TRUE)
              
              df <- df %>% filter(key == lastyearavailable)
              
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
              col_pal <- brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
              if(length(unique(df$key))<=4){
                p <- ggplot(df, aes(key, value, fill = Grouping, text = mytext)) +
                  geom_bar(stat= 'identity', position = 'dodge') +
                  scale_fill_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                        axis.title.y = element_text(size = 8))
              } else {
                p <- ggplot(df, aes(key, value, group = Grouping, color = Grouping, text = mytext)) +
                  geom_point() +
                  geom_line() +
                  scale_color_manual(name = '', values = col_pal)+
                  labs(x = 'Year', y = y_axis, title = plot_title) +
                  theme_bw() +
                  theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                        axis.title.y = element_text(size = 8))
                
              }
              
              if(nrow(p$data)==0){
                NULL
              } else {
                fig <- ggplotly(p, tooltip = 'text') %>%
                  config(displayModeBar = F)
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
      if(input$data_sector != "Transport Road"){
        output$data_table <- DT::renderDataTable({
          ind_name <- input$data_indicator
          sector_name <- input$data_sector
          sector_name <- c(sector_name, 'National')
          df <- data_tab()
          if(is.null(df)){
            NULL
          } else if(nrow(df)==0){
            NULL
          } else {
            # make value numeric
            df$value <- round(as.numeric(df$value), 2)
            
            # spread data
            df <- df %>% spread(key = 'key', value = 'value')
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
        h3("This data is available on request. Please contact the Library Network for access")
      }
      
    })
    
    
    
    
    # Reactive data frame prepared to download data by all indicators
    data_tab_all_indc <- reactive({
      ic <- input$data_indicator
      
      # get data
      df <- infrasap::dat %>%
        filter(`Indicator Name` == ic)
      
      return(df)
      
    })
    
    
    # Reactive data frame to download data by specific indicator
    data_tab_filtered_csv <- reactive({
      df <- data_tab()
      # make value numeric
      df$value <- round(as.numeric(df$value), 2)
      
      # spread data
      df <- df %>% spread(key = 'key', value = 'value')
      return(df)
    })
    
    
    # Download button fucntionality for all indicators
    output$downloadDataAllIndicator <- downloadHandler(
      filename = function() {
        paste0(str_glue('All_{input$data_indicator}'), ".csv")
      },
      content = function(file) {
        write.csv(data_tab_all_indc(), file)
      }
    )
    
    # Download button functionality for specific indicator
    output$downloadDataFilteredCSV <- downloadHandler(
      filename = function() {
        paste0(str_glue('Filtered'), ".csv")
      },
      content = function(file) {
        write.csv(data_tab_filtered_csv(), file)
      }
    )
    
    
    # Reactive data with prepared chart
    data_chart_download <- reactive({
      ic <- input$data_indicator
      sc <- input$data_sector
      sc <- c(sc, 'National')
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
        col_pal <- brewer.pal(n = length(unique(df$Grouping)), name = 'Set1')
        if(length(unique(df$key))<=4){
          p <- ggplot(df, aes(key, value, fill = Grouping, text = mytext)) +
            geom_bar(stat= 'identity', position = 'dodge') +
            scale_fill_manual(name = '', values = col_pal)+
            labs(x = 'Year', y = ic, title = plot_title) +
            theme_bw() +
            theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                  axis.title.y = element_text(size = 8))
        } else {
          p <- ggplot(df, aes(key, value, group = Grouping, color = Grouping, text = mytext)) +
            geom_point() +
            geom_line() +
            scale_color_manual(name = '', values = col_pal)+
            labs(x = 'Year', y = ic, title = plot_title) +
            theme_bw() +
            theme(axis.text.x = element_text(angle=90, vjust = 0.5),
                  axis.title.y = element_text(size = 8))
          
          
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
      filename = function() { paste('Chart', '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = data_chart_download(), device = "png", height=12, width=15)
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
