#' scd_tab_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scd_tab_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             selectInput(ns('scd_country'),
                         label = 'Select a country',
                         choices = sort(unique(scd_dat$`Country Name`)),
                         selected = 'Kenya')
      ),
      column(6,
             selectInput(ns('scd_sector'),
                         label = 'Select a sector',
                         choices = c('Energy', 
                                     'Digital Development',
                                     'Transport', 
                                     'Transport Road', 
                                     'Transport Railways', 
                                     'Transport Port'),
                         selected = 'Energy')
      )
    ),
    fluidRow(
      column(6,
             uiOutput(ns('scd_bm_ui'))
      ),
      column(6,
             uiOutput(ns('scd_countries_ui'))
      )
    ),
    
    br(), 
    br(),
    
    fluidRow(
      column(12,
             div(id = "scd_table_style_id",
                 DT::dataTableOutput(ns('scd_table')) %>% withSpinner(type = 7,color = "#28323d")
             )
      )
    )
  )
}
    
#' scd_tab_module Server Functions
#'
#' @noRd 
mod_scd_tab_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    # Module Body
    
    
    #------- Initialize the Memory ----------
    selected_vals = reactiveValues(scd_country_name = 'Kenya', 
                                   scd_sector_name = "Energy",
                                   scd_benchmark_name = NULL,
                                   scd_countries_name = NULL,
                                   scd_year_name = NULL
    )
    
    
    # get benchmarks for country selected
    output$scd_bm_ui <- renderUI({
      cn <- input$scd_country
      
      # get benchmark info for country selected
      bm_choices<- infrasap::scd_dat %>%
        filter(`Country Name` == cn) %>%
        select(4:12) %>%
        distinct() %>%
        gather() %>%
        drop_na() %>%
        .$value
      
      # only keep choices that have corresponding data on in benchmarks
      bm <- sort(unique(scd_bm$Grouping))
      
      bm_choices <- intersect(bm_choices, bm)
      
      selectizeInput(inputId = ns('scd_benchmark'),
                     label = 'Select a benchmark',
                     choices = bm_choices,
                     selected = bm_choices[1],
                     multiple = TRUE,
                     options = list(
                       'plugins' = list('remove_button'),
                       'create' = TRUE,
                       'persist' = FALSE
                     )
      )
      
      
      
    }) # /output$scd_bm_ui
    
    
    # ui output for years based on inputs selected
    output$scd_year_ui <- renderUI({
      sc <- input$scd_sector
      sc <- c(sc, 'National')
      cn <- input$scd_country
      bm <- input$scd_benchmark
      
      if(is.null(bm)){
        NULL
      } else {
        # get country data
        df <- infrasap::scd_dat %>% 
          filter(`Country Name`== cn) %>% 
          filter(`Indicator Sector` %in% sc) %>%
          select(`Country Name`,`Indicator Name`,`1990`:`2020`) %>% 
          gather(key = 'key', value = 'value',-`Indicator Name`, -`Country Name`) %>% 
          drop_na()
        
        
        # get benchmark data
        df_bm <- infrasap::scd_bm %>%
          filter(Grouping %in% bm) %>%
          filter(Sector %in% sc) %>%
          select(Indicator, `1990`:`2020`) %>%
          gather(key = 'key', value = 'value',-`Indicator`) %>% 
          drop_na()
        
        # get intersection of indicators
        int_ic <- intersect(df$`Indicator Name`, df_bm$Indicator)
        
        # subset data by common intersection
        df <- df %>% filter(`Indicator Name` %in% int_ic)
        df_bm<- df_bm%>% filter(`Indicator` %in% int_ic)
        
        year_choices <- intersect(unique(df$key), unique(df_bm$key))
        
        selectInput(ns('scd_year'), 
                    label = 'Select year',
                    choices = c("Latest year available", year_choices), 
                    selected  = year_choices[length(year_choices)]
        )
        
      }
      
    }) # /output$scd_year_ui
    
    output$scd_countries_ui <- renderUI({
      sc <- input$scd_sector
      sc <- c(sc, 'National')
      cn <- input$scd_country
      
      
      df <- infrasap::scd_dat %>%
        filter(`Country Name`==cn) %>%
        filter(`Indicator Sector` %in% sc ) %>%
        select(`Indicator Name`,`Region`) %>%
        drop_na() %>%
        select(`Region`, `Indicator Name`)
      
      ic <- sort(unique(df$`Indicator Name`))
      rn <- unique(df$Region)
      
      # subset data by region pillar and sector to get countries from that region with available data
      df <- infrasap::scd_dat %>%
        filter(`Indicator Sector` %in% sc ) %>%
        filter(Region == rn) %>%
        filter(`Indicator Name` %in% ic) %>%
        select(`Country Name`) %>%
        drop_na()
      
      # get region names 
      cnames <- sort(unique(df$`Country Name`))
      
      # get all countries 
      all_cnames <- sort(unique(dat$`Country Name`))
      
      # remove region names fro all countries
      all_cnames <- all_cnames[!all_cnames %in% cnames]
      
      # combine region on top of all names 
      cnames <- c(cnames, all_cnames)
      
      
      selectizeInput(inputId = ns('scd_countries'),
                     label = 'Select countries for comparison',
                     choices = cnames[cnames != input$scd_country],
                     selected = cnames[1],
                     multiple = TRUE,
                     options = list(
                       maxItems = 3,
                       'plugins' = list('remove_button'),
                       'create' = TRUE,
                       'persist' = FALSE
                     )
      )
      
      
      
    })
    
    
    table_reactive <- reactive({
      cn <- input$scd_country
      bm <- input$scd_benchmark
      sc <- input$scd_sector
      sc <- c(sc, 'National')
      cc <- input$scd_countries
      
      yr <- as.character(get_year_scd(cn = cn, sc = sc, bm = bm, year_position = "last"))
      
      # get data based on inputs
      df <- infrasap::scd_dat %>% 
        filter(`Country Name`%in% cn) %>% 
        filter(`Indicator Sector` %in% sc) %>%
        select(`Country Name`,`Indicator Name`, `Type of Benchmark`, yr) 
      
      
      df <- df %>% 
        mutate(`Type of Benchmark` = dplyr::if_else(is.na(`Type of Benchmark`), 'Upper', `Type of Benchmark`))
      
      available_years <- c()
      range <- c((as.numeric(yr) - 1):(as.numeric(get_year_scd(cn = cn, sc = sc, bm = bm))))
      range <- as.character(range)
      
      df <- df %>%
        dplyr::mutate(
          year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
        ) 
      
      
      map(1:length(range), function(x){
        df <<- fill_missing_values_in_years_scd(df = df,
                                                based_year = yr,
                                                year_step_back = range[x],
                                                country = cn,
                                                sector = sc)
      })
      
      # Years to delete
      available_years <- as.character(
        as.numeric(range)[!(as.numeric(range) %in% unique(df$year_pop))]
      )
      
      # Years in use
      available_years_in_use <- as.character(unique(df$year_pop))
      available_years_in_use <- available_years_in_use[!is.na(available_years_in_use)]
      yr <- as.character(max(unique(df$year_pop), na.rm = TRUE))
      
      df_years_col <- df %>% select(`Indicator Name`, `year_pop`)
      
      # benchmark data 
      # TO DO ADD Benchmark length for multiple choices
      df <- infrasap::scd_bm %>% 
        filter(Grouping %in% bm) %>%
        filter(Sector %in% sc) %>%
        select(Grouping, Indicator, available_years_in_use) %>%
        dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
      
      df <- df %>%
        mutate(year_tooltip = year_pop)
      
      
      df <- df %>% select(-available_years)
      
      map(1:length(available_years_in_use), function(b){
        df <<- df %>%
          mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(str_glue("{available_years_in_use[b]}.x")), year_pop)
          )
      })[length(available_years_in_use)]
      
      df <- df %>%
        rename(
          !!col_sym_conv((df %>% select(`Country Name`) %>% pull() %>% unique())) := !!col_sym_conv(str_glue("{yr}.y"))
        ) %>% select(-contains(".x"), -contains(".y"), -`Country Name`) %>% 
        pivot_wider(names_from = `Grouping`, values_from = `year_pop`)
      
      
      # Countries list to compare
      if((!is.null(cc)) && (length(cc) > 0)){
        
        # get infrasap data based on inputs to get the benchmark type and join
        df_cn <- infrasap::scd_dat %>% 
          dplyr::filter(`Country Name`%in% cc) %>% 
          dplyr::filter(`Indicator Sector` %in% sc) %>%
          dplyr::select(`Country Name`,`Indicator Name`, available_years_in_use) %>%
          dplyr::select(-c(`Country Name`, available_years_in_use))
        
        if(length(cc) == 1) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], sc, available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        if(length(cc) == 2) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], sc, available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[2], sc, available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        if(length(cc) == 3) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], sc, available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[2], sc, available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[3], sc, available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        
        df <- df %>%
          dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
        
      }
      
      
      df <- df %>% 
        select(year_tooltip, `Indicator`, cn, everything()) 
      
      
      
      if((!is.null(cc)) && (length(cc) > 0)){
        map(1:length(input$scd_countries), function(x){
          df <<- scd_color_encode(df = df, 
                                  value = str_glue('value_c{x}'), 
                                  cn = input$scd_country, 
                                  bm = input$scd_countries[x])
        })
      }
      
      if((!is.null(bm)) && (length(bm) > 0)){
        map(1:length(input$scd_benchmark), function(x){
          df <<- scd_color_encode(df = df, 
                                  value = str_glue('value_b{x}'), 
                                  cn = input$scd_country, 
                                  bm = input$scd_benchmark[x])
        })
      }
      
      
      df <- df %>% select(-`Type of Benchmark`)
      
      # get rid of string named `NA`
      df <- df[names(df) != "NA"]
      
      a <- infrasap::scd_dat %>% select(`Indicator Name`) %>% distinct()
      b <- infrasap::scd_bm %>% select(`Indicator`) %>% distinct()
      df_ind <- a %>% full_join(b, by = c("Indicator Name" = "Indicator"))
      df <- df %>% full_join(df_ind, by = c("Indicator" = "Indicator Name"))
      
      
      return(df)
      
    })
    
    
    # Table for scd 
    output$scd_table <- DT::renderDataTable({
      bm <- input$scd_benchmark
      cc <- input$scd_countries
      req(table_reactive())
      df <- table_reactive()
      
      hiddenColNum <- c()
      
      if(!is.null(bm)){
        map(1:length(input$scd_benchmark), function(x){
          hiddenColNum <<- c(hiddenColNum, which( colnames(df)== as.character(str_glue('value_b{x}'))))
        })
      }
      
      if(!is.null(cc)){
        map(1:length(input$scd_countries), function(x){
          hiddenColNum <<- c(hiddenColNum, which( colnames(df)== as.character(str_glue('value_c{x}'))))
        })
      }
      
      df <- df %>% mutate(across(where(is.numeric), ~round(., 2)))
      
      
      # datatable(df) 
      dtable <- DT::datatable(df,
                              extensions = 'Buttons',
                              escape=FALSE,
                              options = list(pageLength = nrow(df),
                                             rowCallback = JS(
                                               "function(row, data) {",
                                               "var full_text = 'This row values extracted from ' + data[1] +  ' year'",
                                               "$('td', row).attr('title', full_text);",
                                               "console.log(data)",
                                               "}"
                                             ),
                                             info = FALSE, 
                                             dom='Bfrti', 
                                             columnDefs = list(list(visible=FALSE, 
                                                                    targets=c(hiddenColNum, 1)
                                             )
                                             ),
                                             buttons = list(
                                               list(extend = "csv",
                                                    # To export only visible columns without color code
                                                    exportOptions = list(columns = ":visible")
                                               )
                                             )
                              ),
                              selection = 'none'
      ) 
      
      
      if(!is.null(cc)) {
        map(1:length(input$scd_countries), function(x) {
          dtable <<- dtable %>% formatStyle(
            as.character(input$scd_countries[x]), as.character(str_glue('value_c{x}')),
            backgroundColor = styleEqual(
              c(0, 1, 2, 3),
              c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
            )
          )
        })
      }
      
      if(!is.null(bm)) {
        map(1:length(input$scd_benchmark), function(x) {
          dtable <<- dtable %>% formatStyle(
            as.character(input$scd_benchmark[x]), as.character(str_glue('value_b{x}')),
            backgroundColor = styleEqual(
              c(0, 1, 2, 3),
              c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
            )
          )
        })
      }
      
      
      dtable
      
    }) #/ rendertable
    
    
    
  })
}
    
## To be copied in the UI
# mod_scd_tab_module_ui("scd_tab_module_ui_1")
    
## To be copied in the server
# mod_scd_tab_module_server("scd_tab_module_ui_1")
