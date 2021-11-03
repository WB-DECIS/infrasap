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
    # fluidRow(
    #   column(6,
    #          selectInput(ns('scd_country'),
    #                      label = 'Select a country',
    #                      choices = sort(unique(scd_dat$`Country Name`)),
    #                      selected = 'Kenya')
    #   ),
    #   column(6,
    #          selectInput(ns('scd_sector'),
    #                      label = 'Select a sector',
    #                      choices = c('Energy', 
    #                                  'Digital Development',
    #                                  'Transport', 
    #                                  'Transport Road', 
    #                                  'Transport Railways', 
    #                                  'Transport Port'),
    #                      selected = 'Energy')
    #   )
    # ),
    fluidRow(
      column(4,
             selectInput(ns('scd_country'),
                         label = 'Select a country',
                         choices = sort(unique(scd_dat$`Country Name`)),
                         selected = 'Kenya')
      ),
      column(4,
             uiOutput(ns('scd_bm_ui'))
      ),
      column(4,
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
                                   scd_benchmark_name = NULL,
                                   scd_countries_name = NULL,
                                   scd_year_name = NULL
    )
    
    
    # get benchmarks for country selected
    output$scd_bm_ui <- renderUI({
      cn <- input$scd_country
      
      # get benchmark info for country selected
      bm_cn<- infrasap::scd_dat %>%
        # filter(`Country Name` == "Kenya") %>%
        filter(`Country Name` == cn) %>%
        select(4:12) %>%
        distinct() %>%
        gather() %>%
        drop_na() %>%
        .$value
      
      # only keep choices that have corresponding data on in benchmarks
      bm <- sort(unique(scd_bm$Grouping))
      
      # bm_choices <- intersect(bm_choices, bm)
      
      # keep only regional and income group benchmarks 
      bm_keep <- 'East Asia & Pacific|Europe & Central Asia|Latin America & Caribbean|Middle East & North Africa|North America|South Asia|Sub-Saharan Africa|High income|Low income|Lower middle income|Upper middle income'
      bm_choices <- bm[grepl(bm_keep, bm)]
      bm_selected <- infrasap::dat_country_income_region %>%
        filter(`Country Name` == input$scd_country)
      
      # Check after deployment (Sometimes AWS )
      bm_choices <- list("Region" = bm_choices[!str_detect(bm_choices, 'income')],
        "Income Groups" = bm_choices[str_detect(bm_choices, 'income')])
      
      selectizeInput(inputId = ns('scd_benchmark'),
                     label = 'Select a benchmark',
                     choices = bm_choices,
                     # selected = bm_cn[1],
                     # selected = bm_choices[[1]][1],
                     selected = bm_selected$Region,
                     multiple = TRUE,
                     options = list(
                       'plugins' = list('remove_button'),
                       'create' = TRUE,
                       'persist' = FALSE
                     )
      )
      
      
      
    }) # /output$scd_bm_ui
    
    observeEvent(input$scd_country, {
      bn_selected_cr <- infrasap::dat_country_income_region %>%
        filter(`Country Name` == input$scd_country)

      updateSelectInput(session, "scd_benchmark",
                        selected = c(bn_selected_cr$Region)
      )
    })

    # ui output for years based on inputs selected
    output$scd_year_ui <- renderUI({
      cn <- input$scd_country
      bm <- input$scd_benchmark
      
      if(is.null(bm)){
        NULL
      } else {
        # get country data
        df <- infrasap::scd_dat %>% 
          # filter(`Country Name`== "Kenya") %>% 
          filter(`Country Name`== cn) %>%
          # filter(`Indicator Sector` %in% sc) %>%
          select(`Country Name`,`Indicator Name`,`1990`:`2020`) %>% 
          gather(key = 'key', value = 'value',-`Indicator Name`, -`Country Name`) %>% 
          drop_na()
        
        
        # get benchmark data
        df_bm <- infrasap::scd_bm %>%
          filter(Grouping %in% bm) %>%
          # filter(Sector %in% sc) %>%
          select(Indicator, `1990`:`2020`) %>%
          gather(key = 'key', value = 'value',-`Indicator`) %>% 
          drop_na()
        
        # get intersection of indicators
        int_ic <- intersect(df$`Indicator Name`, df_bm$Indicator)
        
        # subset data by common intersection
        df <- df %>% filter(`Indicator Name` %in% int_ic)
        df_bm <- df_bm%>% filter(`Indicator` %in% int_ic)
        
        year_choices <- intersect(unique(df$key), unique(df_bm$key))
        
        selectInput(ns('scd_year'), 
                    label = 'Select year',
                    choices = c("Latest year available", year_choices), 
                    selected  = year_choices[length(year_choices)]
        )
        
      }
      
    }) # /output$scd_year_ui
    
    output$scd_countries_ui <- renderUI({
      cn <- input$scd_country
      
      
      df <- infrasap::scd_dat %>%
        filter(`Country Name`==cn) %>%
        # filter(`Indicator Sector` %in% sc ) %>%
        select(`Indicator Name`,`Region`) %>%
        drop_na() %>%
        select(`Region`, `Indicator Name`)
      
      ic <- sort(unique(df$`Indicator Name`))
      rn <- unique(df$Region)
      
      # subset data by region pillar and sector to get countries from that region with available data
      df <- infrasap::scd_dat %>%
        # filter(`Indicator Sector` %in% sc ) %>%
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
      cc <- input$scd_countries
      
      # cn <- "Kenya"
      # bm <- "East Asia & Pacific"
      # cc <- "Angola"
      
      yr <- as.character(get_year_scd(cn = cn, bm = bm, year_position = "last"))
      
      # get data based on inputs
      # infrasap::scd_dat_names_unique %>% select(`Indicator Name`, `Indicator Sector`) %>% distinct()
      df <- infrasap::scd_dat %>% 
        filter(`Country Name`%in% cn) %>% 
        # filter(`Indicator Sector` %in% sc) %>%
        select(`Country Name`,`Indicator Name`, `Type of Benchmark`, yr) 
      
        
      df <- df %>% 
        mutate(`Type of Benchmark` = dplyr::if_else(is.na(`Type of Benchmark`), 'Upper', `Type of Benchmark`))
      
      available_years <- c()
      range <- c((as.numeric(yr) - 1):(as.numeric(get_year_scd(cn = cn, bm = bm))))
      range <- as.character(range)
      
      df <- df %>%
        dplyr::mutate(
          year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
        ) 
      
      
      map(1:length(range), function(x){
        df <<- fill_missing_values_in_years_scd(df = df,
                                                based_year = yr,
                                                year_step_back = range[x],
                                                country = cn
                                                # ,
                                                # sector = sc
                                                )
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
        # filter(Sector %in% sc) %>%
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
          # dplyr::filter(`Indicator Sector` %in% sc) %>%
          dplyr::select(`Country Name`,`Indicator Name`, available_years_in_use) %>%
          dplyr::select(-c(`Country Name`, available_years_in_use))
        
        if(length(cc) == 1) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        if(length(cc) == 2) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[2], available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        if(length(cc) == 3) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[2], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[3], available_years_in_use, df_years_col)
            ) %>% 
            distinct()
        }
        
        
        df <- df %>%
          dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
        
      }
      

      df <- df %>% 
        select(year_tooltip, `Indicator`, cn, everything()) 
      
      
      
      if((!is.null(cc)) && (length(cc) > 0)){
        # map(1:length(cc), function(x){
        map(1:length(input$scd_countries), function(x){
          df <<- scd_color_encode(df = df, 
                                  value = str_glue('value_c{x}'), 
                                  cn = input$scd_country,
                                  bm = input$scd_countries[x]
                                  # cn = cn, 
                                  # bm = bm[x]
                                  )
        })
      }
      
      if((!is.null(bm)) && (length(bm) > 0)){
        # map(1:length(bm), function(x){
        map(1:length(input$scd_benchmark), function(x){
          df <<- scd_color_encode(df = df, 
                                  value = str_glue('value_b{x}'), 
                                  cn = input$scd_country,
                                  bm = input$scd_benchmark[x]
                                  # cn = cn, 
                                  # bm = bm[x]
                                  )
        })
      }
      
      
      df <- df %>% select(-`Type of Benchmark`)
      
      # get rid of string named `NA`
      df <- df[names(df) != "NA"]
      
      a <- infrasap::scd_dat %>% select(`grouping`, `Indicator Name`) %>% distinct()
      b <- infrasap::scd_bm %>% select(`Indicator`) %>% distinct()
      df_ind <- a %>% full_join(b, by = c("Indicator Name" = "Indicator"))
      df <- df %>% full_join(df_ind, by = c("Indicator" = "Indicator Name"))
      
      # print(infrasap::scd_indicators)
      # print(df)
      # glimpse(df)
      
      df <- infrasap::scd_indicators %>% left_join(df, by = c("Indicator Name" = "Indicator", 
                                                              "grouping" = "grouping")) %>%
            rename(Indicator = `Indicator Name`)
      
      df <- df %>% 
              select(year_tooltip, `grouping`, everything()) %>%
              mutate(`grouping` = forcats::as_factor(`grouping`)) %>%
              mutate(`grouping` = forcats::lvls_revalue(`grouping`, c(
                                                                    'Economic Linkages',
                                                                    'Poverty and Equity',
                                                                    'Quality of Infrastructure',
                                                                    'Governance',
                                                                    'Finance',
                                                                    'Climate change'))) %>%
              arrange(`grouping`)
      
      
      return(df)
      
    })
    
    
    # Table for scd 
    output$scd_table <- DT::renderDataTable({
      bm <- input$scd_benchmark
      cc <- input$scd_countries
      req(table_reactive())
      df <- table_reactive()
      
      hiddenColNum <- c()
      
      # print(df)
      
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
      
      hiddenColNum <- hiddenColNum-1
      
      df <- df %>% mutate(across(where(is.numeric), ~round(., 2))) 
                # %>%
                # arrange(factor(`grouping`, 
                #                levels = c('Economic Linkages', 
                #                           'Poverty and Equity', 
                #                           'Quality of Infrastructure', 
                #                           'Governance', 
                #                           'Finance', 
                #                           'Climate change')
                # ))
      
      # print(
      #   factor(df$`grouping`, 
      #          levels = c('Economic Linkages', 
      #                     'Poverty and Equity', 
      #                     'Quality of Infrastructure', 
      #                     'Governance', 
      #                     'Finance', 
      #                     'Climate change')
      #   )
      # )
      # 
      # print(df)
      
      
      
      
      # datatable(df) 
      dtable <- DT::datatable(df,
                              rownames = FALSE,
                              extensions = 'Buttons',
                              escape=FALSE,
                              options = list(pageLength = nrow(df),
                                             ordering = FALSE,
                                             rowCallback = JS(
                                               "function(row, data) {",
                                               "var full_text = 'This row values extracted from ' + data[0] +  ' year'",
                                               "$('td', row).attr('title', full_text);",
                                               "console.log(data)",
                                               "}"
                                             ),
                                             rowsGroup = list(1), # merge cells of column 1, 2
                                             info = FALSE, 
                                             dom='Bfrti', 
                                             columnDefs = list(list(visible=FALSE,
                                                                    targets=c(hiddenColNum, 0)
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
      
      
      
      dep <- htmltools::htmlDependency(
        "RowsGroup", "2.0.0",
        src = c(href = 'www'), script = "script.js", package = 'infrasap')
      
      dtable$dependencies <- c(dtable$dependencies, list(dep))
      dtable
      
    }) #/ rendertable
    
    
    
  })
}
    
## To be copied in the UI
# mod_scd_tab_module_ui("scd_tab_module_ui_1")
    
## To be copied in the server
# mod_scd_tab_module_server("scd_tab_module_ui_1")
