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
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::selectInput(ns('scd_country'),
                                       label = 'Select a country',
                                       choices = sort(unique(infrasap::scd_dat$`Country Name`)),
                                       selected = 'Kenya')
      ),
      shiny::column(4,
                    shiny::uiOutput(ns('scd_bm_ui'))
      ),
      shiny::column(4,
                    shiny::uiOutput(ns('scd_countries_ui'))
      )
    ),
    
    shiny::br(), 
    shiny::br(),
    
    shiny::fluidRow(
      shiny::column(12,
                    shiny::uiOutput(ns("scd_tab_table_help_text")),
                    shiny::div(id = "scd_table_style_id",
                    DT::dataTableOutput(ns('scd_table')) %>% shinycssloaders::withSpinner(type = 7,color = "#154164")
             )
      )
    )
  )
}
    
#' scd_tab_module Server Functions
#'
#' @noRd 
mod_scd_tab_module_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Module Body
    
    
    #------- Initialize the Memory ----------
    selected_vals = shiny::reactiveValues(scd_country_name = 'Kenya', 
                                          scd_benchmark_name = NULL,
                                          scd_countries_name = NULL,
                                          scd_year_name = NULL
    )
    
    
    # get benchmarks for country selected
    output$scd_bm_ui <- shiny::renderUI({
      cn <- input$scd_country
      # get benchmark info for country selected
      bm_cn<- infrasap::scd_dat %>%
        # filter(`Country Name` == "Kenya") %>%
        dplyr::filter(.data$`Country Name` == cn) %>%
        dplyr::select(4:12) %>%
        dplyr::distinct() %>%
        tidyr::gather() %>%
        tidyr::drop_na() %>%
        dplyr::pull(.data$value)
      
      # only keep choices that have corresponding data on in benchmarks
      bm <- sort(unique(infrasap::scd_bm$Grouping))
      
      # bm_choices <- intersect(bm_choices, bm)
      
      # keep only regional and income group benchmarks 
      bm_keep <- 'East Asia & Pacific|Europe & Central Asia|Latin America & Caribbean|Middle East & North Africa|North America|South Asia|Sub-Saharan Africa|High income|Low income|Lower middle income|Upper middle income|Fragile|Isolated|Low Human Capital|Low Population Density|Mountainous|OECD members|Oil Exporter'
      bm_choices <- bm[grepl(bm_keep, bm)]
      bm_selected <- infrasap::dat_country_income_region %>%
        dplyr::filter(.data$`Country Name` == input$scd_country)
      
      # Check after deployment (Sometimes AWS )
      bm_choices <- list(
        "Region" = bm_choices[stringr::str_detect(bm_choices, 'Asia|America|Africa')],
        "Income Groups" = bm_choices[stringr::str_detect(bm_choices, 'income')],
        'Exogenous' = bm_choices[!stringr::str_detect(bm_choices,'Asia|America|Africa|income')]
      )
      # bm_choices %>% print()
      
      shiny::selectizeInput(inputId = ns('scd_benchmark'),
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
    
    shiny::observeEvent(input$scd_country, {
      bn_selected_cr <- infrasap::dat_country_income_region %>%
        dplyr::filter(.data$`Country Name` == input$scd_country)

      shiny::updateSelectInput(session, "scd_benchmark",
                               selected = c(bn_selected_cr$Region)
      )
    })

    # ui output for years based on inputs selected
    output$scd_year_ui <- shiny::renderUI({
      cn <- input$scd_country
      bm <- input$scd_benchmark
      
      if(is.null(bm)){
        NULL
      } else {
        # get country data
        df <- infrasap::scd_dat %>% 
          # filter(`Country Name`== "Kenya") %>% 
          dplyr::filter(.data$`Country Name`== cn) %>%
          # filter(`Indicator Sector` %in% sc) %>%
          dplyr::select(.data$`Country Name`,.data$`Indicator Name`,.data$`1990`:.data$`2020`) %>% 
          tidyr::gather(key = 'key', value = 'value',-.data$`Indicator Name`, -.data$`Country Name`) %>% 
          tidyr::drop_na()
        
        
        # get benchmark data
        df_bm <- infrasap::scd_bm %>%
          dplyr::filter(.data$Grouping %in% bm) %>%
          # filter(Sector %in% sc) %>%
          dplyr::select(.data$Indicator, .data$`1990`:.data$`2020`) %>%
          tidyr::gather(key = 'key', value = 'value',-.data$`Indicator`) %>% 
          tidyr::drop_na()
        
        # get intersection of indicators
        int_ic <- dplyr::intersect(df$`Indicator Name`, df_bm$Indicator)
        
        # subset data by common intersection
        df <- df %>% dplyr::filter(.data$`Indicator Name` %in% int_ic)
        df_bm <- df_bm %>% dplyr::filter(.data$`Indicator` %in% int_ic)
        
        year_choices <- dplyr::intersect(unique(df$key), unique(df_bm$key))
        
        shiny::selectInput(ns('scd_year'), 
                           label = 'Select year',
                           choices = c("Latest year available", year_choices), 
                           selected  = year_choices[length(year_choices)]
        )
        
      }
      
    }) # /output$scd_year_ui
    
    output$scd_countries_ui <- shiny::renderUI({
      cn <- input$scd_country
      
      
      df <- infrasap::scd_dat %>%
              dplyr::filter(.data$`Country Name`==cn) %>%
              # filter(`Indicator Sector` %in% sc ) %>%
              dplyr::select(.data$`Indicator Name`,.data$`Region`) %>%
              tidyr::drop_na() %>%
              dplyr::select(.data$`Region`, .data$`Indicator Name`)
      
      ic <- sort(unique(df$`Indicator Name`))
      rn <- unique(df$Region)

      # subset data by region pillar and sector to get countries from that region with available data
      df <- infrasap::scd_dat %>%
        # filter(`Indicator Sector` %in% sc ) %>%
        dplyr::filter(.data$Region == rn) %>%
        dplyr::filter(.data$`Indicator Name` %in% ic) %>%
        dplyr::select(.data$`Country Name`) %>%
        tidyr::drop_na()
      
      # get region names 
      cnames <- sort(unique(df$`Country Name`))
      
      # get all countries 
      all_cnames <- trimws(infrasap::dat$`Country Name`[!duplicated(infrasap::dat$`Country Code`)])
      
      # remove region names from all countries
      all_cnames <- setdiff(all_cnames, cnames)
      
      # combine region on top of all names 
      #cnames <- c(cnames, all_cnames)
      
      shiny::selectizeInput(inputId = ns('scd_countries'),
                             label = 'Select countries for comparison',
                             choices = all_cnames[all_cnames != input$scd_country],
                             multiple = TRUE,
                             options = list(
                               maxItems = 5,
                               'plugins' = list('remove_button'),
                               'create' = TRUE,
                               'persist' = FALSE
                             )
      )
      
      
      
    })
    
    
    table_reactive <- shiny::reactive({
      cn <- input$scd_country
      bm <- input$scd_benchmark
      cc <- input$scd_countries
      
      # cn <- "Kenya"
      # bm <- "East Asia & Pacific"
      # cc <- c("Angola", "Belgium")
      
      yr <- as.character(get_year_scd(cn = cn, bm = bm, year_position = "last"))
      
      # get data based on inputs
      # infrasap::scd_dat_names_unique %>% select(`Indicator Name`, `Indicator Sector`) %>% distinct()
      df <- infrasap::scd_dat %>% 
        dplyr::filter(.data$`Country Name`%in% cn) %>% 
        # filter(`Indicator Sector` %in% sc) %>%
        dplyr::select(.data$`Country Name`,.data$`Indicator Name`, .data$`Type of Benchmark`, yr) 
      
        
      df <- df %>% 
        dplyr::mutate(`Type of Benchmark` = dplyr::if_else(is.na(.data$`Type of Benchmark`), 'Upper', .data$`Type of Benchmark`))
      
      available_years <- c()
      range <- c((as.numeric(yr) - 1):(as.numeric(get_year_scd(cn = cn, bm = bm))))
      range <- as.character(range)
      
      df <- df %>%
        dplyr::mutate(
          year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
        ) 
      

      purrr::map(1:length(range), function(x) {
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
      
      df_years_col <- df %>% dplyr::select(.data$`Indicator Name`, .data$`year_pop`)
      # benchmark data 
      # TO DO ADD Benchmark length for multiple choices
      df <- infrasap::scd_bm %>% 
        dplyr::filter(.data$Grouping %in% bm) %>%
        # filter(Sector %in% sc) %>%
        dplyr::select(.data$Grouping, .data$Indicator, available_years_in_use) %>%
        dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
      
      df <- df %>%
        dplyr::mutate(year_tooltip = .data$year_pop)
      
      
      df <- df %>% dplyr::select(-available_years)
      
      purrr::map(1:length(available_years_in_use), function(b){
        df <<- df %>%
          dplyr::mutate(year_pop = dplyr::if_else(.data$year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}.x")), .data$year_pop)
          )
      })[length(available_years_in_use)]
      
      df <- df %>%
        dplyr::rename(
          !!col_sym_conv((df %>% dplyr::select(.data$`Country Name`) %>% dplyr::pull() %>% unique())) := !!col_sym_conv(stringr::str_glue("{yr}.y"))
        ) %>% dplyr::select(-dplyr::contains(".x"), -dplyr::contains(".y"), -.data$`Country Name`) %>% dplyr::distinct() %>%
        tidyr::pivot_wider(names_from = .data$`Grouping`, values_from = .data$`year_pop`)
      # value now contains list of values for indicator "Capital Budget Execution Ratio" in bm col.

      # Countries list to compare
      if((!is.null(cc)) && (length(cc) > 0)){
        
        # get infrasap data based on inputs to get the benchmark type and join
        df_cn <- infrasap::scd_dat %>% 
          dplyr::filter(.data$`Country Name`%in% cc) %>% 
          # dplyr::filter(`Indicator Sector` %in% sc) %>%
          dplyr::select(.data$`Country Name`,.data$`Indicator Name`, available_years_in_use) %>%
          dplyr::select(-c(.data$`Country Name`, available_years_in_use))
        
        if(length(cc) == 1) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::distinct() 
          
          ### List removal from column
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], replace_null_to_na)
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], `[[`, 1)
          df_cn[cc[1]][[1]] <- as.numeric(df_cn[cc[1]][[1]])
        }
        
        if(length(cc) == 2) {
          df_cn <- df_cn %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[1], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::full_join(
              country_to_compare_scd(cc[2], available_years_in_use, df_years_col)
            ) %>% 
            dplyr::distinct()
          
          
          ### List removal from column
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], replace_null_to_na)
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], `[[`, 1)
          df_cn[cc[1]][[1]] <- as.numeric(df_cn[cc[1]][[1]])
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], replace_null_to_na)
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], `[[`, 1)
          df_cn[cc[2]][[1]] <- as.numeric(df_cn[cc[2]][[1]])
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
            dplyr::distinct()
          
          ### List removal from column
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], replace_null_to_na)
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], `[[`, 1)
          df_cn[cc[1]][[1]] <- as.numeric(df_cn[cc[1]][[1]])
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], replace_null_to_na)
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], `[[`, 1)
          df_cn[cc[2]][[1]] <- as.numeric(df_cn[cc[2]][[1]])
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], replace_null_to_na)
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], `[[`, 1)
          df_cn[cc[3]][[1]] <- as.numeric(df_cn[cc[3]][[1]])
          
        }
        
        
        if(length(cc) == 4) {
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
            dplyr::full_join(
              country_to_compare_scd(cc[4], available_years_in_use, df_years_col)
            ) %>%
            dplyr::distinct()
          
          ### List removal from column
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], replace_null_to_na)
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], `[[`, 1)
          df_cn[cc[1]][[1]] <- as.numeric(df_cn[cc[1]][[1]])
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], replace_null_to_na)
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], `[[`, 1)
          df_cn[cc[2]][[1]] <- as.numeric(df_cn[cc[2]][[1]])
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], replace_null_to_na)
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], `[[`, 1)
          df_cn[cc[3]][[1]] <- as.numeric(df_cn[cc[3]][[1]])
          df_cn[cc[4]][[1]] <- lapply(df_cn[cc[4]][[1]], replace_null_to_na)
          df_cn[cc[4]][[1]] <- lapply(df_cn[cc[4]][[1]], `[[`, 1)
          df_cn[cc[4]][[1]] <- as.numeric(df_cn[cc[4]][[1]])
          
        }
        
        
        if(length(cc) == 5) {
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
            dplyr::full_join(
              country_to_compare_scd(cc[4], available_years_in_use, df_years_col)
            ) %>%
            dplyr::full_join(
              country_to_compare_scd(cc[5], available_years_in_use, df_years_col)
            ) %>%
            dplyr::distinct()
          
          ### List removal from column
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], replace_null_to_na)
          df_cn[cc[1]][[1]] <- lapply(df_cn[cc[1]][[1]], `[[`, 1)
          df_cn[cc[1]][[1]] <- as.numeric(df_cn[cc[1]][[1]])
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], replace_null_to_na)
          df_cn[cc[2]][[1]] <- lapply(df_cn[cc[2]][[1]], `[[`, 1)
          df_cn[cc[2]][[1]] <- as.numeric(df_cn[cc[2]][[1]])
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], replace_null_to_na)
          df_cn[cc[3]][[1]] <- lapply(df_cn[cc[3]][[1]], `[[`, 1)
          df_cn[cc[3]][[1]] <- as.numeric(df_cn[cc[3]][[1]])
          df_cn[cc[4]][[1]] <- lapply(df_cn[cc[4]][[1]], replace_null_to_na)
          df_cn[cc[4]][[1]] <- lapply(df_cn[cc[4]][[1]], `[[`, 1)
          df_cn[cc[4]][[1]] <- as.numeric(df_cn[cc[4]][[1]])
          df_cn[cc[5]][[1]] <- lapply(df_cn[cc[5]][[1]], replace_null_to_na)
          df_cn[cc[5]][[1]] <- lapply(df_cn[cc[5]][[1]], `[[`, 1)
          df_cn[cc[5]][[1]] <- as.numeric(df_cn[cc[5]][[1]])
          
        }
        
        
        df <- df %>%
          dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
        
      }
      

      df <- df %>% 
        dplyr::select(.data$year_tooltip, .data$`Indicator`, cn, dplyr::everything()) 
      
      
      
      if((!is.null(cc)) && (length(cc) > 0)){
        purrr::map(1:length(input$scd_countries), function(x){
          df <<- case_when_for_value_setting_chr(df, input$scd_country, input$scd_countries[x], paste0('value_c', x), tab = 'scd')
        })
      }
      
      if((!is.null(bm)) && (length(bm) > 0)){
        purrr::map(1:length(input$scd_benchmark), function(x){
          df <<- case_when_for_value_setting_chr(df, input$scd_country, input$scd_benchmark[x],paste0('value_b', x), tab = 'scd')
        })
      }
      
      
      df <- df %>% dplyr::select(-.data$`Type of Benchmark`)
      
      # get rid of string named `NA`
      df <- df[names(df) != "NA"]
      
      a <- infrasap::scd_dat %>% dplyr::select(.data$`grouping`, .data$`Indicator Name`) %>% dplyr::distinct()
      b <- infrasap::scd_bm %>% dplyr::select(.data$`Indicator`) %>% dplyr::distinct()
      df_ind <- a %>% dplyr::full_join(b, by = c("Indicator Name" = "Indicator"))
      df <- df %>% dplyr::full_join(df_ind, by = c("Indicator" = "Indicator Name"))
      
      # print(infrasap::scd_indicators)
      # print(df)
      # glimpse(df)
      
      df <- infrasap::scd_indicators %>% dplyr::left_join(df, by = c("Indicator Name" = "Indicator", 
                                                                     "grouping" = "grouping")) %>%
            dplyr::rename(Indicator = .data$`Indicator Name`)
      
      df <- df %>% 
              dplyr::select(.data$year_tooltip, .data$`grouping`, dplyr::everything()) %>%
              dplyr::mutate(`grouping` = forcats::as_factor(.data$`grouping`)) %>%
              dplyr::mutate(`grouping` = forcats::lvls_revalue(.data$`grouping`, c(
                                                                            'Economic Linkages',
                                                                            'Poverty and Equity',
                                                                            'Quality of Infrastructure',
                                                                            'Governance',
                                                                            'Finance',
                                                                            'Climate change'))) %>%
              dplyr::arrange(.data$`grouping`)
      
      
      return(df)
      
    })
    
    output$scd_tab_table_help_text <- shiny::renderUI({
      htmltools::HTML(
        as.character(stringr::str_glue('<h4 style="color: #808080; text-align: justify; margin-bottom: 20px; line-height: 1.5;">
                                        The table below displays indicator data from the latest year available for {add_article_to_selected_country(input$scd_country)}. 
                                        Comparison countries {countries_to_compare_into_one_string(input$scd_countries)} display data for the same year. 
                                        The year of data for each indicator can be seen by hovering the mouse over the data.
                                        </h4>
                                        '))
      )
    })
    
    # Table for scd 
    output$scd_table <- DT::renderDataTable({
      bm <- input$scd_benchmark
      cc <- input$scd_countries
      shiny::req(table_reactive())
      df <- table_reactive()
      
      hiddenColNum <- c()
      
      # print(df)

      if(!is.null(bm)){
        purrr::map(1:length(input$scd_benchmark), function(x){
          hiddenColNum <<- c(hiddenColNum, which(colnames(df) == as.character(stringr::str_glue('value_b{x}'))))
        })
      }
      
      if(!is.null(cc)){
        purrr::map(1:length(input$scd_countries), function(x){
          hiddenColNum <<- c(hiddenColNum, which( colnames(df) == as.character(stringr::str_glue('value_c{x}'))))
        })
      }
      
      hiddenColNum <- hiddenColNum-1
      
      df <- df %>% dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), round, 2))
              # %>%
                # mutate(!!col_sym_conv(input$scd_country) := as.character(str_glue('<span>{Kenya}<sub>{year_tooltip}</sub></span>')))
      
      # df[[input$scd_country]] <- as.character(str_glue('<span>{ df[[input$scd_country]]}<sub>{ df$year_tooltip}</sub></span>'))
      
      # datatable(df) 
      dtable <- DT::datatable(df,
                              rownames = FALSE,
                              extensions = 'Buttons',
                              escape=FALSE,
                              options = list(pageLength = nrow(df),
                                             ordering = FALSE,
                                             rowCallback = DT::JS(
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
                                                                    targets=c(hiddenColNum)
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
        purrr::map(1:length(input$scd_countries), function(x) {
          dtable <<- dtable %>% DT::formatStyle(
            as.character(input$scd_countries[x]), as.character(stringr::str_glue('value_c{x}')),
            backgroundColor = DT::styleEqual(
              c(0, 1, 2, 3),
              c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
            )
          )
        })
      }
      
      if(!is.null(bm)) {
        purrr::map(1:length(input$scd_benchmark), function(x) {
          dtable <<- dtable %>% DT::formatStyle(
            as.character(input$scd_benchmark[x]), as.character(stringr::str_glue('value_b{x}')),
            backgroundColor = DT::styleEqual(
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
