#' infrasap_tab_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infrasap_tab_module_ui <- function(id){
  ns <- NS(id)
  htmltools::tagList(
    shiny::div(class = "controlSection",
               shiny::fluidRow(
                 shiny::column(4, 
                               shiny::selectInput(inputId = ns('db_country'),
                                                   label = 'Select country',
                                                   choices = sort(unique(infrasap::dat$`Country Name`)),
                                                   selected = 'Kenya')
          ),
          shiny::column(4, 
                        shiny::selectInput(inputId = ns('db_sector'),
                                           label = 'Select sector',
                                           choices = c('Energy', 
                                                       'Cross-cutting',
                                                       'Digital Development',
                                                       'Transport'),
                                           selected = 'Energy')
                 
          ),
          shiny::column(4, 
                        shiny::selectInput(inputId = ns('db_pillar'),
                                           label = 'Select a pillar',
                                           choices = c('Connectivity', 'Finance', 'Governance'),
                                           selected = 'Connectivity')
          )
        ),
        shiny::fluidRow(
          shiny::column(4, 
                        shiny::uiOutput(ns('countriestc')),
                        shiny::downloadButton(ns("report_pdf"), "Generate report")
          ),
          shiny::column(4, 
                        shiny::selectizeInput(inputId = ns('db_benchmark'),
                                              label = 'Select benchmark',
                                              choices = NULL,
                                              selected = NULL,
                                              multiple = TRUE,
                                              options = list(
                                                maxItems = 3,
                                                'plugins' = list('remove_button'),
                                                'create' = TRUE,
                                                'persist' = FALSE
                                              )
                 )
          ),
          shiny::column(4, 
                        shiny::selectInput(inputId = ns('db_year'),
                                     label = 'Select year',
                                     choices = NULL,
                                     selected = NULL)
          )
      )
    ),
    shiny::div(id = "inrasaptablecomp",
               shiny::uiOutput(ns('emptyDataTableMSG')),
               DT::dataTableOutput(ns('db_table'))
    )
  )
}
    
#' infrasap_tab_module Server Functions
#'
#' @noRd 
mod_infrasap_tab_module_server <- function(id){
  
  infrasap_dat_mod_modified <- infrasap::dat %>%
    dplyr::filter(`irf_data` == FALSE)
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  infrsap_dat_bm_mod_modfied <- infrasap::dat_bm
  infrsap_dat_bm_mod_modfied$Sector[infrsap_dat_bm_mod_modfied$Sector == "National"] <- "Cross-cutting"
  
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Module Body
    #------- Initialize the Memory ----------
    selected_vals = shiny::reactiveValues(db_country_name = 'Kenya', 
                                          db_sector_name = 'Energy',
                                          db_pillar_name = 'Connectivity',
                                          db_benchmark_name = 'Region',
                                          db_countries_name = NULL,
                                          db_year_name = "Latest year available"
    )
    
    shiny::observe({
      shiny::req(input$db_country, input$db_sector, input$db_year, input$db_benchmark, input$country_to_compare_id, input$db_pillar)
      selected_vals$db_country_name <- input$db_country
      selected_vals$db_sector_name <- input$db_sector
      selected_vals$db_benchmark_name <- input$db_benchmark
      selected_vals$db_year_name <- input$db_year
      selected_vals$db_countries_name <- input$country_to_compare_id
      selected_vals$db_pillar_name <- input$db_pillar
    })
    
    shiny::observeEvent(input$db_sector, {
      if(input$db_sector == 'Cross-cutting') {
        shiny::updateSelectInput(session,
                          'db_pillar',
                          choices = c('Finance', 'Governance')
                          )
      } else {
        shiny::updateSelectInput(session,
                          'db_pillar',
                          choices = c('Connectivity', 'Finance', 'Governance'),
                          selected = selected_vals$db_pillar_name
        )
      }
    })
    
    # Update year field data according to selections in the other fields
    shiny::observe({
      # add national automatically to sector (as in the excel tool)
      if(is.null(input$db_benchmark)) {
        NULL
      } else {
        year_choices <- get_years(infrasap_dat_mod_modified, infrsap_dat_bm_mod_modfied)
        
        if(selected_vals$db_year_name %in% year_choices){
          selected_year <- selected_vals$db_year_name
        } else {
          selected_year <- "Latest year available"
        }
        year_choices <- sort(year_choices, decreasing = T)
        shiny::updateSelectInput(session = session, 
                                  inputId = "db_year",
                                  choices = c("Latest year available", year_choices),
                                  selected = selected_year)
      }
    })
    
    # Update benchmark field data according to selections in the fields
    shiny::observe({
      bm_list <- benchmark_dropdown_manipulation(infrasap_dat_mod_modified, infrsap_dat_bm_mod_modfied, input$db_country)
      shiny::updateSelectizeInput(session = session, 
                                   inputId = "db_benchmark",
                                   choices = bm_list,
                                   selected = selected_vals$db_benchmark_name)
    })
    
    #Deleting the redundant code about creating db_benchmark selectInput
    # Create benchmark field
    # output$db_benchmark_ui <- shiny::renderUI({....})
    
    # Array of countries selected
    countriesOptionsInput <- shiny::reactive({
      shiny::req(input$db_country)
      selectedCountryOptions <- country_to_compare_vec(infrasap_dat_mod_modified, input$db_country, input$db_sector, input$db_pillar)
      return(selectedCountryOptions)
    })
    
    # Create countries field
    output$countriestc <- shiny::renderUI({
      shiny::req(countriesOptionsInput())
      countryList <- country_to_compare_list(infrasap_dat_mod_modified, input$db_sector, input$db_pillar)
      
      if(is.null(selected_vals$db_countries_name)) {
        shiny::selectizeInput(inputId = ns('country_to_compare_id'),
                                       label = 'Countries to compare to',
                                       choices = sort(unique(countryList)),
                                       selected = countriesOptionsInput(),
                                       multiple = TRUE,
                                       options = list(
                                         maxItems = 3,
                                         'plugins' = list('remove_button'),
                                         'create' = TRUE,
                                         'persist' = FALSE
                                       )
        )
      } else {
        shiny::selectizeInput(inputId = ns('country_to_compare_id'),
                                         label = 'Countries to compare to',
                                         choices = sort(unique(countryList)),
                                         selected = selected_vals$db_countries_name,
                                         multiple = TRUE,
                                         options = list(
                                           maxItems = 3,
                                           'plugins' = list('remove_button'),
                                           'create' = TRUE,
                                           'persist' = FALSE
                                         )
        )
      }
    })
    
    
    # Update country to compare field data according to selections in the fields
    shiny::observeEvent(input$db_country,{
      shiny::updateSelectizeInput(session,
                                 "country_to_compare_id",
                                 choices = sort(unique(infrasap::dat$`Country Name`))[sort(unique(infrasap::dat$`Country Name`)) != input$db_country],
                                 # selected = countriesOptionsInput()
                                 selected = selected_vals$db_countries_name
      )
      
      if(input$db_country %in% input$country_to_compare_id) {
        shiny::updateSelectizeInput(session,
                                   "country_to_compare_id",
                                   choices = sort(unique(infrasap::dat$`Country Name`))[sort(unique(infrasap::dat$`Country Name`)) != input$db_country],
                                   # selected = countriesOptionsInput()
                                   selected = selected_vals$db_countries_name[selected_vals$db_countries_name != input$db_country]
        )
      }
      
    })
    
    # reactive data frame to prepare data for tables
    infrasap_table <- shiny::reactive({
      cn <- input$db_country
      sc <- input$db_sector
      bm <- input$db_benchmark
      yr <- input$db_year
      pi <- input$db_pillar
      # cn <- "Kenya"
      # sc <- c("National", "Energy")
      # bm <- "Region"
      # yr <- "Latest year availbale"
      # pi <- "Connectivity"
      # add national automatically to sector (as in the excel tool)
      if(is.null(yr)){
        NULL
      } else {
        if(yr == "Latest year available") {
          if(length(input$db_benchmark) == 2 & !is.null(input$db_benchmark)) { 
            # bm <- "Region"
            # cn <- "Kenya"
            # sc <- "Energy"
            # pi <- "Finance"
            # pi <- "Connectivity"
            yr <- as.character(get_last_year(cn, sc, bm))
            
            available_years <- c()
            if(as.numeric(yr) == 2015){
              range <- c(2015)
            } else {
              range <- c((as.numeric(yr) - 1):(as.numeric(2015)))
            }
            range <- as.character(range)
            
            # get infrasap data based on inputs to get the benchmark type and join
            df_r <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Country Name` == cn) %>%
              dplyr::filter(`Indicator Sector` %in% sc) %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr, `Region`)
            
            
            df_r <- df_r %>%
              dplyr::mutate(
                year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
              )
            
            
            
            range <- range[range != yr]
            
            a <- purrr::map(1:length(range), function(x){
              df_r <<- fill_missing_values_in_years(df = df_r,
                                                    based_year = yr,
                                                    year_step_back = range[x],
                                                    country = cn,
                                                    sector = sc,
                                                    pillar = pi)
            })[[length(range)]]
            
            # df_r <- a
            
            # Years to delete
            available_years <- as.character(
              range[!(range %in% unique(df_r$year_pop))]
            )
            
            # Years in use
            available_years_in_use <- as.character(unique(df_r$year_pop))
            available_years_in_use <- available_years_in_use[!is.na(available_years_in_use)]
            yr <- as.character(max(unique(df_r$year_pop), na.rm = TRUE))
            
            
            # get benchmark type for benchmark selected
            bm_type <- unique(df_r[, "Region"])
            
            # get benchmark data based on inputs
            df_r <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Grouping == bm_type) %>%
              dplyr::filter(`Sector` %in% sc) %>%
              dplyr::select(`Indicator`, available_years_in_use) %>%
              dplyr::right_join(df_r, by = c('Indicator'='Indicator Name'))
            
            df_r <- df_r %>% dplyr::select(-available_years)
            
            
            df_r <- df_r %>%
              dplyr::mutate(year_tooltip = year_pop)
            
            purrr::map(1:length(available_years_in_use), function(b){
              df_r <<- df_r %>%
                dplyr::mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}.x")), year_pop)
                )
            })[length(available_years_in_use)]
            
            df_r <- df_r %>% dplyr::select(-`Region`)
   
            
            # Find the column where the latest year value saved 
            year_find_max_vector <- as.character(c(2020:2015))
            for (i in 1:length(year_find_max_vector)) {
              if((names(df_r)[stringr::str_detect(names(df_r), pattern = year_find_max_vector[i])] %>% length()) > 0) {
                if((names(df_r)[stringr::str_detect(names(df_r), pattern = year_find_max_vector[i])] %>% length()) == 1) {
                  yr_max_column <- year_find_max_vector[i]
                } else {
                  yr_max_column <- as.character(stringr::str_glue("{year_find_max_vector[i]}.y"))
                }
                break
              }
            }
            
            
            df_r <- df_r %>% dplyr::rename(
              # !!col_sym_conv(cn) := !!col_sym_conv(stringr::str_glue("{yr}.y")),
              !!col_sym_conv(cn) := !!col_sym_conv(yr_max_column),
              `Region` := `year_pop`
            ) %>% dplyr::select(-dplyr::contains(".x"), -dplyr::contains(".y"))
            
            
            # get names of the two columns to compare
            bm_col <- names(df_r)[grepl('.x', names(df_r), fixed = TRUE)]
            data_col <- names(df_r)[grepl('.y', names(df_r), fixed = TRUE)]
            
            # rename columns in data
            names(df_r)[names(df_r) == bm_col] <- 'Region'
            names(df_r)[names(df_r)==data_col] <- cn
            
            
            # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
            df_r$`Type of Benchmark`[is.na(df_r$`Type of Benchmark`)] <- 'Upper'
            df_r$`Type of Benchmark`[df_r$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
            
            
            
            df_r <- df_r %>%
              dplyr::mutate(value_r = dplyr::case_when(
                (df_r %>% dplyr::select(dplyr::contains(cn))) >= Region & (`Type of Benchmark` == "Upper") ~ "3",
                ((df_r %>% dplyr::select(dplyr::contains(cn))) >= (Region * 0.9) & (df_r %>% dplyr::select(dplyr::contains(cn))) < Region) & (`Type of Benchmark` == "Upper") ~ "2",
                (df_r %>% dplyr::select(dplyr::contains(cn))) < (Region * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                (df_r %>% dplyr::select(dplyr::contains(cn))) <= Region & (`Type of Benchmark` == "Lower") ~ "3",
                ((df_r %>% dplyr::select(dplyr::contains(cn))) <= (Region * 1.1) & (df_r %>% dplyr::select(dplyr::contains(cn))) > Region) & (`Type of Benchmark` == "Lower") ~ "2",
                (df_r %>% dplyr::select(dplyr::contains(cn))) > (Region * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                TRUE ~ "0"
              )
              ) %>% 
              # Fill NAs with grey color
              dplyr::mutate(value_r = dplyr::case_when(
                is.na(df_r %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                TRUE ~ value_r
              )) %>%
              dplyr::mutate(value_r = value_r %>% as.numeric())
            
            
            
            
            # make table with all combinations of pillar, sub-pillar, and topic
            df_large <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::filter(`Indicator Sector`== sc) %>%
              dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
              dplyr::summarise(counts = dplyr::n()) %>%
              dplyr::select(-counts)
            
            # join data
            df_r <- dplyr::left_join(df_large, df_r)
            
            
            # Rename columns
            df_r <- df_r %>%
              dplyr::rename(
                `Sub-Pillar` = `Indicator Sub-Pillar`,
                `Topic`= `Indicator Topic`
              )
            
            
            # subset data by the columns used in the table
            df_r <- df_r %>% dplyr::select(`Sub-Pillar`,
                                           `Topic`, 
                                           `Indicator`, 
                                           cn, 
                                           `Region`,
                                           value_r,
                                           year_tooltip)
            
            
            # round numbers
            df_r[[cn]] <- round(df_r[[cn]], 2)
            df_r[['Region']] <- round(df_r[['Region']], 2)
            
            
            
            # get infrasap data based on inputs to get the benchmark type and join
            df_i <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Country Name` == cn) %>%
              dplyr::filter(`Indicator Sector` %in% sc) %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr, `IncomeGroup`)
            
            
            df_i <- df_i %>%
              dplyr::mutate(
                year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
              )
            
            range <- range[range != yr]
            
            a <- purrr::map(1:length(range), function(x){
              df_i <<- fill_missing_values_in_years(df = df_i,
                                                    based_year = yr,
                                                    year_step_back = range[x],
                                                    country = cn,
                                                    sector = sc,
                                                    pillar = pi)
            })[[length(range)]]
            
            # df_i <- a
            
            # Years to delete
            available_years <- as.character(
              range[!(range %in% unique(df_i$year_pop))]
            )
            
            # Years in use
            available_years_in_use <- as.character(unique(df_i$year_pop))
            available_years_in_use <- available_years_in_use[!is.na(available_years_in_use)]
            yr <- as.character(max(unique(df_i$year_pop), na.rm = TRUE))
            
            df_years_col <- df_i %>% dplyr::select(`Indicator Name`, `year_pop`)
            
            
            # get benchmark type for benchmark selected
            bm_type <- unique(df_i[, "IncomeGroup"])
            
            # get benchmark data based on inputs
            df_i <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Grouping == bm_type) %>%
              dplyr::filter(`Sector` %in% sc) %>%
              dplyr::select(`Indicator`, available_years_in_use) %>%
              dplyr::right_join(df_i, by = c('Indicator'='Indicator Name'))
            
            df_i <- df_i %>% dplyr::select(-available_years)
            
            
            purrr::map(1:length(available_years_in_use), function(b){
              df_i <<- df_i %>%
                dplyr::mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}.x")), year_pop)
                )
            })[length(available_years_in_use)]
            
            df_i <- df_i %>% dplyr::select(-`IncomeGroup`)
            
            year_find_max_vector <- as.character(c(2020:2015))
            for (i in 1:length(year_find_max_vector)) {
              if((names(df_i)[stringr::str_detect(names(df_i), pattern = year_find_max_vector[i])] %>% length()) > 0) {
                if((names(df_i)[stringr::str_detect(names(df_i), pattern = year_find_max_vector[i])] %>% length()) == 1) {
                  yr_max_column <- year_find_max_vector[i]
                } else {
                  yr_max_column <- as.character(stringr::str_glue("{year_find_max_vector[i]}.y"))
                }
                break
              }
            }
            
            df_i <- df_i %>% dplyr::rename(
              # !!col_sym_conv(cn) := !!col_sym_conv(stringr::str_glue("{yr}.y")),
              !!col_sym_conv(cn) := !!col_sym_conv(yr_max_column),
              `IncomeGroup` := `year_pop`
            ) %>% dplyr::select(-dplyr::contains(".x"), -dplyr::contains(".y"))
            
            
            
            # get names of the two columns to compare
            bm_col <- names(df_i)[grepl('.x', names(df_i), fixed = TRUE)]
            data_col <- names(df_i)[grepl('.y', names(df_i), fixed = TRUE)]
            
            # rename columns in data
            names(df_i)[names(df_i) == bm_col] <- 'IncomeGroup'
            names(df_i)[names(df_i)==data_col] <- cn
            
            df_i$`Type of Benchmark`[is.na(df_i$`Type of Benchmark`)] <- 'Upper'
            df_i$`Type of Benchmark`[df_i$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
            
            df_i <- df_i %>%
              dplyr::mutate(value_i = dplyr::case_when(
                (df_i %>% dplyr::select(dplyr::contains(cn))) >= IncomeGroup & (`Type of Benchmark` == "Upper") ~ "3",
                ((df_i %>% dplyr::select(dplyr::contains(cn))) >= (IncomeGroup * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < IncomeGroup) & (`Type of Benchmark` == "Upper") ~ "2",
                (df_i %>% dplyr::select(dplyr::contains(cn))) < (IncomeGroup * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                (df_i %>% dplyr::select(dplyr::contains(cn))) <= IncomeGroup & (`Type of Benchmark` == "Lower") ~ "3",
                ((df_i %>% dplyr::select(dplyr::contains(cn))) <= (IncomeGroup * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > IncomeGroup) & (`Type of Benchmark` == "Lower") ~ "2",
                (df_i %>% dplyr::select(dplyr::contains(cn))) > (IncomeGroup * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                TRUE ~ "0"
              )
              ) %>% 
              # Fill NAs with grey color
              dplyr::mutate(value_i = dplyr::case_when(
                is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                TRUE ~ value_i
              )) %>%
              dplyr::mutate(value_i = value_i %>% as.numeric())
            
            
            
            if(!is.null(input$country_to_compare_id)){
              
              
              # get infrasap data based on inputs to get the benchmark type and join
              df_cn <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
                dplyr::select(-c(`Country Name`, available_years_in_use))
              
              
              if(length(input$country_to_compare_id) == 1) {
                df_cn <- df_cn %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::distinct()
              }
              
              if(length(input$country_to_compare_id) == 2) {
                df_cn <- df_cn %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::distinct()
              }
              
              if(length(input$country_to_compare_id) == 3) {
                df_cn <- df_cn %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::full_join(
                  country_to_compare(input$country_to_compare_id[3], sc, pi, available_years_in_use, df_years_col)
                ) %>% dplyr::distinct()
              }
              
              
              df_i <- df_i %>%
                dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
              
              
            }
            
            
            
            # Country to compare equals 1
            if(length(input$country_to_compare_id) == 1){
              cn1 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
            }
            
            if(length(input$country_to_compare_id) == 2){
              cn2 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn2[1])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn2[1])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c2 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn2[2])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn2[2])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c2 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c2
                )) %>%
                dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
              
            }
            
            
            if(length(input$country_to_compare_id) == 3){
              cn3 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[1])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[1])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c2 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[2])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[2])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c2 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c2
                )) %>%
                dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c3 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[3])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[3])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[3])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[3])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c3 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c3
                )) %>%
                dplyr::mutate(value_c3 = value_c3 %>% as.numeric())
              
            }
            
            
            
            # join data
            df_i <- dplyr::left_join(df_large, df_i)
            
            # Rename columns
            df_i <- df_i %>%
              dplyr::rename(
                `Sub-Pillar` = `Indicator Sub-Pillar`,
                `Topic`= `Indicator Topic`
              )
            
            
            if(length(input$country_to_compare_id) == 1) {
              # subset data by the columns used in the table
              df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1)
              df_i[[input$country_to_compare_id]] <- round(df_i[[input$country_to_compare_id]], 2)
            } else {
              if(length(input$country_to_compare_id) == 2){
                df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2)
                df_i[[input$country_to_compare_id[1]]] <- round(df_i[[input$country_to_compare_id[1]]], 2)
                df_i[[input$country_to_compare_id[2]]] <- round(df_i[[input$country_to_compare_id[2]]], 2)
              } else {
                if(length(input$country_to_compare_id) == 3){
                  df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3)
                  df_i[[input$country_to_compare_id[1]]] <- round(df_i[[input$country_to_compare_id[1]]], 2)
                  df_i[[input$country_to_compare_id[2]]] <- round(df_i[[input$country_to_compare_id[2]]], 2)
                  df_i[[input$country_to_compare_id[3]]] <- round(df_i[[input$country_to_compare_id[3]]], 2)
                } else {
                  # subset data by the columns used in the table
                  df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i)
                }
              }
            }
            
            
            
            # round numbers
            df_i[[cn]] <- round(df_i[[cn]], 2)
            df_i[['IncomeGroup']] <- round(df_i[['IncomeGroup']], 2)
            
            
            df <- dplyr::full_join(df_r, df_i) 
            
            df <- df %>% dplyr::mutate(year_tooltip_b = year_tooltip) %>% dplyr::select(-year_tooltip) %>% dplyr::rename(year_tooltip = year_tooltip_b)
            
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
            }
            
            if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
            }
            
            if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
            }
            
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
            }
            
            
            if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
            }
            
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
            }
            
            if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
            }
            
            return(df)
 
            
          } else {
            
            if(length(input$db_benchmark) == 0 & is.null(input$db_benchmark)) {

              
              yr <- as.character(get_last_year(cn, sc))
              # yr <- as.character(get_last_year("Kenya", "Energy", "Region"))
              # bm <- NULL
              # cn <- "Kenya"
              # sc <- "Energy"
              # pi <- "Finance"
              # pi <- "Connectivity"
              
              
              
              # get infrasap data based on inputs to get the benchmark type and join
              df <- infrasap_dat_mod_modified %>%
                # dplyr::filter(`Country Name` == "Kenya") %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                # dplyr::filter(`Indicator Sector` %in% "Energy") %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                # dplyr::filter(`Indicator Pillar` == "Finance") %>%
                dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr )
              
              
              
              available_years <- c()
              if(as.numeric(yr) == 2015) {
                range <- c(as.numeric(2015))
              } else {
                range <- c((as.numeric(yr) - 1):(as.numeric(2015)))
              }
              
              range <- as.character(range)
              
              
              df <- df %>%
                dplyr::mutate(
                  year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
                )
              
              a <- purrr::map(1:length(range), function(x){
                df <<- fill_missing_values_in_years(df = df,
                                                    based_year = yr,
                                                    year_step_back = range[x],
                                                    country = cn,
                                                    sector = sc,
                                                    pillar = pi)
              })[[length(range)]]
              
              # df <- a
              
              # df <- df %>% dplyr::mutate(year_tooltip = year_pop)
              
              # Years to delete
              available_years <- as.character(
                as.numeric(range)[!(as.numeric(range) %in% unique(df$year_pop))]
              )
              
              # Years in use
              available_years_in_use <- as.character(unique(df$year_pop))
              available_years_in_use <- available_years_in_use[!is.na(available_years_in_use)]
              yr <- as.character(max(unique(df$year_pop), na.rm = TRUE))

              df_years_col <- df %>% dplyr::select(`Indicator Name`, `year_pop`)
              
              # get benchmark type for benchmark selected
              # bm_type <- unique(df[, bm])
              
              # get benchmark data based on inputs
              # df <- infrsap_dat_bm_mod_modfied %>%
              #   dplyr::filter(Grouping == bm_type) %>%
              #   dplyr::filter(`Sector` %in% sc) %>%
              #   dplyr::select(`Indicator`, available_years_in_use) %>%
              #   dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
              
              
              
              df <- df %>% dplyr::select(-available_years)
              
              
              
              # purrr::map(1:length(available_years_in_use), function(b){
              #   df <<- df %>%
              #     dplyr::mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}.x")), year_pop)
              #     )
              # })[length(available_years_in_use)]
              
              # df <- df %>% dplyr::select(-bm)
              
              # Find the column where the latest year value saved 
              year_find_max_vector <- as.character(c(2020:2015))
              for (i in 1:length(year_find_max_vector)) {
                if((names(df)[stringr::str_detect(names(df), pattern = year_find_max_vector[i])] %>% length()) > 0) {
                  if((names(df)[stringr::str_detect(names(df), pattern = year_find_max_vector[i])] %>% length()) == 1) {
                    yr_max_column <- year_find_max_vector[i]
                  } else {
                    yr_max_column <- as.character(stringr::str_glue("{year_find_max_vector[i]}.y"))
                  }
                  break
                }
              }
              
              
              
              df <- df %>% dplyr::rename(
                # !!col_sym_conv(cn) := !!col_sym_conv(stringr::str_glue("{yr}.y")),
                !!col_sym_conv(cn) := !!col_sym_conv(yr_max_column),
                # ,
                # !!col_sym_conv(bm) := `year_pop`
                year_tooltip = `year_pop`
              ) %>% dplyr::select(-dplyr::contains(".x"), -dplyr::contains(".y"), -dplyr::contains(year_find_max_vector)) 
              
              
              # get names of the two columns to compare
              # bm_col <- names(df)[grepl('.x', names(df), fixed = TRUE)]
              data_col <- names(df)[grepl('.y', names(df), fixed = TRUE)]
              
              # rename columns in data
              # names(df)[names(df)==bm_col] <- bm
              names(df)[names(df)==data_col] <- cn
              
              # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
              df$`Type of Benchmark`[is.na(df$`Type of Benchmark`)] <- 'Upper'
              df$`Type of Benchmark`[df$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
              
              # Countries list to compare
              if(!is.null(input$country_to_compare_id)){
                
                
                # get infrasap data based on inputs to get the benchmark type and join
                df_cn <- infrasap_dat_mod_modified %>%
                  # dplyr::filter(`Country Name` %in% "Angola") %>%
                  dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::filter(`Indicator Pillar` == pi) %>%
                  dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
                  dplyr::select(-c(`Country Name`, available_years_in_use))
                
                
                if(length(input$country_to_compare_id) == 1) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                    # country_to_compare("Angola", sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                if(length(input$country_to_compare_id) == 2) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                if(length(input$country_to_compare_id) == 3) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[3], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                
                
                
                df <- df %>%
                  dplyr::rename(
                    Indicator =`Indicator Name`
                  ) %>%
                  dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
                
                
              }
              
              # df <- df %>%
              #   dplyr::mutate(value = dplyr::case_when(
              #     # TRUE ~ "0",
              #     (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
              #     ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
              #     (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
              #     
              #     (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
              #     ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
              #     (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
              #     TRUE ~ "0"
              #   )
              #   ) %>% 
              #   # Fill NAs with grey color
              #   dplyr::mutate(value = dplyr::case_when(
              #     is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
              #     TRUE ~ value
              #   )) %>%
              #   dplyr::mutate(value = value %>% as.numeric())
               
              
              
              # Country to compare equals 1 
              if(length(input$country_to_compare_id) == 1) {
                cn1 <- input$country_to_compare_id
                # cn1 <- "Angola"
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
              }
              
              if(length(input$country_to_compare_id) == 2){
                cn2 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
              }
              
              
              if(length(input$country_to_compare_id) == 3){
                cn3 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[3])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[3])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[3])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[3])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c3
                  )) %>%
                  dplyr::mutate(value_c3 = value_c3 %>% as.numeric())
                
              }
              
              
              
              # make table with all combinations of pillar, sub-pillar, and topic
              df_large <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::filter(`Indicator Sector`== sc) %>%
                dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
                dplyr::summarise(counts = dplyr::n()) %>%
                dplyr::select(-counts)
              
              # # join data
              df <- dplyr::left_join(df_large, df)
              
              
              
              #Rename columns
              df <- df %>%
                dplyr::rename(
                  `Sub-Pillar` = `Indicator Sub-Pillar`,
                  `Topic`= `Indicator Topic`
                )
              
              if(length(input$country_to_compare_id) == 1) {
                # subset data by the columns used in the table
                df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id, value_c1, year_tooltip)
                df[[input$country_to_compare_id]] <- round(df[[input$country_to_compare_id]], 2)
              } else {
                if(length(input$country_to_compare_id) == 2){
                  df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, year_tooltip)
                  df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                  df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                } else {
                  if(length(input$country_to_compare_id) == 3){
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3, year_tooltip)
                    df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                    df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                    df[[input$country_to_compare_id[3]]] <- round(df[[input$country_to_compare_id[3]]], 2)
                  } else {
                    # subset data by the columns used in the table
                    
                    
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, year_tooltip, cn)
                    
                    
                  }
                }
              }
              
              
              # round numbers
              df[[cn]] <- round(df[[cn]], 2)
              # df[[bm]] <- round(df[[bm]], 2)
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
              }
              print(df)
              
              return(df)
              
            } else {
              
              yr <- as.character(get_last_year(cn, sc, bm))
              # yr <- as.character(get_last_year("Kenya", "Energy", "Region"))
              # bm <- "Region"
              # cn <- "Kenya"
              # sc <- "Energy"
              # # pi <- "Finance"
              # pi <- "Connectivity"
              
              # get infrasap data based on inputs to get the benchmark type and join
              df <- infrasap_dat_mod_modified %>%
                # dplyr::filter(`Country Name` == "Kenya") %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                # dplyr::filter(`Indicator Sector` %in% "Energy") %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                # dplyr::filter(`Indicator Pillar` == "Finance") %>%
                dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`,yr, bm )
              
              
              
              available_years <- c()
              if(as.numeric(yr) == 2015) {
                range <- c(as.numeric(2015))
              } else {
                range <- c((as.numeric(yr) - 1):(as.numeric(2015)))
              }
              
              range <- as.character(range)
              
              
              df <- df %>%
                dplyr::mutate(
                  year_pop = dplyr::if_else(!is.na(!!col_sym_conv(yr)), as.numeric(yr), !!col_sym_conv(yr))
                )
              
              a <- purrr::map(1:length(range), function(x){
                df <<- fill_missing_values_in_years(df = df,
                                                    based_year = yr,
                                                    year_step_back = range[x],
                                                    country = cn,
                                                    sector = sc,
                                                    pillar = pi)
              })[[length(range)]]
              
              # df <- a
              
              df <- df %>% dplyr::mutate(year_tooltip = year_pop)
              
              # Years to delete
              available_years <- as.character(
                as.numeric(range)[!(as.numeric(range) %in% unique(df$year_pop))]
              )
              
              # Years in use
              available_years_in_use <- as.character(unique(df$year_pop))
              available_years_in_use <- available_years_in_use[!is.na(available_years_in_use)]
              yr <- as.character(max(unique(df$year_pop), na.rm = TRUE))
              
              df_years_col <- df %>% dplyr::select(`Indicator Name`, `year_pop`)
              
              # get benchmark type for benchmark selected
              bm_type <- unique(df[, bm])
              
              # get benchmark data based on inputs
              df <- infrsap_dat_bm_mod_modfied %>%
                dplyr::filter(Grouping == bm_type) %>%
                dplyr::filter(`Sector` %in% sc) %>%
                dplyr::select(`Indicator`, available_years_in_use) %>%
                dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
              
              
              
              df <- df %>% dplyr::select(-available_years)
              
              
              purrr::map(1:length(available_years_in_use), function(b){
                df <<- df %>%
                  dplyr::mutate(year_pop = dplyr::if_else(year_pop == available_years_in_use[b], !!col_sym_conv(stringr::str_glue("{available_years_in_use[b]}.x")), year_pop)
                  )
              })[length(available_years_in_use)]
              
              df <- df %>% dplyr::select(-bm)
              
              # Find the column where the latest year value saved 
              year_find_max_vector <- as.character(c(2020:2015))
              for (i in 1:length(year_find_max_vector)) {
                if((names(df)[stringr::str_detect(names(df), pattern = year_find_max_vector[i])] %>% length()) > 0) {
                  if((names(df)[stringr::str_detect(names(df), pattern = year_find_max_vector[i])] %>% length()) == 1) {
                    yr_max_column <- year_find_max_vector[i]
                  } else {
                    yr_max_column <- as.character(stringr::str_glue("{year_find_max_vector[i]}.y"))
                  }
                  break
                }
              }
              
              
              
              df <- df %>% dplyr::rename(
                # !!col_sym_conv(cn) := !!col_sym_conv(stringr::str_glue("{yr}.y")),
                !!col_sym_conv(cn) := !!col_sym_conv(yr_max_column),
                !!col_sym_conv(bm) := `year_pop`
              ) %>% dplyr::select(-dplyr::contains(".x"), -dplyr::contains(".y")) 
              
              
              
              # get names of the two columns to compare
              bm_col <- names(df)[grepl('.x', names(df), fixed = TRUE)]
              data_col <- names(df)[grepl('.y', names(df), fixed = TRUE)]
              
              # rename columns in data
              names(df)[names(df)==bm_col] <- bm
              names(df)[names(df)==data_col] <- cn
              
              # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
              df$`Type of Benchmark`[is.na(df$`Type of Benchmark`)] <- 'Upper'
              df$`Type of Benchmark`[df$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
              
              # Countries list to compare
              if(!is.null(input$country_to_compare_id)){
                
                
                # get infrasap data based on inputs to get the benchmark type and join
                df_cn <- infrasap_dat_mod_modified %>%
                  dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::filter(`Indicator Pillar` == pi) %>%
                  dplyr::select(`Country Name`, `Indicator Name`, available_years_in_use) %>%
                  dplyr::select(-c(`Country Name`, available_years_in_use))
                
                
                if(length(input$country_to_compare_id) == 1) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                if(length(input$country_to_compare_id) == 2) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                if(length(input$country_to_compare_id) == 3) {
                  df_cn <- df_cn %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[1], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[2], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::full_join(
                    country_to_compare(input$country_to_compare_id[3], sc, pi, available_years_in_use, df_years_col)
                  ) %>% dplyr::distinct()
                }
                
                
                
                
                df <- df %>%
                  dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
                
                
              }
              
              df <- df %>%
                dplyr::mutate(value = dplyr::case_when(
                  # TRUE ~ "0",
                  (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  
                  (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>% 
                # Fill NAs with grey color
                dplyr::mutate(value = dplyr::case_when(
                  is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value
                )) %>%
                dplyr::mutate(value = value %>% as.numeric())
              
              
              
              
              # Country to compare equals 1 
              if(length(input$country_to_compare_id) == 1){
                cn1 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
              }
              
              if(length(input$country_to_compare_id) == 2){
                cn2 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
              }
              
              
              if(length(input$country_to_compare_id) == 3){
                cn3 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[3])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[3])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[3])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[3])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c3
                  )) %>%
                  dplyr::mutate(value_c3 = value_c3 %>% as.numeric())
                
              }
              
              
              
              # make table with all combinations of pillar, sub-pillar, and topic
              df_large <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::filter(`Indicator Sector`== input$db_sector) %>%
                dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
                dplyr::summarise(counts = dplyr::n()) %>%
                dplyr::select(-counts)
              
              # # join data
              df <- dplyr::left_join(df_large, df)
              
              
              
              #Rename columns
              df <- df %>%
                dplyr::rename(
                  `Sub-Pillar` = `Indicator Sub-Pillar`,
                  `Topic`= `Indicator Topic`
                )
              
              if(length(input$country_to_compare_id) == 1) {
                # subset data by the columns used in the table
                df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id, value_c1, year_tooltip)
                df[[input$country_to_compare_id]] <- round(df[[input$country_to_compare_id]], 2)
              } else {
                if(length(input$country_to_compare_id) == 2){
                  df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, year_tooltip)
                  df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                  df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                } else {
                  if(length(input$country_to_compare_id) == 3){
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3, year_tooltip)
                    df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                    df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                    df[[input$country_to_compare_id[3]]] <- round(df[[input$country_to_compare_id[3]]], 2)
                  } else {
                    # subset data by the columns used in the table
                    
                    
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, year_tooltip, cn, bm, value)
                    
                    
                  }
                }
              }
              
              
              # round numbers
              df[[cn]] <- round(df[[cn]], 2)
              df[[bm]] <- round(df[[bm]], 2)
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
              }

              return(df)
              
            }
            
            
            
          }
          
        } else {
          # If not latest year selected
          
          if(length(input$db_benchmark) == 2 & !is.null(input$db_benchmark)) {
            # get infrasap data based on inputs to get the benchmark type and join
            df_r <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Country Name` == cn) %>%
              dplyr::filter(`Indicator Sector` %in% sc) %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr, `Region`)
            
            # get benchmark type for benchmark selected
            bm_type <- unique(df_r[, "Region"])
            
            # get benchmark data based on inputs
            df_r <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Grouping == bm_type) %>%
              dplyr::filter(`Sector` %in% sc) %>%
              dplyr::select(`Indicator`, yr) %>%
              dplyr::right_join(df_r, by = c('Indicator'='Indicator Name'))
            
            df_r <- df_r %>% dplyr::select(-`Region`)
            
            # get names of the two columns to compare
            bm_col <- names(df_r)[grepl('.x', names(df_r), fixed = TRUE)]
            data_col <- names(df_r)[grepl('.y', names(df_r), fixed = TRUE)]
            
            # rename columns in data
            names(df_r)[names(df_r) == bm_col] <- 'Region'
            names(df_r)[names(df_r)==data_col] <- cn
            
            
            # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
            df_r$`Type of Benchmark`[is.na(df_r$`Type of Benchmark`)] <- 'Upper'
            df_r$`Type of Benchmark`[df_r$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
            
            df_r <- df_r %>%
              dplyr::mutate(value_r = dplyr::case_when(
                # TRUE ~ "0",
                (df_r %>% dplyr::select(dplyr::contains(cn))) >= Region & (`Type of Benchmark` == "Upper") ~ "3",
                ((df_r %>% dplyr::select(dplyr::contains(cn))) >= (Region * 0.9) & (df_r %>% dplyr::select(dplyr::contains(cn))) < Region) & (`Type of Benchmark` == "Upper") ~ "2",
                (df_r %>% dplyr::select(dplyr::contains(cn))) < (Region * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                (df_r %>% dplyr::select(dplyr::contains(cn))) <= Region & (`Type of Benchmark` == "Lower") ~ "3",
                ((df_r %>% dplyr::select(dplyr::contains(cn))) <= (Region * 1.1) & (df_r %>% dplyr::select(dplyr::contains(cn))) > Region) & (`Type of Benchmark` == "Lower") ~ "2",
                (df_r %>% dplyr::select(dplyr::contains(cn))) > (Region * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                TRUE ~ "0"
              )
              ) %>% 
              # Fill NAs with grey color
              dplyr::mutate(value_r = dplyr::case_when(
                is.na(df_r %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                TRUE ~ value_r
              )) %>%
              dplyr::mutate(value_r = value_r %>% as.numeric())
            
            # make table with all combinations of pillar, sub-pillar, and topic
            df_large <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::filter(`Indicator Sector`== input$db_sector) %>%
              dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
              dplyr::summarise(counts = dplyr::n()) %>%
              dplyr::select(-counts)
            
            # join data
            df_r <- dplyr::left_join(df_large, df_r)
            
            # Rename columns
            df_r <- df_r %>%
              dplyr::rename(
                `Sub-Pillar` = `Indicator Sub-Pillar`,
                `Topic`= `Indicator Topic`
              )
            
            
            # subset data by the columns used in the table
            df_r <- df_r %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `Region`, value_r)
            
            # round numbers
            df_r[[cn]] <- round(df_r[[cn]], 2)
            df_r[['Region']] <- round(df_r[['Region']], 2)
            
            
            
            # get infrasap data based on inputs to get the benchmark type and join
            df_i <- infrasap_dat_mod_modified %>%
              dplyr::filter(`Country Name` == cn) %>%
              dplyr::filter(`Indicator Sector` %in% sc) %>%
              dplyr::filter(`Indicator Pillar` == pi) %>%
              dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`, yr, `IncomeGroup`)
            
            # get benchmark type for benchmark selected
            bm_type <- unique(df_i[, "IncomeGroup"])
            
            # get benchmark data based on inputs
            df_i <- infrsap_dat_bm_mod_modfied %>%
              dplyr::filter(Grouping == bm_type) %>%
              dplyr::filter(`Sector` %in% sc) %>%
              dplyr::select(`Indicator`, yr) %>%
              dplyr::right_join(df_i, by = c('Indicator'='Indicator Name'))
            
            df_i <- df_i %>% dplyr::select(-`IncomeGroup`)
            
            # get names of the two columns to compare
            bm_col <- names(df_i)[grepl('.x', names(df_i), fixed = TRUE)]
            data_col <- names(df_i)[grepl('.y', names(df_i), fixed = TRUE)]
            
            # rename columns in data
            names(df_i)[names(df_i) == bm_col] <- 'IncomeGroup'
            names(df_i)[names(df_i)==data_col] <- cn
            
            df_i$`Type of Benchmark`[is.na(df_i$`Type of Benchmark`)] <- 'Upper'
            df_i$`Type of Benchmark`[df_i$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
            
            df_i <- df_i %>%
              dplyr::mutate(value_i = dplyr::case_when(
                (df_i %>% dplyr::select(dplyr::contains(cn))) >= IncomeGroup & (`Type of Benchmark` == "Upper") ~ "3",
                ((df_i %>% dplyr::select(dplyr::contains(cn))) >= (IncomeGroup * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < IncomeGroup) & (`Type of Benchmark` == "Upper") ~ "2",
                (df_i %>% dplyr::select(dplyr::contains(cn))) < (IncomeGroup * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                (df_i %>% dplyr::select(dplyr::contains(cn))) <= IncomeGroup & (`Type of Benchmark` == "Lower") ~ "3",
                ((df_i %>% dplyr::select(dplyr::contains(cn))) <= (IncomeGroup * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > IncomeGroup) & (`Type of Benchmark` == "Lower") ~ "2",
                (df_i %>% dplyr::select(dplyr::contains(cn))) > (IncomeGroup * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                TRUE ~ "0"
              )
              ) %>% 
              # Fill NAs with grey color
              dplyr::mutate(value_i = dplyr::case_when(
                is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                TRUE ~ value_i
              )) %>%
              dplyr::mutate(value_i = value_i %>% as.numeric())
            
            
            if(!is.null(input$country_to_compare_id)){
              
              # get infrasap data based on inputs to get the benchmark type and join
              df_cn <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::select(`Country Name`, `Indicator Name`, yr) %>%
                tidyr::pivot_wider(
                  names_from = `Country Name`,
                  values_from = yr
                )
              
   
              df_i <- df_i %>%
                dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
              
              
            }
            
            
            
            # Country to compare equals 1
            if(length(input$country_to_compare_id) == 1){
              cn1 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
            }
            
            if(length(input$country_to_compare_id) == 2){
              cn2 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn2[1])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn2[1])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c2 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn2[2])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn2[2])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c2 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c2
                )) %>%
                dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
              
            }
            
            
            if(length(input$country_to_compare_id) == 3){
              cn3 <- input$country_to_compare_id
              df_i <- df_i %>%
                dplyr::mutate(value_c1 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[1])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[1])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c1 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c1
                )) %>%
                dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c2 = dplyr::case_when(
                  # TRUE ~ "0",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[2])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[2])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c2 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c2
                )) %>%
                dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
              
              df_i <- df_i %>%
                dplyr::mutate(value_c3 = dplyr::case_when(
                  (df_i %>% dplyr::select(dplyr::contains(cn))) >= (df_i %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) >= ((df_i %>% dplyr::select(cn3[3])) * 0.9) & (df_i %>% dplyr::select(dplyr::contains(cn))) < (df_i %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) < ((df_i %>% dplyr::select(cn3[3])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) <= (df_i %>% dplyr::select(cn3[3])) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df_i %>% dplyr::select(dplyr::contains(cn))) <= ((df_i %>% dplyr::select(cn3[3])) * 1.1) & (df_i %>% dplyr::select(dplyr::contains(cn))) > (df_i %>% dplyr::select(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df_i %>% dplyr::select(dplyr::contains(cn))) > ((df_i %>% dplyr::select(cn3[3])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>%
                # Fill NAs with grey color
                dplyr::mutate(value_c3 = dplyr::case_when(
                  is.na(df_i %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value_c3
                )) %>%
                dplyr::mutate(value_c3 = value_c3 %>% as.numeric())

            }
            
            
            
            # join data
            df_i <- dplyr::left_join(df_large, df_i)
            
            # Rename columns
            df_i <- df_i %>%
              dplyr::rename(
                `Sub-Pillar` = `Indicator Sub-Pillar`,
                `Topic`= `Indicator Topic`
              )
            
            
            if(length(input$country_to_compare_id) == 1) {
              # subset data by the columns used in the table
              df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1)
              df_i[[input$country_to_compare_id]] <- round(df_i[[input$country_to_compare_id]], 2)
            } else {
              if(length(input$country_to_compare_id) == 2){
                df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2)
                df_i[[input$country_to_compare_id[1]]] <- round(df_i[[input$country_to_compare_id[1]]], 2)
                df_i[[input$country_to_compare_id[2]]] <- round(df_i[[input$country_to_compare_id[2]]], 2)
              } else {
                if(length(input$country_to_compare_id) == 3){
                  df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3)
                  df_i[[input$country_to_compare_id[1]]] <- round(df_i[[input$country_to_compare_id[1]]], 2)
                  df_i[[input$country_to_compare_id[2]]] <- round(df_i[[input$country_to_compare_id[2]]], 2)
                  df_i[[input$country_to_compare_id[3]]] <- round(df_i[[input$country_to_compare_id[3]]], 2)
                } else {
                  # subset data by the columns used in the table
                  df_i <- df_i %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, `IncomeGroup`, value_i)
                }
              }
            }
            
            
            
            # round numbers
            df_i[[cn]] <- round(df_i[[cn]], 2)
            df_i[['IncomeGroup']] <- round(df_i[['IncomeGroup']], 2)
            
            
            df <- dplyr::full_join(df_r, df_i)
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
            }
            
            if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
            }
            
            if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
            }
            
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
            }
            
            if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
            }
            
            
            if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
            }
            
            if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
              df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
            }
            
            return(df)
            
          } else {
            
            if(length(input$db_benchmark) == 0 & is.null(input$db_benchmark)) {
              
              # cn <- "Kenya"
              # sc <- "Energy"
              # # pi <- "Finance"
              # pi <- "Connectivity"
              # yr <- "2019"
              
              
              # get infrasap data based on inputs to get the benchmark type and join
              df <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`,yr)
              
              
              # get benchmark type for benchmark selected
              # bm_type <- unique(df[, bm])
              
              # get benchmark data based on inputs
              # df <- infrsap_dat_bm_mod_modfied %>%
              #   # dplyr::filter(Grouping == bm_type) %>%
              #   dplyr::filter(`Sector` %in% sc) %>%
              #   dplyr::select(`Indicator`,yr) %>%
              #   dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
              
              # df <- df %>% dplyr::select(-bm)
              
              # get names of the two columns to compare
              # bm_col <- names(df)[grepl('.x', names(df), fixed = TRUE)]
              # data_col <- names(df)[grepl('.y', names(df), fixed = TRUE)]
              
              # rename columns in data
              # names(df)[names(df)==bm_col] <- bm
              names(df)[names(df)==yr] <- cn
              
              # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
              df$`Type of Benchmark`[is.na(df$`Type of Benchmark`)] <- 'Upper'
              df$`Type of Benchmark`[df$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
              
              # Countries list to compare
              if(!is.null(input$country_to_compare_id)){
                
                # get infrasap data based on inputs to get the benchmark type and join
                df_cn <- infrasap_dat_mod_modified %>%
                  dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                  # dplyr::filter(`Country Name` %in% "Angola") %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::filter(`Indicator Pillar` == pi) %>%
                  dplyr::select(`Country Name`, `Indicator Name`, yr) %>%
                  tidyr::pivot_wider(
                    names_from = `Country Name`, 
                    values_from = yr
                  ) 
                
                
                df <- df %>%
                  dplyr::rename(
                    `Indicator` = `Indicator Name`
                  ) %>%
                  dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
                
                
              }
              
              # df <- df %>%
              #   dplyr::mutate(value = dplyr::case_when(
              #     (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
              #     ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
              #     (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
              #     (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
              #     ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
              #     (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
              #     TRUE ~ "0"
              #   )
              #   ) %>% 
              #   # Fill NAs with grey color
              #   dplyr::mutate(value = dplyr::case_when(
              #     is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
              #     TRUE ~ value
              #   )) %>%
              #   dplyr::mutate(value = value %>% as.numeric())
              
              
              # Country to compare equals 1 
              if(length(input$country_to_compare_id) == 1){
                # cn1 <- "Angola"
                cn1 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
              }
              
              if(length(input$country_to_compare_id) == 2){
                cn2 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                
              }
              
              
              if(length(input$country_to_compare_id) == 3){
                cn3 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn3[2])))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn3[2])))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn3[3])))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn3[3])))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c3
                  )) %>%
                  dplyr::mutate(value_c3 = value_c3 %>% as.numeric())
                
              }
              
              
              
              # make table with all combinations of pillar, sub-pillar, and topic
              df_large <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::filter(`Indicator Sector`== sc) %>%
                dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
                dplyr::summarise(counts = dplyr::n()) %>%
                dplyr::select(-counts)
              
              # # join data
              df <- dplyr::left_join(df_large, df)
              
              
              #Rename columns
              df <- df %>%
                dplyr::rename(
                  `Sub-Pillar` = `Indicator Sub-Pillar`,
                  `Topic`= `Indicator Topic`
                )
              
              if(length(input$country_to_compare_id) == 1) {
                # subset data by the columns used in the table
                df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id, value_c1)
                df[[input$country_to_compare_id]] <- round(df[[input$country_to_compare_id]], 2)
              } else {
                if(length(input$country_to_compare_id) == 2){
                  df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2)
                  df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                  df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                } else {
                  if(length(input$country_to_compare_id) == 3){
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3)
                    df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                    df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                    df[[input$country_to_compare_id[3]]] <- round(df[[input$country_to_compare_id[3]]], 2)
                  } else {
                    # subset data by the columns used in the table
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn)
                  }
                }
              }
              
              
              # round numbers
              df[[cn]] <- round(df[[cn]], 2)
              # df[[bm]] <- round(df[[bm]], 2)
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
              }
              # print(df)
              
              return(df)
              
            } else {
              
              # get infrasap data based on inputs to get the benchmark type and join
              df <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Country Name` == cn) %>%
                dplyr::filter(`Indicator Sector` %in% sc) %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::select(`Country Name`,`Indicator Sector`,`Indicator Sub-Pillar` ,`Indicator Name`, `Indicator Topic`, `Type of Benchmark`,yr, bm )
              
              
              # get benchmark type for benchmark selected
              bm_type <- unique(df[, bm])
              
              # get benchmark data based on inputs
              df <- infrsap_dat_bm_mod_modfied %>%
                dplyr::filter(Grouping == bm_type) %>%
                dplyr::filter(`Sector` %in% sc) %>%
                dplyr::select(`Indicator`,yr) %>%
                dplyr::right_join(df, by = c('Indicator'='Indicator Name'))
              
              df <- df %>% dplyr::select(-bm)
              
              # get names of the two columns to compare
              bm_col <- names(df)[grepl('.x', names(df), fixed = TRUE)]
              data_col <- names(df)[grepl('.y', names(df), fixed = TRUE)]
              
              # rename columns in data
              names(df)[names(df)==bm_col] <- bm
              names(df)[names(df)==data_col] <- cn
              
              # for now (will get more data later), fill NA or ambigious in type of benchmark with Upper
              df$`Type of Benchmark`[is.na(df$`Type of Benchmark`)] <- 'Upper'
              df$`Type of Benchmark`[df$`Type of Benchmark` =='Ambiguous'] <- 'Upper'
              
              # Countries list to compare
              if(!is.null(input$country_to_compare_id)){
                
                # get infrasap data based on inputs to get the benchmark type and join
                df_cn <- infrasap_dat_mod_modified %>%
                  dplyr::filter(`Country Name` %in% input$country_to_compare_id) %>%
                  dplyr::filter(`Indicator Sector` %in% sc) %>%
                  dplyr::filter(`Indicator Pillar` == pi) %>%
                  dplyr::select(`Country Name`, `Indicator Name`, yr) %>%
                  tidyr::pivot_wider(
                    names_from = `Country Name`, 
                    values_from = yr
                  ) 
                
                
                df <- df %>%
                  dplyr::left_join(df_cn, by = c('Indicator'='Indicator Name'))
                
                
              }
              
              df <- df %>%
                dplyr::mutate(value = dplyr::case_when(
                  (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Upper") ~ "3",
                  ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Upper") ~ "2",
                  (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(bm))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                  (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(bm))) & (`Type of Benchmark` == "Lower") ~ "3",
                  ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(bm)))) & (`Type of Benchmark` == "Lower") ~ "2",
                  (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(bm))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                  TRUE ~ "0"
                )
                ) %>% 
                # Fill NAs with grey color
                dplyr::mutate(value = dplyr::case_when(
                  is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                  TRUE ~ value
                )) %>%
                dplyr::mutate(value = value %>% as.numeric())
              
              
              # Country to compare equals 1 
              if(length(input$country_to_compare_id) == 1){
                cn1 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn1))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn1))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn1)))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn1))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
              }
              
              if(length(input$country_to_compare_id) == 2){
                cn2 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn2[2])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn2[2])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn2[2])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn2[2])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn2[2]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn2[2])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                
              }
              
              
              if(length(input$country_to_compare_id) == 3){
                cn3 <- input$country_to_compare_id
                df <- df %>%
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(cn3[1])) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(cn3[1])) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(cn3[1])) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(cn3[1])) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(cn3[1]))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(cn3[1])) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c1 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c1
                  )) %>%
                  dplyr::mutate(value_c1 = value_c1 %>% as.numeric())
                
                
                
                df <- df %>%
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn3[2]))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn3[2])))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn3[2]))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn3[2])))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn3[2]))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c2 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c2
                  )) %>%
                  dplyr::mutate(value_c2 = value_c2 %>% as.numeric())
                
                df <- df %>%
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    (df %>% dplyr::select(dplyr::contains(cn))) >= (df %>% dplyr::select(dplyr::contains(cn3[3]))) & (`Type of Benchmark` == "Upper") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) >= ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 0.9) & (df %>% dplyr::select(dplyr::contains(cn))) < (df %>% dplyr::select(dplyr::contains(cn3[3])))) & (`Type of Benchmark` == "Upper") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) < ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 0.9) & (`Type of Benchmark` == "Upper") ~ "1",
                    (df %>% dplyr::select(dplyr::contains(cn))) <= (df %>% dplyr::select(dplyr::contains(cn3[3]))) & (`Type of Benchmark` == "Lower") ~ "3",
                    ((df %>% dplyr::select(dplyr::contains(cn))) <= ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 1.1) & (df %>% dplyr::select(dplyr::contains(cn))) > (df %>% dplyr::select(dplyr::contains(cn3[3])))) & (`Type of Benchmark` == "Lower") ~ "2",
                    (df %>% dplyr::select(dplyr::contains(cn))) > ((df %>% dplyr::select(dplyr::contains(cn3[3]))) * 1.1) & (`Type of Benchmark` == "Lower") ~ "1",
                    TRUE ~ "0"
                  )
                  ) %>% 
                  # Fill NAs with grey color
                  dplyr::mutate(value_c3 = dplyr::case_when(
                    is.na(df %>% dplyr::select(dplyr::contains(cn))) ~ "0",
                    TRUE ~ value_c3
                  )) %>%
                  dplyr::mutate(value_c3 = value_c3 %>% as.numeric())
                
              }
              
              
              
              # make table with all combinations of pillar, sub-pillar, and topic
              df_large <- infrasap_dat_mod_modified %>%
                dplyr::filter(`Indicator Pillar` == pi) %>%
                dplyr::filter(`Indicator Sector`== input$db_sector) %>%
                dplyr::group_by(`Indicator Sub-Pillar`, `Indicator Topic`) %>%
                dplyr::summarise(counts = dplyr::n()) %>%
                dplyr::select(-counts)
              
              # # join data
              df <- dplyr::left_join(df_large, df)
              
              
              #Rename columns
              df <- df %>%
                dplyr::rename(
                  `Sub-Pillar` = `Indicator Sub-Pillar`,
                  `Topic`= `Indicator Topic`
                )
              
              if(length(input$country_to_compare_id) == 1) {
                # subset data by the columns used in the table
                df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id, value_c1)
                df[[input$country_to_compare_id]] <- round(df[[input$country_to_compare_id]], 2)
              } else {
                if(length(input$country_to_compare_id) == 2){
                  df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2)
                  df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                  df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                } else {
                  if(length(input$country_to_compare_id) == 3){
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value, input$country_to_compare_id[1], value_c1, input$country_to_compare_id[2], value_c2, input$country_to_compare_id[3], value_c3)
                    df[[input$country_to_compare_id[1]]] <- round(df[[input$country_to_compare_id[1]]], 2)
                    df[[input$country_to_compare_id[2]]] <- round(df[[input$country_to_compare_id[2]]], 2)
                    df[[input$country_to_compare_id[3]]] <- round(df[[input$country_to_compare_id[3]]], 2)
                  } else {
                    # subset data by the columns used in the table
                    df <- df %>% dplyr::select(`Sub-Pillar`, `Topic`, `Indicator`, cn, bm, value)
                  }
                }
              }
              
              
              # round numbers
              df[[cn]] <- round(df[[cn]], 2)
              df[[bm]] <- round(df[[bm]], 2)
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__governance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__governance)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Governance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__governance)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__connectivity)
              }
              
              if(input$db_sector %in% c('Digital Development') && input$db_pillar %in% c('Connectivity')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$digital__connectivity)
              }
              
              
              if(input$db_sector %in% c('Energy') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$energy__finance)
              }
              
              if(input$db_sector %in% c('Transport') && input$db_pillar %in% c('Finance')) {
                df <- join_df_with_ordered_layout(df, infrasap::dat_layout$transport__finance)
              }
              return(df)
              
            }
            
          }
          
        } # End else `if not latest year selected`
      } # end else `if is not null`
      
      
    })
    
    observe({
      df_length_check <-  infrasap_dat_mod_modified %>%
        dplyr::filter(`Country Name` == input$db_country) %>%
        dplyr::filter(`Indicator Sector` %in% input$db_sector) %>%
        dplyr::filter(`Indicator Pillar` == input$db_pillar) 
      
      if(nrow(df_length_check) < 1) {
        output$emptyDataTableMSG <- shiny::renderUI({
            htmltools::tagList(shiny::h3(class = "header-style-no-data", "No data available"))
          
        })
      } else {
        output$emptyDataTableMSG <- shiny::renderUI({
          NULL
        })
      }
    })
    
    
    
    # Render the table with the traffic light
    output$db_table <- DT::renderDataTable({
      if(is.null(infrasap_table())){
        NULL
      } else {
        
        
        if(length(input$db_benchmark) == 2) {
          
          if(length(input$country_to_compare_id) == 3){
            
            if(input$db_year == "Latest year available"){
              dtable <- DT::datatable(infrasap_table(),
                                  extensions = 'Buttons',
                                  rownames = FALSE,
                                  options = list(
                                    rowCallback = DT::JS(
                                      "function(row, data) {",
                                      "var full_text = 'This row values extracted from ' + data[14] +  ' year'",
                                      "$('td', row).attr('title', full_text);",
                                      "console.log(data)",
                                      "}"),
                                    rowsGroup = list(0, 1), # merge cells of column 1, 2
                                    dom='Bfrti',
                                    columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11, 13, 14))),
                                    pageLength = -1,
                                    ordering=F,
                                    buttons = list(
                                      list(extend = "csv",
                                           # To export only visible columns without color code
                                           exportOptions = list(columns = ":visible")
                                      )
                                    )
                                  ),
                                  selection = 'none'
              )
            } else {
              dtable <- DT::datatable(infrasap_table(),
                                  rownames = FALSE,
                                  extensions = 'Buttons',
                                  options = list(
                                    rowsGroup = list(0, 1), # merge cells of column 1, 2
                                    dom='Bfrti',
                                    columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11, 13))),
                                    pageLength = -1,
                                    ordering=F,
                                    buttons = list(
                                      list(extend = "csv",
                                           # To export only visible columns without color code
                                           exportOptions = list(columns = ":visible")
                                      )
                                    )
                                    
                                  ),
                                  selection = 'none'
              )
            }
            dtable <- dtable %>% DT::formatStyle(
              'Region','value_r',
              backgroundColor = DT::styleEqual(
                c(0, 1, 2, 3),
                c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
              )
            ) %>% DT::formatStyle(
              'IncomeGroup','value_i',
              backgroundColor = DT::styleEqual(
                c(0, 1, 2, 3),
                c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
              )
            ) %>% DT::formatStyle(
              names(infrasap_table())[9],'value_c1',
              backgroundColor = DT::styleEqual(
                c(0, 1, 2, 3),
                c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
              )
            ) %>% DT::formatStyle(
              names(infrasap_table())[11],'value_c2',
              backgroundColor = DT::styleEqual(
                c(0, 1, 2, 3),
                c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
              )
            ) %>% DT::formatStyle(
              names(infrasap_table())[13],'value_c3',
              backgroundColor = DT::styleEqual(
                c(0, 1, 2, 3),
                c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
              )
            ) 
            
          } else {
            if(length(input$country_to_compare_id) == 2){
              if(input$db_year == "Latest year available"){
                dtable <- DT::datatable(infrasap_table(),
                                    rownames = FALSE,
                                    extensions = 'Buttons',
                                    options = list(
                                      rowCallback = DT::JS(
                                        "function(row, data) {",
                                        "var full_text = 'This row values extracted from ' + data[12] +  ' year'",
                                        "$('td', row).attr('title', full_text);",
                                        "console.log(data)",
                                        "}"),
                                      rowsGroup = list(0, 1), # merge cells of column 1, 2
                                      dom='Bfrti',
                                      columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11, 12))),
                                      pageLength = -1,
                                      ordering=F,
                                      buttons = list(
                                        list(extend = "csv",
                                             # To export only visible columns without color code
                                             exportOptions = list(columns = ":visible")
                                        )
                                      )

                                    ),
                                    selection = 'none'
                ) 
              } else {
                dtable <- DT::datatable(infrasap_table(),
                                    rownames = FALSE,
                                    extensions = 'Buttons',
                                    options = list(
                                      rowsGroup = list(0, 1), # merge cells of column 1, 2
                                      dom='Bfrti',
                                      columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11))),
                                      pageLength = -1,
                                      ordering=F,
                                      buttons = list(
                                        list(extend = "csv",
                                             # To export only visible columns without color code
                                             exportOptions = list(columns = ":visible")
                                        )
                                      )
                                      
                                    ),
                                    selection = 'none'
                ) 
              }
              dtable <- dtable %>% DT::formatStyle(
                'Region','value_r',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                'IncomeGroup','value_i',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[9],'value_c1',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[11],'value_c2',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) 
              
            } else {
              if(length(input$country_to_compare_id) == 1){
                if(input$db_year == "Latest year available"){
                  dtable <- DT::datatable(infrasap_table(),
                                      rownames = FALSE,
                                      extensions = 'Buttons',
                                      options = list(
                                        rowCallback = DT::JS(
                                          "function(row, data) {",
                                          "var full_text = 'This row values extracted from ' + data[10] +  ' year'",
                                          "$('td', row).attr('title', full_text);",
                                          "console.log(data)",
                                          "}"),
                                        rowsGroup = list(0, 1), # merge cells of column 1, 2
                                        dom='Bfrti',
                                        columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 10))),
                                        pageLength = -1,
                                        ordering=F,
                                        buttons = list(
                                          list(extend = "csv",
                                               # To export only visible columns without color code
                                               exportOptions = list(columns = ":visible")
                                          )
                                        )
                                        
                                      ),
                                      selection = 'none'
                  ) 
                } else {
                  dtable <- DT::datatable(infrasap_table(),
                                      rownames = FALSE,
                                      extensions = 'Buttons',
                                      options = list(
                                        rowsGroup = list(0, 1), # merge cells of column 1, 2
                                        dom='Bfrti',
                                        columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9))),
                                        pageLength = -1,
                                        ordering=F,
                                        buttons = list(
                                          list(extend = "csv",
                                               # To export only visible columns without color code
                                               exportOptions = list(columns = ":visible")
                                          )
                                        )
                                        
                                      ),
                                      selection = 'none'
                  )
                }
                
                dtable <- dtable %>% DT::formatStyle(
                  'Region','value_r',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  'IncomeGroup','value_i',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  names(infrasap_table())[9],'value_c1',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) 
                
              } else {
                if(input$db_year == "Latest year available"){
                  dtable <- DT::datatable(infrasap_table(),
                                      rownames = FALSE,
                                      extensions = 'Buttons',
                                      options = list(
                                        rowCallback = DT::JS(
                                          "function(row, data) {",
                                          "var full_text = 'This row values extracted from ' + data[8] +  ' year'",
                                          "$('td', row).attr('title', full_text);",
                                          "console.log(data)",
                                          "}"),
                                        rowsGroup = list(0, 1), # merge cells of column 1, 2
                                        dom='Bfrti',
                                        columnDefs = list(list(visible=FALSE, targets=c(5, 7, 8))),
                                        pageLength = -1,
                                        ordering=F,
                                        buttons = list(
                                          list(extend = "csv",
                                               # To export only visible columns without color code
                                               exportOptions = list(columns = ":visible")
                                          )
                                        )
                                      ),
                                      selection = 'none'
                  ) %>% DT::formatStyle(
                    'IncomeGroup','year_tooltip',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  )
                } else {
                  dtable <- DT::datatable(infrasap_table(),
                                      rownames = FALSE,
                                      extensions = 'Buttons',
                                      options = list(
                                        rowsGroup = list(0, 1), # merge cells of column 1, 2
                                        dom='Bfrti',
                                        columnDefs = list(list(visible=FALSE, targets=c(5, 7))),
                                        pageLength = -1,
                                        ordering=F,
                                        buttons = list(
                                          list(extend = "csv",
                                               # To export only visible columns without color code
                                               exportOptions = list(columns = ":visible")
                                          )
                                        )
                                      ),
                                      selection = 'none'
                  ) 
                }
                
                dtable <- dtable %>% DT::formatStyle(
                  'Region','value_r',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  'IncomeGroup','value_i',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                )
                
              }
            }
          }
          
          
          dep <- htmltools::htmlDependency(
            "RowsGroup", "2.0.0",
            src = c(href = 'www'), script = "script.js", package = 'infrasap')
          
          dtable$dependencies <- c(dtable$dependencies, list(dep))
          dtable
          
        } else {
          
          if(length(input$db_benchmark) == 0) {
            
            if(length(input$country_to_compare_id) == 3){
              if(input$db_year == "Latest year available"){
                dtable <- DT::datatable(infrasap_table(),
                                        rownames = FALSE,
                                        extensions = 'Buttons',
                                        options = list(
                                          rowCallback = DT::JS(
                                            "function(row, data) {",
                                            "var full_text = 'This row values extracted from ' + data[10] +  ' year'",
                                            "$('td', row).attr('title', full_text);",
                                            "console.log(data)",
                                            "}"),
                                          rowsGroup = list(0, 1), # merge cells of column 1, 2
                                          dom='Bfrti',
                                          columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 10))),
                                          pageLength = -1,
                                          ordering=F,
                                          buttons = list(
                                            list(extend = "csv",
                                                 # To export only visible columns without color code
                                                 exportOptions = list(columns = ":visible")
                                            )
                                          )
                                        ),
                                        selection = 'none'
                ) %>% DT::formatStyle(
                  names(infrasap_table())[11], 'year_tooltip',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                )
              } else {
                dtable <- DT::datatable(infrasap_table(),
                                        rownames = FALSE,
                                        extensions = 'Buttons',
                                        options = list(
                                          rowsGroup = list(0, 1), # merge cells of column 1, 2
                                          dom='Bfrti',
                                          columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9))),
                                          pageLength = -1,
                                          ordering=F,
                                          buttons = list(
                                            list(extend = "csv",
                                                 # To export only visible columns without color code
                                                 exportOptions = list(columns = ":visible")
                                            )
                                          )
                                        ),
                                        selection = 'none'
                )
              }
              
              dtable <- dtable %>% DT::formatStyle(
                names(infrasap_table())[5],'value_c1',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[7],'value_c2',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[9],'value_c3',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              )
            } else {
              if(length(input$country_to_compare_id) == 2){
                
                if(input$db_year == "Latest year available"){
                  dtable <- DT::datatable(infrasap_table(),
                                          rownames = FALSE,
                                          extensions = 'Buttons',
                                          options = list(
                                            rowCallback = DT::JS(
                                              "function(row, data) {",
                                              "var full_text = 'This row values extracted from ' + data[8] +  ' year'",
                                              "$('td', row).attr('title', full_text);",
                                              "console.log(data)",
                                              "}"),
                                            rowsGroup = list(0, 1), # merge cells of column 1, 2
                                            dom='Bfrti',
                                            columnDefs = list(list(visible=FALSE, targets=c(5, 7, 8))),
                                            pageLength = -1,
                                            ordering=F,
                                            buttons = list(
                                              list(extend = "csv",
                                                   # To export only visible columns without color code
                                                   exportOptions = list(columns = ":visible")
                                              )
                                            )
                                          ),
                                          selection = 'none'
                  ) %>% DT::formatStyle(
                    names(infrasap_table())[8],'year_tooltip',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  ) 
                } else {
                  
                  dtable <- DT::datatable(infrasap_table(),
                                          rownames = FALSE,
                                          extensions = 'Buttons',
                                          options = list(
                                            rowsGroup = list(0, 1), # merge cells of column 1, 2
                                            dom='Bfrti',
                                            columnDefs = list(list(visible=FALSE, targets=c(5, 7))),
                                            pageLength = -1,
                                            ordering=F,
                                            buttons = list(
                                              list(extend = "csv",
                                                   # To export only visible columns without color code
                                                   exportOptions = list(columns = ":visible")
                                              )
                                            )
                                          ),
                                          selection = 'none'
                  )
                  
                }
                
                dtable <-  dtable %>% DT::formatStyle(
                  names(infrasap_table())[5],'value_c1',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  names(infrasap_table())[7],'value_c2',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) 
              } else {
                if(length(input$country_to_compare_id) == 1){
                  if(input$db_year == "Latest year available"){
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This row values extracted from ' + data[6] +  ' year'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom = 'Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5, 6))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none'
                    )
                  } else {
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none'
                    )
                  }
                  
                  dtable <- dtable %>% DT::formatStyle(
                    names(infrasap_table())[5],'value_c1',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  )
                  
                  
                  
                } else {
                  
                  if(input$db_year == "Latest year available"){
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This row values extracted from ' + data[3] +  ' year'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(3, 6))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none',
                                            escape = FALSE
                    ) %>% DT::formatStyle(
                      names(infrasap_table())[6],'value',
                      backgroundColor = DT::styleEqual(
                        c(0, 1, 2, 3),
                        c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                      )
                    ) 
                    
                    
                    
                  } else {
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This rows values are :' + data[0] + ',' + data[1] + '...'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none',
                                            escape = FALSE
                    ) %>% DT::formatStyle(
                      names(infrasap_table())[5],'value',
                      backgroundColor = DT::styleEqual(
                        c(0, 1, 2, 3),
                        c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                      )
                    ) 
                    
                  }
                  
                }
              }
            }
            
            
            dep <- htmltools::htmlDependency(
              "RowsGroup", "2.0.0",
              src = c(href = 'www'), script = "script.js", package = 'infrasap')
            
            dtable$dependencies <- c(dtable$dependencies, list(dep))
            dtable
            
          } else {
            if(length(input$country_to_compare_id) == 3){
              if(input$db_year == "Latest year available"){
                dtable <- DT::datatable(infrasap_table(),
                                        rownames = FALSE,
                                        extensions = 'Buttons',
                                        options = list(
                                          rowCallback = DT::JS(
                                            "function(row, data) {",
                                            "var full_text = 'This row values extracted from ' + data[12] +  ' year'",
                                            "$('td', row).attr('title', full_text);",
                                            "console.log(data)",
                                            "}"),
                                          rowsGroup = list(0, 1), # merge cells of column 1, 2
                                          dom='Bfrti',
                                          columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11, 12))),
                                          pageLength = -1,
                                          ordering=F,
                                          buttons = list(
                                            list(extend = "csv",
                                                 # To export only visible columns without color code
                                                 exportOptions = list(columns = ":visible")
                                            )
                                          )
                                        ),
                                        selection = 'none'
                ) %>% DT::formatStyle(
                  names(infrasap_table())[12], 'year_tooltip',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                )
              } else {
                dtable <- DT::datatable(infrasap_table(),
                                        rownames = FALSE,
                                        extensions = 'Buttons',
                                        options = list(
                                          rowsGroup = list(0, 1), # merge cells of column 1, 2
                                          dom='Bfrti',
                                          columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 11))),
                                          pageLength = -1,
                                          ordering=F,
                                          buttons = list(
                                            list(extend = "csv",
                                                 # To export only visible columns without color code
                                                 exportOptions = list(columns = ":visible")
                                            )
                                          )
                                        ),
                                        selection = 'none'
                )
              }
              
              dtable <- dtable %>% DT::formatStyle(
                names(infrasap_table())[5],'value',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[7],'value_c1',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[9],'value_c2',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              ) %>% DT::formatStyle(
                names(infrasap_table())[11],'value_c3',
                backgroundColor = DT::styleEqual(
                  c(0, 1, 2, 3),
                  c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                )
              )
            } else {
              if(length(input$country_to_compare_id) == 2){
                
                if(input$db_year == "Latest year available"){
                  dtable <- DT::datatable(infrasap_table(),
                                          rownames = FALSE,
                                          extensions = 'Buttons',
                                          options = list(
                                            rowCallback = DT::JS(
                                              "function(row, data) {",
                                              "var full_text = 'This row values extracted from ' + data[10] +  ' year'",
                                              "$('td', row).attr('title', full_text);",
                                              "console.log(data)",
                                              "}"),
                                            rowsGroup = list(0, 1), # merge cells of column 1, 2
                                            dom='Bfrti',
                                            columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9, 10))),
                                            pageLength = -1,
                                            ordering=F,
                                            buttons = list(
                                              list(extend = "csv",
                                                   # To export only visible columns without color code
                                                   exportOptions = list(columns = ":visible")
                                              )
                                            )
                                          ),
                                          selection = 'none'
                  ) %>% DT::formatStyle(
                    names(infrasap_table())[10],'year_tooltip',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  ) 
                } else {
                  
                  dtable <- DT::datatable(infrasap_table(),
                                          rownames = FALSE,
                                          extensions = 'Buttons',
                                          options = list(
                                            rowsGroup = list(0, 1), # merge cells of column 1, 2
                                            dom='Bfrti',
                                            columnDefs = list(list(visible=FALSE, targets=c(5, 7, 9))),
                                            pageLength = -1,
                                            ordering=F,
                                            buttons = list(
                                              list(extend = "csv",
                                                   # To export only visible columns without color code
                                                   exportOptions = list(columns = ":visible")
                                              )
                                            )
                                          ),
                                          selection = 'none'
                  )
                  
                }
                
                dtable <-  dtable %>% DT::formatStyle(
                  names(infrasap_table())[5],'value',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  names(infrasap_table())[7],'value_c1',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) %>% DT::formatStyle(
                  names(infrasap_table())[9],'value_c2',
                  backgroundColor = DT::styleEqual(
                    c(0, 1, 2, 3),
                    c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                  )
                ) 
              } else {
                if(length(input$country_to_compare_id) == 1){
                  if(input$db_year == "Latest year available"){
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This row values extracted from ' + data[8] +  ' year'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom = 'Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5, 7, 8))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none'
                    )
                  } else {
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5, 7))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none'
                    )
                  }
                  
                  dtable <- dtable %>% DT::formatStyle(
                    names(infrasap_table())[5],'value',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  ) %>% DT::formatStyle(
                    names(infrasap_table())[7],'value_c1',
                    backgroundColor = DT::styleEqual(
                      c(0, 1, 2, 3),
                      c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                    )
                  )
                  
                  
                  
                } else {
                  
                  if(input$db_year == "Latest year available"){
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This row values extracted from ' + data[3] +  ' year'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(3, 6))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none',
                                            escape = FALSE
                    ) %>% DT::formatStyle(
                      names(infrasap_table())[6],'value',
                      backgroundColor = DT::styleEqual(
                        c(0, 1, 2, 3),
                        c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                      )
                    ) 
                    
                    
                    
                  } else {
                    dtable <- DT::datatable(infrasap_table(),
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(
                                              rowCallback = DT::JS(
                                                "function(row, data) {",
                                                "var full_text = 'This rows values are :' + data[0] + ',' + data[1] + '...'",
                                                "$('td', row).attr('title', full_text);",
                                                "console.log(data)",
                                                "}"),
                                              rowsGroup = list(0, 1), # merge cells of column 1, 2
                                              dom='Bfrti',
                                              columnDefs = list(list(visible=FALSE, targets=c(5))),
                                              pageLength = -1,
                                              ordering=F,
                                              buttons = list(
                                                list(extend = "csv",
                                                     # To export only visible columns without color code
                                                     exportOptions = list(columns = ":visible")
                                                )
                                              )
                                            ),
                                            selection = 'none',
                                            escape = FALSE
                    ) %>% DT::formatStyle(
                      names(infrasap_table())[5],'value',
                      backgroundColor = DT::styleEqual(
                        c(0, 1, 2, 3),
                        c('#d3d3d370', '#fb9494', '#ffff6b', '#9be27d')
                      )
                    ) 
                    
                  }
                  
                }
              }
            }
            
            
            dep <- htmltools::htmlDependency(
              "RowsGroup", "2.0.0",
              src = c(href = 'www'), script = "script.js", package = 'infrasap')
            
            dtable$dependencies <- c(dtable$dependencies, list(dep))
            dtable
          }
          
        }
        
      }
      
    })
    
    
    output$report_pdf <- shiny::downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "infrasap_pillar_table_pdf.Rmd")
        file.copy("infrasap_pillar_table_pdf.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(country = input$db_country,
                       benchmark = input$db_benchmark,
                       table_data = infrasap_table(),
                       country_to_compare = input$country_to_compare_id
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    # /Module Body /end
  })
}

## To be copied in the UI
# mod_infrasap_tab_module_ui("infrasap_tab_module_ui_1")
    
## To be copied in the server
# mod_infrasap_tab_module_server("infrasap_tab_module_ui_1")
