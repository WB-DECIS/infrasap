##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import dplyr
#' @import RColorBrewer
#' @import leaflet
#' @import tidyverse
#' @import ggplot2
#' @import tidyr
#' @import gsheet
#' @import plotly
#' @import DT
#' @import shinyMobile
#' @import purrr
#' @import rlang
#' @import stringr
#' @import mapview
#' @import sjmisc
#' @import stringi
#' @import shinycssloaders
#' 


options(DT.options = list(pageLength = 20))

app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      
      dashboardHeader (title = tags$a(tags$img(src='www/logo2.jpg', alt = '', height = '50'))),
      dashboardSidebar(
        width = 230,
        sidebarMenu(
          id = 'tabs',
          menuItem(
            text="Infrasap Dashboard",
            tabName="landing_page"),
          menuItem(
            text = 'About',
            tabName = 'about')
        )),
      dashboardBody(
        tags$head(tags$style(".skin-blue .main-header .logo { padding: 0px;}")),
        
        tags$head(tags$style(HTML(
          ' .headerTitleCust {
          font-size: 25px;
          line-height: 50px;
          text-align: center;
          font-family: "Open Sans";
          padding: 0px 0px 0px 0px;
          overflow: hidden;
          color: white;
          margin-right: 20%;
                                            
        }
    '))),
        tags$script(HTML('
            $(document).ready(function() {
              $(".headerTitleCust").remove()
              $("header").find("nav").append(\'<div class="headerTitleCust"> Infrastructure Global Indicators Dashboard</div>\');
            })
           ')),
        tabItems(
          tabItem(
            tabName="landing_page",
            tabsetPanel(
              tabPanel('Indicator Trends',
                       # Begin data tab
                       fluidPage(
                         
                         fluidRow(
                           column(12, 
                                  mod_indicator_trend_tab_module_ui("indicator_trend_tab")
                           ),
                           
                           
                         ))), # End data tab
              tabPanel('InfraSAP Pillars', # begin infrasap tab
                       fluidPage(
                         
                         fluidRow(
                           column(12, 
                                  mod_infrasap_tab_module_ui("infrasap_tab")
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  DT::dataTableOutput('db_table'))
                           
                         )
                       )), # end infrasap tab
              tabPanel('Systematic Country Diagnostic', # being SCD tab
                       br(), br(),
                       fluidPage(
                         
                         # scd_tab_module_ui("scd_tab")
                         mod_scd_tab_module_ui("scd_tab")
                         
                       )
              ), # End SCD tab
              tabPanel('World maps', # begin world charts tab
                       fluidPage(
                         h3('All Indicators: World Map'),
                         
                         mod_map_tab_module_ui("map_tab")
                         
                       )
              ) # end world charts tab
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                column(12,
                       tags$h2(style = 'color:#28323d', 'Infrastructure Global Indicators Dashboard'),
                       tags$p(class = "about_p", 'The Infrastructure Global Indicators Dashboard is a comprehensive 
                              compilation of all infrastructure indicators available from a wide 
                              variety of sources that permits rapid identification of country performance 
                              indicators and trends and provides an agile capability for benchmarking 
                              against different peer groups. The dashboard also contains specific 
                              indicators from the Infrastructure Sector Assessment Program (InfraSAP) 
                              Analytical Tool and the Systematic Country Diagnostic (SCD).'
                       ),
                       tags$p(class = "about_p", 'The InfraSAP 2.0 Analytical Tool analyzes data from over 120 indicators
                               for over 200 countries and territories to provide a comprehensive overview 
                               of the state of infrastructure in those countries/territories and the main 
                               infrastructure challenges a country faces from a connectivity, finance, 
                               and governance perspective. It also pinpoints relevant governance and 
                               finance contributing factors that may help explain any shortcomings in 
                               infrastructure that those countries have.'
                       ),
                       tags$p(class = "about_p", 'The SCD tool will help users to collect some of the most important indicators 
                              from each of the three infrastructure sectors- Power, Digital development and 
                              Transport. The aim of this shortlist of indicators is to provide a very quick 
                              high level snapshot of the current situation in the country and help initiate 
                              the discussion for infrastructure inclusion in the SCD.'
                       )
                )
                
              )
            )
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'infrasap')
  )
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "www/img/wbi-fav.png"),
  )
}
