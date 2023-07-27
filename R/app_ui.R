##################################################
# UI
##################################################

options(DT.options = list(pageLength = 20))

app_ui <- function(request) {
  options(scipen = '999')
  
  shiny::tagList(
    mobile_golem_add_external_resources(),
    shinydashboard::dashboardPage(title = "Infrasap dashboard",
      shinydashboard::dashboardHeader(title = tags$a(tags$img(src='www/logo2.jpg', alt = '', height = '50')),
                       titleWidth = 260),
      shinydashboard::dashboardSidebar(
        width = 260,
        shinydashboard::sidebarMenu(
          id = 'tabs',
          shinydashboard::menuItem(
            text="Global Infrastructure Dashboard",
            tabName="landing_page"),
          shinydashboard::menuItem(
            text = 'About',
            tabName = 'about')
        )),
      shinydashboard::dashboardBody(
        tags$head(tags$style(".skin-blue .main-header .logo { padding: 0px;}")),
        shinyjs::useShinyjs(),
        tags$head(tags$style(HTML(
          ' .headerTitleCust {
          font-size: 25px;
          line-height: 50px;
          text-align: center;
          font-family: "Open Sans";
          padding: 0px 0px 0px 0px;
          overflow: hidden;
          color: white;
          margin-right: 10%;
                                            
        }
    '))),
        tags$script(HTML('
            $(document).ready(function() {
              $(".headerTitleCust").remove()
              $("header").find("nav").append(\'<div class="headerTitleCust"> Global Infrastructure Dashboard</div>\');
            })
           ')),
        # change tab color when selected
        tags$style(HTML("
        .tabbable > .nav > li > a    {background-color: #DCDCDC;  color:black}
         .tabbable > .nav > li[class=active]  > a {background-color: skyblue; color:white}
      ")),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName="landing_page",
            shiny::tabsetPanel(
              shiny::tabPanel('Indicator Trends',
                       # Begin data tab
                       shiny::fluidPage(
                         
                         shiny::fluidRow(
                           shiny::column(12, 
                                  mod_indicator_trend_tab_module_ui("indicator_trend_tab")
                           ),
                         ))), # End data tab
              shiny::tabPanel('InfraSAP Pillars', # begin infrasap tab
                shiny::fluidPage(
                  shiny::fluidRow(
                    shiny::column(12, 
                                  mod_infrasap_tab_module_ui("infrasap_tab")
                           ),
                         ),
                  shiny::fluidRow(
                    shiny::column(12,
                                  DT::dataTableOutput('db_table'))
                         )
                       )), # end infrasap tab
                       shiny::tabPanel('Systematic Country Diagnostic', # being SCD tab
                       shiny::br(), shiny::br(),
                       shiny::fluidPage(
                         
                         # scd_tab_module_ui("scd_tab")
                         mod_scd_tab_module_ui("scd_tab")
                         
                       )
              ), # End SCD tab
              shiny::tabPanel('World maps', # begin world charts tab
                       shiny::fluidPage(
                         shiny::h3('All Indicators: World Map'),
                         
                         mod_map_tab_module_ui("map_tab")
                         
                       )
              ) # end world charts tab
            )
          ),
          shinydashboard::tabItem(
            tabName = 'about',
            shiny::fluidPage(
              shiny::fluidRow(
                shiny::column(12,
                       # tags$h2(style = 'color:#28323d', 'Infrastructure Global Indicators Dashboard'),
                       tags$p(class = "about_p", 'The Infrastructure Global Indicators Dashboard is a comprehensive compilation of all infrastructure indicators available from a wide variety of sources that permits rapid identification of country performance indicators and trends and provides an agile capability for benchmarking against different peer groups. The dashboard also contains specific indicators from the Infrastructure Sector Assessment Program (InfraSAP) Analytical Tool and the Systematic Country Diagnostic (SCD).
'
                       ),
                       tags$h2(style = 'color:#28323d', 'Indicator Trends'),
                       
                       tags$p(class = "about_p", 'The Indicator Trends tab provides access to infrastructure indicators across Digital Development, Energy and Transport (transport data is further separated into sub-sectors including roads, rail or ports). Users can select a country, sector and indicator and compare with data from other countries or a regional or income group benchmark. Some indicators values can also be compared to a second indicator for the country. Data can be displayed in chart form for the latest available year, or for a selected range of years and downloaded as an image file. The data can also be exported as CSV.'
                       ),
                       tags$h2(style = 'color:#28323d', 'InfraSAP Pillars'),
                       
                       tags$p(class = "about_p", 'The InfraSAP 2.0 Analytical Tool analyzes data from over 120 indicators for over 200 countries and territories to provide a comprehensive overview of the state of infrastructure in those countries/territories and the main infrastructure challenges a country faces from a connectivity, finance, and governance perspective. It also pinpoints relevant governance and finance contributing factors that may help explain any shortcomings in infrastructure that those countries have.
Users can select a country, sector and pillar (perspective). The country can be compared to a selection of other countries and benchmarks. Data is displayed for a single year, or for the latest year available. Color codes are used to indicate how the selected country compares with the comparison country or benchmark with green indicating better performance, yellow equivalent performance and red poorer performance.'),
                       tags$h2(style = 'color:#28323d', 'Systematic Country Diagnostic
'),
                       tags$p(class = "about_p", 'The SCD tool will help users to collect some of the most important indicators from each of the three infrastructure sectors- Power, Digital Development and Transport. The aim of this shortlist of indicators is to provide a very quick high level snapshot of the current situation in the country and help initiate the discussion for infrastructure inclusion in the SCD.
Users can select a country and compare with other countries or benchmarks. Color codes indicate how the selected country performed compared to the comparator, with green indicating better performance, yellow equivalent performance and red poorer performance. Data can be exported as CSV.'),
                       tags$h2(style = 'color:#28323d', 'World Maps'),
                       tags$p(class = "about_p", 'The World Maps tab provides a geographical view of an indicator. Users can select a sector, year and indicator and produce a world or regional choropleth map which can be downloaded as an image file.'),
                       
                ),
                
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
  shiny::addResourcePath(
    'www', system.file('app/www', package = 'infrasap')
  )
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "www/img/wbi-fav.png"),
  )
}
