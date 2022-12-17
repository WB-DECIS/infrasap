#' map_tab_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_tab_module_ui <- function(id){
  
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::fluidRow(
      shiny::column(3, 
                    shiny::selectInput(inputId = ns('world_sector'), 
                                       label = 'Select a Sector',
                                       choices = sort(unique(infrasap_dat_mod_modified$`Indicator Sector`)),
                                       selected = 'Energy'
                                       )
             ),
      
      shiny::column(3,
                    shiny::selectInput(ns('world_year'), 
                                       'Select year',
                                       choices = c('2010',
                                                   '2011',
                                                   '2012',
                                                   '2013', 
                                                   '2014',
                                                   '2015',
                                                   '2016', 
                                                   '2017', 
                                                   '2018', 
                                                   '2019'
                                                   ),
                                       selected = '2015'
                                       )
             ),
      shiny::column(3,
                    shiny::selectInput(ns('world_region'), 
                                       'Select a region',
                                       choices = c('Entire World', 
                                                   'East Asia & Pacific',
                                                   'Europe & Central Asia', 
                                                   'Latin America & Caribbean', 
                                                   'Middle East & North Africa', 
                                                   'North America', 'South Asia', 
                                                   'Sub-Saharan Africa'
                                                   )
                                       )
             ),
      
      
      
      shiny::column(3,
                    shiny::uiOutput(ns('world_ind_ui')) 
      ),
      shiny::fluidRow(
        shiny::column(3,
                      shiny::div(class = "form-group shiny-input-container",
                                 shiny::downloadButton(ns('downloadMap'), 'Download Map')
               )
               ),
        shiny::column(3),
        shiny::column(3),
        shiny::column(3)
      ),
      shiny::fluidRow(
        shiny::column(12, 
               align = 'center',
               leaflet::leafletOutput(ns('world_map'), height = 700, width = 1000) %>% shinycssloaders::withSpinner(type = 7, color = "#154164")
        )
      )
    )
  )
}
    
#' map_tab_module Server Functions
#'
#' @noRd 
mod_map_tab_module_server <- function(id) {
  
  infrasap_dat_mod_modified <- infrasap::dat
  infrasap_dat_mod_modified$`Indicator Sector`[infrasap_dat_mod_modified$`Indicator Sector` == "National"] <- "Cross-cutting"
  
  
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    # ui for world ind
    output$world_ind_ui <- shiny::renderUI({
      sc <- input$world_sector
      yr <- input$world_year
      rn <- input$world_region
      
      if(rn == 'Entire World'){
        # subset data to get indicator 
        df <- infrasap_dat_mod_modified %>%
          dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
          dplyr::select(.data$`Indicator Name`, yr) %>% 
          tidyr::drop_na()
      } else {
        # subset data to get indicator 
        df <- infrasap_dat_mod_modified %>%
          dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
          dplyr::filter(.data$Region == rn) %>%
          dplyr::select(.data$`Indicator Name`, yr) %>% 
          tidyr::drop_na()
      }
      ic_choices <- sort(unique(df$`Indicator Name`))
      shiny::fluidRow(
        shiny::selectInput(inputId = ns('world_ind'), 
                            'Select an Indicator',
                            choices =ic_choices,
                            selected = 'Access to electricity (% of population)')
      )
    })
    
    
    #renderMAP
    mapToShow <- shiny::reactive({
      
      # these countries dont have corresponding shape file, likely because of names: "South Sudan", "Curacao", "Sint Maarten (Dutch part)", "Kosovo","Channel Islands" 
      sc <- input$world_sector
      yr <- input$world_year
      rn <- input$world_region
      ic <- input$world_ind
      
      map <- infrasap::world
      
      if(is.null(ic)){
        NULL
      } else {
        if(rn == 'Entire World'){
          # for now just visualize entire world 
          df <- infrasap_dat_mod_modified %>% 
            dplyr::filter(.data$`Indicator Name`== ic) %>% 
            dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
            dplyr::select(.data$`Country Name`, .data$`Country Code`, .data$`Indicator Sector`,yr)
        } else {
          # for now just visualize entire world 
          df <- infrasap_dat_mod_modified %>% 
            dplyr::filter(.data$`Indicator Name`== ic) %>% 
            dplyr::filter(.data$`Indicator Sector` %in% sc) %>%
            dplyr::filter(.data$Region == rn) %>%
            dplyr::select(.data$`Country Name`, .data$`Country Code`, .data$`Indicator Sector`,yr)
        }
        
        
        if(nrow(df) == 0){
          world_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1,
                                                                          maxZoom = 10)) %>%
            leaflet::addProviderTiles('CartoDB.VoyagerNoLabels') %>%
            leaflet::setView(lat = 0, lng = 0 , zoom = 1.7) %>%
            htmlwidgets::onRender(
                                  "function(el, x) {
                                                    L.control.zoom({position:'bottomright'}).addTo(this);
                                  }"
                                 )
        } else {
          # join with shp files
          map@data <- map@data %>% dplyr::left_join(df, by = c('ISO_A3'= 'Country Code'))
          names(map@data)[ncol(map@data)] <- 'value'
          
          
          # get region location (from manually created data in "create_data.R" file in data-raw folder)
          loc <- infrasap::map_location %>% dplyr::filter(.data$region == rn)
          lat <- loc$lat
          lon <- loc$lon
          zoom_level <- loc$zoom
          
          # generate map
          map_palette <- leaflet::colorNumeric(palette = RColorBrewer::brewer.pal(9, "Greens"), domain=map@data$value, na.color="transparent")
          map_text <- paste(
            "Indicator: ",  ic,"<br>",
            "Country: ", as.character(map@data$`Country Name`),"<br/>",
            'Value: ', paste0(round(map@data$value, digits = 2)),  "<br/>",
            "Year: ", as.character(yr),"<br/>",
            sep = "") %>%
            lapply(htmltools::HTML)
          
          world_map <- leaflet::leaflet(map, options = leaflet::leafletOptions(minZoom = 1, maxZoom = 10, preferCanvas = TRUE)) %>%
            leaflet::addProviderTiles('Esri.WorldShadedRelief') %>%
            leaflet::addPolygons(
              color = 'black',
              fillColor = ~map_palette(value), 
              stroke = TRUE,
              fillOpacity = 0.9,
              weight = 1,
              label = map_text,
              highlightOptions = leaflet::highlightOptions( 
                weight = 1,
                fillColor = 'white',
                fillOpacity = 1,
                color = "white",
                opacity = 1.0, 
                bringToFront = TRUE,
                sendToBack = TRUE
              ),
              labelOptions = leaflet::labelOptions(
                noHide = FALSE,
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              ) 
            ) %>% leaflet::setView(lat = lat, lng = lon , zoom = zoom_level) %>%
            leaflet::addLegend(pal = map_palette, title = yr, values = ~value, opacity=0.9, position = "bottomleft", na.label = "NA" )
        } 
        world_map %>% htmlwidgets::onRender(
                                      "function(el, x) {
                                        L.control.zoom({
                                          position:'bottomright'
                                        }).addTo(this);
                                       }
                                      "
                                    )
      }
      
    })
    
    
    # MAP
    output$world_map <- leaflet::renderLeaflet({
      
      mapToShow()
      
    })
    
    
    map_title <- shiny::reactiveValues(
      ic_name_map = NULL,
      ic_name_region = NULL,
      ic_name_sector = NULL,
      ic_name_year = NULL
    )
    
    shiny::observe({
      shiny::req(input$world_ind, input$world_region, input$world_sector, input$world_year)
      
      map_title$ic_name_map <- as.character(stringr::str_replace_all(string = input$world_ind, pattern = " ", replacement = "_"))
      map_title$ic_name_region <- as.character(stringr::str_replace_all(string = input$world_region, pattern = " ", replacement = "_"))
      map_title$ic_name_sector <- as.character(stringr::str_replace_all(string = input$world_sector, pattern = " ", replacement = "_"))
      map_title$ic_name_year <- as.character(input$world_year)
      
    })
    
    
    filenameReact <- shiny::reactive({
      rr <- shiny::div(
        shiny::HTML(as.character(stringr::str_glue('<h4 style="margin: 0;">{input$world_ind}</h4> 
                                                    <h6 style="margin: 0;">Region: {input$world_region}</h6>
                                                    <h6 style="margin: 0;">Sector: {input$world_sector}</h6>
                                                    <h6 style="margin: 0;">Year: {input$world_year}</h6>')
                          )
             )
      )
      
      mapToShow() %>% leaflet::addControl(rr, position = "topleft") 
      
    })
    
    
    output$downloadMap <- shiny::downloadHandler(
      filename =  function() {
        as.character(stringr::str_glue("{map_title$ic_name_map}.png"))
      },
      
      content = function(file) {
        shiny::withProgress(
          message = paste0("Downloading", ' Map', " (It usually takes a few minutes)"),
          value = 0,
          {
            shiny::incProgress(1/10)
            Sys.sleep(1)
            shiny::incProgress(5/10)
            mapview::mapshot(filenameReact(), file = file)
          }
        )
      }
      
    )
    
    
  })
}
    
## To be copied in the UI
# mod_map_tab_module_ui("map_tab_module_ui_1")
    
## To be copied in the server
# mod_map_tab_module_server("map_tab_module_ui_1")
