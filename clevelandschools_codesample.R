
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(prettyR)
library(vegan)
library(bslib)
library(lubridate)
library(janitor)
library(stringr)
library(leaflet.extras)
library(leaflegend)
library(scales)

cleveland_districts <- st_read("cleveland_districts_samplecode.shp")
cleveland_schoolsgeo_0918 <- st_read("cleveland_schools_0918_samplecode.shp")

bounds <- st_bbox(cleveland_districts)

col_custom <- c("#ffffb2", 
                "#fecc5c", 
                "#fd8d3c", 
                "#f03b20", 
                "#bd0026")
legend_labels <- c("1st Quintile",
                   "2nd Quintile",
                   "3rd Quintile",
                   "4th Quintile",
                   "5th Quintile")

pal1 <- colorQuantile(palette = col_custom, n = 5, 
                      domain = cleveland_districts$simpsoni)
pal2 <- colorQuantile(palette = col_custom, n = 5, 
                      domain = cleveland_districts$stutch_r)
pal3 <- colorQuantile(palette = col_custom, n = 5, 
                      domain = cleveland_districts$frrdc_ln_r)
pal4 <- colorQuantile(palette = col_custom, n = 5,
                      domain = cleveland_districts$per_pup_rv)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cosmo"),
  navbarPage("Cleveland Area Schools and Districts",
             tabPanel("Interactive Map",
                      HTML("<h1>Racial Diversity in Cleveland Area Schools</h1><br>
                           <strong>A map by Kristan Wong Karinen</strong><br>
                           <i>Considerable evidence demonstrates a clear link between racial segregation in schools, 
                           poverty, and the distribution of educational resources. Explore the relationship between 
                           race and various school map below, and use the 'Year' slider to see how the distribution of 
                           students and resources has changed over time. Select different individual school-level 
                           variables to filter schools by their demographics. Click on polygons or points to learn more
                           about the school districts or individual schools.</i><br><br>"),
                      fluidRow(
                        column(6, sliderInput("year", 
                                              tags$strong("Select Year:"),
                                              min = min(cleveland_districts$year),
                                              max = max(cleveland_districts$year),
                                              value = max(cleveland_districts$year),
                                              step = 1)),
                        column(6, radioButtons("var", 
                                               tags$strong("Select School-Level Variable:"), 
                                               selected = "Above 50% Economically Disadvantaged", 
                                               choices = c("Above 50% Economically Disadvantaged", 
                                                           "Racial Diversity Index Above 0.5", 
                                                           "Racial Diversity Index Below 0.25",
                                                           "All Schools")
                        )
                        )
                      ),
                      fluidRow(
                        column(8,
                               leafletOutput("map", height = "600px", width = "100%"),
                               HTML("<br>")),
                        column(2,
                               HTML(
                                 paste(
                                   "<table border='1' cellpadding='5' style='border-collapse: collapse;'>",
                                   "<thead><tr><th colspan='2' style='background-color: #f2f2f2; padding: 5px; text-align: 
                          center;'>School District Variables</th></tr></thead>",
                          "<tbody>",
                          paste(
                            sapply(1:length(col_custom), function(i) {
                              paste(
                                "<tr>",
                                "<td style='background-color:", col_custom[i], "; width: 30px;'></td>",
                                "<td style='padding: 5px;'>", legend_labels[i], "</td>",
                                "</tr>"
                              )
                            }),
                            collapse = ""
                          ),
                          "</tbody>",
                          "</table>")
                               )
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    filter(cleveland_districts, year == input$year)
  })
  selected_variable_data <- reactive({
    var <- input$var
    
    if (var == "Above 50% Economically Disadvantaged") {
      return(cleveland_schoolsgeo_0918[cleveland_schoolsgeo_0918$srvy_yr == input$year & 
                                         cleveland_schoolsgeo_0918$total > 200 & 
                                         cleveland_schoolsgeo_0918$ecn_ds > 0.5, ])
    } else if (var == "Racial Diversity Index Above 0.5") {
      return(cleveland_schoolsgeo_0918[cleveland_schoolsgeo_0918$srvy_yr == input$year & 
                                         cleveland_schoolsgeo_0918$total > 200 & 
                                         between(cleveland_schoolsgeo_0918$simpson, 0.5, 0.99), ])
    } else if (var == "Racial Diversity Index Below 0.25") {
      return(cleveland_schoolsgeo_0918[cleveland_schoolsgeo_0918$srvy_yr == input$year & 
                                         cleveland_schoolsgeo_0918$total > 200 & 
                                         between(cleveland_schoolsgeo_0918$simpson, 0.01, 0.25), ])
    } else if (var == "All Schools") {
      return(cleveland_schoolsgeo_0918[cleveland_schoolsgeo_0918$srvy_yr == input$year & 
                                         cleveland_schoolsgeo_0918$total > 200, ])
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      fitBounds(bounds[[1]], 
                bounds[[2]], 
                bounds[[3]], 
                bounds[[4]]) |>
      addMiniMap(position = "bottomleft",
                 toggleDisplay = TRUE,
                 minimized = TRUE) |>
      addMapPane("cleveland_districts", 
                 zIndex = 399)
  })
  
  observe({
    variable_data <- selected_variable_data()
    icons <- awesomeIconList(
      "public" = makeAwesomeIcon(
        icon = "asterisk",
        markerColor = "lightblue",
        iconColor = "blue",
        library = "glyphicon"
      ),
      "charter" = makeAwesomeIcon(
        icon = "plus",
        iconColor = "darkgreen",
        markerColor = "lightgreen",
        library = "glyphicon"
      ))
    proxy <-
      leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearGroup("type") |>
      clearControls()
    
    proxy |>
      addPolygons(data = filtered_data(),
                  fillOpacity = 0.7,
                  fillColor = ~pal1(simpsoni),
                  color = "white",
                  weight = 1,
                  label = ~lea_name,
                  options = pathOptions(pane = "cleveland_districts"),
                  group = "Racial Diversity Index",
                  popup = ~paste("<strong>District Name:</strong>",
                                 filtered_data()$lea_name,
                                 "<br><strong>Number of Schools:</strong>",
                                 filtered_data()$all_sch,
                                 "<br><strong>Total Enrollment:</strong>",
                                 filtered_data()$enrollment,
                                 "<br><strong>Racial Diversity Index:</strong>",
                                 filtered_data()$simpsoni),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 3, 
                                                      bringToFront = TRUE)) |>
      addPolygons(data = filtered_data(),
                  fillOpacity = 0.7,
                  fillColor = ~pal2(stutch_r),
                  color = "white",
                  weight = 1,
                  label = ~lea_name,
                  options = pathOptions(pane = "cleveland_districts"),
                  group = "Student-Teacher Ratio",
                  popup = ~paste("<strong>District Name:</strong>",
                                 filtered_data()$lea_name,
                                 "<br><strong>Number of Schools:</strong>",
                                 filtered_data()$all_sch,
                                 "<br><strong>Total Enrollment:</strong>",
                                 filtered_data()$enrollment,
                                 "<br><strong>Student-Teacher Ratio:</strong>",
                                 filtered_data()$stutch_r),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 3, 
                                                      bringToFront = TRUE)) |>
      addPolygons(data = filtered_data(),
                  fillOpacity = 0.7,
                  fillColor = ~pal3(frrdc_ln_r),
                  color = "white",
                  weight = 1,
                  label = ~lea_name,
                  options = pathOptions(pane = "cleveland_districts"),
                  group = "Free and Reduced Lunch Ratio",
                  popup = ~paste("<strong>District Name:</strong>",
                                 filtered_data()$lea_name,
                                 "<br><strong>Number of Schools:</strong>",
                                 filtered_data()$all_sch,
                                 "<br><strong>Total Enrollment:</strong>",
                                 filtered_data()$enrollment,
                                 "<br><strong>Free or Reduced Lunch Ratio:</strong>",
                                 filtered_data()$frrdc_ln_r),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 3, 
                                                      bringToFront = TRUE)) |>
      addPolygons(data = filtered_data(),
                  fillOpacity = 0.7,
                  fillColor = ~pal4(per_pup_rv),
                  color = "white",
                  weight = 1,
                  label = ~lea_name,
                  options = pathOptions(pane = "cleveland_districts"),
                  group = "Revenue Per Pupil",
                  popup = ~paste("<strong>District Name:</strong>",
                                 filtered_data()$lea_name,
                                 "<br><strong>Number of Schools:</strong>",
                                 filtered_data()$all_sch,
                                 "<br><strong>Total Enrollment:</strong>",
                                 filtered_data()$enrollment,
                                 "<br><strong>Revenue Per Pupil:</strong>",
                                 filtered_data()$per_pup_rv),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 3, 
                                                      bringToFront = TRUE)) |>
      addAwesomeMarkers(
        data = variable_data,
        icon = ~icons[type],
        label = ~variable_data$schl_nm,
        popup = ~paste(
          "<strong>School Name:</strong>",
          variable_data$schl_nm,
          "<br><strong>Racial Diversity Index:</strong>",
          variable_data$simpson,
          "<br><strong>Economically Disadvantaged Ratio:</strong>",
          variable_data$ecn_ds,
          ifelse(variable_data$mjrty_w == 1, "<br><strong><i>Majority White Student Body<i></strong>",
                 "<br><strong><i>Majority Non-White Student Body</i></strong>")
        ),
        group = ~variable_data$type
      ) |>
      addLegendAwesomeIcon(iconSet = icons,
                           orientation = 'horizontal',
                           title = htmltools::tags$div(
                             style = 'font-size: 16px',
                             'School Types'
                           ),
                           labelStyle = "font-size: 14px;",
                           position = "bottomright") |>
      addLayersControl(
        baseGroups = c("Racial Diversity Index",
                       "Student-Teacher Ratio",
                       "Free and Reduced Lunch Ratio",
                       "Revenue Per Pupil"),
        overlayGroups = c("public", "charter"),
        options = layersControlOptions(collapsed = FALSE))
  })
}

shinyApp(ui, server)

