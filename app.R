library(shiny)
library(shinydashboard)
library(plotly)
library(sqldf)
library(reactable)
library(shinyWidgets)
library(leaflet)
library(rnaturalearth)
library(dplyr)

#================================
data_eur <- readRDS("data_eur.Rdata")
data_eur$date <- as.Date(data_eur$date)
d <- readRDS("sf_data_eur.Rdata")
#================================

# Define UI for application that draws a histogram
ui <-  dashboardPage(skin = "green",
                     dashboardHeader(title="Covid-19 Dashboard"),
                     dashboardSidebar(
                       sidebarSearchForm(textId = "search_country", buttonId = "search_Button", 
                                         label = "Search country"),
                       pickerInput(inputId = "id_sel", 
                                   label = "Select a country: ",
                                   choices = c("All",unique(data_eur$location)),
                                   multiple=TRUE
                       ),
                       sidebarMenu(
                         menuItem("Data Explorer", tabName = "dtexp", icon = icon("database")),
                         menuItem("Deaths", tabName = "dth", icon = icon("skull-crossbones") ),
                         menuItem("Cases", tabName = "cs", icon = icon("suitcase-medical")),
                         menuItem("Tests", tabName = "ts", icon = icon("vial-virus")),
                         menuItem("Hospitalizations", tabName = "hpt", icon = icon("bed-pulse")),
                         menuItem("Vaccinations", tabName = "vac", icon = icon("syringe"))
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "dtexp",
                                 fluidRow(
                                   infoBox("Goal", 
                                           "This app offers COVID-19 updates for European 
                                      countries until 2022, featuring key stats like 
                                      confirmed cases, deaths, vaccinations, 
                                      hospitalizations, and tests conducted.", 
                                           icon = icon("circle-question"), 
                                           color = "green", 
                                           width = 12
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", reactableOutput("dtexp_table1"),"Here is an overview of the database.", icon = icon("table-cells"))
                                   )
                                 )
                         ),
                         tabItem(tabName = "dth",
                                 fluidRow(
                                   infoBox("Infromation", 
                                           "Explore COVID-19 death data for European 
                                      countries up to 2022. Select countries to 
                                      see detailed stats.", 
                                           icon = icon("circle-info"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   infoBox("Comment", 
                                           "A zero value in the graph might indicate
                                      missing data, not necessarily zero deaths.", 
                                           icon = icon("note-sticky"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", icon = icon("table-cells"),reactableOutput("dth_table")),
                                     tabPanel("Map", "Data on world Map", icon = icon("earth-africa"), leafletOutput("dth_map")),
                                     tabPanel("Chart", "Chart", icon = icon("chart-line"), plotlyOutput("dth_chart"))
                                   )
                                 )
                         ),
                         tabItem(tabName = "cs",
                                 fluidRow(
                                   infoBox("Infromation", 
                                           "Explore COVID-19 case data for European 
                                      countries up to 2022. Select countries to 
                                      see detailed stats.", 
                                           icon = icon("circle-info"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   infoBox("Comment", 
                                           "A zero value in the graph might indicate 
                                      missing data, not necessarily zero cases reported. ", 
                                           icon = icon("note-sticky"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", "Data Table", icon = icon("table-cells"),reactableOutput("cs_table")),
                                     tabPanel("Map", "Data on world Map", icon = icon("earth-africa"),leafletOutput("cs_map")),
                                     tabPanel("Chart", "Chart", icon = icon("chart-line"),plotlyOutput("cs_chart"))
                                   )
                                 )
                                 
                         ),
                         tabItem(tabName = "ts",
                                 fluidRow(
                                   infoBox("Infromation", 
                                           "Explore COVID-19 testing data for European 
                                      countries up to 2022. Select countries to 
                                      see detailed stats.", 
                                           icon = icon("circle-info"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   infoBox("Comment", 
                                           "A zero value or discontinuty in the graph might indicate 
                                      missing data, not necessarily zero tests 
                                      conducted.", 
                                           icon = icon("note-sticky"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", "Data Table", icon = icon("table-cells"),reactableOutput("ts_table")),
                                     tabPanel("Map", "Data on world Map", icon = icon("earth-africa"),leafletOutput("ts_map")),
                                     tabPanel("Chart", "Chart", icon = icon("chart-line"),plotlyOutput("ts_chart"))
                                   )
                                 )
                                 
                         ),
                         tabItem(tabName = "hpt",
                                 fluidRow(
                                   infoBox("Infromation", 
                                           "Explore COVID-19 hospitalization data for 
                                      European countries up to 2022. Select 
                                      countries to see detailed stats.", 
                                           icon = icon("circle-info"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   infoBox("Comment", 
                                           "A zero value or discontinuty in the graph might indicate 
                                      missing data, not necessarily zero hospitalizations. ", 
                                           icon = icon("note-sticky"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", "Data Table", icon = icon("table-cells"),reactableOutput("hpt_table")),
                                     tabPanel("Map", "Data on world Map", icon = icon("earth-africa"),leafletOutput("hpt_map")),
                                     tabPanel("Chart", "Chart", icon = icon("chart-line"),plotlyOutput("hpt_chart"))
                                   )
                                 )
                                 
                         ),
                         tabItem(tabName = "vac",
                                 fluidRow(
                                   infoBox("Infromation", 
                                           "Explore COVID-19 vaccination data for 
                                      European countries up to 2022. 
                                      Select countries to see detailed stats.", 
                                           icon = icon("circle-info"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   infoBox("Comment", 
                                           "A zero value or discontinuity in the graph 
                                      might indicate missing data, not necessarily zero vaccinations.", 
                                           icon = icon("note-sticky"), 
                                           color = "green", 
                                           width = 6
                                   ),
                                   tabBox(
                                     id = "tabset1", height = "450px", width = 12,
                                     tabPanel("Table", "Data Table", icon = icon("table-cells"),reactableOutput("vac_table")),
                                     tabPanel("Map", "Data on world Map", icon = icon("earth-africa"),leafletOutput("vac_map")),
                                     tabPanel("Chart", "Chart", icon = icon("chart-line"),plotlyOutput("vac_chart"))
                                   )
                                 ) 
                         )
                       )
                     )
)


# Define server logic required to draw a histogram
server <-function(input, output, session) {
  
  # Réaction pour mettre à jour les données en fonction de la recherche
  search_reactive <- reactive({
    search_text <- input$search_country
    if (search_text != "") {
      data_eur[data_eur$location == search_text,]
    } else {
      data_eur
    }
  })
  
  # Réaction pour mettre à jour les données en fonction du pays sélectionné
  select_reactive <- reactive({
    req(input$id_sel)
    if ("All" %in% input$id_sel) {
      search_reactive()
    } else {
      data_eur[data_eur$location %in% input$id_sel,]
    }
  })
  
  output$dtexp_table1 <- renderReactable({
    reactable(select_reactive(), pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  }) 
  
  # Réaction pour mettre à jour les données en fonction de l'onglet sélectionné
  output$dth_table <- renderReactable({
    reactable(
      select_reactive()[, c("iso_code","continent","location","date", "total_deaths", 
                            "new_deaths", "new_deaths_smoothed", "new_deaths_per_million", 
                            "new_deaths_smoothed_per_million")],
      pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  })
  
  output$dth_chart <- renderPlotly({
    req(input$id_sel)
    # Créer le graphique interactif
    plot_ly(select_reactive(), x = ~date, y = ~total_deaths, color = ~iso_code, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Total Deaths"),
             title = "Total Deaths Over Time",
             hovermode = "closest",
             hoverlabel = list(bgcolor = "white"),
             hoverinfo = "text",
             text = ~paste("Location: ", location, "<br>Continent: ", continent, "<br>Date: ", date, "<br>Total Deaths: ", total_deaths)
      )
  })
  
  output$dth_map <- renderLeaflet({
    # Créer une palette de couleurs pour représenter la variable total_deaths
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = d$total_deaths
    )
    
    # Ajouter une colonne pour représenter la couleur en fonction de total_deaths
    d$color <- color_palette(d$total_deaths)
    
    # Créer la carte
    map <- leaflet(d) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        fillOpacity = 0.5,
        weight = 2,
        color = "white",
        dashArray = "2",
        highlight = highlightOptions(
          weight = 5,
          color = "red"
        ),
        label = ~paste(name, ", Total Deaths:", total_deaths)
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~total_deaths,
        title = "Total Deaths",
        opacity = 1
      )
  })
  
  output$cs_table <- renderReactable({
    reactable(
      select_reactive()[, c("iso_code","continent","location","date", "total_cases", 
                            "new_cases", "new_cases_smoothed", "new_cases_per_million", 
                            "new_cases_smoothed_per_million")],
      pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  })
  
  output$cs_chart <- renderPlotly({
    req(input$id_sel)
    # Créer le graphique interactif
    plot_ly(select_reactive(), x = ~date, y = ~total_cases, color = ~iso_code, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Total Cases"),
             title = "Total Cases Over Time",
             hovermode = "closest",
             hoverlabel = list(bgcolor = "white"),
             hoverinfo = "text",
             text = ~paste("Location: ", location, "<br>Continent: ", continent, "<br>Date: ", date, "<br>Total Cases: ", total_cases)
      )
  })
  
  output$cs_map <- renderLeaflet({
    # Créer une palette de couleurs pour représenter la variable total_deaths
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = d$total_cases
    )
    
    # Ajouter une colonne pour représenter la couleur en fonction de total_deaths
    d$color <- color_palette(d$total_cases)
    
    # Créer la carte
    map <- leaflet(d) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        fillOpacity = 0.5,
        weight = 2,
        color = "white",
        dashArray = "2",
        highlight = highlightOptions(
          weight = 5,
          color = "red"
        ),
        label = ~paste(name, ", Total Cases:", total_cases)
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~total_cases,
        title = "Total Cases",
        opacity = 1
      )
  })
  
  output$ts_table <- renderReactable({
    reactable(
      select_reactive()[, c("iso_code","continent","location","date", "total_tests", 
                            "new_tests", "new_tests_smoothed", "new_tests_per_thousand", 
                            "new_tests_smoothed_per_thousand", "total_tests_per_thousand",
                            "positive_rate", "tests_per_case", "tests_units")],
      pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  })
  
  output$ts_chart <- renderPlotly({
    req(input$id_sel)
    # Créer le graphique interactif
    plot_ly(select_reactive(), x = ~date, y = ~total_tests, color = ~iso_code, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Total Tests"),
             title = "Total Tests Over Time",
             hovermode = "closest",
             hoverlabel = list(bgcolor = "white"),
             hoverinfo = "text",
             text = ~paste("Location: ", location, "<br>Continent: ", continent, "<br>Date: ", date, "<br>Total Tests: ", total_tests)
      )
  })
  
  output$ts_map <- renderLeaflet({
    # Créer une palette de couleurs pour représenter la variable total_deaths
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = d$total_tests
    )
    
    # Ajouter une colonne pour représenter la couleur en fonction de total_deaths
    d$color <- color_palette(d$total_tests)
    
    # Créer la carte
    map <- leaflet(d) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        fillOpacity = 0.5,
        weight = 2,
        color = "white",
        dashArray = "2",
        highlight = highlightOptions(
          weight = 5,
          color = "red"
        ),
        label = ~paste(name, ", Total Tests:", total_tests)
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~total_tests,
        title = "Total Tests",
        opacity = 1
      )
  })
  
  output$hpt_table <- renderReactable({
    reactable(
      select_reactive()[, c("iso_code","continent","location","date", "icu_patients", 
                            "icu_patients_per_million", "hosp_patients", "hosp_patients_per_million", 
                            "weekly_icu_admissions", "weekly_icu_admissions_per_million",
                            "weekly_hosp_admissions", "weekly_hosp_admissions_per_million")],
      pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  })
  
  output$hpt_chart <- renderPlotly({
    req(input$id_sel)
    # Créer le graphique interactif
    plot_ly(select_reactive(), x = ~date, y = ~hosp_patients, color = ~iso_code, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Total Hospitalizations"),
             title = "Total Hospitalizations Over Time",
             hovermode = "closest",
             hoverlabel = list(bgcolor = "white"),
             hoverinfo = "text",
             text = ~paste("Location: ", location, "<br>Continent: ", continent, "<br>Date: ", date, "<br>Total Hospitalizations: ", hosp_patients)
      )
  })
  
  output$hpt_map <- renderLeaflet({
    # Créer une palette de couleurs pour représenter la variable total_deaths
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = d$hosp_patients
    )
    
    # Ajouter une colonne pour représenter la couleur en fonction de total_deaths
    d$color <- color_palette(d$hosp_patients)
    
    # Créer la carte
    map <- leaflet(d) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        fillOpacity = 0.5,
        weight = 2,
        color = "white",
        dashArray = "2",
        highlight = highlightOptions(
          weight = 5,
          color = "red"
        ),
        label = ~paste(name, ", Total Hospitalizations:", hosp_patients)
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~hosp_patients,
        title = "Total Hospitalizations",
        opacity = 1
      )
  })
  
  output$vac_table <- renderReactable({
    reactable(
      select_reactive()[, c("iso_code","continent","location","date", "total_vaccinations", 
                            "people_vaccinated", "people_fully_vaccinated", "total_boosters", 
                            "new_vaccinations", "new_vaccinations_smoothed",
                            "total_vaccinations_per_hundred", "people_vaccinated_per_hundred",
                            "people_fully_vaccinated_per_hundred", "total_boosters_per_hundred",
                            "new_vaccinations_smoothed_per_million", "new_people_vaccinated_smoothed",
                            "new_people_vaccinated_smoothed_per_hundred")],
      pagination = TRUE, highlight = TRUE, height = 350, bordered = TRUE)
  })
  
  output$vac_chart <- renderPlotly({
    req(input$id_sel)
    # Créer le graphique interactif
    plot_ly(select_reactive(), x = ~date, y = ~total_vaccinations, color = ~iso_code, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Total Vaccinationss"),
             title = "Total vaccinations Over Time",
             hovermode = "closest",
             hoverlabel = list(bgcolor = "white"),
             hoverinfo = "text",
             text = ~paste("Location: ", location, "<br>Continent: ", continent, "<br>Date: ", date, "<br>Total Vaccinations: ", total_vaccinations)
      )
  })
  
  output$vac_map <- renderLeaflet({
    # Créer une palette de couleurs pour représenter la variable total_deaths
    color_palette <- colorNumeric(
      palette = "YlOrRd",
      domain = d$total_vaccinations
    )
    
    # Ajouter une colonne pour représenter la couleur en fonction de total_deaths
    d$color <- color_palette(d$total_vaccinations)
    
    # Créer la carte
    map <- leaflet(d) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color,
        fillOpacity = 0.5,
        weight = 2,
        color = "white",
        dashArray = "2",
        highlight = highlightOptions(
          weight = 5,
          color = "red"
        ),
        label = ~paste(name, ", Total Vaccinations:", total_vaccinations)
      ) %>%
      addLegend(
        pal = color_palette,
        values = ~total_vaccinations,
        title = "Total vaccinations",
        opacity = 1
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
