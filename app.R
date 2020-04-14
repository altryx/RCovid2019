
library(shiny)
library(shinyjs)
library(leaflet)
library(readr)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(DT)
library(kableExtra)
library(magrittr)
library(plotly)

# Required to resolve issue with image transparency when rendered using shiny-server
if (.Platform$OS.type == "unix") { options(shiny.usecairo = FALSE) }

# Misc parameters
ptn_column_date <- "(.+)/(.+)/(.+)"
# US Latitude and Longitude for summary calculations from individual county/state data
US_Lat <- 37.090200; US_Long <- -95.712900
# Data folder path and files
folder_prefix <- "./data/"
url_base <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
data_files <- c("raw_global_confirmed"="time_series_covid19_confirmed_global.csv", "raw_global_deaths"="time_series_covid19_deaths_global.csv", "raw_global_recoveries"="time_series_covid19_recovered_global.csv", "raw_US_deaths"="time_series_covid19_deaths_US.csv", "raw_US_confirmed"="time_series_covid19_confirmed_US.csv")
url_list <- paste0(url_base, data_files) %>% set_names(value = names(data_files))
file_list <- paste0(folder_prefix, data_files) %>% set_names(value = names(data_files))
geo_list <- c("")

# Function that checks if the data files exist and if they have been downloaded in the last 6 hours
# If files exist, they are loaded into appropriate variables
# Data (C) Johns Hopkins University CSSE


data_load <- function (force = FALSE) {
    
    time_condition <- all(file.mtime(file_list %>% set_names(value = file_list)) > (Sys.time() - hours(6)) )

    if (force || !time_condition || is.na(time_condition)) {
        walk(names(data_files), function(x) { download.file(url = url_list[x], destfile = file_list[x], quiet = TRUE) })
    } 
    
    suppressMessages({
        walk(names(data_files), function(x) { temp_df <- read_csv(file_list[x]); assign(x, temp_df, envir = .GlobalEnv) }) })
    
}

#rm(list=ls()[grep(pattern="raw_.+", x = ls())])

data_load()

# Tidy data

# Global confirmed cases, excl. US, per Country/State, chronological order as reported
df_global_confirmed <- raw_global_confirmed %>% select("State"="Province/State", "Country"="Country/Region", "Lat", "Long", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>% mutate(Date=parse_date(`Date`, format = "%m/%d/%y")) %>% 
    group_by (Country, State, Date, Lat, Long) %>% summarize(Confirmed = sum(Confirmed))

# Global death figures, excl. US, per Country/State, chronological order as reported
df_global_deaths <- raw_global_deaths %>% select("State"="Province/State", "Country"="Country/Region", "Lat", "Long", matches(ptn_column_date)) %>% 
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Deaths") %>% mutate(Date = parse_date(`Date`, format = "%m/%d/%y")) %>%
    group_by (Country, State, Date, Lat, Long) %>% summarize(Deaths = sum(Deaths))

# US confirmed cases, per county and state, chronological order as reported
df_US_confirmed_detail <- raw_US_confirmed %>% mutate(Country = "US") %>% select("State"="Province_State", "Country", "Lat", "Long"="Long_", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>% mutate(Date = parse_date(`Date`, format = "%m/%d/%y"))

# US deaths, per county and state, chronological order as reported
df_US_deaths_detail <- raw_US_deaths %>% mutate(Country = "US") %>% select("State"="Province_State", "Country", "Lat", "Long"="Long_", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Deaths") %>% mutate(Date = parse_date(`Date`, format = "%m/%d/%y"))

# US confirmed cases, summary per state, chronlogical order as reported
df_US_confirmed_perstate <- df_US_confirmed_detail %>% group_by (Country, State, Date) %>% summarize(Lat = US_Lat, Long = US_Long, Confirmed = sum(Confirmed))
# US deaths, summary per state, chronological order as reported
df_US_deaths_perstate <- df_US_deaths_detail %>% group_by(Country, State, Date) %>% summarize(Lat = US_Lat, Long = US_Long, Deaths = sum(Deaths))

# Country/State data, including US, Chronological order
# Global confirmed cases
#df_global_confirmed <- bind_rows(df_global_confirmed1, df_US_confirmed_perstate %>% group_by(Country, Date) %>% summarize(Confirmed = sum(Confirmed), Lat = mean(Lat), Long = mean(Long)))
# Global deaths
#df_global_deaths <- bind_rows(df_global_deaths, df_US_deaths_perstate %>% group_by(Country, Date) %>% summarize(Deaths = sum(Deaths), Lat = mean(Lat), Long = mean(Long)))
# Global recoveries
df_global_recoveries <- raw_global_recoveries %>% select("State"="Province/State", "Country"="Country/Region", "Lat", "Long", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Recoveries") %>%
    mutate(Date = parse_date(`Date`, format = "%m/%d/%y")) %>% 
    group_by (Country, State, Date, Lat, Long) %>% summarize(Recoveries = sum(Recoveries))

# Latest data, per Country/State, with deltas
df_covid <- left_join(df_global_confirmed, df_global_deaths, by = c("Country", "State", "Date", "Lat", "Long")) %>% left_join(df_global_recoveries, by = c("Country", "State", "Date", "Lat", "Long")) 

# Summary of latest data only, per country, with deltas
# IN USE - MAP
df_covid_summary <- df_covid %>% group_by(Country, State) %>% arrange(desc(Date)) %>% 
    mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0), dDeaths = Deaths -lead(Deaths, default = 0), dRecoveries = Recoveries - lead(Recoveries, default = 0), 
           rtoDeathsConfirmed = Deaths/Confirmed, Active = Confirmed - Recoveries - Deaths, dActive = Active - lead(Active)) 


# df_covid_summary_extended <- df_covid %>% group_by(Country, Date) %>% summarize(Confirmed = max(Confirmed), Deaths = max(Deaths), Recoveries = max(Recoveries)) %>% 
#     group_by(Country) %>% arrange(Country, desc(Date)) %>% top_n(n = 2, wt = Date) %>% group_by(Country) %$% 
#     left_join( (.) %>% slice(1), (.) %>% slice(2) %>% select(Country, lstConfirmed = Confirmed, lstDeaths = Deaths, lstRecoveries = Recoveries), by = "Country") %>% 
#     mutate(Active = Confirmed - Recoveries, dConfirmed = Confirmed - lstConfirmed, dDeaths = Deaths - lstDeaths, dRecoveries = Recoveries - lstRecoveries, rtoDeathsConfirmed = Deaths/Confirmed)



# Data summary for the table 
df_covid_summary_percountry <- df_covid %>% ungroup() %>% select (-Lat, -Long) %>% group_by(Country, Date) %>% 
    arrange(desc(Date)) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE), Deaths = sum(Deaths, na.rm = TRUE), Recoveries = sum(Recoveries, na.rm = TRUE)) %>% group_by(Country) %>% arrange(desc(Date)) %>%
    mutate(dConfirmed = Confirmed - lead(Confirmed), dDeaths = Deaths -lead(Deaths), dRecoveries = Recoveries - lead(Recoveries), rtoDeathsConfirmed = Deaths/Confirmed, Active = Confirmed - Recoveries, dActive = Active - lead(Active)) 

# Data summary for the cumulative daily world figure
df_covid_daily_world_summary <- df_covid_summary %>% group_by(Date) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE)) %>% arrange(desc(Date)) %>% mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0))
#plot(df_covid_daily_world_summary$Date, df_covid_daily_world_summary$dConfirmed)
#group_by(Country, Date) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE))



territory_list_summary <- unique(df_covid_summary$Country)
territory_list <- paste0(df_covid_summary$Country, ifelse(is.na(df_covid_summary$State),"", paste0(", ", df_covid_summary$State)))

# Define UI 

ui <- bootstrapPage(
    useShinyjs(),
    tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
    tags$head(includeCSS("style.css")),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height="100%"),
    
    # Map controls
    absolutePanel(top = 80, left = 12, width = "30px", height = "30px", actionButton("showHistory", label = NULL, icon = icon("history"), class = "btn-custom", title = "Historical data")),
    absolutePanel(top = 120, left = 12, width = "30px", height = "30px", actionButton("showRegionSel", label = NULL, icon = icon("search-location"), class = "btn-custom", title = "Location summary")),
    absolutePanel(top = 160, left = 12, width = "30px", height = "30px", actionButton("resetMap", label = NULL, icon = icon("globe-americas"), class = "btn-custom", title = "Reset map view")),
    absolutePanel(top = 200, left = 12, width = "30px", height = "30px", actionButton("getLocation", label = NULL, icon = icon("location-arrow"), class = "btn-custom", title = "Zoom to your location")),
    absolutePanel(top = 240, left = 12, width = "30px", height = "30px", actionButton("githubLink", label = NULL, icon = icon("github"), class = "btn-custom", title = "Visit project page on GitHub", onclick ="window.open('https://altryx.dev/rcovid2019', '_blank')")),
    absolutePanel(bottom = 6, right = 265, width = "235px", height = "10px", fixed = TRUE, div(id = "copyrightmsg", "Map data | (C) 2020 Johns Hopkins University")),
    
    # Additional panels for map controls
    hidden(absolutePanel(top = 80, left = 45, height = "80px", id = "panelHistory", class = "panel panel-default", 
                         sliderInput("MaxDate", label = NULL, min = min(df_covid$Date), max = max(df_covid$Date), step = 3, value = max(df_covid$Date), timeFormat = "%d %b", animate = animationOptions(interval = 2500, loop = FALSE)))),
    hidden(absolutePanel(top = 120, left = 45, height = "40px", id = "panelRegionSel", class = "panel panel-default", 
                        selectInput("territory", label = NULL, choices = territory_list_summary))),
    
    # Summary indicators
    absolutePanel(top = 10, right = 10, width = "250px", height = "50px", htmlOutput("total_cases", class = "panel summarydisplay confirmedcases", title = "Total number of confirmed COVID-19 cases as of date")),
    absolutePanel(top = 70, right = 10, width = "250px", height = "50px", htmlOutput("total_deaths", class = "panel summarydisplay deaths", title = "Total number of deaths attributed to COVID-19 as of date")),
    absolutePanel(top = 130, right = 10, width = "250px", height = "50px", htmlOutput("total_recoveries", class = "panel summarydisplay recoveries", title = "Total number of recovered patients as of date")),
    absolutePanel(top = 190, right = 10, width = "250px", height = "25px", htmlOutput("lastUpdated", class = "panel summarydisplay lastupdated", title = "Date of last update. Data is automatically refreshed every 6 hours from the source.")),
    
    # Growth in Confirmed cases plot
    absolutePanel(top = 235, right = 10, width = "250px", height = "200px", id = "panelGrowthPlot", class = "panel panel-default", title = "Daily movement in the number of confirmed cases.", plotOutput("plt_confirmed_growth", width = "100%", height = "100%")),
    
    # Top10 Tables
    absolutePanel(top = 455, right = 10, width = "250px", height = 350, fixed = TRUE, class = "panel panel-default", 
                  tabsetPanel(type = "pills", 
                              tabPanel("Confirmed", tableOutput("tbl_top10_confirmed")), 
                              tabPanel("Recoveries", tableOutput("tbl_top10_recoveries")), 
                              tabPanel("Deaths", tableOutput("tbl_top10_deaths"))
                              ))
)


# Define server logic
server <- function(input, output, session) {
    
    covid_summary_table <- reactive({
        df_covid_summary %>% filter(Date <= input$MaxDate) %>% filter(Date == max(Date)) 
    })
    
    covid_daily_world_summary <- reactive({
        df_covid_summary %>% filter(Date <= input$MaxDate) %>% group_by(Date) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE)) %>% arrange(desc(Date)) %>% mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0)) 
    })
   
    
    # Map Setup
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.1, minZoom = 3.2)) %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            setMaxBounds(-180, -70, 180, 80) %>%
            setView(zoom = 3.2, lat = 20, lng = 0) %>%
                    addLayersControl(position = "bottomleft",
                                     baseGroups = c("Confirmed cases", "Active cases", "Deaths", "New cases (last 24h)"),
                                     options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup(c("Active cases", "Deaths", "New cases (last 24h)"))
    })
    
    # Observer for map layer changes
    observe({
        leafletProxy("map", data = covid_summary_table()) %>%
            clearMarkers() %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(Confirmed)^(1/4), weight = 1, color = rgb(1,0,0), fillOpacity = 0.3, group = "Confirmed cases",
                             popup = ~sprintf("<h4>%s</h4>Updated on %s<br><br>Confirmed cases: %s<br>Deaths: %s<br>Recoveries: %s<br>Active cases: %s<br>",
                                 ifelse(is.na(State), Country, paste(State, Country, sep = ", ")), 
                                                    format(Date, "%Y-%m-%d"),
                                                    format(Confirmed, big.mark = " "), 
                                                    format(Deaths, big.mark = " "), 
                                                    ifelse(is.na(Recoveries), "N/A", format(Recoveries, big.mark = " ")),
                                                    ifelse(is.na(Recoveries), "N/A", format(Confirmed - Recoveries, big.mark = " ")) 
                             )) %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(Active)^(1/4), weight = 1, color = rgb(1,0.7,0), fillOpacity = 0.3, group = "Active cases", 
                             popup = ~sprintf("<h4>%s</h4>Updated on %s<br><br>Active cases: %s<br>Change since last update: %s<br>",
                                              ifelse(is.na(State), Country, paste(State, Country, sep = ", ")),
                                              format(Date, "%Y-%m-%d"),
                                              format(Active, big.mark = " "), 
                                              format(dActive, big.mark = " ")) 
                             ) %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(dConfirmed)^(1/3), weight = 1, color = rgb(0,0.5,0.5), fillOpacity = 0.3, group = "New cases (last 24h)", 
                             popup = ~sprintf("<h4>%s</h4>For the 24h (ending %s):<br><br>Confirmed cases: %s<br>Deaths: %s<br>Recoveries: %s<br>",
                                             ifelse(is.na(State), Country, paste(State, Country, sep = ", ")),
                                             format(Date, "%Y-%m-%d"),
                                             format(dConfirmed, big.mark = " "), 
                                             format(dDeaths, big.mark = " "), 
                                             ifelse(is.na(dRecoveries), "N/A", format(dRecoveries, big.mark = " "))
                             )) %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(Deaths)^(1/3), weight = 1, color = rgb(0.7,0.7,0.7), fillOpacity = 0.3, group = "Deaths", 
                             popup = ~sprintf("<h4>%s</h4>Updated on %s<br><br>Deaths: %s<br>Change since last update: %s<br>",
                                              ifelse(is.na(State), Country, paste(State, Country, sep = ", ")),
                                              format(Date, "%Y-%m-%d"),
                                              format(Deaths, big.mark = " "),
                                              format(dDeaths, big.mark = " "))
                             ) 
    })
    

    
    fmt_num <- function(num) { format(num, nsmall = 0, big.mark = ",") }
    
    fmt_btm_titles <- function(text, var) { sprintf("%s<div class=\"summarydisplay summaryfigure\">%s</div>", ifelse(is.null(text), "", paste0(text, "<br>")), format(var, nsmall = 0, big.mark = " ")) } 
    
    
    output$total_cases <- renderText({ fmt_btm_titles("Confirmed Cases", sum(covid_summary_table()$Confirmed, na.rm = TRUE)) }) 
    output$total_deaths <- renderText({ fmt_btm_titles("Deaths", sum(covid_summary_table()$Deaths, na.rm = TRUE)) })
    output$total_recoveries <- renderText({ fmt_btm_titles("Recoveries", sum(covid_summary_table()$Recoveries, na.rm = TRUE)) }) 
    output$lastUpdated <- renderText({ fmt_btm_titles(NULL, input$MaxDate) })
    
    # Plot of daily growth in confirmed cases
    output$plt_confirmed_growth <- renderPlot({
        ggplot(data = covid_daily_world_summary(), aes(x = Date, y = dConfirmed)) + geom_col() + theme_bw() + xlab(NULL) + ylab("New cases") + labs(fill="white") + theme_bw() + 
            theme(plot.background = element_blank(), axis.text = element_text(colour = "white", family = "Arial", size = 12), text = element_text(face = "bold", colour = "white", family = "Arial", size = 14)) +
            scale_y_continuous(na.value = 0, expand = expansion(mult = c(0, 0.05)), labels = function(x) {trans = x; x <- (x>=1000 & !is.na(x>=1000)); trans[x] <- trans[x] / 1000 ; trans[x] <- paste0(trans[x], "k"); trans }) +
             scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(add = 0))
    }, bg = "transparent", execOnResize = TRUE)
    
    
    # Top 10 countries by Confirmed Cases
    output$tbl_top10_confirmed <- function() {
            covid_summary_table() %>% ungroup() %>% group_by(Country) %>% filter(Date == max(Date)) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE), dConfirmed = sum(dConfirmed, na.rm = TRUE)) %>% 
            arrange(desc(Confirmed)) %>% top_n(n = 10, wt = Confirmed) %>% ungroup() %>% 
            mutate(Rank = row_number(), Confirmed = paste0(format(Confirmed, big.mark = " "), " ", ifelse(dConfirmed > 0, "<i class=\"glyphicon glyphicon-triangle-top\"></i>", "<i class=\"glyphicon glyphicon-triangle-bottom\"></i>"))) %>% 
            select(Rank, Country, Confirmed) %>% 
            mutate(Rank = cell_spec(Rank, color = "black", background = "gray")) %>% 
            kableExtra::kable(format = "html", col.names = NULL, escape = FALSE) %>% kable_styling(bootstrap_options = c("condensed"), full_width = TRUE) %>% 
            row_spec(1:10, color = "white") %>% column_spec(1, width = "1em") %>%  column_spec(3, extra_css = "text-align:right;") %>%
            scroll_box(height = "350px", extra_css = "overflow-y:auto !important; border: none !important;margin-bottom: 25px;")
    }
    
    # Top 10 countries by Recoveries
    output$tbl_top10_recoveries <- function() {
        covid_summary_table() %>% ungroup() %>% group_by(Country) %>% filter(Date == max(Date)) %>% summarize(Recoveries = sum(Recoveries, na.rm = TRUE), dRecoveries = sum(dRecoveries, na.rm = TRUE)) %>%
            arrange(desc(Recoveries)) %>% top_n(n = 10, wt = Recoveries) %>% ungroup() %>%
            mutate(Rank = row_number(), Recoveries = paste0(format(Recoveries, big.mark = " "), " ", ifelse(dRecoveries > 0, "<i class=\"glyphicon glyphicon-triangle-top\"></i>", "<i class=\"glyphicon glyphicon-triangle-bottom\"></i>"))) %>% 
            select(Rank, Country, Recoveries) %>%
            mutate(Rank = cell_spec(Rank, color = "black", background = "gray")) %>%
            kableExtra::kable(format = "html", col.names = NULL, escape = FALSE, ) %>% kable_styling(bootstrap_options = c("condensed"), full_width = TRUE) %>% 
            row_spec(1:10, color = "white") %>% column_spec(1, width = "1em") %>% 
            column_spec(3, extra_css = "text-align:right;") %>%
            scroll_box(height = "350px", extra_css = "overflow-y:auto !important; border: none !important;")
    }
    
    
    # Top 10 countries by Deaths
    output$tbl_top10_deaths <- function() {
        covid_summary_table() %>% ungroup() %>% group_by(Country) %>% filter(Date == max(Date)) %>% summarize(Deaths = sum(Deaths, na.rm = TRUE), dDeaths = sum(dDeaths, na.rm = TRUE)) %>% 
            arrange(desc(Deaths)) %>% top_n(n = 10, wt = Deaths) %>% ungroup() %>% 
            mutate(Rank = row_number(), Deaths = paste0(format(Deaths, big.mark = " "), " ", ifelse(dDeaths > 0, "<i class=\"glyphicon glyphicon-triangle-top\"></i>", "<i class=\"glyphicon glyphicon-triangle-bottom\"></i>"))) %>% 
            select(Rank, Country, Deaths) %>%
            mutate(Rank = cell_spec(Rank, color = "black", background = "gray")) %>% 
            kableExtra::kable(format = "html", col.names = NULL, escape = FALSE) %>% 
            kable_styling(bootstrap_options = c("condensed"), full_width = TRUE) %>% row_spec(1:10, color = "white") %>% 
            column_spec(1, width = "1em") %>% column_spec(3, extra_css = "text-align:right;") %>% 
            scroll_box(height = "350px", extra_css = "overflow-y:auto !important; border: none !important;")
    }
    
    observeEvent(input$showHistory, { hide("panelRegionSel"); toggle("panelHistory")} )
    observeEvent(input$showRegionSel, { hide("panelHistory"); toggle("panelRegionSel")})
    
    # Basic country selection with zoom
    observeEvent(input$territory, ignoreInit = TRUE, {
        #locations <- isolate({covid_summary_table()}) %>% filter((Country == str_remove(input$territory, ",.+") & State == str_remove(input$territory, ".+, ")) || Country == input$territory)  %>% select(Lat, Long)
        locations <- isolate({ covid_summary_table() }) %>% filter(Country == input$territory & is.na(State))  %>% select(Lat, Long)
        leafletProxy("map") %>% setView(lat = locations$Lat, lng = locations$Long, zoom = 6)
    })
    
    observeEvent(input$getLocation, {
        req(input$getLocation)
        leafletProxy("map") %>% setView(lat = input$lat, lng = input$long, zoom = 4)
    })
    
    # Reset map to center and initial zoom level
    observeEvent(input$resetMap, {
        hide("panelRegionSel")
        leafletProxy("map") %>% setView(lat = 20, lng = 0, zoom = 3.2)
        #     output$map <- leaflet("map") %>% setView(zoom = 3.2, lat = 0, lng = 0)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
