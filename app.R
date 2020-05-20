library(ggplot2)
library(shiny)
library(shinyjs)
library(leaflet)
library(readr)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(magrittr)
library(plotly)
library(DT)

# Required to resolve issue with image transparency when rendered using shiny-server on linux
if (.Platform$OS.type == "unix") { options(shiny.usecairo = FALSE) }

try(expr = rm("tbr_left", envir = .GlobalEnv))

# Misc parameters
ptn_column_date <- "(.+)/(.+)/(.+)" # Regex for column date pattern
tbar_start_left <- 370 # Toolbar start position


# Data load ---------------------------------------------------------------

# Data folder path and files
folder_prefix <- "./data/"
url_base <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
data_files <- c("raw_global_confirmed"="time_series_covid19_confirmed_global.csv", "raw_global_deaths"="time_series_covid19_deaths_global.csv", "raw_global_recoveries"="time_series_covid19_recovered_global.csv", "raw_US_deaths"="time_series_covid19_deaths_US.csv", "raw_US_confirmed"="time_series_covid19_confirmed_US.csv")
url_list <- paste0(url_base, data_files) %>% set_names(value = names(data_files))
file_list <- paste0(folder_prefix, data_files) %>% set_names(value = names(data_files))
geo_list <- c(df_geoloc=paste0(folder_prefix, "locationdb.csv"))

# Function that checks if the data files exist and if they have been downloaded in the last 6 hours
# If files exist, they are loaded into appropriate variables
# Data (C) Johns Hopkins University CSSE


data_load <- function (force = FALSE) {
    
    time_condition <- all(file.mtime(file_list %>% set_names(value = file_list)) > (Sys.time() - hours(6)) )

    if (force || !time_condition || is.na(time_condition)) {
        walk(names(data_files), function(x) { download.file(url = url_list[x], destfile = file_list[x], quiet = TRUE) })
    } 
    
    suppressMessages({
        walk(names(data_files), function(x) { temp_df <- read_csv(file_list[x]); assign(x, temp_df, envir = .GlobalEnv) }) 
        assign(names(geo_list), read_csv(geo_list), envir = .GlobalEnv )
        })
    
    
}


data_load()

# Data cleanup
raw_global_confirmed %<>% filter(!`Province/State` %in% "Recovered")


# Source tables -----------------------------------------------------------


# Global confirmed cases, excl. US, per Country/State, chronological order as reported
df_global_confirmed <- raw_global_confirmed %>% 
    select("State"="Province/State", "Country"="Country/Region", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>% 
    mutate(Date=parse_date(`Date`, format = "%m/%d/%y")) %>% 
    group_by (Country, State, Date) %>% 
    summarize(Confirmed = sum(Confirmed, na.rm = TRUE))

# Global death figures, excl. US, per Country/State, chronological order as reported
df_global_deaths <- raw_global_deaths %>% 
    select("State"="Province/State", "Country"="Country/Region", matches(ptn_column_date)) %>% 
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Deaths") %>% 
    mutate(Date = parse_date(`Date`, format = "%m/%d/%y")) %>%
    group_by (Country, State, Date) %>% 
    summarize(Deaths = sum(Deaths, na.rm = TRUE))

# US confirmed cases, per county and state, chronological order as reported
df_US_confirmed_detail <- raw_US_confirmed %>% 
    mutate(Country = "US") %>% 
    select("State"="Province_State", "Country", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>% 
    mutate(Date = parse_date(`Date`, format = "%m/%d/%y"))

# US deaths, per county and state, chronological order as reported
df_US_deaths_detail <- raw_US_deaths %>% mutate(Country = "US") %>% 
    select("State"="Province_State", "Country", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Deaths") %>% 
    mutate(Date = parse_date(`Date`, format = "%m/%d/%y"))

# Global recoveries
df_global_recoveries <- raw_global_recoveries %>% 
    select("State"="Province/State", "Country"="Country/Region", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Recoveries") %>%
    mutate(Date = parse_date(`Date`, format = "%m/%d/%y")) %>% 
    group_by (Country, State, Date) %>% 
    summarize(Recoveries = sum(Recoveries, na.rm = TRUE))

# Global confirmed cases, including US per state level
df_global_confirmed <- bind_rows(df_global_confirmed %>% filter(!Country == "US"), df_US_confirmed_detail %>% group_by(Country, State, Date) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE)))

# Global deaths
df_global_deaths <- bind_rows(df_global_deaths %>% filter(!Country == "US"), df_US_deaths_detail %>% group_by(Country, State, Date) %>% summarize(Deaths = sum(Deaths, na.rm = TRUE)))


# Latest data, per Country/State, with deltas
df_covid <- full_join(df_global_confirmed, df_global_deaths, by = c("Country", "State", "Date")) %>% full_join(df_global_recoveries, by = c("Country", "State", "Date")) %>% left_join(df_geoloc %>% filter(is.na(Admin)), by = c("Country", "State")) 

# Summary per country and state
df_covid_summary <- df_covid %>% 
    group_by(Country, State) %>% 
    arrange(desc(Date)) %>% 
    mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0), 
           dDeaths = Deaths -lead(Deaths, default = 0), 
           dRecoveries = Recoveries - lead(Recoveries, default = 0), 
           rtoDeathsConfirmed = Deaths/Confirmed, 
           rtoRecoveryConfirmed = Recoveries/Confirmed,
           #rtoConfirmedPopulation = Confirmed/Population * 100000,
           Active = Confirmed - Recoveries - Deaths, 
           dActive = Active - lead(Active, default = 0)) 

# Summary per country
df_covid_summary_country <- df_covid %>% 
    ungroup() %>% 
    select (-Lat, -Long) %>% 
    group_by(Country, Date) %>% 
    arrange(desc(Date)) %>% 
    summarize(Confirmed = sum(Confirmed, na.rm = TRUE), 
              Deaths = sum(Deaths, na.rm = TRUE), 
              Recoveries = sum(Recoveries, na.rm = TRUE)) %>% 
    group_by(Country) %>% 
    arrange(desc(Date)) %>%
    mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0), 
           dDeaths = Deaths -lead(Deaths, default = 0), 
           dRecoveries = Recoveries - lead(Recoveries, default = 0), 
           rtoDeathsConfirmed = Deaths/Confirmed,
           rtoRecoveryConfirmed = Recoveries/Confirmed,
           #rtoConfirmedPopulation = Confirmed/Population * 100000,
           Active = Confirmed - Recoveries - Deaths, 
           dActive = Active - lead(Active, default = 0)) %>%
    left_join(df_geoloc %>% filter(is.na(Admin) & is.na(State)), by = c("Country")) %>% 
    select(-Admin)


# Data summary for the world
df_covid_daily_summary_world <- df_covid_summary %>% 
    group_by(Date) %>% 
    summarize(Confirmed = sum(Confirmed, na.rm = TRUE)) %>% 
    arrange(desc(Date)) %>% 
    mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0))


territory_list_summary <- unique(df_covid_summary$Country)
territory_list <- paste0(df_covid_summary$Country, ifelse(is.na(df_covid_summary$State),"", paste0(", ", df_covid_summary$State)))

loc_list <- anti_join(df_covid_summary, df_covid_summary_country) %>% 
  full_join(df_covid_summary_country) %>% 
  select(Country, State, Region, Continent, Lat, Long) %>% 
  mutate(Location = paste0(Country, ifelse(is.na(State), "", paste0(", ", State)))) %>%
  distinct()

# Not in use
# loc_list1 <- {
#   sapply(unique(loc_list$Country), function(x) {
#     state_list <- loc_list %>% ungroup() %>% filter(Country == x & !Region == "Other") %>% select(State) %>% mutate(State = replace_na(State, x)) %>% pull(State)
#     #y = c(loc_list[loc_list$Country==x, "State"])
#     y = c(state_list)
#     
#     #names(y)=""
#     #if(is.na(y)) { x  } else {y} })
#     if (length(y) > 1) { y } else { x }
#   })}

loc_list2 <- {
  country_list <- loc_list %>% ungroup() %>% select(Country) %>% distinct() %>% arrange() %>% pull(Country)
  state_list <- sapply(unique(loc_list$Country), function(x) {
    state_list <- loc_list %>% ungroup() %>% filter(Country == x & !Region == "Other" & !is.na(State)) %>% select(State) %>% mutate(State = replace_na(State, x)) %>% pull(State)})
  c(country_list, state_list)
}

tbr_add_left <- function(...) {
  if (!exists("tbr_left", where = .GlobalEnv)) { tbr_left <<- as.numeric(tbar_start_left)}
  if (...length() > 0) {
    tbr_left <<- tbr_left + sum(...)
  }
  
  tbr_left
}

# Define UI 

# UI ----------------------------------------------------------------------

# Dropdown generator for table sort UI
dropdownInput <- function(caption, items, caret = TRUE) {
  
  optCaret <- ifelse(caret, "<span class=\"caret\"></span>", "")
  
  HTML(paste0(
  "<button class=\"btn btn-primary dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" title=\"Change sort order\"><i class=\"fas fa-sort-amount-down\"></i>", 
  optCaret, 
  "</button><ul class=\"dropdown-menu\"><li><a id=\"ddnAbsoluteVal\" href=\"#\" onclick=\"Shiny.setInputValue('sortDelta', false, {priority: 'event'});\">Absolute values</a>
  </li><li><a id=\"ddnChanges\" href=\"#\" onclick=\"Shiny.setInputValue('sortDelta', true, {priority: 'event'});\">Changes</a></li></ul>")
  )
}




ui <- bootstrapPage(
    div(id = "loadingmsg"), #, img(src="loading2.gif")),
    useShinyjs(),
    tags$script(src = "rcovid2019.js"),
  
    tags$head(includeCSS("style.css")),
    leafletOutput("map", width = "100%", height="100%"),
    
    
    # Map controls
    # Change to CSS from inline
    absolutePanel(id = "pnlSiteLogo", HTML("<img src=\"COVID-19-banner.png\"></img>")),
    absolutePanel(id = "pnlMainNav", class = "panel panel-default",
                  #div(style = "display: block; position: relative; height: 2%;",
                      uiOutput("ddnVarSelect"),
                      uiOutput("ddnSortCriteria"),
                      #tabsetPanel(id = "tblSelect", type = "pills", tabPanel("Confirmed"), tabPanel("Recoveries"), tabPanel("Deaths"), tabPanel("Active")), 
                                  #div(id = "ddnSortCriteria", class = "dropdown", dropdownInput(NULL,NULL))),  
                  #div(style = "display: block; position: relative; height: calc(98% - 2px); overflow-y: auto; width:100%;", 
                      tableOutput("tblStats")),
    
    div(id = "copyrightmsg", "Map data | (C) 2020 Johns Hopkins University"),
    
    # Additional visualisations
    #absolutePanel(top = 17, left = 42, height = 30, actionButton("showTrends", label = "Trends"), class = "btn-custom", title = "Trends"),
    
    # Search bar
    div(id = "pnlToolbar",
    absolutePanel(top = 12, left = tbr_add_left(), height = 30, id = "panelSearch", 
                  selectizeInput("locSelect", width = 250, choices = loc_list2, selected = NULL, options = list(placeholder = "Search locations"),  label = NULL, multiple = FALSE)),
    absolutePanel(top = 12, left = tbr_add_left(251), height = 30, actionButton("resetMap", label = NULL, icon = icon("globe-americas"), class = "btn-custom", title = "Reset map view")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("getLocation", label = NULL, icon = icon("location-arrow"), class = "btn-custom", title = "Zoom to your location")),
    absolutePanel(top = 12, left = tbr_add_left(31, 31), width = 30, height = 30, actionButton("showHistory", label = NULL, icon = icon("history"), class = "btn-custom", title = "Historical data")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("showOptions", label = NULL, icon = icon("sliders-h"), class = "btn-custom", title = "Display Options")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("githubLink", label = NULL, icon = icon("github"), class = "btn-custom", title = "Visit project page on GitHub", onclick = "openGithubLink")),
    ),
    
    # Additional panels for map controls
    hidden(absolutePanel(top = 80, left = 45, height = "80px", id = "panelHistory", class = "panel panel-default", 
                         sliderInput("MaxDate", label = NULL, min = min(df_covid$Date), max = max(df_covid$Date), step = 3, value = max(df_covid$Date), timeFormat = "%d %b", animate = animationOptions(interval = 1500, loop = FALSE)))),
    hidden(absolutePanel(top = 120, left = 45, height = "40px", id = "panelRegionSel", class = "panel panel-default", 
                        selectInput("territory", label = NULL, choices = territory_list_summary))),
    
    # Summary indicators
    div(id = "pnlGlobalStats",
    absolutePanel(top = 10, right = 10, width = "250px", height = "50px", htmlOutput("total_cases", class = "panel summarydisplay confirmedcases", title = "Total number of confirmed COVID-19 cases as of date")),
    absolutePanel(top = 70, right = 10, width = "250px", height = "50px", htmlOutput("total_deaths", class = "panel summarydisplay deaths", title = "Total number of deaths attributed to COVID-19 as of date")),
    absolutePanel(top = 130, right = 10, width = "250px", height = "50px", htmlOutput("total_recoveries", class = "panel summarydisplay recoveries", title = "Total number of recovered patients as of date")),
    absolutePanel(top = 190, right = 10, width = "250px", height = "25px", htmlOutput("lastUpdated", class = "panel summarydisplay lastupdated", title = "Date of last update. Data is automatically refreshed every 6 hours from the source.")),
    
    # Growth in Confirmed cases plot
    absolutePanel(top = 235, right = 10, width = "250px", height = "200px", id = "panelGrowthPlot", class = "panel panel-default", title = "Daily increase in the number of confirmed cases.", plotOutput("plt_confirmed_growth", width = "100%", height = "100%")),
    ),
   
    # Detail country info panel
    hidden(
      absolutePanel(id = "pnlDetailInfo", class = "panel panel-default",
                    div(id = "btnPnlClose", class = "fa fa-times-circle", title = "Close window", onclick = "hideCountryDetail();"),
                    div(id = "pnlDetailTop", htmlOutput("dtlCountry")),
                    div(id = "pnlDetailPlots",
                        div(id = "divPltCumGrowth", class = "pltDiv", htmlOutput("dtlPltCumGrowthTitle", class = "pltTitle"), plotlyOutput("dtlPltCumGrowth", width = "90%", height = "auto")),
                        div(id = "divPltNewCases", class = "pltDiv", htmlOutput("dtlPltNewCasesTitle", class = "pltTitle"), plotlyOutput("dtlPltNewCases", width = "90%", height = "auto")),
                        div(id = "divPltCaseOutcomes", class = "pltDiv", htmlOutput("dtlPltCaseOutcomesTitle", class = "pltTitle"), plotlyOutput("dtlPltCaseOutcomes", width = "90%", height = "auto"))
                        )
      )
    )
)
    
    

#c("World top 5", "Neighbors", "Continent top 5", "None")


# Server ------------------------------------------------------------------

# Define server logic
server <- function(input, output, session) {
    
    covid_summary_table <- reactive({
        df_covid_summary %>% filter(Date <= input$MaxDate) %>% filter(Date == max(Date)) %>% filter(Confirmed > 0) 
    })
    
    covid_daily_country_summary <- reactive({
        df_covid_summary_country %>% filter(Date <= input$MaxDate) %>% filter(Date == max(Date)) %>% filter(Confirmed > 0) 
    })
    
    #fix
    covid_daily_summary_world <- reactive({
        df_covid_summary %>% filter(Date <= input$MaxDate) %>% group_by(Date) %>% summarize(Confirmed = sum(Confirmed, na.rm = TRUE)) %>% arrange(desc(Date)) %>% mutate(dConfirmed = Confirmed - lead(Confirmed, default = 0)) 
    })
    
    map_providers = c(providers$CartoDB.DarkMatter, providers$CartoDB.Positron)
    
    # Map Setup
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0, zoomDelta = 0.05, minZoom = 3.2, zoomControl = FALSE, worldCopyJump = TRUE)) %>% 
            addProviderTiles(map_providers[1]) %>% 
            #setMaxBounds(-180, -70, 180, 80) %>%
            setView(zoom = 3.2, lat = 20, lng = 0) %>%
                    addLayersControl(position = "bottomright",
                                     baseGroups = c("Confirmed cases", "Active cases", "Deaths", "New cases (last 24h)"),
                                     options = layersControlOptions(collapsed = FALSE)) %>%
            hideGroup(c("Active cases", "Deaths", "New cases (last 24h)"))
    })
    
    # Map data changes depending on zoom level
    mapMarkerData <- reactive({
      if (input$map_zoom <= 4) { covid_daily_country_summary() } else { covid_summary_table() }
    })
    
    # Save values for previous date and previous zoom level. Used to update map markers
    mapTriggers <- reactiveValues(zoom = 4, date = NULL)
    
    observeEvent(c(input$map_zoom, input$MaxDate), {
      mapTriggers$zoom <- c(tail(mapTriggers$zoom, 1), input$map_zoom)
      mapTriggers$date <- c(tail(mapTriggers$date, 1), input$MaxDate)
    })
    
    # Update map markers only if zoom level or date has changed
    observe({
      req(input$map_zoom)
  
      if (!(all(mapTriggers$zoom > 4) | all(mapTriggers$zoom < 4 )) | (length(unique(mapTriggers$date)) != 1)) {
      leafletProxy("map", data = mapMarkerData()) %>%
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
        addCircleMarkers(data = { mapMarkerData() %>% filter(Active > 0) },  lat = ~Lat, lng = ~Long, radius = ~(Active)^(1/4), weight = 1, color = rgb(1,0.7,0), fillOpacity = 0.3, group = "Active cases",
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
        )}
    })
    
    fmt_num <- function(num) { format(num, nsmall = 0, big.mark = ",") }
    
    fmt_btm_titles <- function(text, var) { sprintf("%s<div class=\"summarydisplay summaryfigure\">%s</div>", ifelse(is.null(text), "", paste0(text, "<br>")), format(var, nsmall = 0, big.mark = " ")) } 
    
    
    output$total_cases <- renderText({ fmt_btm_titles("Confirmed Cases", sum(covid_summary_table()$Confirmed, na.rm = TRUE)) }) 
    output$total_deaths <- renderText({ fmt_btm_titles("Deaths", sum(covid_summary_table()$Deaths, na.rm = TRUE)) })
    output$total_recoveries <- renderText({ fmt_btm_titles("Recoveries", sum(covid_summary_table()$Recoveries, na.rm = TRUE)) }) 
    output$lastUpdated <- renderText({ fmt_btm_titles(NULL, input$MaxDate) })
    
    # Plot of daily growth in confirmed cases
    output$plt_confirmed_growth <- renderPlot({
        ggplot(data = covid_daily_summary_world(), aes(x = Date, y = dConfirmed)) + geom_col() + theme_bw() + xlab(NULL) + ylab("New cases") + labs(fill="white") + theme_bw() + 
            theme(plot.background = element_blank(), axis.text = element_text(colour = "white", family = "Arial", size = 12), text = element_text(face = "bold", colour = "white", family = "Arial", size = 14)) +
            scale_y_continuous(na.value = 0, expand = expansion(mult = c(0, 0.05)), labels = function(x) {trans = x; x <- (x>=1000 & !is.na(x>=1000)); trans[x] <- trans[x] / 1000 ; trans[x] <- paste0(trans[x], "k"); trans }) +
             scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(add = 1))
    }, bg = "transparent", execOnResize = TRUE)
    
    # Remove 
    tbl_count_rows <- function(df) {
      assign("rowcount", nrow(df), envir = parent.env(parent.frame()))
      df
    }
    
    #observe({print(input$sortDelta); print(input$tblSelect); print(is.null(input$sortDelta))})
   
    # Change the formatting of the table sort selection dropdown depending on choice
     # observeEvent(input$sortDelta, {
     #   #if (!is.null(input$sortDelta)) {
     #     if (input$sortDelta) {
     #       removeClass(id = "ddnAbsoluteVal", class = "ddnSelected")
     #       addClass(id = "ddnChanges", class = "ddnSelected")
     #     } else {
     #       addClass(id = "ddnAbsoluteVal", class = "ddnSelected")
     #       removeClass(id = "ddnChanges", class = "ddnSelected")
     #     }
     #   #}
     #   })
    
    # Generate Stats table - streamline CSS
    output$tblStats <- function() {
      if (!is.null(input$sortDelta)) {
      covid_daily_country_summary() %>%
        ungroup() %>%
        arrange(desc(!! sym(ifelse((is.null(input$sortDelta) | input$sortDelta), paste0("d", input$tblSelect), input$tblSelect)))) %>% 
        mutate(Rank = cell_spec(row_number(), color = "black", background = "gray"),
               (!! sym(input$tblSelect)) := paste0(format(!! sym(input$tblSelect), big.mark = " "), 
                                                   "<br>", 
                                                   case_when(input$tblSelect != "Active" ~ "<span class=\"tblDelta\">&Delta; ", 
                                                             ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) > 0) ~ "<span class=\"tblDeltaPos\">",
                                                             ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) < 0) ~ "<span class=\"tblDeltaNeg\">",
                                                             TRUE ~ "<span class=\"tblDelta\">"), 
                                                   format(abs(!! sym(paste0("d", input$tblSelect))), big.mark = " "), 
                                                   "</span>"),
               Country = paste0("<a class=\"fas fa-map-marker-alt table-sym\" href=\"#\" onclick=\"Shiny.setInputValue('locSelect', '", Country, "', {priority: 'event'});\"></a>", " ", 
                                paste0("<a class=\"table-link\" onclick=\"displayCountryDetail('", Country, "');\">", Country, "</a>")) ) %>%
        select(Rank, Country, input$tblSelect)  %>%
        kableExtra::kable(format = "html", col.names = NULL, escape = FALSE, table.attr = "id=\"tblStats\" class=\"table table-condensed\"")
      
        # output$tblStats <- function() {
        #   if (!is.null(input$sortDelta)) {
        #     covid_daily_country_summary() %>%
        #       ungroup() %>%
        #       arrange(desc(!! sym(ifelse((is.null(input$sortDelta) | input$sortDelta), paste0("d", input$tblSelect), input$tblSelect)))) %>% 
        #       mutate(Rank = cell_spec(row_number(), color = "black", background = "gray"),
        #              (!! sym(input$tblSelect)) := paste0(format(!! sym(input$tblSelect), big.mark = " "), 
        #                                                  "<br>", 
        #                                                  case_when(input$tblSelect != "Active" ~ "<span class=\"table-delta\">&Delta; ", 
        #                                                            ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) > 0) ~ "<span class=\"table-delta-pos\">+ ",
        #                                                            ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) < 0) ~ "<span class=\"table-delta-neg\">- ",
        #                                                            TRUE ~ "<span class=\"table-delta\">"), 
        #                                                  format(abs(!! sym(paste0("d", input$tblSelect))), big.mark = " "), 
        #                                                  "</span>"),
        #              Country = paste0("<a class=\"fas fa-map-marker-alt table-sym\" href=\"#\" onclick=\"Shiny.setInputValue('locSelect', '", Country, "', {priority: 'event'});\"></a>", " ", 
        #                               paste0("<a class=\"table-link\" onclick=\"displayCountryDetail('", Country, "');\">", Country, "</a>")) ) %>%
        #       select(Rank, Country, input$tblSelect)  %>%
        #       kableExtra::kable(format = "html", col.names = NULL, escape = FALSE, table.attr = "id=\"tblStats\" class=\"table table-condensed\"")
        #     
        #     
      
      
      # covid_daily_country_summary() %>%
      #   ungroup() %>%
      #   select(Country, input$tblSelect, paste0("d", input$tblSelect)) %>% 
      #   arrange(desc(!! sym(input$tblSelect))) %>% 
      #   mutate(Rank = row_number(), 
      #          (!! sym(input$tblSelect)) := format(!! sym(input$tblSelect), big.mark = " "),
      #          (!! sym(paste0("d", input$tblSelect))) := paste0(ifelse(!! sym(paste0("d", input$tblSelect)) > 0, "+", "-"), format(abs(!! sym(paste0("d", input$tblSelect))), big.mark = " "))) %>%
      #   ungroup() %>% 
      #   select(Rank, Country, input$tblSelect, paste0("d", input$tblSelect)) %>% 
      #   pivot_longer(cols = c(input$tblSelect, paste0("d", input$tblSelect) ), names_to = "Var", values_to = "Val") %>% select(-Var) %>%
      #   mutate(deltaCol = ifelse(row_number() %% 2 == 0, "&Delta;", ""),
      #          Country = ifelse(row_number() %% 1 == 0, 
      #                           paste0("<a class=\"fas fa-map-marker-alt\" style=\"text-decoration: none; cursor: pointer;\" href=\"#\" onclick=\"Shiny.setInputValue('locSelect', '", Country, "', {priority: 'event'});\"></a>", " ", 
      #                                  paste0("<a class=\"table-link\" onclick=\"shinyjs.show('detailInfo')\">", Country, "</a>")), Country)) %>% 
      #   select(Rank, Country, deltaCol, Val) %>%
      #   tbl_count_rows() %>%
      #   mutate(Rank = cell_spec(Rank, color = "black", background = "gray", extra_css = "text-align:right;")) %>%
      #   kableExtra::kable(format = "html", col.names = NULL, escape = FALSE) %>%
      #   kable_styling(bootstrap_options = c("condensed"), full_width = TRUE) %>% row_spec(1:rowcount, color = "white") %>%
      #   column_spec(1, width = "8px") %>% column_spec(4, width = "75px", extra_css = "text-align: right;") %>% column_spec(3, width = "10px") %>%
      #   collapse_rows(columns = 1:2, valign = "middle")  #%>% column_spec(1:2, extra_css = "vertical-align: middle;")
    }}
    
    
    # Rework
    lst_comp_countries_plt <- c("Spain", "Italy", "France", "Germany", "US", "United Kingdom", "Canada", "South Korea", "Japan")
    #df_covid_summary_country %>% filter(Date <= input$MaxDate) %>% filter(Date == max(Date)) %>% filter(Confirmed > 0) 
    
    dtlPlotData <- reactive({ 
      req(input$tblCountrySelect)
      
      
      df_covid_summary_country %>%
        ungroup() %>%
        filter(Country %in% c(input$tblCountrySelect, lst_comp_countries_plt), Confirmed >= 50) %>% 
        #select(Country, Date, !!plotParams$plotVar) %>% 
        group_by(Country) %>% 
        arrange(Date) %>%
        mutate(Day_Confirmed = row_number()) %>% ungroup() 
      
      
      # switch(input$selectPlot, 
      #        "pltGrowth50" =
      #          {
      #            df_covid_summary_country %>%
      #              ungroup() %>%
      #              filter(Country %in% c(input$tblCountrySelect, lst_comp_countries_plt), Confirmed >= 50) %>% 
      #              select(Country, Date, !!plotParams$plotVar) %>% 
      #              group_by(Country) %>% 
      #              arrange(Date) %>%
      #              mutate(Day_Confirmed = row_number()) 
      #          },
      #        "pltProgression" = 
      #          {
      #            df_covid_summary_country %>% filter(Country %in% c(input$tblCountrySelect, lst_comp_countries_plt)) %>% arrange(Date)
      #          }
      #        
      #        
      #        
      #        )
      
      })
    
    genDdnList <- function(id, class = NULL, title = NULL, caption, caret, listItems, preHTML = NULL, postHTML = NULL) {
      # id        = dropdown list ID
      # class     = class applied to both the dropdown div and the button
      # title     = tooltip for the dropdown list
      # caption   = text/default choice for the dropdown
      # caret     = one of c("down", "updown", "none")
      # listItems = list of dropdown items in the format list("item_id" = c("title" = "Tooltip text", "caption" = "Menu item text"))
      # prefixHTML = HTML block inserted prior to the dropdown list block
      
      function() {
        excludedParams = c("caption", "innerHTML")
        
        ddn_header <- paste0("<div id=\"", id, "\" class=\"btn-group ", class, "\"> <button class=\"btn dropdown-toggle ", class, "\" type=\"button\" data-toggle=\"dropdown\" title=\"", title, "\">", caption, 
                               switch(caret, 
                                      "down" = "<span class=\"caret\"></span>", 
                                      "updown" = "<span class=\"fa fa-stack\"><i class=\"fa fa-caret-down\"></i><i class=\"fa fa-caret-up\"></i></span>", 
                                      "none" = "",
                                      ""
                                      ),
                              "</button>")
        ddn_body <- paste0("<ul class=\"dropdown-menu\">",
                           paste(
                                  map2(listItems, names(listItems), function (ddnItemParams, ddnItemID) {
                                    
                                    ddnItemParamsNames <- names(ddnItemParams)
                                    if (all(ddnItemParamsNames %in% excludedParams) & !is.null(ddnItemParamsNames) & (length(ddnItemParamsNames) > 1)) { warning("Both innerHTML and caption parameter have been specified. Ignoring caption.")}
                                    
                                    if (ddnItemID == "divider") { 
                                      "<li class=\"divider\"></li>" 
                                      } else {
                                        paste0("<li><a id=\"", ddnItemID, "\" ",
                                               paste0(
                                                 names(ddnItemParams)[!names(ddnItemParams) %in% excludedParams], "=\"", 
                                                 ddnItemParams[!names(ddnItemParams) %in% excludedParams], "\"",  
                                                 collapse = " "
                                               ),
                                               ">", ifelse(is.na(ddnItemParams["innerHTML"]), ddnItemParams["caption"], ddnItemParams["innerHTML"]), "</a></li>")
                                      }
                                    }
                                    ),
                                  collapse = "\n"
                                  ),
                           "</ul></div>")
        
        paste0(preHTML, ddn_header, ddn_body, postHTML)
        
        # test command:
        # abc = genDdnList("ddnCumGrowth","ddn-custom", caption = "Confirmed cases", caret = "updown", title = "Select a plot to display", listItems = list("ddnCumGrowth_Confirmed"=c("title"="Confirmed cases only", "label" = "Confirmed Cases"), "ddnCumGrowth_Recoveries"=c("title"="Recoveries only", "label" = "Recoveries")))
      }
    }
    
    output$ddnVarSelect <- genDdnList(id = "ddnVarSelect", class = "ddn-custom ddn-custom-alt", title = "Select a variable to display", caption = "Confirmed cases", caret = "down",
                                      listItems = list(
                                        ddnVarSelect_Confirmed = c(title = "Confirmed cases per country/territory", caption = "Confirmed cases"),
                                        ddnVarSelect_Deaths = c(title = "Number of deceased per country/territory", caption = "Deaths"),
                                        ddnVarSelect_Recoveries = c(title = "Recoveries per country/territory", caption = "Recoveries"),
                                        ddnVarSelect_Active = c(title = "Active cases per country/territory", caption = "Active cases")
                                      ))
    
    output$ddnSortCriteria <- genDdnList(id = "ddnSortCriteria", class = "ddn-custom ddn-custom-alt", title = "Select sort criteria", caption = "<i class=\"fas fa-sort-amount-down\"></i>", caret = "down", 
                                         listItems = list(
                                           ddnAbsoluteVal = c(title = "Sort based on absolute value of the selected variable", caption = "Absolute value"),
                                           ddnChanges = c(title = "Sort based on the change in value for the selected variable", caption = "Change in value")
                                         ))
    
    output$dtlPltCumGrowthTitle <- genDdnList(id = "ddnCumGrowth", class = "ddn-custom", title = "Select a plot to display", caption = "Confirmed cases", caret = "updown",
                                              preHTML = "<h4 class=\"pltTitle\">Cumulative Growth</h4>",
                                              listItems = list(
                                                ddnCumGrowth_Confirmed = c(title = "Confirmed cases only", caption = "Confirmed cases"),
                                                ddnCumGrowth_Recoveries = c(title = "Recoveries only", caption = "Recoveries"),
                                                ddnCumGrowth_Deaths = c(title = "Deceased only", caption = "Deaths"),
                                                ddnCumGrowth_Active = c(title = "Active cases only", caption = "Active cases"),
                                                divider = c(),
                                                ddnCumGrowth_chkLog = c(innerHTML = "<div class=\"checkbox\"><label><input id=\"chkPltCumGrowthLog\" type=\"checkbox\">Log scale</input></label></div>")
                                              ))
    
    output$dtlPltNewCasesTitle <- genDdnList(id = "ddnNewCases", class = "ddn-custom", title = "Select a plot to display", caption = "Confirmed cases", caret = "updown",
                                             preHTML = "<h4 class=\"pltTitle\">New Cases</h4>",
                                             listItems = list(
                                               ddnNewCases_dConfirmed = c(title = "Confirmed cases only", caption = "Confirmed cases"),
                                               ddnNewCases_dRecoveries = c(title = "Recoveries only", caption = "Recoveries"),
                                               ddnNewCases_dDeaths = c(title = "Deceased only", caption = "Deaths"),
                                               ddnNewCases_dActive = c(title = "Active cases only", caption = "Active cases")
                                             ))
    
    output$dtlPltCaseOutcomesTitle <- function() {
      HTML(paste0("
                  <h4 class=\"pltTitle\">Case Outcomes</h4>
                  "))
    }
    
    # output$dtlPltCumGrowthTitle <- function() {
    # HTML(paste0("
    # <h4 class=\"pltTitle\">Cumulative Growth</h4>
    # <div id=\"ddnCumGrowth\" class=\"btn-group ddn-custom\"><button class=\"btn dropdown-toggle ddn-custom\" type=\"button\" data-toggle=\"dropdown\" title=\"Select a plot to display\">
    # Confirmed cases<span class=\"fa fa-stack\"><i class=\"fa fa-caret-down\"></i><i class=\"fa fa-caret-up\"></i></span></button>
    # <ul class=\"dropdown-menu\">
    # <li class=\"dropdown-header\">Variables</li>
    # <li><a id=\"ddnCumGrowth_Confirmed\" href=\"#\" title=\"Confirmed cases only\">Confirmed cases</a></li>
    # <li><a id=\"ddnCumGrowth_Recoveries\" href=\"#\" title=\"Recoveries only\">Recoveries</a></li>
    # <li><a id=\"ddnCumGrowth_Deaths\" href=\"#\" title=\"\">Deaths</a></li>
    # <li class=\"divider\"></li>
    # <li><a id=\"ddnCumGrowth_chkLog\" href=\"#\" onclick=\"\"><div class=\"checkbox\"><label><input id=\"chkPltCumGrowthLog\" type=\"checkbox\">Log scale</input></label></div></a></li>
    # </ul></div>")
    #      )  
    # }
    
    
    # onclick=\"Shiny.setInputValue('pltCumGrowth', $('#checkbox1').is(\":checked\"), { priority: 'event' });\"

    

    
    plotlyDefConfig <- function(p) { config(p, displayModeBar = FALSE) }
    plotlyDefLayout <- function(p) { 
      layout(p,
             #modebar = list(bgcolor="transparent", color = "#444a4a47", activecolor = "#105652")
             xaxis = list(gridcolor = "gray"), 
             yaxis = list(gridcolor = "gray"),
             font = list(family = "Arial", size = 12, color = "white"),
             plot_bgcolor = "#444a4a47",
             paper_bgcolor = "rgba(0, 0, 0, 0)"
      )
    }
    
    output$dtlPltCumGrowth <- 
      renderPlotly({
        plotHeight <- input$dtlPlotHeight
        plotParams <- unlist(input$pltCumGrowthParams)
        
        plot_ly(data = dtlPlotData(), height = plotHeight) %>% add_lines(x = ~Day_Confirmed, y = ~get(plotParams["plotVar"]), color = ~Country) %>%
          plotlyDefLayout %>%
          layout(#legend = list(orientation = "h"),
            yaxis = list(title = switch(plotParams["plotVar"], 
                                        "Confirmed" = "Confirmed cases", 
                                        "Deaths" = "Deaths attributed to COVID-19", 
                                        "Recoveries" = "Recovered patients", 
                                        "Active" = "Active cases"), 
                         gridcolor = "gray", 
                         type = ifelse(plotParams["plotLog"], "log", "linear")
                         ),
            xaxis = list(title = "Days since 50 confirmed cases")
          ) %>%
          plotlyDefConfig
        
      })
    
    output$dtlPltNewCases <- 
      renderPlotly({
        plotHeight <- input$dtlPlotHeight
        plotParams <- unlist(input$pltNewCasesParams)
        
        plot_ly(data = dtlPlotData(), height = plotHeight) %>% add_lines(x = ~Date, y = ~get(plotParams["plotVar"]), color = ~Country) %>%
          plotlyDefLayout %>%
          layout(
            yaxis = list(title = switch(plotParams["plotVar"], "dConfirmed"="Confirmed cases", "dDeaths" = "Deaths attributed to COVID-19", "dRecoveries" = "Patients recovered", "dActive" = "Active cases"), gridcolor = "gray"),
            xaxis = list(title = NULL, showgrid = FALSE)
          ) %>%
          plotlyDefConfig
      })
      
    output$dtlPltCaseOutcomes <- 
      renderPlotly({
        plotHeight <- input$dtlPlotHeight
        plotParams <- unlist(input$pltCaseOutcomesParams)
        
        plot_ly(data = dtlPlotData(), height = plotHeight) %>% add_bars(x = ~Date, y = ~get(plotParams["plotVar"]), color = ~Country) %>%
          plotlyDefLayout %>%
          layout(
            yaxis = list(title = "Reported cases")
          ) %>%
          plotlyDefConfig
        
      })

    
    
    # URL parser for bookmark management - fix
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['country']])) {
        
        #updateSelectizeInput(session, inputId = "tblCountrySelect", selected = query[['country']])
      }})
    
    # Detailed data table for detail panel - not used!!
    output$tblCountryDetail <- function() {
      if (!is.null(input$locSelect)) {
        df_covid_summary %>% filter(Country == input$locSelect) %>% select(Date, Confirmed, Deaths, Recoveries, Active) %>% 
        kableExtra::kable(format = "html", col.names = NULL, escape = FALSE, table.attr = "id=\"tblCountryDetail\" class=\"table table-condensed\"")
      }
    }
    
    
    # Render output for the country detail data
    output$dtlCountry <- function() {
      fmtLgFig <- function(fig) { format(fig, big.mark = " ") }
      fmtPcg <- function(fig) { sprintf("%1.1f %%", fig*100) }
      chkSign <- function(fig) {
        if (fig > 0) { return(" dtlDeltaPos")}
        if (fig < 0) { return(" dtlDeltaNeg")}
        }
      
      paste0(
        "<div class=\"dtlCountryLabel\">", reacDetailTable()$Country, "</div>",
        "<div class=\"dtlLastUpdated\">", "Last updated: ", format(max(reacDetailTable()$Date), "%Y-%m-%d"), "</div>",
        "<ul class=\"dtlStats\">", 
        "<li>",
        "<div class=\"dtlLabel\">", "Confirmed cases", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Confirmed), "</div>",
        "<div class=\"dtlDelta", chkSign(reacDetailTable()$dConfirmed), "\">", fmtLgFig(reacDetailTable()$dConfirmed), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Recoveries", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Recoveries), "</div>",
        "<div class=\"dtlDelta", chkSign(reacDetailTable()$dRecoveries), "\">", fmtLgFig(reacDetailTable()$dRecoveries), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Deaths", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Deaths), "</div>",
        "<div class=\"dtlDelta", chkSign(reacDetailTable()$dDeaths), "\">", fmtLgFig(reacDetailTable()$dDeaths), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Active cases", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Active), "</div>",
        "<div class=\"dtlDelta", chkSign(reacDetailTable()$dActive), "\">", fmtLgFig(reacDetailTable()$dActive), "</div>",
        "</li></ul>",
        "<ul class=\"dtlStats\">", 
        "<li>",
        "<div class=\"dtlLabel\">", "Population", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Population), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Case Fatality rate", "</div>",
        "<div class=\"dtlValue\">", fmtPcg(reacDetailTable()$rtoDeathsConfirmed), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Case Recovery rate", "</div>",
        "<div class=\"dtlValue\">", fmtPcg(reacDetailTable()$rtoRecoveryConfirmed), "</div>",
        "</li></ul>",
        ""
             )
    }
    
    # Display data for the detail panel
    reacDetailTable <- reactive({
      req(input$tblCountrySelect)
      df_covid_summary_country %>% filter(Country == input$tblCountrySelect) %>% filter(Date == max(Date))
    })

    
    output$ddnPlotComparison <- function() {
      HTML(paste0(
        "<button class=\"btn btn-primary dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" title=\"Additional data for comparison\">", 
        switch(input$dtlPlotComp, "compNone"="None", "compTop10"="Top 5", "pltOutcomes"="Outcomes"),
        "<span class=\"fa fa-stack\"><i class=\"fa fa-caret-down\"></i><i class=\"fa fa-caret-up\"></i></span></button>
        <ul class=\"dropdown-menu\">
        <li><a id=\"ddnPltCompNone\" href=\"#\" onclick=\"Shiny.setInputValue('dtlPlotComp', 'compNone', {priority: 'event'});\">Case Growth</a></li>
        <li><a id=\"ddnPltCompTop10\" href=\"#\" onclick=\"Shiny.setInputValue('dtlPlotComp', 'compTop5', {priority: 'event'});\">Top 5 by confirmed cases</a></li>
        <li><a id=\"ddnPltCompPreSel\" href=\"#\" onclick=\"Shiny.setInputValue('dtlPlotComp', 'compPreSel', {priority: 'event'});\">Spain, Italy, France, Germany, US, United Kingdom, South Korea, Japan</a></li>
        </ul>"
      ))
    }
      
    output$pltProgression <- renderPlotly({ 
      plot_ly(data = plotdata()) %>% add_lines(x = ~Day_Confirmed, y = ~Confirmed)})
    
    
    observeEvent(input$showHistory, { hide("panelRegionSel", anim = TRUE, animType = "slide"); toggle("panelHistory", anim = TRUE, animType = "slide") })
    observeEvent(input$showRegionSel, { hide("panelHistory"); toggle("panelRegionSel") })
    
    observeEvent(input$zoomUserLocation, {
      req(input$geoLocEnabled)
      leafletProxy("map") %>% setView(lat = input$geoLocLat, long = input$geoLocLong, zoom = 7)
    })
    
    # Basic country selection with zoom
    observeEvent(input$locSelect, ignoreInit = TRUE, {
      req(input$locSelect)
      lat_long <- loc_list %>% ungroup() %>% filter((Country == input$locSelect & is.na(State) )| State == input$locSelect) %>% select(Lat, Long)
      leafletProxy("map") %>% setView(lat = lat_long$Lat, lng = lat_long$Long, zoom = 7)
    })
    
    
    # Capture the click event for the map symbols
    observeEvent(input$map_marker_click, { #print(input$map_marker_click) 
      })
    
    
    # Reset map to center and initial zoom level
    observeEvent(input$resetMap, {
        hide("panelRegionSel")
        leafletProxy("map") %>% setView(lat = 20, lng = 0, zoom = 3.2)
    })
  
    hide("loadingmsg", anim = TRUE, animType = "fade")

}

# Run the application 
shinyApp(ui = ui, server = server)

