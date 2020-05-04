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
    useShinyjs(),
    #tags$script(src = "js/getloc.js"),
    tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }

   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
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
    # Change to CSS from inline
    absolutePanel(top = 12, left = 10, height = 110, HTML("<img id=\"siteLogo\" src=\"COVID-19-banner.png\"></img>")),
    absolutePanel(top = 125, left = 10, bottom = 0, width = 305, class = "panel panel-default", style = "display: block;",
                  div(style = "display: block; position: relative; height: 2%;",
                      tabsetPanel(id = "tblSelect", type = "pills", tabPanel("Confirmed"), tabPanel("Recoveries"), tabPanel("Deaths"), tabPanel("Active")), 
                                  div(id = "ddnSortCriteria", class = "dropdown", dropdownInput(NULL,NULL))),  
                  div(style = "display: block; position: relative; height: calc(98% - 2px); overflow-y: auto; width:100%;", 
                      tableOutput("tblStats"))),
    
    absolutePanel(bottom = 6, right = 265, width = "235px", height = "10px", fixed = TRUE, div(id = "copyrightmsg", "Map data | (C) 2020 Johns Hopkins University")),
    
    # Additional visualisations
    #absolutePanel(top = 17, left = 42, height = 30, actionButton("showTrends", label = "Trends"), class = "btn-custom", title = "Trends"),
    
    # Search bar
    absolutePanel(top = 12, left = tbr_add_left(), height = 30, id = "panelSearch", 
                  selectizeInput("locSelect", width = 250, choices = loc_list2, selected = NULL, options = list(placeholder = "Search locations"),  label = NULL, multiple = FALSE)),
    absolutePanel(top = 12, left = tbr_add_left(251), height = 30, actionButton("resetMap", label = NULL, icon = icon("globe-americas"), class = "btn-custom", title = "Reset map view")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("getLocation", label = NULL, icon = icon("location-arrow"), class = "btn-custom", title = "Zoom to your location")),
    absolutePanel(top = 12, left = tbr_add_left(31, 31), width = 30, height = 30, actionButton("showHistory", label = NULL, icon = icon("history"), class = "btn-custom", title = "Historical data")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("showOptions", label = NULL, icon = icon("sliders-h"), class = "btn-custom", title = "Display Options")),
    absolutePanel(top = 12, left = tbr_add_left(31), width = 30, height = 30, actionButton("githubLink", label = NULL, icon = icon("github"), class = "btn-custom", title = "Visit project page on GitHub", onclick = "window.open('https://altryx.dev/rcovid2019', '_blank')")),
    
    # Additional panels for map controls
    hidden(absolutePanel(top = 80, left = 45, height = "80px", id = "panelHistory", class = "panel panel-default", 
                         sliderInput("MaxDate", label = NULL, min = min(df_covid$Date), max = max(df_covid$Date), step = 3, value = max(df_covid$Date), timeFormat = "%d %b", animate = animationOptions(interval = 1500, loop = FALSE)))),
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
    
    hidden(absolutePanel(id = "dlgDetailInfo", class = "panel panel-default", 
                         div(id = "btnDlgClose", class = "fa fa-window-close", title = "Close window", onclick = "shinyjs.hide('dlgDetailInfo');"),
                         div(id = "dlgDetailTop", htmlOutput("dtlCountry")),
                             #div(id = "dlgCountryDetail", style = "display: inline-block; position: relative; width: 50%; height: 50%;", ),
                         div(id = "dlgCountryPlots", style = "display: block; position: relative; width: 100%;",
                             htmlOutput("dropdownInput2"),
                             plotlyOutput("plt_country_case_growth50", width = "100%", height = "90%")), #style = "display: inline-block; position: relative;"
                             #),
                         div(style = "display: block; position: relative; height: 25%;", radioButtons("plt_growth50_comparison", label = "Compare to", choices = c("World top 5", "Neighbors", "Continent top 5", "None"))))))


# Server ------------------------------------------------------------------


# Define server logic
server <- function(input, output, session) {
  
    # Initialize inputs that don't have associated input controls - remove locSelect as it does nothing
    runjs("Shiny.setInputValue('sortDelta', false, {priority: 'event'}); 
          Shiny.setInputValue('tblCountrySelect', '', {priority: 'event'}); 
          Shiny.setInputValue('locSelect', null, {priority: 'event'});
          Shiny.setInputValue('selectPlot', 'ddnPlotProgression50', {priority: 'event'});
          ")
    
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
        leaflet(options = leafletOptions(zoomSnap = 0.1, minZoom = 3.2, zoomControl = FALSE, worldCopyJump = TRUE)) %>% 
            addProviderTiles(map_providers[1]) %>% 
            #setMaxBounds(-180, -70, 180, 80) %>%
            setView(zoom = 3.2, lat = 20, lng = 0) %>%
                    addLayersControl(position = "bottomright",
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
            addCircleMarkers(data = { covid_daily_country_summary() %>% filter(Active > 0) },  lat = ~Lat, lng = ~Long, radius = ~(Active)^(1/4), weight = 1, color = rgb(1,0.7,0), fillOpacity = 0.3, group = "Active cases",
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
     observeEvent(input$sortDelta, {
       #if (!is.null(input$sortDelta)) {
         if (input$sortDelta) {
           removeClass(id = "ddnAbsoluteVal", class = "ddnSelected")
           addClass(id = "ddnChanges", class = "ddnSelected")
         } else {
           addClass(id = "ddnAbsoluteVal", class = "ddnSelected")
           removeClass(id = "ddnChanges", class = "ddnSelected")
         }
       #}
       })
    
    # Generate Stats table
    output$tblStats <- function() {
      if (!is.null(input$sortDelta)) {
      covid_daily_country_summary() %>%
        ungroup() %>%
        arrange(desc(!! sym(ifelse((is.null(input$sortDelta) | input$sortDelta), paste0("d", input$tblSelect), input$tblSelect)))) %>% 
        mutate(Rank = cell_spec(row_number(), color = "black", background = "gray"),
               (!! sym(input$tblSelect)) := paste0(format(!! sym(input$tblSelect), big.mark = " "), 
                                                   "<br>", 
                                                   case_when(input$tblSelect != "Active" ~ "<span class=\"table-delta\">&Delta; ", 
                                                             ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) > 0) ~ "<span class=\"table-delta-pos\">+ ",
                                                             ((input$tblSelect == "Active") & (!! sym(paste0("d", input$tblSelect))) < 0) ~ "<span class=\"table-delta-neg\">- ",
                                                             TRUE ~ "<span class=\"table-delta\">"), 
                                                   format(abs(!! sym(paste0("d", input$tblSelect))), big.mark = " "), 
                                                   "</span>"),
               Country = paste0("<a class=\"fas fa-map-marker-alt table-sym\" href=\"#\" onclick=\"Shiny.setInputValue('locSelect', '", Country, "', {priority: 'event'});\"></a>", " ", 
                                paste0("<a class=\"table-link\" onclick=\"Shiny.setInputValue('tblCountrySelect', '", Country, "', {priority: 'event'}); shinyjs.show('dlgDetailInfo');\">", Country, "</a>")) ) %>%
        select(Rank, Country, input$tblSelect)  %>%
        kableExtra::kable(format = "html", col.names = NULL, escape = FALSE, table.attr = "id=\"tblStats\" class=\"table table-condensed\"")
      
      
      
      
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
    
    
    # Plot of daily case growth vs. days since 50 cases were recorded
    
    lst_comp_countries_plt <- c("Spain", "Italy", "France", "Germany", "US", "United Kingdom", "Canada", "South Korea", "Japan")
    #df_covid_summary_country %>% filter(Date <= input$MaxDate) %>% filter(Date == max(Date)) %>% filter(Confirmed > 0) 
    
    plotdata <- reactive({ 
      req(input$locSelect)
      df_covid_summary_country %>%
        ungroup() %>%
        filter(Country %in% c(input$locSelect, lst_comp_countries_plt)) %>% 
        filter(Confirmed >= 50) %>% 
        select(Country, Date, Confirmed) %>% 
        group_by(Country) %>% 
        arrange(Date) %>%
        mutate(Day_Confirmed = row_number()) 
      #%>% ungroup() %>% arrange(Day_Confirmed)
      })

    # remove - not necessary    
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['country']])) {
        updateSelectizeInput(session, inputId = "tblCountrySelect", selected = query[['country']])
      }})
    
    # Detailed data table for detail panel
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
      
      paste0(
        "<div class=\"dtlCountryLabel\">", reacDetailTable()$Country, "</div>",
        "<div class=\"dtlLastUpdated\">", "Last updated: ", format(max(reacDetailTable()$Date), "%Y-%m-%d"), "</div>",
        "<ul class=\"dtlStats\">", 
        "<li>",
        "<div class=\"dtlLabel\">", "Confirmed cases", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Confirmed), "</div>",
        "<div class=\"dtlDelta\">", fmtLgFig(reacDetailTable()$dConfirmed), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Recoveries", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Recoveries), "</div>",
        "<div class=\"dtlDelta\">", fmtLgFig(reacDetailTable()$dRecoveries), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Deaths", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Deaths), "</div>",
        "<div class=\"dtlDelta\">", fmtLgFig(reacDetailTable()$dDeaths), "</div>",
        "</li><li>",
        "<div class=\"dtlLabel\">", "Active cases", "</div>",
        "<div class=\"dtlValue\">", fmtLgFig(reacDetailTable()$Active), "</div>",
        "<div class=\"dtlDelta\">", fmtLgFig(reacDetailTable()$dActive), "</div>",
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
    
    
    # Dropdown generator for detail info plots - rework
    output$dropdownInput2 <- function() {
      HTML(paste0(
        "<button class=\"btn btn-primary dropdown-toggle\" type=\"button\" data-toggle=\"dropdown\" title=\"Select a plot to display\">", switch(input$selectPlot, "ddnPlotProgression"="Progression", "ddnPlotGrowth50"="Growth"),
        "<span class=\"fa fa-stack\"><i class=\"fa fa-caret-down\"></i><i class=\"fa fa-caret-up\"></i></span></button>
    <ul class=\"dropdown-menu\">
    <li><a id=\"ddnPlotGrowth50\" href=\"#\" onclick=\"Shiny.setInputValue('selectPlot', 'ddnPlotGrowth50', {priority: 'event'});\">Case Growth</a></li>
    <li><a id=\"ddnPlotProgression\" href=\"#\" onclick=\"Shiny.setInputValue('selectPlot', 'ddnPlotProgression', {priority: 'event'});\">Progression</a></li>
    </ul>"
      ))
    }
    
    
    
    # Rework variable and function names
    output$plt_country_case_growth50 <- renderPlotly({
      
      plot_ly(data = plotdata()) %>% add_lines(x = ~Day_Confirmed, y = ~Confirmed, color = ~Country) %>%
        layout(#legend = list(orientation = "h"), 
               yaxis = list(type = "log"), 
               xaxis = list(title = "Days since 50 confirmed cases"),
               font = list(family = "Arial", size = 12))
    })
    
    # Todo
    hide_all_elements <- function() {}
    
    observeEvent(input$showHistory, { hide("panelRegionSel"); toggle("panelHistory") })
    observeEvent(input$showRegionSel, { hide("panelHistory"); toggle("panelRegionSel") })
    
    # Basic country selection with zoom
    observeEvent(input$locSelect, ignoreInit = TRUE, {
      req(input$locSelect)
      lat_long <- loc_list %>% ungroup() %>% filter((Country == input$locSelect & is.na(State) )| State == input$locSelect) %>% select(Lat, Long)
      leafletProxy("map") %>% setView(lat = lat_long$Lat, lng = lat_long$Long, zoom = 7)
    })
    
    observeEvent(input$map_zoom, {
      if (input$map_zoom <= 4) { summary_table = covid_daily_country_summary() } else { summary_table = covid_summary_table() }
      
        leafletProxy("map", data = summary_table) %>%
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
          addCircleMarkers(data = { covid_daily_country_summary() %>% filter(Active > 0) },  lat = ~Lat, lng = ~Long, radius = ~(Active)^(1/4), weight = 1, color = rgb(1,0.7,0), fillOpacity = 0.3, group = "Active cases",
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
    
    # Capture the click event for the map symbols
    observeEvent(input$map_marker_click, { print(input$map_marker_click) })
    
    
    # Reset map to center and initial zoom level
    observeEvent(input$resetMap, {
        hide("panelRegionSel")
        leafletProxy("map") %>% setView(lat = 20, lng = 0, zoom = 3.2)
        #     output$map <- leaflet("map") %>% setView(zoom = 3.2, lat = 0, lng = 0)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

