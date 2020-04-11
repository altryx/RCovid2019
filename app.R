
library(shiny)
library(shinyjs)
library(leaflet)
library(readr)
library(lubridate)
library(purrr)
library(dplyr)
library(tidyr)
library(DT)
library(magrittr)
library(plotly)

# TO DO
# Format the titles at the bottom, figure out spacing
# Create 
# Format the top 10 countries table - remove all controls
# Add dialog to filter data (min cases, etc.)
# Activate interaction to filter map data
# Set zoom boundaries and map boundaries on load
# Calculate growth delta for different countries
# Calculate/show graphs for relative growth comparison
# Calculate ratios death/confirmed cases and recoveries/confirmed cases
# Filter data to remove recoveries and assign into appropriate position
# DONE Add check to prevent re-download of data
# DONE Define column definitions for read_csv to remove warnings 
# DONE Remove readr:: from parse_date
# Remove styling from tags$style and into a separate CSS file
# Specify columns to join to remove warnings
# Add GitHub link for the source code (button)


# Misc
ptn_column_date <- "(.+)/(.+)/(.+)"
# US Latitude and Longitude for summary calculations from individual county/state data
US_Lat <- 37.090200; US_Long <- -95.712900
# Data folder path and files
folder_prefix <- "./data/"
url_base <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/"
data_files <- c("raw_global_confirmed"="time_series_covid19_confirmed_global.csv", "raw_global_deaths"="time_series_covid19_deaths_global.csv", "raw_global_recoveries"="time_series_covid19_recovered_global.csv", "raw_US_deaths"="time_series_covid19_deaths_US.csv", "raw_US_confirmed"="time_series_covid19_confirmed_US.csv")
url_list <- paste0(url_base, data_files) %>% set_names(value = names(data_files))
file_list <- paste0(folder_prefix, data_files) %>% set_names(value = names(data_files))

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

# Get Data from CSSE GitHub site

data_load()

# 
# 
# raw_global_confirmed <- suppressMessages(read_csv(url_global_confirmed_cases))
# raw_global_recoveries <- suppressMessages(read_csv(url_global_recoveries))
# raw_global_deaths <- suppressMessages(read_csv(url_global_deaths))
# raw_US_confirmed <- suppressMessages(read_csv(url_US_confirmed_cases))
# raw_US_deaths <- suppressMessages(read_csv(url_US_deaths))



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
df_global_confirmed <- bind_rows(df_global_confirmed, df_US_confirmed_perstate %>% group_by(Country, Date) %>% summarize(Confirmed = sum(Confirmed), Lat = mean(Lat), Long = mean(Long)))
# Global deaths
df_global_deaths <- bind_rows(df_global_deaths, df_US_deaths_perstate %>% group_by(Country, Date) %>% summarize(Deaths = sum(Deaths), Lat = mean(Lat), Long = mean(Long)))
# Global recoveries
df_global_recoveries <- raw_global_recoveries %>% select("State"="Province/State", "Country"="Country/Region", "Lat", "Long", matches(ptn_column_date)) %>%
    pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Recoveries") %>%
    mutate(Date=readr::parse_date(`Date`, format = "%m/%d/%y")) %>% 
    group_by (Country, State, Date, Lat, Long) %>% summarize(Recoveries = sum(Recoveries))

# Latest data, per Country/State, with deltas
df_covid <- left_join(df_global_confirmed, df_global_deaths) %>% left_join(df_global_recoveries) 

df_covid_summary <- df_covid %>% group_by(Country, State, Lat, Long) %>% summarize(Date = max(Date), Recoveries = max(Recoveries), Deaths = max(Deaths), Confirmed = max(Confirmed))

# Summary of latest data only, per country, with deltas
df_covid_summary_extended <- df_covid %>% group_by(Country, Date) %>% summarize(Confirmed = max(Confirmed), Deaths = max(Deaths), Recoveries = max(Recoveries)) %>% 
    group_by(Country) %>% arrange(Country, desc(Date)) %>% top_n(n = 2, wt = Date) %>% group_by(Country) %$% 
    left_join( (.) %>% slice(1), (.) %>% slice(2) %>% select(Country, lstConfirmed = Confirmed, lstDeaths = Deaths, lstRecoveries = Recoveries), by = "Country") %>% 
    mutate(Active = Confirmed - Recoveries, dConfirmed = Confirmed - lstConfirmed, dDeaths = Deaths - lstDeaths, dRecoveries = Recoveries - lstRecoveries, rtoDeathsConfirmed = Deaths/Confirmed)

df_covid_summary_extended %>% arrange(desc(dConfirmed))
plot_ly(data = df_covid_summary_extended %>% arrange(desc(dConfirmed)) %>% filter(dConfirmed > 100)) %>% add_bars(x = ~reorder(Country, desc(dConfirmed)), y = ~dConfirmed)

# df_US_confirmed <- df_US_confirmed %>% mutate(Country = "US") %>% select("State"="Province_State", "Country", "Lat", "Long"="Long_", matches(ptn_column_date)) %>% 
#     pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>%
#     group_by(Country, State, Date) %>% summarize(Confirmed = sum(Confirmed))
# 
# df_US_deaths <- df_US_deaths %>% mutate(Country = "USA") %>% select("State"="Province_State", "Country", "Lat", "Long"="Long_", matches(ptn_column_date))
# 
# df_covid <- df_global_confirmed %>% pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Confirmed") %>% 
#     left_join(bind_rows(df_global_deaths, df_US_deaths) %>% pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Deaths")) %>%
#     left_join(df_global_recoveries %>% pivot_longer(cols = matches(ptn_column_date), names_to = "Date", values_to = "Recoveries")) %>%
#     mutate(Date=readr::parse_date(`Date`, format="%m/%d/%y"))
# 
# df_covid_summary <- df_covid %>% group_by(Country, State, Lat, Long) %>% summarise(Date=max(Date), Confirmed = max(Confirmed), Deaths=max(Deaths), Recoveries=max(Recoveries))
# 
# # Summary data without map details
# df_covid_summary <- df_covid %>% select(Country, State, Date, Confirmed, Deaths, Recoveries) %>% group_by(Country, State) %>% top_n(n = 1, wt = Date) %>% 
#     left_join(df_covid %>% select(Country, State, Date, lstConfirmed = Confirmed, lstDeaths = Deaths, lstRecoveries = Recoveries) %>% group_by(Country, State) %>% arrange(desc(Date)) %>% top_n(n=2, wt=Date) %>% slice(2) %>% select(-Date))
#     group_by(Country, State, repDate=max(Date)) %>% summarize_at(c("Confirmed", "Deaths", "Recoveries"), list(min, max)) 
#     summarize(Confirmed=Confirmed[Date==max(Date) & Country == Country & State == State], lastConfirmed=list(Confirmed[Date==min(Date) & Country == Country & State == State]), Deaths = list(Deaths[Date==max(Date)]), lastDeaths = list(Deaths[Date==min(Date)])) %>%
#     unnest_auto() #cols =c(Confirmed, lastConfirmed, Deaths, lastDeaths)) %>% 
#     #mutate(dConfirmed=Confirmed-lastConfirmed)
# df_prev_date <- df_covid %>% select(Date) %>% distinct() %>% arrange(desc(Date)) %>% slice(2) %>% pull()
# df_covid_current <- df_covid %>% group_by(Country, State, Lat, Long) %>% filter(Date == max(Date)) %>% summarise(Date = max(Date), Confirmed = max(Confirmed), Deaths = max(Deaths), Recoveries = max(Recoveries))
# df_covid_prev <- df_covid %>% group_by(Country, State, Lat, Long) %>% distinct() %>% filter(Date == df_prev_date) %>% summarise(Prev_Confirmed = max(Confirmed), Prev_Deaths = max(Deaths), Prev_Recoveries = max(Recoveries))
# 
# df_covid_summary <- df_covid_current %>% left_join(df_covid_prev) 

territory_list_summary <- unique(df_covid_summary$Country)
territory_list <- paste0(df_covid_summary$Country, ifelse(is.na(df_covid_summary$State),"", paste0(", ", df_covid_summary$State)))

# Define UI 

ui <- bootstrapPage(
    useShinyjs(),
    tags$head(includeCSS("style.css")),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height="100%"),
    absolutePanel(top = 80, left = 12, width = "30px", height = "30px", actionButton("showHistory", label = NULL, icon = icon("history"), class = "btn-custom")),
    absolutePanel(top = 120, left = 12, width = "30px", height = "30px", actionButton("showRegionSel", label = NULL, icon = icon("search"), class = "btn-custom")),
    hidden(absolutePanel(top = 80, left = 45, height = "80px", id = "panelHistory", class = "panel panel-default", 
                         sliderInput("MaxDate", label = NULL, min = min(df_covid$Date), max = max(df_covid$Date), step = 3, value = max(df_covid$Date), timeFormat = "%d %b", animate = animationOptions(interval = 2500, loop = FALSE)))),
    hidden(absolutePanel(top = 120, left = 45, height = "40px", id = "panelRegionSel", class = "panel panel-default", 
                        selectInput("territory", label = NULL, choices = territory_list_summary))),
    absolutePanel(bottom = 20, right = 10, id = "controls", class = "panel panel-default", DTOutput("top10_table")),
    absolutePanel(top = 10, right = 10, width = "250px", height = "50px", htmlOutput("total_cases", class = "panel summarydisplay confirmedcases")),
    absolutePanel(top = 70, right = 10, width = "250px", height = "50px", htmlOutput("total_deaths", class = "panel summarydisplay deaths")),
    absolutePanel(top = 130, right = 10, width = "250px", height = "50px", htmlOutput("total_recoveries", class = "panel summarydisplay recoveries")),
    absolutePanel(top = 190, right = 10, width = "250px", height = "25px", htmlOutput("lastUpdated", class = "panel summarydisplay lastupdated"))
)





# Define server logic
server <- function(input, output, session) {
    
    fmt_num <- function(num) { format(num, nsmall = 0, big.mark = ",") }
    
    fmt_btm_titles <- function(text, var) { sprintf("%s<div class=\"summarydisplay summaryfigure\">%s</div>", ifelse(is.null(text), "", paste0(text, "<br>")), format(var, nsmall = 0, big.mark = " ")) } 
    
    fmt_map_popup <- function(...) { sprintf("<b>%s</b><br>Confirmed cases: %s<br>Deaths: %s<br>Recoveries: %s<br>", ...) }
    
    covid_summary_table <- reactive({
        df_covid %>% filter(Date <= input$MaxDate) %>% group_by(Country, State, Lat, Long) %>% summarize(Date = max(Date), Recoveries = max(Recoveries), Deaths = max(Deaths), Confirmed = max(Confirmed))
    })
    
    output$map <- renderLeaflet({
        leaflet(df_covid_summary) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% setView(zoom = 3, lat = 0, lng = 0) %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(Confirmed)^(1/4), weight = 1, color = rgb(1,0,0), fillOpacity = 0.3, 
                       popup = ~fmt_map_popup(ifelse(is.na(State), Country, paste(State, Country, sep = ", ")), format(Confirmed, big.mark = " "), format(Deaths, big.mark = " "), ifelse(is.na(Recoveries), "N/A", format(Recoveries, big.mark = " "))))
                       #popup = ~paste("Country:", Country, ifelse(is.na(State), "", paste("<br>Province/State:", State)), "<br>Confirmed cases:", Confirmed, "<br>Deaths:", Deaths, "<br>Recoveries:", Recoveries, "<br>"))
        
    })
    

    output$total_cases <- renderText({ fmt_btm_titles("Confirmed Cases", sum(covid_summary_table()$Confirmed, na.rm = TRUE)) }) 
    output$total_deaths <- renderText({ fmt_btm_titles("Deaths", sum(covid_summary_table()$Deaths, na.rm = TRUE)) })
    output$total_recoveries <- renderText({ fmt_btm_titles("Recoveries", sum(covid_summary_table()$Recoveries, na.rm = TRUE)) }) 
    output$lastUpdated <- renderText({ fmt_btm_titles(NULL, input$MaxDate) })
    
    # Top 10 countries by Confirmed Cases
    output$top10_table <- renderDT(
        isolate({covid_summary_table()}) %>% group_by(Country) %>% summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recoveries = sum(Recoveries)) %>% arrange(desc(Confirmed)) %>% top_n(n = 10, wt = Confirmed), 
        options = list(dom = "t"), class="compact") #%>% formatStyle( 0, target= 'row', color = 'black', backgroundColor = 'yellow', fontWeight ='bold', lineHeight='70%')  # arrange(desc(Confirmed)))

    # Observer for map layer changes
    observe({
        leafletProxy("map", data = covid_summary_table()) %>%
            clearMarkers() %>%
            addCircleMarkers(lat = ~Lat, lng = ~Long, radius = ~(Confirmed)^(1/4), weight = 1, color = rgb(1,0,0), fillOpacity = 0.3, 
                             popup = ~fmt_map_popup(ifelse(is.na(State), Country, paste(State, Country, sep = ", ")), format(Confirmed, big.mark = " "), format(Deaths, big.mark = " "), ifelse(is.na(Recoveries), "N/A", format(Recoveries, big.mark = " "))))
    })
    
    # Observer for table updates
    observe({
        replaceData(dataTableProxy("top10_table"), data = {covid_summary_table() %>% group_by(Country) %>% summarize(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recoveries = sum(Recoveries)) %>% arrange(desc(Confirmed)) %>% top_n(n = 10, wt = Confirmed)})
    })
    
    observeEvent(input$showHistory, { hide("panelRegionSel"); toggle("panelHistory")} )
    observeEvent(input$showRegionSel, { hide("panelHistory"); toggle("panelRegionSel")})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
