#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(rjson)
library(gt)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "styles.css",
                div(
                    h4("COVID-19 and Nursing Homes")
                ),
                div(id = "description",width=pct(100),
                    p("The Nursing Home COVID-19 Public File includes data reported by nursing homes to the CDC’s National Health Safety Network (NHSN) system COVID-19 Long Term Care Facility Module, including Resident Impact, Facility Capacity, Staff & Personnel, and Supplies & Personal Protective Equipment, and Ventilator Capacity and Supplies Data Elements. For more information, click "),
                    a(href = "https://cms-staging.demo.socrata.com/Special-Programs-Initiatives/COVID-19-Nursing-Home-Dataset/m7e6-jpc9","here")
                ),
    column(width = 3,
           wellPanel(
               #selectInput("finder","Find Grandma",selectize=TRUE,choices = c("a","b","c"))
               selectizeInput("finder","Find Grandma",choices = c("a","b","c"),
                                  options = list(
                                      placeholder = 'Search for a nursing home',
                                      onInitialize = I('function() { this.setValue(""); }')
                                  )
                              )
               # searchInput(
               #     inputId = "finder", label = "Find Grandma",
               #     placeholder = "Search for a nursing home",
               #     btnSearch = icon("search"),
               #     width = "100%"
               # ),
           ),
           wellPanel("Cases", gt_output(outputId = "totalCasesTable")),
           wellPanel("Deaths", gt_output(outputId = "totalDeathsTable")),
           wellPanel("Resident Infections", gt_output(outputId = "residentInfectionsTable")),
           wellPanel("Staff Infections", gt_output(outputId = "staffInfectionsTable")),
           wellPanel("Resident Deaths", gt_output(outputId = "residentDeathsTable")),
           wellPanel("Staff Deaths", gt_output(outputId = "staffDeathsTable"))
           ),
    column(width = 7, 
           wellPanel(
               plotlyOutput(outputId = "map")
               ),
           div(
               column(width = 6,
                      wellPanel(
                          plotlyOutput(outputId = "totalCasesTimeline")
                          )),
               column(width = 6,
                      wellPanel(
                          plotlyOutput(outputId = "totalDeathsTimeline")
                      ))
           )
           ),
    column(width = 2,
           wellPanel("Homes reporting in the US", gt_output(outputId = "reportingUsTotal")),
           wellPanel("Homes reporting by state", gt_output(outputId = "reportingByState"))
           )
    )


get_gt_table <- function(summary_data,filterString){
    gt <- summary_data %>% filter(grepl(filterString,key)) %>% standard_gt() %>%
        summary_rows(columns = vars(value), fns = list(total = "sum"), decimals = 0) %>%
        tab_options(column_labels.hidden = TRUE) 
    
    return(gt)
}

get_residents_and_staff_table <- function(residents,staff){
    print(residents)
    print(staff)
    t <- tibble(c("Residents","Staff","Total"),c(residents,staff,(residents+staff)))
    gt <- t %>% standard_gt() %>% tab_options(column_labels.hidden = TRUE) 
    gt
}

get_home_data_gt_table <- function(homes_with_incidents_total,homes_with_incidents_this_week){
    t <- tibble(c("# Homes","#Homes this week"),c(homes_with_incidents_total,homes_with_incidents_this_week))
    gt <- t %>% standard_gt() %>% tab_options(column_labels.hidden = TRUE) 
    gt
}

standard_gt <- function(df){
    gt <- gt(df) %>%
        tab_options(table.width = pct(100),
                    table.font.size = px(10),
                    data_row.padding = px(4)) %>%
        opt_row_striping(row_striping = TRUE)
    gt
}

# Define server logic required to draw a histogram
server <- function(session,input, output) {
    
    # Get the data
    nursing_home_data <- reactive({
        nursing_data_url <-"https://cms-staging.demo.socrata.com/Special-Programs-Initiatives/COVID-19-Nursing-Home-Dataset/m7e6-jpc9"
        
        lat_regex <-  "-?[0-9]+\\.[0-9]+"
        lon_regex <- "(?<=\\s)[0-9]+\\.[0-9]+"
        
        cached_file_path <- "cached.csv"
        
        if(file.exists(cached_file_path))
            data <- read.csv(cached_file_path, stringsAsFactors = FALSE) %>%
                mutate(date = as.Date(date,"%Y-%m-%d"))
            
        else {
            email = Sys.getenv("SOCRATA_USER")
            password = Sys.getenv("SOCRATA_PASSWORD")
          
            raw_data <-read.socrata(nursing_data_url, email = email, password = password)
            
            data <- raw_data %>%
                rename(date = week_ending,
                       provider_name = provider_name,
                       state = provider_state,
                       location = geocoded_column) %>%
                select(date,provider_name,location,
                       residents_weekly_admissions,
                       residents_total_admissions, 
                       residents_weekly_confirmed,
                       residents_total_confirmed,
                       residents_weekly_suspected,
                       residents_total_suspected,
                       residents_weekly_all_deaths,
                       residents_total_all_deaths,
                       staff_weekly_confirmed_covid,
                       staff_total_confirmed_covid,
                       staff_weekly_suspected_covid,
                       staff_total_suspected_covid,
                       staff_weekly_covid_19_deaths,
                       staff_total_covid_19_deaths,
                       state,
                       provider_zip_code) %>%
                mutate(lat = as.numeric(str_extract(location,lat_regex))) %>%
                mutate(lon = as.numeric(str_extract(location,lon_regex)))
            
            #This probably should cache the raw input, not the post-processed stuff...
            write.csv(data, cached_file_path)
        }
        updateSelectizeInput(session,inputId = "finder",choices = c(sort(unique(data$provider_name))),selected = NULL)
        
        data
    })
    
    summary_stats <- reactive({
        #Add up all off the cases, deaths, etc for the US
        summary <- nursing_home_data() %>%
            summarize_if(is.numeric,sum,na.rm = TRUE) 
#            gather()
    })
    
    state_summary <- reactive({
      summary <- nursing_home_data() %>%
          group_by(state) %>%
          summarize_all(~ (sum(!is.na(.)))) %>%
          rename(total_homes = provider_name,
                 homes_reporting = residents_total_admissions) %>%
          mutate(percentage_reporting = homes_reporting / total_homes) %>%
          select(state,total_homes,homes_reporting,percentage_reporting)
      
      summary
    })
    
    home_summary <- reactive({
        summary <- nursing_home_data() %>%
            group_by(date) %>%
            summarize_all(~ (sum(. > 0, na.rm = TRUE))) %>% 
            top_n(1,date)
        summary
    })
    
    timeline_stats <- reactive({
        timeline <- nursing_home_data() %>%
            select(date,residents_total_confirmed,staff_total_confirmed_covid,residents_total_all_deaths,staff_total_covid_19_deaths) %>% 
            group_by(date) %>% 
            summarize_all(sum, na.rm = TRUE)
        
        timeline
    })
    
    latest_report <- reactive({
        latest <- nursing_home_data() %>%
            filter(!is.na(residents_total_confirmed)) %>%
            group_by(provider_name) %>%
            top_n(1,date)
        latest
    })
    
    output$totalCasesTable <- render_gt({
        summary <- summary_stats()
        gt <- get_residents_and_staff_table(summary$residents_total_confirmed,summary$staff_total_confirmed_covid)
        
        #gt <- get_gt_table(summary_stats(),"*total_confirmed*")
        gt
    })
    
    output$totalDeathsTable <- render_gt({
        summary <- summary_stats()
        gt <- get_residents_and_staff_table(summary$residents_total_all_deaths,summary$staff_total_covid_19_deaths)
        #gt <- get_gt_table(summary_stats(),"*total.*deaths*")
        gt 
    })
    
    output$residentInfectionsTable <- render_gt({
        home <- home_summary()
        gt <- get_home_data_gt_table(home$residents_total_confirmed,home$residents_weekly_confirmed)
        gt
    })
    
    output$staffInfectionsTable <- render_gt({
        home <- home_summary()
        gt <- get_home_data_gt_table(home$staff_total_confirmed_covid,home$staff_weekly_confirmed_covid)
        gt
    })
    
    output$residentDeathsTable <- render_gt({
        home <- home_summary()
        gt <- get_home_data_gt_table(home$residents_total_all_deaths,home$residents_weekly_all_deaths)
        gt
    })
    
    output$staffDeathsTable <- render_gt({
        home <- home_summary()
        gt <- get_home_data_gt_table(home$staff_total_covid_19_deaths,home$staff_weekly_covid_19_deaths)
        gt
    })
    
    output$reportingUsTotal <- render_gt({
        
        states <- state_summary()
        total_homes_reporting <- sum(states$homes_reporting)
        total_homes <- sum(states$total_homes)
        t <- tibble("US", total_homes_reporting / total_homes)
        gt <- t %>% standard_gt() %>% tab_options(column_labels.hidden = TRUE) 
        # # %>% fmt_percent(
        # #     columns = vars(percentage_reporting),
        # #     decimals = 1
        # )
        gt
    })
    
    output$reportingByState <- render_gt({
        gt <- state_summary() %>% select(state,percentage_reporting) %>% standard_gt() %>% tab_options(column_labels.hidden = TRUE) %>% fmt_percent(
            columns = vars(percentage_reporting),
            decimals = 1
        )
        gt
    })
    
    output$totalCasesTimeline <- renderPlotly({
        plt <- plot_ly(data = timeline_stats()) %>% 
            add_trace(x = ~date, y=~residents_total_confirmed, mode = "lines", name = "Residents") %>% 
            add_trace(x = ~date, y=~staff_total_confirmed_covid, mode = "lines",name = "Staff") %>% 
            layout(legend = list(x = 0.1, y = 1.0))
    })
    
    output$totalDeathsTimeline <- renderPlotly({
        plt <- plot_ly(data = timeline_stats()) %>% 
            add_trace(x = ~date, y=~residents_total_all_deaths, mode = "lines", name = "Residents") %>% 
            add_trace(x = ~date, y=~staff_total_covid_19_deaths, mode = "lines",name = "Staff") %>% 
            layout(legend = list(x = 0.1, y = 1.0))
    })

    output$map <- renderPlotly({
        
        df_zipToFips <- read.csv("zip-fips.csv",stringsAsFactors = FALSE,colClasses=c("STCOUNTYFP"="character")) %>%
            rename(zip = `ZIP`,
                   fips = `STCOUNTYFP`,
                   county = `COUNTYNAME`) %>%
            mutate(fips = as.numeric(fips)) %>%
            #Take the top county when the zip cuts through multiple counties
            group_by(zip) %>%
            top_n(1,fips) %>%
            select(zip,fips,county)
        
        # df_zipToCountyFips <- read.csv("zip-county-fips.csv",stringsAsFactors = FALSE) %>%
        #     rename(zip = `ZIP`,
        #            fips = `COUNTY`) %>%
        #     #Take the top county when the zip cuts through multiple counties
        #     group_by(zip) %>%
        #     top_n(1,fips) %>%
        #     select(zip,fips)
        
        df_latest_report <- latest_report()
    
        #Group by fips to help the map render faster
        df_data_with_fips <- df_latest_report %>%
            left_join(df_zipToFips, by = c("provider_zip_code" = "zip")) %>%
            group_by(fips,county) %>%
            summarize(confirmed = sum(residents_total_confirmed,rm.na=TRUE))
        
        
        url <- 'https://raw.githubusercontent.com/plotly/dash-opioid-epidemic-demo/master/us-counties.json'
            #'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
        counties <- rjson::fromJSON(file=url)
#        url2<- "https://raw.githubusercontent.com/plotly/datasets/master/fips-unemp-16.csv"
#        df <- read.csv(url2, colClasses=c(fips="character"))
        m <- list(
            l = 0,
            r = 0,
            b = 0,
            t = 0,
            pad = 4,
            autoexpand = TRUE
        )
        # 
        # g <- list(
        #     scope = 'usa',
        #     projection = list(type = 'albers usa'),
        #     showlakes = TRUE,
        #     lakecolor = toRGB('white')
        # )
        
        
        fig <- plot_ly()
        fig <- fig %>% add_trace(
            type="choroplethmapbox",
            geojson=counties,
            locations=df_data_with_fips$fips,
            z=df_data_with_fips$confirmed,
            colorscale="Viridis",
            text=~df_data_with_fips$county,
            zmin=0,
            zmax=13,
            marker=list(line=list(
                width=0)
            )
        ) %>% layout(
            mapbox=list(
                style="carto-positron",
                zoom =3,
                center=list(lon= -95.71, lat=37.09),
                height=400),
            margin = m
        )
    
        selected_home <- df_latest_report %>%
            filter(provider_name == input$finder) %>%
            group_by(provider_name) %>%
            top_n(1,date)
      
        if(!is.null(selected_home)) {
            fig <- fig %>% add_trace(
                type="scattermapbox",
                name=paste0(selected_home$provider_name," Total Cases: ", selected_home$residents_total_confirmed),
                lat = selected_home$lon,
                lon = selected_home$lat,
                size = 6,
                marker = list(color = "fuchsia"))
        }

#        fig <- fig %>% colorbar(title = "Confirmed COVID cases")
        fig <- fig %>% layout(
            title = "Confirmed COVID cases"
        ) %>% hide_colorbar() %>%
            hide_legend()
        # 
        # fig <- fig %>% layout(
        #     geo = g
        # )
        
        return(fig)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
