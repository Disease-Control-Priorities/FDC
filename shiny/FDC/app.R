library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(shinydashboard)
library(formattable)
library(DT)

#load data
df<-read.csv("for_download.csv", stringsAsFactors = F)
plot_df<-read.csv("plot_data.csv", stringsAsFactors = F)
locs<-unique(df$location)

#significant digit formatter function
so_formatter <- function(x) {
    dplyr::case_when(
        x < 1e3 ~ as.character(x),
        x < 1e6 ~ paste0(as.character(x/1e3), " thousand"),
        x < 1e9 ~ paste0(as.character(x/1e6), " million"),
    )
} #end of function


# Define UI for application #
ui <- fluidPage(

    # Application title
    h3("Worldwide health gains from fixed-dose combination therapies for primary and secondary prevention of atherosclerotic cardiovascular disease"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Select a location:", c("All locations", locs)),
            br(),
            h5(strong("Download disaggregated data:")),
            downloadButton("downloadData", "Download data"),
            br(),br(),
            h5(strong("Download Figure 1:")),
            downloadButton("downloadPlot", "Download plot"),
            br(),br(),
            h5(strong("Download Table 2 data:")),
            downloadButton("downloadTable", "Download table")
            ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
          tabPanel("Results",
           h4(strong("Figure 1."), "Worldwide cumulative CVD events and deaths averted by FDCs over 2023-2030 compared to maintaining current care."),
           plotOutput("plot1"),
           h5(em("Note: Worldwide cumulative CVD events are the aggregate of 
           nonfatal myocardial infarction, stroke, and heart failure events.
           'FDC with aspirin' refers to a primary 
              prevention FDC containing aspirin; secondary prevention 
              FDCs are assumed to contain aspirin.")),
           br(),
           h4(strong("Table 2."), "Worldwide impact of FDCs in the year 2050 as compared to maintaining current care over 2020-2050."),
           formattableOutput("table2"),
           h5(em("Note: 'CVD events' are reported as the population-level aggregate 
              of acute episodes of nonfatal myocardial infarction, stroke, and heart 
              failure. Events and deaths averted are calculated as the difference 
              between each scenario (1-4) in the year 2050 and the current care 
              scenario in the year 2050. Baseline CVD epidemiology in 2020 is provided 
              as a reference. All estimates are reported to two significant figures; 
              some numbers may not add up due to rounding. 'With aspirin' refers to a 
              primary prevention FDC containing aspirin; secondary prevention FDCs are 
              assumed to contain aspirin. See table 1 for details of each of the four 
              scenarios."))
          ),
          tabPanel("Coverage", )
        ) #tabsetPanel
        ) #mainPanel
    ) #sidebarLayout
) #ui

# Define server logic
server <- function(input, output) {
    
    #set up reactive values
    rv<-reactiveValues() 
    
    observeEvent(input$country,{
        if(input$country=="All locations"){
            rv$data<-df
        }
        else{
            rv$data<-df%>%filter(location==input$country)
        }
    })
    
    output$plot1 <- renderPlot({
        
        plot<-plot_df%>%filter(location==input$country)%>%
            mutate(Intervention = factor(Intervention, levels=c("Scenario 4", "Scenario 3", "Scenario 2", "Scenario 1")))%>%
            arrange(desc(Intervention))%>%mutate(value = ifelse(value<0,0,value))
        
        p<-ggplot(plot, aes(x=year, y=value))+
            geom_area(aes(fill=Intervention), position = 'stack', alpha=0.6 , size=.5, colour="white") +
            facet_grid(metric~Scenario)+
            theme_bw()+
            xlim(2023,2050)+
            xlab("Year")+
            ylab("Cumulative CVD events/deaths averted")+
            scale_fill_viridis(discrete = T)  
        
        rv$p2<-p
        p
        
    })
    
    output$table2<-renderFormattable({
        if(input$country == "All locations"){
            
            table<-df%>%
                filter(year==2050 & intervention!="Current care")%>%
                bind_rows(., df%>%filter(year %in% c(2020,2050)
                                               & intervention=="Current care" 
                                               & Scenario == "FDC with aspirin"))%>%
                group_by(location, year, intervention, Scenario)%>%
                summarise(deaths = sum(CVD_deaths),
                          events = sum(Incidence),
                          prev = sum(Prevalent_CVD))%>%
                group_by(year, intervention, Scenario)%>%
                summarise("CVD deaths" = sum(deaths),
                          "New CVD events"  = sum(events),
                          "Prevalent CVD cases" = sum(prev))%>%
                rename(Intervention = intervention,
                       Year = year)
            
        }
        else{
            
            table<-df%>%filter(location==input$country)%>%
                filter(year==2050 & intervention!="Current care")%>%
                bind_rows(., df%>%filter(location==input$country)%>%
                              filter(year %in% c(2020,2050)
                                               & intervention=="Current care" 
                                               & Scenario == "FDC with aspirin"))%>%
                group_by(location, year, intervention, Scenario)%>%
                summarise(deaths = sum(CVD_deaths),
                          events = sum(Incidence),
                          prev = sum(Prevalent_CVD))%>%
                group_by(year, intervention, Scenario)%>%
                summarise("CVD deaths" = sum(deaths),
                          "New CVD events"  = sum(events),
                          "Prevalent CVD cases" = sum(prev))%>%
                rename(Intervention = intervention,
                       Year = year)
            
        }
        
        base_deaths<-as.numeric(table[2,4])
        base_inc<-as.numeric(table[2,5])
        base_prev<-as.numeric(table[2,6])
        
        table<-table%>%
            mutate(`CVD deaths` = ifelse(`CVD deaths`>base_deaths, base_deaths, `CVD deaths`),
                    `New CVD events` = ifelse(`New CVD events`>base_inc, base_inc, `New CVD events`),
                    `Prevalent CVD cases` = ifelse(`New CVD events`==base_inc, base_prev, `Prevalent CVD cases`),
                   "Deaths averted" = base_deaths- `CVD deaths`,
                   "CVD events averted" = base_inc - `New CVD events`,
                   "Change in CVD prevalence" = 100*(`Prevalent CVD cases`-base_prev)/base_prev,
                   Intervention = ifelse(Intervention!="Current care" & Scenario=="FDC with aspirin",
                                         paste0(Intervention," (with aspirin)"),
                                         Intervention)
            )%>%
            select(-Scenario)%>%
            rename(Scenario = Intervention)
        
        #reorder and clean up
        table<-table[,c(1,2,4,7,5,8,3,6)]
        table<-table[c(1,2,4,3,6,5,8,7,10,9),]
        
        table$`New CVD events`<-so_formatter(signif(table$`New CVD events`, digits=2))
        table$`CVD events averted`<-so_formatter(signif(table$`CVD events averted`, digits=2))
        table$`Prevalent CVD cases`<-so_formatter(signif(table$`Prevalent CVD cases`, digits=2))
        table$`Change in CVD prevalence`<-paste0(signif(table$`Change in CVD prevalence`, digits=2), "%")
        table$`CVD deaths`<-so_formatter(signif(table$`CVD deaths`, digits=2))
        table$`Deaths averted`<-so_formatter(signif(table$`Deaths averted`, digits=2))
        
        table[1:2,c(4,6,8)]<-NA
        
        rv$table<-table%>%mutate(location=input$country)
        formattable(table)
        
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("FDC_full_",paste(input$country, ".csv", sep=""))
        },
        content = function(file) {
            write.csv(rv$data, file, row.names = FALSE)
        }
    )
    
    output$downloadTable <- downloadHandler(
        filename = function() {
            paste("FDC_table1_",paste(input$country, ".csv", sep=""))
        },
        content = function(file) {
            write.csv(rv$table, file, row.names = FALSE)
        }
    )
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$country, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = rv$p2, device = "png", height = 6, width=8)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
