rm(list = ls())

library("shiny")
library("shinydashboard")
library("shinythemes")
library("data.table")
library("plyr")
library("dplyr")
library("plotly")
library("maps")
library("flexdashboard")

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicmFtb25yYW1vbnJhIiwiYSI6ImNrdG1ybjRlZDI4ZnYyb3BleDg5cDNjcW0ifQ.PWMtNwPlCbehie0bvt8Ccg')
map_dat <- map_data("world", "tanzania") %>% filter(is.na(subregion))
data  <- fread("data.csv", data.table = F) %>% mutate(date_recorded = as.Date(date_recorded))
data$date_recorded <- sample(seq(as.Date('1999-01-01'), as.Date('2020-01-01'), by="day"), nrow(data), replace = T)
rownames(data) <- 1:nrow(data)


ui <- bootstrapPage(
  navbarPage(
    id="nav",
    theme = shinytheme("flatly"), 
    collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">TANZANIA PUMPS</a>'),
    windowTitle = "TANZANIA PUMPS"),
    tabPanel("TANZANIA PUMPS",
             div(class="outer",
                 tags$head(includeCSS("styles.css")),
                 plotlyOutput("map", width="100%", height="100%"),
                 
                 absolutePanel(id = "map_type_pamel", class = "panel panel-default",
                               top = "10%", left = "5%", width = "10%", height = "auto", fixed=TRUE,
                               draggable = FALSE,
                               column(12,
                                      selectInput("map_type", label = "Select map type", 
                                                  choices = c("basic", "streets", "outdoors", "light", "dark", "satellite", "satellite-streets"),
                                                  selected = "light"),
                                      checkboxGroupInput("func_filter", "Selecct pumps", choices = unique(data$status_group), selected = unique(data$status_group)
                                                         )
                               )
                               
                 ),
                 absolutePanel(id = "slider_panel", class = "panel panel-default",
                               top = "77%", left = "5%", width = "93%", height = "9%", 
                               fixed=TRUE,
                               draggable = FALSE, 
                               fluidRow(align="center",
                                        sliderInput("Date_range_selector", "Select construction year date range",
                                                    min = min(data$date_recorded),
                                                    max = max(data$date_recorded),
                                                    value = c(min(data$date_recorded), max(data$date_recorded)),
                                                    width = "95%", ticks = F)
                               )
                               
                 ),

                 absolutePanel(id = "plots_panel", class = "panel panel-default",
                               top = "10%", left = "83%", width = "15%", height = "65%",
                               fixed=TRUE, draggable = TRUE, 
                               plotOutput("functional_plt",  height = "40%"),
                               plotOutput("extraction_type_plt",  height = "40%"),
                               flexdashboard::gaugeOutput("population_card", width = "100%", height = "20%")
                 )
                 
                
             )
    )
  )






server <- function(input, output, session){
  
  map_layout <- eventReactive(input$Date_range_selector, {
    lay_out <- event_data("plotly_relayout")
    return(lay_out)
  })
  
  plt_data <- reactive({
    selected <- event_data("plotly_selected")
    min_date <- input$Date_range_selector[1]
    max_date <- input$Date_range_selector[2]
    
    if(length(selected) == 0){
      plt_data <- data %>% filter(between(date_recorded, min_date, max_date)) %>% filter(status_group %in% input$func_filter)
      
    }else{
      plt_data <- data %>% filter(between(date_recorded, min_date, max_date)) %>% filter(status_group %in% input$func_filter)
      functional_rows <- (selected %>% filter(curveNumber == 1) %>% .$pointNumber) + 1
      non_functional_rows <- (selected %>% filter(curveNumber == 2) %>% .$pointNumber) + 1
      repair_rows <- (selected %>% filter(curveNumber == 3) %>% .$pointNumber) + 1
      
      functional_data <- plt_data %>% filter(status_group == "functional") %>% slice(functional_rows)
      non_functional_data <- plt_data %>% filter(status_group == "non functional") %>% slice(non_functional_rows)
      repair_data <- plt_data %>% filter(status_group == "functional needs repair") %>% slice(repair_rows)
      
      plt_data <- rbind(functional_data, non_functional_data, repair_data)
    }
    
    return(plt_data)
  })
  
  output$functional_plt <- renderPlot({
    plt_data <-  plt_data()
    if(nrow(plt_data) == 0){return(NULL)}
    plt <- plt_data %>% mutate(counter = 1) %>% 
      ddply(~status_group, summarise, Count = sum(counter)) %>%
      ggplot(aes(status_group, Count, fill = status_group)) + 
      scale_fill_manual("Status", values = c("functional" = "#94F2B0", "non functional" = "#F29494", "functional needs repair" = "#5CBDF5")) + 
      geom_col() +
      geom_text(aes(status_group, Count, label = format(Count, big.mark = ",")),  vjust = 1.5) +
      theme_classic() +
      xlab("") +
      theme(plot.background = element_rect(fill = "transparent"), 
            legend.position = "None",
            panel.background = element_rect(fill = "transparent",
                                            colour = "transparent"),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank()
            ) +
      scale_x_discrete(breaks=unique(plt_data$status_group), 
                       labels=addline_format(unique(plt_data$status_group)))
    
    return(plt)
    
  })
  
  output$extraction_type_plt <- renderPlot({
    plt_data <-  plt_data()
    if(nrow(plt_data) == 0){return(NULL)}
    plt <- plt_data %>% 
      mutate(counter = 1) %>%
      ddply(~extraction_type_class, summarise,  Count = sum(counter)) %>%
      mutate(Count_por = Count / nrow(data) * 100) %>%
      ggplot(aes(x="", y=Count_por, fill=extraction_type_class)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) + 
      xlab("") + 
      ylab("%") + 
      theme(plot.background = element_rect(fill = "transparent"), 
            panel.background = element_rect(fill = "transparent",
                                            colour = "transparent"),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank()) + labs(fill = "Extraction type")
    
    return(plt)
    
  })
  
  output$map <- renderPlotly({
    
    min_date <- input$Date_range_selector[1]
    max_date <- input$Date_range_selector[2]
    plt_data <- data %>% filter(between(date_recorded, min_date, max_date)) %>% filter(status_group %in% input$func_filter)
    
    functional_data <- plt_data %>% filter(status_group == "functional")
    non_functional_data <- plt_data %>% filter(status_group == "non functional")
    repair_data <- plt_data %>% filter(status_group == "functional needs repair")
    
    ##### Base plot #####
    
    plt <- plot_mapbox(mode = "markers") 
    plt <- plt %>% add_trace(data = map_dat, x = ~long, y = ~lat, mode = "lines",  
                             line = list(color = "grey", width = 2),   hoverinfo = 'text', showlegend = F)
    
    ##### Markers #####
    
    plt <- plt %>% add_trace(data = functional_data,
                             x = ~longitude,
                             y = ~latitude,
                             mode   = 'markers',
                             marker = list(size = 5, color = "#94F2B0"),
                             text = ~wpt_name,
                             hoverinfo = 'text',
                             name = "Functional"
    )
    
    plt <- plt %>% add_trace(data = non_functional_data,
                             x = ~longitude,
                             y = ~latitude,
                             mode   = 'markers',
                             marker = list(size = 5, color = "#F29494"),
                             text = ~wpt_name,
                             hoverinfo = 'text',
                             name = "Non Functional"
    )
    
    plt <- plt %>% add_trace(data = repair_data,
                             x = ~longitude,
                             y = ~latitude,
                             mode   = 'markers',
                             marker = list(size = 5, color = "#5CBDF5"),
                             text = ~wpt_name,
                             hoverinfo = 'text',
                             name = "Functional needs repair"
    )
    
    ##### Layout #####
    lay_out <- map_layout()
    old_zoom  <- lay_out$mapbox.zoom[1]
    old_center_lat <- lay_out$mapbox.center$lat
    old_center_lon <- lay_out$mapbox.center$lon
    
    if(is.null(old_zoom)){
      old_zoom  <- 5.3
      old_center_lat <- median(map_dat$lat)+0.5
      old_center_lon <- median(map_dat$long) + 1
    }
    
    plt <- plt  %>%
      layout(
        paper_bgcolor='transparent',
        showlegend = T, 
        mapbox = list(
          style = input$map_type,
          zoom = old_zoom,
          center = list(lat = old_center_lat,
                        lon = old_center_lon)
        ),
        legend = list(
          orientation = 'h',
          xanchor = "center",
          x = 0.5,
          font = list(size = 12)
        )
      )
    
    return(plt)
    
    
  })
  
  output$population_card = flexdashboard::renderGauge({
    plt_data <- plt_data()
    if(nrow(plt_data) == 0){return(NULL)}
    pop <- sum(plt_data$population)
    
    flexdashboard::gauge(pop,
                         symbol = ' People',
                         min = 0, 
                         max = sum(data$population))
  })
  
  
  session$onSessionEnded(function(){stopApp()})
}









shinyApp(ui=ui,server=server)