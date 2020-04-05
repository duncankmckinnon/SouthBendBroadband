#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard)
library(sf)

# Load data
load('Data/CombinedData.robj')

# Load aggregation script and helper functions
source('aggregation_fn.R')
source('app_helpers.R')

# Setup execution list
fnames <- c('BlockGroup', 'Tract')
dfs_ <- list(
  Combined_BlockGroup_Places,
  Combined_Tracts_Places
)
names(dfs_) <- fnames

# Run aggregation
dfs <- modeling_aggregations(dfs_, F, T)

# Read all shape files
shapenames <- c('tl_2019_18_bg',
                'tl_2019_18_tract',
                'tl_2019_18141_areawater', 
                'tl_2019_18141_linearwater',
                'School_Boundaries')

finalnames <- c('blockgroup', 'tract', 'awater', 'lwater', 'school')
shapefunc <- function(nm){
  path <- paste('Data/Shapefiles/',nm,'/',nm,'.shp',  sep = '')
  return(st_read(path, stringsAsFactors = F))
}  
shapes <- shapenames %>% sapply(FUN = shapefunc, simplify = F, USE.NAMES = F)
names(shapes) <- finalnames


# Add GEOID to each dataset to join with shape files
dfs$BlockGroup <- dfs$BlockGroup %>% 
  mutate('GEOID' = as.character(BlockGroup))
dfs$Tract <- dfs$Tract %>% 
  mutate('GEOID' = as.character(TractCode))

# Create final list of datasets by joining dfs with shapes
# (By using nested lists can select dataset directly from app inputs)
finalData <- list('BlockGroup', 'Tract')
finalData$BlockGroup <- shapes$blockgroup %>% 
  inner_join(dfs$BlockGroup, by = 'GEOID')
finalData$Tract <- shapes$tract %>% 
  inner_join(dfs$Tract,  by = 'GEOID')

# Shiny App setup ui and server
ui <- dashboardPage(
  dashboardHeader(title = 'EDA'),
  dashboardSidebar(
    selectInput(
      inputId = 'level',
      label = 'Census Level',
      choices = c('BlockGroup', 'Tract'),
      selected = 'BlockGroup',
      multiple = F
    ),
    selectInput(
      inputId = 'field',
      label = 'Grouping',
      choices = c(
        'Income', 
        'Education', 
        'Housing', 
        'Demographic', 
        'Commute', 
        'WebAccess'
      ),
      selected = 'Education',
      multiple = F
    ),
    metric_panel('Income', 'Income_Workers_PCT', 'BlockGroup', dfs),
    metric_panel('Education', 'Education_College_PCT', 'BlockGroup', dfs),
    metric_panel('Housing', 'Housing_Units_Tot', 'BlockGroup', dfs),
    metric_panel('Demographic', 'Demographic_Population_Density_Tot', 'BlockGroup', dfs),
    metric_panel('Commute', 'Commute_Alone_PCT', 'BlockGroup', dfs),
    metric_panel('WebAccess', 'WebAccess_Land_PCT', 'BlockGroup', dfs),
    metric_panel('Income', 'Income_Workers_PCT', 'Tract', dfs),
    metric_panel('Education', 'Education_College_PCT', 'Tract', dfs),
    metric_panel('Housing', 'Housing_Units_Tot', 'Tract', dfs),
    metric_panel('Demographic', 'Demographic_Population_Density_Tot', 'Tract', dfs),
    metric_panel('Commute', 'Commute_Transit_PCT', 'Tract', dfs),
    metric_panel('WebAccess', 'WebAccess_Land_PCT', 'Tract', dfs),
    selectInput(
      inputId = 'broadband',
      label = 'Broadband Coverage Metric',
      choices = get_names(dfs, 'Broadband', 'BlockGroup'),
      selected = 'Broadband_Computer_Plus_Internet_PCT',
      multiple = F
    )
  ),
  dashboardBody(
    fluidPage(
      fluidRow(
        column('Metric Level By BlockGroup', plotOutput('metp'), width = 6),
        column('Broadband By Metric Level', plotOutput('webp'), width = 6),    
        height = 6)#,
      # fluidRow(
      #   column('Metric Distribution', plotOutput('summary'), width = 6),
      #   # column('Correlation', plotOutput('corr'), width = 4),
      #   height = 6)
    )
  )
)

server <- function(input, output){
  f <- reactive({ 
    metric <- paste('metric', input$field, input$level, sep = '_')
    return( input[[metric]] )
  })
  d <- reactive({get_field_data(finalData, input$level, input$field, dfs)})
  output$metp <- renderPlot({
    e <- d()
    g <- f()
    ggplot(e) +
      geom_sf() +
      aes_string(fill = g) + 
      labs(fill = str_trunc(g, 15))
  })
  output$webp <- renderPlot({
    e <- d()
    g <- f()
    ggplot(e, aes_string(x = g, 
                         y = input$broadband, 
                         color = 'Demographic_Population_Tot')) +
      geom_point() + 
      geom_smooth() + 
      labs(col = 'Total\nPopulation')
  })
  # output$summary <- renderPlot({
  #   e <- d()
  #   g <- f()
  #   ggplot(e, aes_string(x = g, fill = 'Demographic_Population_Tot')) + 
  #     geom_histogram(bins = 10) + 
  #     labs(fill = 'Total\nPopulation')
  # })
}
# Run the application 
shinyApp(ui = ui, server = server)

