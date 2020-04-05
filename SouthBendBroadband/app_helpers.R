library(tidyverse)
library(shiny)

# get character vector of metrics by start of field name
# (instead of storing list of metrics by field name separately)
get_names <- function(d, nm, level){
  return( d[[level]] %>%
            dplyr::select(starts_with(nm)) %>%
            names()
  )
}

# Function used in app to extract subset of data from input selections
get_field_data <- function(d, level, fname, df){
  nm <- get_names(df, fname, level)
  bb <- get_names(df, 'Broadband', level)
  data <- d[[level]] %>% 
    dplyr::select_at( vars( nm, bb, 
      'Demographic_Population_Tot',
      'GEOID', 
      'geometry' 
    ) 
  )
  return(data)
} 

# Function to create conditional panel with id and choices dependent only on field and a default selected  
metric_panel <- function(f_id, f_default, level = 'BlockGroup', df){
  cond <- paste("input.field==='", f_id, "' && input.level==='", level, "'", sep = '')
  id <- paste('metric', f_id, level, sep = '_')
  return(
    shiny::conditionalPanel(cond,
                     selectInput(
                       inputId = id,
                       label = 'Metric',
                       choices = get_names(df, f_id, level),
                       selected = f_default,
                       multiple = F
                     )
    )
  )
}