
# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)

# data --------------------------------------------------------------------

# Read in Egypt Shapefile

egypt <- 
  st_read("../Input/egy_admbnda_adm1_capmas_20170421/egy.shp")

# Read in Detention Data

detention <- 
  read_csv("../Input/Full_Detention_Data.csv")

# Read in Population Data

egypt_population <- 
  read_csv('../Input/egypt_population_estimate_compas_2012_v1.1_0_fis.csv') %>% 
  select(ADM1_EN, 'Population' = Total)


# user interface ----------------------------------------------------------

# the following code chunk establishes the user interface

# We'll be using a dashboard page here

ui <- 
  dashboardPage(
    
    # set the title
    
    dashboardHeader(title = 'Arbitrary Detention app'),
    
    # set the sidebar options
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem('Map',
                 icon = icon('map'),
                 tabName = 'maps'),
        
        menuItem('Summary Tables',
                 icon = icon('table'),
                 tabName = 'tables'),
        
        menuItem('Static Charts',
                 icon = icon('chart-line'),
                 tabName = 'charts')
      )
    ),
    
    # settings for the body of the dashboard
    
    dashboardBody(
      tags$head(
        tags$link(
          rel = 'stylesheet',
          type = 'text/css',
          href = 'dashboard_styles.css'
        )
      ),
      
      tabItems(
        
        # setting the map tab and the checkbox included within it
        
        tabItem(
          tabName = 'maps',
          h2('Map'),
          checkboxInput(
            inputId = 'population_adjust',
            label = 'Adjust for per million population?'),
          tmapOutput(outputId = 'detention_map')),
        
        # setting the tables tab and corresponding dropdown box
        
        tabItem(
          tabName = 'tables',
          h2('Summary table'),
          selectInput(
            inputId = 'table',
            label = 'Table',
            choices = c('Detention Conditions', 
                        'Detention By Governate')),
          dataTableOutput(outputId = 'summary_table')),
        
        # setting the charts view and associates radio buttons
        
        tabItem(
          tabName = 'charts',
          radioButtons(
            inputId = 'plot',
            label = 'Chart Topic',
            choiceNames = c('Age', 'Torture'),
            choiceValues = c('Age', 'Torture')),
          h2('Trend'),
          plotOutput(outputId = 'plot_output'))
      )
    )
  )

# server ------------------------------------------------------------------

server <- function(input, output) {

# Data subsetting and summarizing -------------------------------------
  
# standardize naming conventions so data can be joined
  
  typo_bs <- c("بنى سويف\n" ,"بنى سويف", "بني سويف") 
  correct_bs <- 'بنى سويف'
  
  typo_alex <- "الاسكندرية\n"
  correct_alex <- "الإسكندرية"
  
  typo_bh <- "البحيرة\n"
  correct_bh <- 'البحيرة'
  
  typo_dum <- "دمياط\n"
  correct_dum <- "دمياط"
  
  typo_ism <- "الإسماعيلية\n"
  correct_ism <- "الإسماعيلية"
  
  typo_ks <- "كفر الشيخ\n"
  correct_ks <- "كفر الشيخ"
  
  typo_qub <- "القليوبية\n"
  correct_qub <- 'القليوبية'
  
  typo_min <- "المنيا\n"
  correct_min <- "المنيا"
  
  typo_mun <- "المنوفية\n"
  correct_mun <- "المنوفية"
  
  typo_ss <- "شمال سيناء\n"
  correct_ss <- "شمال سيناء"
  
  typo_sharq <- "الشرقية\n"
  correct_sharq <- "الشرقية"
  
  # change the names to the standardized form
  
  egypt <- 
    egypt %>% 
    mutate(ADM1_AR = case_when(
      ADM1_AR %in% typo_bs ~ correct_bs,
      ADM1_AR %in% typo_alex ~ correct_alex,
      ADM1_AR %in% typo_bh ~ correct_bh,
      ADM1_AR %in% typo_dum ~ correct_dum,
      ADM1_AR %in% typo_ism ~ correct_ism,
      ADM1_AR %in% typo_ks ~ correct_ks,
      ADM1_AR %in% typo_qub ~ correct_qub,
      ADM1_AR %in% typo_min ~ correct_min,
      ADM1_AR %in% typo_mun ~ correct_mun,
      ADM1_AR %in% typo_ss ~ correct_ss,
      ADM1_AR %in% typo_sharq ~ correct_sharq,
      TRUE ~ ADM1_AR))
  
  detention <-
    detention %>% 
    mutate(governate = case_when(
      governate %in% typo_bs ~ correct_bs,
      governate %in% typo_alex ~ correct_alex,
      governate %in% typo_bh ~ correct_bh,
      governate %in% typo_dum ~ correct_dum,
      governate %in% typo_ism ~ correct_ism,
      governate %in% typo_ks ~ correct_ks,
      governate %in% typo_qub ~ correct_qub,
      governate %in% typo_min ~ correct_min,
      governate %in% typo_mun ~ correct_mun,
      governate %in% typo_ss ~ correct_ss,
      governate %in% typo_sharq ~ correct_sharq,
      TRUE ~ governate))
  
  # join data
  
  joined_data <-
    egypt %>% 
    left_join(detention, by = c("ADM1_AR" = "governate"))
  
  # set sf_use_s2 to false to prevent use of the use the s2 spherical geometry 
  # package for geographical coordinate operations- which causes the map to
  # throw an error

  sf_use_s2(FALSE)  


# Maps --------------------------------------------------------------------

  
  # Initially I was going to have population adjusted as another interactive 
  # layer but I needed an excuse to use the checkbox. For the final I think
  # I'd rather do it as another layer
  
  # two interactive maps with tmap- one adjusted by population per million
  
  regular_map <- 
    joined_data %>%
    #as_tibble() %>% 
    group_by(ADM1_REF) %>% 
    summarise(n = n()) %>% 
    
    tm_shape(name = 'total detention') +
    tm_polygons(col = 'n') 
  
  population_adjusted_map <-
    joined_data %>%
    group_by(ADM1_EN) %>% 
    left_join(egypt_population, by = 'ADM1_EN') %>% 
    mutate(n = n(),
           detention_per_million = (n/Population)*1E6) %>% 
    
    tm_shape(name = 'detention per million') +
    tm_polygons(col = 'detention_per_million')
  
  # function to choose which map is displayed based on checkbox
  
  maps <-
    reactive({
      if (input$population_adjust) {
        population_adjusted_map 
      } else {
        regular_map 
      }
    }) 
  
  # Plots

# Static Plots ------------------------------------------------------------

  # plot showing age distribution of detainees
  
  Age_Distribution <-
      joined_data %>% 
        ggplot(aes(age)) +
        geom_histogram(aes(y =..density..),
                       bins=25,
                       color="black", 
                       fill="dodgerblue",
                       alpha = 0.8) +
        labs(title="Distribution of Detainee Ages",
             x= "Age")+
        geom_density(color = "orange", size = 1.5) +
        ggthemes::theme_few() +
        theme(plot.title = element_text(hjust = 0.5))
  
  # plot showing incidences of torture
  
  Torture <-
      joined_data %>% 
        as_tibble() %>% 
        select(beating, 
               electrocution, 
               threaten_family, 
               hanging) %>% 
        replace(is.na(.), 0) %>% 
        summarise(beating = sum(beating), 
                  electrocution = sum(electrocution), 
                  threaten_family = sum(threaten_family),
                  hanging = sum(hanging)) %>% 
        rename_with(str_to_title) %>% 
        rename("Threaten Family" = Threaten_family) %>% 
        pivot_longer(everything(), 
                     names_to = "Method",
                     values_to = "Incidence") %>% 
        ggplot(aes(reorder(Method, -Incidence), Incidence)) +
        geom_col(fill = "red") +
        coord_flip() +
        labs(title="Subjection to Torture",
             x= "Method")+
        theme_test() +
        theme(plot.title = element_text(hjust = 0.5))

  # plot function
  
  shiny_plots <-
    reactive({
      if (input$plot == 'Age') {
        Age_Distribution 
      } else {
        Torture 
      }
    })
  

# Summary Tables ----------------------------------------------------------
  
  # detention numbers
  
  numbers <- 
    joined_data %>%
    as_tibble() %>% 
    group_by(ADM1_EN) %>% 
    left_join(egypt_population, by = 'ADM1_EN') %>% 
    mutate(Detained = n(),
           detention_per_million = (Detained/Population)*1E6) %>% 
    rename(Governate = ADM1_EN) %>% 
    select(Governate, Detained, detention_per_million) %>% 
    distinct()
  
  # detention conditions
  
  conditions <- 
    joined_data %>% 
    as_tibble() %>% 
    select(health:strike) %>% 
    mutate(across(everything(), as.character)) %>% 
    replace(!is.na(.), '1') %>% 
    replace(is.na(.), '0') %>% 
    mutate(across(everything(), as.numeric)) %>% 
    rename_with(str_to_title) %>% 
    summarise_all(sum) %>% 
    rename("Poor Treatment" = Poor_treatment) %>%
    select(-Strike) %>% 
    pivot_longer(everything(), 
                 names_to = "Condition",
                 values_to = "Incidence")
  
  # tables function
  
  summary_tables <-
    reactive({
      if (input$table == 'Detention Conditions') {
        conditions 
      } else {
        numbers 
      }
    })
    

# Outputs -------------------------------------------------------------

# Map:

  output$detention_map <-
    renderTmap(maps())
  
# Summary table:
  
  output$summary_table <-
    renderDataTable(summary_tables())

  
# Plot:

  output$plot_output <-
    renderPlot(shiny_plots())
}

# knit --------------------------------------------------------------------
shinyApp(ui, server)

