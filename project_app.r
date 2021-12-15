
# setup -------------------------------------------------------------------

library(lubridate)
library(sf)
library(tmap)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

# data --------------------------------------------------------------------

# Read in Egypt Shapefile

egypt <- 
  st_read("Data/egy_admbnda_adm1_capmas_20170421/egy.shp")

# Read in Detention Data

detention <- 
  read_csv("Data/Full_Detention_Data.csv")

# Read in Population Data

egypt_population <- 
  read_csv('Data/egypt_population_estimate_compas_2012_v1.1_0_fis.csv') %>% 
  select(ADM1_EN, 'Population' = Total)

socioeconomic <- 
  read_csv("Data/socio_economic.csv")

# Data Subsetting and Summarizing -----------------------------------------

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

# Define English variable for presence of lawyer

Present <- "حضور"

# Create a variable for Enforced Disappearances

joined_data$disappeared <-
  if_else(is.na(joined_data$days_disappeared) == FALSE, 1, 0)

# Interactive Maps --------------------------------------------------------

tmap_mode('view')

detainee_map <- 
      {joined_data %>%
          #as_tibble() %>% 
          group_by(ADM1_REF) %>% 
          summarise('Total Detainees' = n()) %>% 
          
          tm_shape(name = 'total detention') +
          tm_polygons(col = 'Total Detainees') +
          tm_basemap('Esri.WorldTopoMap') +
          tm_basemap('Esri.WorldStreetMap') +
          
          joined_data %>%
          group_by(ADM1_EN) %>% 
          left_join(egypt_population, by = 'ADM1_EN') %>% 
          mutate(n = n(),
                 detention_per_million = (n/Population)*1E6) %>% 
          
          tm_shape(name = 'detention per million') +
          tm_polygons(col = 'detention_per_million')} %>% 
      
      tmap_leaflet() %>%
      leaflet::hideGroup('detention per million')

socioeconomic_map <- 
  {egypt %>% 
      left_join(socioeconomic %>% 
                  mutate(Region = case_when(
                    Region == "Assuit" ~ 'Assiut',
                    Region == "Kafr El-Sheikh" ~ "Kafr El-Shikh",
                    Region == "Kalyubia"~ "Kalyoubia",
                    Region == "Menya" ~ "Menia",
                    Region == "Souhag" ~ "Suhag",
                    TRUE ~ Region)) %>% 
                  select('Region', 
                         'Poverty (IWI<50)'), 
                by = c("ADM1_EN" = "Region")) %>% 
      
      tm_shape(name = 'Poverty') +
      tm_polygons(col = 'Poverty (IWI<50)') +
      
      egypt %>% 
      left_join(socioeconomic %>% 
                  mutate(Region = case_when(
                    Region == "Assuit" ~ 'Assiut',
                    Region == "Kafr El-Sheikh" ~ "Kafr El-Shikh",
                    Region == "Kalyubia"~ "Kalyoubia",
                    Region == "Menya" ~ "Menia",
                    Region == "Souhag" ~ "Suhag",
                    TRUE ~ Region)) %>% 
                  select('Region', 
                         'Mean Years Schooling'), 
                by = c("ADM1_EN" = "Region")) %>% 
      
      tm_shape(name = 'Mean Years Schooling') +
      tm_polygons(col = 'Mean Years Schooling') +
      
      tm_basemap('Esri.WorldTopoMap') +
      tm_basemap('Esri.WorldStreetMap') +
      
      egypt %>% 
      left_join(socioeconomic %>% 
                  mutate(Region = case_when(
                    Region == "Assuit" ~ 'Assiut',
                    Region == "Kafr El-Sheikh" ~ "Kafr El-Shikh",
                    Region == "Kalyubia"~ "Kalyoubia",
                    Region == "Menya" ~ "Menia",
                    Region == "Souhag" ~ "Suhag",
                    TRUE ~ Region)) %>% 
                  select('Region',
                         'Life expectancy'), 
                by = c("ADM1_EN" = "Region")) %>% 
      
      tm_shape(name = 'Life expectancy') +
      tm_polygons(col = 'Life expectancy')} %>% 
  
  tmap_leaflet() %>%
  leaflet::hideGroup('Life expectancy') %>% 
  leaflet::hideGroup('Mean Years Schooling')

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

# plot showing detention conditions

Detention_Conditions <-
  joined_data %>% 
  as_tibble() %>% 
  select(42:51) %>% 
  mutate(across(everything(), as.character)) %>% 
  replace(!is.na(.), '1') %>% 
  replace(is.na(.), '0') %>% 
  mutate(across(everything(), as.numeric)) %>% 
  rename_with(str_to_title) %>% 
  summarise_all(sum) %>% 
  rename("Poor Treatment" = Poor_treatment,
         "Poor or No Ventilation" = Ventilation,
         "Solitary Confinement" = Solitary,
         "Denial of Education" = Education,
         "Witholding Food" = Food,
         "No Exercie" = Exercise,
         "Medical Negligence" = Health,
         "Denial of Visitation" = Visitation) %>%
  select(-Strike) %>% 
  pivot_longer(everything(), 
               names_to = "Condition",
               values_to = "Incidence") %>% 
  ggplot(aes(reorder(Condition, -Incidence), Incidence)) +
  geom_col(fill = "green") +
  coord_flip() +
  labs(title="Inhumane Conditions",
       x= "Conditions")+
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot presence of lawyer

Legal_Rep <- 
  joined_data %>% 
  as_tibble() %>% 
  select(lawyer_present) %>% 
  drop_na()  %>% 
  mutate(
    lawyer_present = case_when(
      lawyer_present == Present ~ "Lawyer Present",
      lawyer_present != Present ~ "No Legal Representation")) %>% 
  ggplot(aes(lawyer_present)) +
  geom_bar(fill = "blue") +
  labs(title="Access to Legal Representation",
       x= "Was a Lawyer Present?")+
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Plot Enforced Disappearances

Enforced_Disappearance <- 
  joined_data %>% 
  as_tibble() %>% 
  select(official_arrest:type_of_location, disappeared) %>% 
  mutate(disappeared = if_else(
    disappeared == 1, "Disappearance Reported", "No Report")) %>% 
  ggplot(aes(as.factor(disappeared))) +
  geom_bar(fill = "purple") +
  labs(title="Enforced Disappearances",
       x= "Disappeared")+
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5)) 

Days_Disappeared <- 
  joined_data %>% 
  as_tibble() %>% 
  ggplot(aes(days_disappeared)) +
  geom_histogram(fill = "pink", alpha = 0.5, bins = 10) +
  labs(title = "Days Disappeared",
       x = "Days",
       y = "# of Detainees") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5)) 


# Correlation -------------------------------------------------------------

# create a column for presence of torture and correlate with other abuses

abuse_correlation <- 
  cor(joined_data %>% 
        as_tibble() %>% 
        mutate(torture = if_else(
          is.na(beating) == FALSE | 
            is.na(electrocution) == FALSE |
            is.na(threaten_family) == FALSE |
            is.na(hanging) == FALSE,
          1,
          0)) %>% 
        mutate(
          lawyer_present = case_when(
            lawyer_present == Present ~ 1,
            lawyer_present != Present & 
              is.na(lawyer_present) == FALSE ~ 0)) %>% 
        select("Enforced Disappearance" = disappeared, 
               torture, 
               'Legal Representation' = lawyer_present) %>% 
        drop_na())  

# plot correlation

abuse_corrplot <- 
  corrplot(abuse_correlation, 
           type="upper", 
           order="hclust",
           title = "Correlation Between Abuses",
           mar=c(0,0,1,0))

# correlation between abuses and socioeconomic factors

socioeconomic_corr <- 
  cor(joined_data %>% 
        as_tibble() %>% 
        mutate(torture = if_else(
          is.na(beating) == FALSE | 
            is.na(electrocution) == FALSE |
            is.na(threaten_family) == FALSE |
            is.na(hanging) == FALSE,
          1,
          0)) %>% 
        mutate(
          lawyer_present = case_when(
            lawyer_present == Present ~ 1,
            lawyer_present != Present & 
              is.na(lawyer_present) == FALSE ~ 0)) %>% 
        select(ADM1_EN, 
               disappeared, 
               torture, 
               lawyer_present) %>% 
        drop_na() %>%
        group_by(ADM1_EN) %>% 
        summarise(Torture = sum(torture), 
                  "Enforced Disappearance" = sum(disappeared), 
                  "Legal Representation" = sum(lawyer_present)) %>% 
        left_join(socioeconomic %>% 
                    mutate(Region = case_when(
                      Region == "Assuit" ~ 'Assiut',
                      Region == "Kafr El-Sheikh" ~ "Kafr El-Shikh",
                      Region == "Kalyubia"~ "Kalyoubia",
                      Region == "Menya" ~ "Menia",
                      Region == "Souhag" ~ "Suhag",
                      TRUE ~ Region)) %>% 
                    select('Region', 'Poverty (IWI<50)', 
                           'Mean Years Schooling'), 
                  by = c("ADM1_EN" = "Region")) %>% 
        select(-ADM1_EN) %>% 
        drop_na())  

# plot correlation 

socioeconomic_corrplot <- 
  corrplot(socioeconomic_corr, 
           type="upper", 
           order="hclust",
           title = "Socioeconomic Correlation",
           mar=c(0,0,1,0))

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


# Static Maps -------------------------------------------------------------

basemap <-
  st_bbox(joined_data) %>% 
  tmaptools::read_osm()

tmap_mode('plot')

torture_map <- tm_shape(basemap) +
  tm_rgb() +
  
  joined_data %>% 
  mutate(torture = if_else(
    is.na(beating) == FALSE | 
      is.na(electrocution) == FALSE |
      is.na(threaten_family) == FALSE |
      is.na(hanging) == FALSE,
    1,
    0)) %>% 
  group_by(ADM1_EN) %>% 
  summarise('Reported Torture' = sum(torture)) %>% 
  
  tm_shape(name = 'Reported Torture') +
  tm_polygons(col = 'Reported Torture') +
  
  tm_layout(legend.outside = TRUE, 
            legend.outside.position = "right",
            bg.color = "grey85")

rep_map <- tm_shape(basemap) +
  tm_rgb() + 
  
  joined_data %>% 
  mutate(
    lawyer_present = case_when(
      lawyer_present == Present ~ 1,
      lawyer_present != Present ~ 0)) %>% 
  select(lawyer_present, ADM1_EN) %>% 
  drop_na() %>% 
  group_by(ADM1_EN) %>% 
  summarise("Access to Lawyer" = sum(lawyer_present)) %>% 
  
  tm_shape(name = "Access to Lawyer") +
  tm_polygons(col = "Access to Lawyer") +
  tm_layout(legend.outside = TRUE, 
            legend.outside.position = "right",
            bg.color = "grey85")

disappearance_map <- tm_shape(basemap) +
  tm_rgb() + 
  
  joined_data %>% 
  select(disappeared, ADM1_EN) %>% 
  group_by(ADM1_EN) %>% 
  summarise(Disappeared = sum(disappeared)) %>% 
  tm_shape(name = "Disappeared") +
  tm_polygons(col = "Disappeared") +
  tm_layout(legend.outside = TRUE, 
            legend.outside.position = "right",
            bg.color = "grey85")


static_maps <- tmap_arrange(torture_map, 
                            rep_map, 
                            disappearance_map)

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
        menuItem('Home',
                 icon = icon('book'),
                 tabName = 'home'),
        
        menuItem('Interactive Maps',
                 icon = icon('map'),
                 tabName = 'maps1'),
        
        menuItem('Static Maps',
                 icon = icon('map'),
                 tabName = 'maps2'),
        
        menuItem('Charts',
                 icon = icon('chart-line'),
                 tabName = 'charts'),
        
        menuItem('Correlations',
                 icon = icon('calculator'),
                 tabName = 'correlations'),
        
        menuItem('Summary Tables',
                 icon = icon('table'),
                 tabName = 'tables')
        
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
        
        # set landing page and associated text
        
        tabItem(
          tabName = 'home',
          h2('Arbitrary Detention In Egypt'),
          p("Some Text")),
        
        # setting the Interactive map tab and the radio buttons included 
        
        tabItem(
          tabName = 'maps1',
          radioButtons(
            inputId = 'interactive_map',
            label = 'Map Type',
            choiceNames = c('Number of Detainees', 
                            'Socio-Economic Indicators'),
            choiceValues = c('detainee_map', 
                             'socioeconomic_map')),
          h2('Interactive Maps'),
          tmapOutput(outputId = 'interactive_map')),
        
        # Set Static Maps
        
        tabItem(
          tabName = 'maps2',
          h2('Static Maps'),
          plotOutput(outputId = 'static_map')),
        
        # setting the charts view and associates radio buttons
        
        tabItem(
          tabName = 'charts',
          selectInput(
            inputId = 'plot',
            label = 'Chart Topic',
            choices = c('Age', 
                            'Torture',
                            'Detention Conditions',
                            'Legal Representation',
                            'Enforced Disappearance',
                            'Days Disappeared')),
          h2('Detention Data Charts'),
          plotOutput(outputId = 'plot_output')),
        
        # setting the correlations and radio buttons
        
        tabItem(
          tabName = 'correlations',
          radioButtons(
            inputId = 'correlation',
            label = 'Correlation Type',
            choiceNames = c('Correlation Between Abuses', 
                            'Socio-Economic Correlation'),
            choiceValues = c('abuse_corrplot', 
                             'socioeconomic_corrplot')),
          h2('Correlation Plots'),
          plotOutput(outputId = 'correlation_plot')),
        
        # setting the tables tab and corresponding dropdown box
        
        tabItem(
          tabName = 'tables',
          h2('Summary table'),
          selectInput(
            inputId = 'table',
            label = 'Table',
            choices = c('Detention Conditions', 
                        'Detention By Governate')),
          dataTableOutput(outputId = 'summary_table'))
        

      )
    )
  )

# server ------------------------------------------------------------------

server <- function(input, output) {
  
  # interactive map function
  view_maps <-
    reactive({
      if (input$interactive_map == 'detainee_map') {
        detainee_map
      } else {
        socioeconomic_map
      }
    })
  


  # plot function
  
  shiny_plots <-
    reactive({
      if (input$plot == 'Age') {
        Age_Distribution 
      } else if (input$plot == 'Torture') {
        Torture 
      } else if (input$plot == 'Legal Representation') {
        Legal_Rep
      } else if (input$plot == 'Detention Conditions') {
        Detention_Conditions
      } else if (input$plot == 'Enforced Disappearance') {
        Enforced_Disappearance
      } else {
        Days_Disappeared
      }
    })
  
  # correlation function
  
  correlation_plot <-
    reactive({
      if (input$correlation == 'abuse_corrplot') {
        abuse_corrplot
      } else {
        socioeconomic_corrplot
      }
    })
  
  
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
  

# Interactive Map:

  output$interactive_map <-
    renderLeaflet(view_maps())
  
# Static Map
  
  output$static_map <-
    renderPlot(static_maps)
  
# Summary table:
  
  output$summary_table <-
    renderDataTable(summary_tables())
  
# Correlation Plot:
  
  output$correlation_plot <-
    renderPlot(correlation_plot())

  
# Plot:

  output$plot_output <-
    renderPlot(shiny_plots())
}

# knit --------------------------------------------------------------------
shinyApp(ui, server)

