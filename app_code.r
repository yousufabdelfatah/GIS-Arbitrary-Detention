
# Set Up ------------------------------------------------------------------

# load packages

library(tidyverse)
library(sf)
library(tmap)
library(corrr)

# load data

detention <- 
  read_csv("Data/Full_Detention_Data.csv")

egypt <- 
  st_read("Data/egy_admbnda_adm1_capmas_20170421/egy.shp")

egypt_population <- 
  read_csv('Data/egypt_population_estimate_compas_2012_v1.1_0_fis.csv') %>% 
  select(ADM1_EN, 'Population' = Total)

socioeconomic <- 
  read_csv("Data/socio_economic.csv")

# Governate ---------------------------------------------------------------

# How many observations do we have for each governate
# ggplot writes Arabic backwards and it drives me crazy

detention %>% 
  ggplot() +
  geom_bar(aes(governate)) +
  coord_flip()

# there are some typos in the governates in the detention data
# so standardize those

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

# standardize governate names across datasets

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

# join shapefile with detention data

joined_data <-
  egypt %>% 
  left_join(detention, 
            by = c("ADM1_AR" = "governate"))

sf_use_s2(FALSE)

# Mapping the governates in ggplot

joined_data %>%
  #as_tibble() %>% 
  group_by(ADM1_REF) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = 'Detainees') +
  theme_void() + 
  labs(title = 'Number of Detainees Per Governate') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")

# interactive tmap

tmap_mode('view')

# enclosing the map in brackets allows me to implement leaflet widgets to 
# hide layers

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


# Age ---------------------------------------------------------------------

joined_data$arbitrary <- "Y"

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

# Torture -----------------------------------------------------------------

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


# Detention Condition ------------------------------------------------------------

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

# Legal Representation ----------------------------------------------------

# Define English variable for presence of lawyer

Present <- "حضور"

# Plot presence of lawyer

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

# Enforced Disappearances -------------------------------------------------

joined_data$disappeared <-
  if_else(is.na(joined_data$days_disappeared) == FALSE, 1, 0)

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

joined_data %>% 
  as_tibble() %>% 
  ggplot(aes(days_disappeared)) +
  geom_histogram(fill = "pink", alpha = 0.5, bins = 10) +
  labs(title = "Days Disappeared",
       x = "Days",
       y = "# of Detainees") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Socio Economic Factors --------------------------------------------------

# interactive tmap

tmap_mode('view')

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


# Correlation (Statistical Analysis) --------------------------------------

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

corrplot(socioeconomic_corr, 
         type="upper", 
         order="hclust",
         title = "Socioeconomic Correlation",
         mar=c(0,0,1,0))

# Summary Tables ----------------------------------------------------------

# summary table detentions

joined_data %>%
  as_tibble() %>% 
  group_by(ADM1_EN) %>% 
  left_join(egypt_population, by = 'ADM1_EN') %>% 
  mutate(Detained = n(),
         detention_per_million = (Detained/Population)*1E6) %>% 
  rename(Governate = ADM1_EN) %>% 
  select(Governate, Detained, detention_per_million) %>% 
  distinct()


# summary table conditions

joined_data %>% 
  as_tibble() %>% 
  select(42:51) %>% 
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

# summary table torture

joined_data %>% 
  as_tibble() %>% 
  group_by(ADM1_EN) %>% 
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
  rename("Threaten Family" = Threaten_family) 

# Static Maps of Abuses ---------------------------------------------------


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
            legend.outside.position = "right")

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
            legend.outside.position = "right")

disappearance_map <- tm_shape(basemap) +
  tm_rgb() + 
  
  joined_data %>% 
  select(disappeared, ADM1_EN) %>% 
  group_by(ADM1_EN) %>% 
  summarise(Disappeared = sum(disappeared)) %>% 
  tm_shape(name = "Disappeared") +
  tm_polygons(col = "Disappeared") +
  tm_layout(legend.outside = TRUE, 
            legend.outside.position = "right")


tmap_arrange(torture_map, rep_map, disappearance_map)




