
library(tidyverse)
library(sf)

detention <- read_csv("../Input/Full_Detention_Data.csv")

egypt <- st_read("../Input/egy_admbnda_adm1_capmas_20170421/egy.shp")

plot(egypt)

    
# Governate ---------------------------------------------------------------


detention %>% 
  ggplot() +
  geom_bar(aes(governate)) +
  coord_flip()

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

joined_data <-
  egypt %>% 
  left_join(detention, by = c("ADM1_AR" = "governate"))

sf_use_s2(FALSE)

joined_data %>%
  #as_tibble() %>% 
  group_by(ADM1_REF) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = 'Detainees') +
  theme_void() + 
  labs(title = 'Figure 2: Number of Detainees Per Governate') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom")


joined_data %>%
  as_tibble() %>% 
  group_by(ADM1_REF) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# Age ---------------------------------------------------------------------
joined_data$arbitrary <- "Y"

joined_data %>% 
  ggplot(aes(age, arbitrary)) + 
  geom_violin() +
  coord_flip()

joined_data %>% 
  ggplot(aes(age)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Age Distributions") +
  ggthemes::theme_economist()

joined_data %>% 
  ggplot(aes(age)) +
  geom_histogram(aes(y =..density..),
                 bins=25,
                 color="black", 
                 fill="dodgerblue",
                 alpha = 0.8) +
  labs(title="Figure 1: Distribution of Detainee Ages",
       x= "Age")+
  geom_density(color = "orange", size = 1.5) +
  ggthemes::theme_few() +
  theme(plot.title = element_text(hjust = 0.5)) 

# torture -----------------------------------------------------------------


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
            hanging = sum(hanging))

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
  labs(title="Figure 4: Subjection to Torture",
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
  rename("Poor Treatment" = Poor_treatment) %>%
  select(-Strike) %>% 
  pivot_longer(everything(), 
               names_to = "Condition",
               values_to = "Incidence") %>% 
  ggplot(aes(area = Incidence, fill = Condition,
             label = paste(Condition, Incidence, sep = "\n"))) +
  treemapify::geom_treemap() +
  treemapify::geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 15) +
  theme(legend.position = "none") +
  ggtitle("Figure 3: Prevalence of Issues in Detention Conditions") +
  theme(plot.title = element_text(hjust = 0.5))


# Legal Representation ----------------------------------------------------



# Enforced Disappearances -------------------------------------------------




# Socio Economic Factors --------------------------------------------------


  


