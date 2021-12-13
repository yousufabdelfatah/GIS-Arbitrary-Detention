
# load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)

# Load Data ---------------------------------------------------------------

# set file paths

path_165 <- 
  "Data\ Cleaning/Input/Case_165.xlsx"

path_123 <- 
  "Data\ Cleaning/Input/Case_123.xlsx"

path_16850 <- 
  "Data\ Cleaning/Input/Case_16850_2014.xlsx"

path_81 <- 
  "Data\ Cleaning/Input/Case_81_2016.xlsx"

# define function to read all sheets in an excel workbook

read_excel_allsheets <- 
  function(filename, tibble = FALSE) {
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
  }

# read all the sheets

case_165 <- 
  read_excel_allsheets(path_165)

case_123 <- 
  read_excel_allsheets(path_123)

case_16850 <- 
  read_excel_allsheets(path_16850)

case_81 <- 
  read_excel_allsheets(path_81)

# pull the relevant sheets from each workbook 

profile_165 <- 
  case_165[[1]]

enforced_disappearances_165 <- 
  case_165[[3]]

legal_rep_165 <- 
  case_165[[4]]

torture_165 <- 
  case_165[[5]]

detention_centers_165 <- 
  case_165[[6]]

profile_123 <- 
  case_123[[1]]

enforced_disappearances_123 <- 
  case_123[[3]]

legal_rep_123 <- 
  case_123[[4]]

torture_123 <- 
  case_123[[5]]

detention_centers_123 <- 
  case_123[[6]]

profile_16850 <- 
  case_16850[[1]]

enforced_disappearances_16850 <- 
  case_16850[[3]]

legal_rep_16850 <- 
  case_16850[[4]]

torture_16850 <- 
  case_16850[[5]]

detention_centers_16850 <-
  case_16850[[6]]

detention_centers_81 <- 
  case_81[[6]]

torture_81 <- 
  case_81[[7]]

legal_rep_81 <- 
  case_81[[8]]

enforced_disappearances_81 <- 
  case_81[[9]]

profile_81 <- 
  case_81[[11]]


# Clean all Data ----------------------------------------------------------

# save similar dfs together
# profiles <- ls(pattern = "prof")
# enforced_disappearances <- ls(pattern = "enforced")
# legal_rep <- ls(pattern = "legal")
# torture<- ls(pattern = "torture")
# detention <- ls(pattern = "detention")

# standardize column names

names(profile_165) <- 
  tolower(names(profile_165))

names(enforced_disappearances_165) <- 
  tolower(names(enforced_disappearances_165))

names(legal_rep_165) <- 
  tolower(names(legal_rep_165))

names(torture_165) <- 
  tolower(names(torture_165))

names(detention_centers_165) <- 
  tolower(names(detention_centers_165))

names(profile_123) <- 
  tolower(names(profile_123))

names(enforced_disappearances_123) <- 
  tolower(names(enforced_disappearances_123))

names(legal_rep_123) <- 
  tolower(names(legal_rep_123))

names(torture_123) <- 
  tolower(names(torture_123))

names(detention_centers_123) <- 
  tolower(names(detention_centers_123))

names(profile_16850) <- 
  tolower(names(profile_16850))

names(enforced_disappearances_16850) <- 
  tolower(names(enforced_disappearances_16850))

names(legal_rep_16850) <- 
  tolower(names(legal_rep_16850))

names(torture_16850) <- 
  tolower(names(torture_16850))

names(detention_centers_16850) <- 
  tolower(names(detention_centers_16850))

names(profile_81) <- 
  tolower(names(profile_81))

names(enforced_disappearances_81) <- 
  tolower(names(enforced_disappearances_81))

names(legal_rep_81) <- 
  tolower(names(legal_rep_81))

names(torture_81) <- 
  tolower(names(torture_81))

names(detention_centers_81) <- 
  tolower(names(detention_centers_81))

# rename misspelled variables

enforced_disappearances_123 <- 
  enforced_disappearances_123 %>% 
  rename(status = statud)

# keep relevant variables from the sheets

profile_81 <- 
  profile_81 %>% 
  select(name:`charge 17`)

profile_16850 <- 
  profile_16850 %>% 
  select(name: `charge 9`)

profile_165 <- 
  profile_165 %>% 
  select(name: `charge 21`)

profile_123 <- 
  profile_123 %>% 
  select(name: `charge 19`)

torture_123 <- 
  torture_123 %>% 
  select(name:date, 
         beating:med_request, 
         response_to_med_request)

torture_165 <- 
  torture_165 %>% 
  select(name, 
         status, 
         beating:med_request, 
         response_to_med_request, 
         med_confirm_torture)

torture_16850 <- 
  torture_16850 %>% 
  select(name, 
         status, 
         beating:hanging, 
         med_request, 
         response_to_med_request, 
         med_confirm_torture)

torture_81 <- 
  torture_81 %>% 
  select(name, 
         status, 
         beating:med_request, 
         response_to_med_request)

legal_rep_123 <- 
  legal_rep_123 %>% 
  select(name:prosecution_type, 
         first_interrogation, 
         lawyer_present, 
         lawyer_name, 
         lawyer_type, 
         why_no_lawyer, 
         reason_beginning)

legal_rep_165 <- 
  legal_rep_165 %>% 
  select(name, 
         status, 
         first_interrogation_date,
         lawyer_present,
         lawyer_name, 
         lawyer_type,
         why_no_lawyer,
         reason_beginning,
         lawyer_present_date,
         time_no_laywer)

legal_rep_16850 <- 
  legal_rep_16850 %>% 
  select(name, 
         status, 
         lawyer_present, 
         lawyer_name, 
         lawyer_type, 
         why_no_lawyer, 
         reason_beginning, 
         lawyer_attendance_date, 
         time_no_lawyer)

legal_rep_81 <- 
  legal_rep_81 %>% 
  select(name:first_interrogation, 
         lawyer_present:date_lawyer)

enforced_disappearances_123 <-
  enforced_disappearances_123 %>% 
  select(name, 
         status, 
         official_arrest,
         arrest_testimomy_date, 
         family_testimony_date, 
         official_arrest_location, 
         arrest_location_testimony, 
         days_disappeared, 
         location_disappeared)

enforced_disappearances_165 <-
  enforced_disappearances_165 %>% 
  select(name, 
         status, 
         official_arrest,
         arrest_testimomy_date, 
         family_testimony_date, 
         official_arrest_location,
         arrest_location_testimony, 
         location_disappeared, 
         days_disappeared, 
         difference)

enforced_disappearances_16850 <- 
  enforced_disappearances_16850 %>% 
  select(name, 
         status, 
         official_arrest, 
         arrest_testimony_date,
         days_disappeared, 
         family_testimony_date, 
         official_arrest_location, 
         arrest_location_testimony, 
         difference, 
         location_disappeared, 
         type_of_location)

enforced_disappearances_81 <- 
  enforced_disappearances_81 %>% 
  select(name, 
         status, 
         official_arrest,
         arrest_testimony_date,
         family_testimony_date,
         official_arrest_location,
         arrest_location_testimony, 
         difference_between_official_and_testimony, 
         days_disappeared, 
         location_disappeared, 
         assumed_disappearance_location)

detention_centers_123 <- 
  detention_centers_123 %>% 
  select(name:health, 
         solitary, 
         exercise, 
         education, 
         visitation, 
         food, 
         ventilation,
         poor_treatment, 
         overcrowding)

detention_centers_165 <-
  detention_centers_165 %>%
  select(name:health, 
         solitary, 
         strike, 
         exercise, 
         visitation, 
         education, 
         food, 
         ventilation, 
         poor_treatment, 
         overcrowding)

detention_centers_16850 <- 
  detention_centers_16850 %>% 
  select(name:health, 
         solitary,
         exercise, 
         visitation, 
         education, 
         food, 
         ventilation,
         poor_treatment,
         overcrowding)

detention_centers_81 <- 
  detention_centers_81 %>% 
  select(name:overcrowding)

# change values to numeric

torture_165$threaten_family[37] <- 1

torture_165$threaten_family[42] <- 1

torture_165$threaten_family <- 
  as.numeric(torture_165$threaten_family)

# change values to character in enforced disappearances

enforced_disappearances_123$official_arrest <- 
  as.character(enforced_disappearances_123$official_arrest)

enforced_disappearances_16850$official_arrest <- 
  as.character(enforced_disappearances_16850$official_arrest)

enforced_disappearances_81 <- 
  enforced_disappearances_81 %>% 
  separate(days_disappeared, c("days_disappeared", NA))

enforced_disappearances_81$days_disappeared <- 
  as.numeric(enforced_disappearances_81$days_disappeared)

enforced_disappearances_16850$days_disappeared <- 
  as.numeric(enforced_disappearances_16850$days_disappeared)

# Bind rows for various "portfolios"

profiles <-
  bind_rows(profile_123, 
            profile_165,
            profile_16850,
            profile_81)

legal_rep <- 
  bind_rows(legal_rep_123, 
            legal_rep_165, 
            legal_rep_16850, 
            legal_rep_81)

enforced_disappearances <- 
  bind_rows(enforced_disappearances_123,
            enforced_disappearances_165,
            enforced_disappearances_16850,
            enforced_disappearances_81)

detention_centers <- 
  bind_rows(detention_centers_123,
            detention_centers_165, 
            detention_centers_16850, 
            detention_centers_81)

torture <- 
  bind_rows(torture_123,
            torture_165, 
            torture_16850, 
            torture_81)


# Visualize Data Types ----------------------------------------------------

# lets look at types of data
visdat::vis_dat(torture)
visdat::vis_dat(profiles)
visdat::vis_dat(enforced_disappearances)
visdat::vis_dat(legal_rep)
visdat::vis_dat(detention_centers)
# this is because 16850 has a bunch of messed up columns
# here we can also see the distribution of NA values

# there are some columns where all values are NA so drop them

torture <- 
  torture %>% 
  drop_na(name)

legal_rep <-
  legal_rep %>% 
  drop_na(name)

detention_centers <- 
  detention_centers %>% 
  drop_na(name)

enforced_disappearances <- 
  enforced_disappearances %>% 
  drop_na(name)

profiles <- 
  profiles %>% 
  drop_na(name)

# Combine into one big csv

full <- 
  profiles %>% 
  left_join(detention_centers, by = 'name') %>% 
  left_join(legal_rep, by = 'name') %>% 
  left_join(enforced_disappearances, by = 'name') %>% 
  left_join(torture, by = 'name')

#let's look at the column names to see if there are redundancies

names(full)

# drop the excess statuses and date and page

full <- 
  full %>% 
  select(-c(status.x, 
            status.y, 
            status.x.x, 
            status.y.y, 
            date, 
            page, 
            assumed_disappearance_location, 
            date_lawyer))

# unite job and profession

full <- 
  full %>% 
  unite("profession", 
        c(job, profession), 
        sep = '', 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  na_if("")

#unite time_no_lawyer

full <- 
  full %>% 
  unite("time_no_lawyer", 
        c(time_no_laywer, time_no_lawyer), 
        sep = '', 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  na_if("")

#unite lawyer_present_date

full <- 
  full %>% 
  unite("lawyer_present_date", 
        c(lawyer_present_date, lawyer_attendance_date), 
        sep = '', 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  na_if("")

#unite arrest testimony date

full <- 
  full %>% 
  unite("arrest_testimony_date", 
        c(arrest_testimomy_date, arrest_testimony_date), 
        sep = '', 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  na_if("")

#unite difference

full <- 
  full %>% 
  unite("difference", 
        c(difference_between_official_and_testimony, difference), 
        sep = '', 
        remove = TRUE, 
        na.rm = TRUE) %>% 
  na_if("")

# convert the NAs to 0s to represent that there is no record and so we can visualize them in distributions/correlations
#[is.na(full)] <- 0

full

# we'll let age be NA because there 0 actually means something
# full$age[full$age == 0] <- NA

# lets put this in its own csv

full %>% 
  write_csv("Data\ Cleaning/Output/Full_Detention_Data.csv")

names(full)

# lets look at types of data

visdat::vis_dat(full)

# Socio-economic ----------------------------------------------------------

Wealth <- 
  read_csv("Data\ Cleaning/Input/GDL-Wealth.csv") %>% 
  select(Region, `Poverty (IWI<70)`:`Poverty (IWI<35)`)

Human_Development <- 
  read_csv("Data\ Cleaning/Input/GDL-Subnational-Human-Development-2020.csv") %>% 
  select(Region:`Mean Years Schooling`)

socio_economic <- 
  Wealth %>% 
  left_join(Human_Development, by = "Region") %>% 
  write_csv("Data\ Cleaning/Output/socio_economic.csv")
  

















