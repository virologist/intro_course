
###################################
# Ebola outbreak case study
# Script after data cleaning module
###################################

# load packages
###############

pacman::p_load(
     rio,          # for importing data
     here,         # for locating files
     skimr,        # for reviewing the data
     janitor,      # for data cleaning  
     lubridate,    # for date cleaning  
     epikit,       # creating age categories
     gtsummary,    # creating tables  
     scales,       # percents in tables  
     flextable,    # for making pretty tables
     tidyverse     # for data management and visualization
)



# import raw data
#################
surv_raw <- import(here("data", "raw", "surveillance_linelist_20141201.csv"))


# preliminary look at data  
##########################
# column names
surv_raw %>% names()

# gender values
surv_raw %>% 
     tabyl(gender)

# check class of certain columns
class(surv_raw$`onset date`)

# compare consistency across two location columns
surv_raw %>% 
     tabyl(adm3_name_res, adm3_name_det)





# Make clean dataset
####################
surv <- surv_raw %>% 
     
     # automatically clean column names
     clean_names() %>% 
     
     # manually clean column names   
     rename(
          date_onset = onset_date,
          date_report = date_of_report,
          district_res = adm3_name_res,
          district_det = adm3_name_det) %>%
     
     # remove unnecessary column
     select(-row_num) %>% 
     
     # de-duplicate rows  
     distinct() %>% 
     
     # convert date_onset to date class
     mutate(date_onset = mdy(date_onset)) %>% 
     mutate(date_report = mdy(date_report)) %>% 
     
     # create epiweek columns  
     mutate(week_onset = floor_date(date_onset, unit = "week", week_start = 1)) %>% 
     mutate(week_report = floor_date(date_report, unit = "week", week_start = 1)) %>% 
     
     # convert age to numeric class
     mutate(age = as.numeric(age)) %>% 
     
     # properly record missing values
     mutate(across(.cols = where(is.character), .fns = na_if, "")) %>% 
     
     # Make date-difference column  
     mutate(diff = date_report - date_onset) %>% 
     
     # convert negative values to NA
     mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg),
            bmi   = ifelse(bmi < 0,   NA, bmi)) %>% 
     
     # convert gender values to full words
     mutate(gender = case_when(               # re-define gender as: 
          gender == 'm' ~ 'male',                # when "m", change to "male"   
          gender == 'f' ~ 'female',              # when "f", change to "female" 
          TRUE          ~ gender)) %>%           # any other value, remain as before
     
     # create age-in-years
     mutate(age_years = case_when(
          age_unit == "years"  ~ age,            # if age is given in years
          age_unit == "months" ~ age/12,         # if age is given in months
          is.na(age_unit)      ~ age,            # if age unit is missing, assume years
          TRUE                 ~ NA_real_)) %>%  # any other circumstance, assign missing
     
     # create age category column
     mutate(age_cat = age_categories(         # create new column
          age_years,                             # numeric column to make groups from
          lower = 0,
          upper = 70,
          by = 10)) %>% 
     
     # create column marking TRUE if district of residence and detection differ
     mutate(moved = district_res != district_det) %>% 
     
     # create new column that prioritizes district of detection
     mutate(district = coalesce(district_det, district_res)) %>% 
     
     # re-code hospital column
     mutate(hospital = recode(hospital,
                              # for reference: OLD = NEW
                              "Mitilary Hospital"  = "Military Hospital",
                              "Port"               = "Port Hospital",
                              "Port Hopital"       = "Port Hospital",
                              "St. Mark's Maternity Hospital (SMMH)" = "SMMH")) %>% 
     
     # remove suspect cases
     filter(case_def == "Confirmed")







# Export cleaned file  
#####################
rio::export(surv, here("data", "clean", "surveillance_linelist_clean_20141201.rds"))




# Descriptive tables
####################

# counts by district, using {janitor}
district_table <- surv %>% 
     tabyl(district) %>% 
     arrange(desc(n)) %>% 
     adorn_totals() %>% 
     adorn_pct_formatting() %>% 
     qflextable()

# print
district_table

# save
save_as_docx(district_table, path = "district_table.docx")



# table using {dplyr}
hospital_table <- surv %>% 
     group_by(hospital) %>%                                     # get statistics for each hospital
     summarise(
          n_cases   = n(),                                         # number of rows (cases)
          max_onset = max(date_onset, na.rm = T),                  # latest onset date
          under5    = sum(age_years <= 5, na.rm = T),              # number of children under 5
          vomit_n   = sum(vomit == "yes", na.rm=T),                # number vomiting
          vomit_pct = percent(vomit_n / n_cases),                  # percent vomiting
          max_wt_male = max(wt_kg[gender == "male"], na.rm = T)) %>%     # max weight among men
     flextable::qflextable()

# print
hospital_table

# save
save_as_docx(hospital_table, path = "hospital_table.docx")




# table using gtsummary
surv %>% 
     select(district, age_cat, fever, chills, cough, aches, vomit, wt_kg, ht_cm, gender) %>% 
     tbl_summary(by = gender)





