library(pacman)
p_load(
     rio,
     here,
     tidyverse,
     ggExtra,
     gghighlight,
     DataEditR
)

p_load(
     rio,
     here,
     skimr,
     janitor,
     lubridate,
     epikit,
     gtsummary,
     tidyverse
)

surv_raw <- import(here('data', "surveillance_linelist_12012014.csv"))

# import(here("data", "clean", "surveillance_linelist_clean_20141201.rds"))
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
     
     # convert age to numeric class
     mutate(age = as.numeric(age)) %>% 
     
     # properly record missing values
     mutate(across(.cols = where(is.character), .fns = na_if, "")) %>% 
     
     
     # Make date-difference column  
     mutate(diff = date_report - date_onset) %>% 
     
     # convert negative values to NA
     mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg),
            bmi   = ifelse(bmi < 0,   NA, bmi)) |> 
     
     mutate(gender = case_when(
          gender == "m" ~ "male",
          gender == "f" ~ "female",
          TRUE ~ gender)) |> 
     
     mutate(age_years = case_when(
          age_unit == "moonths" ~ age/12,
          age_unit == "years" ~ age,
          is.na(age_unit) ~ age,
          TRUE  ~ NA_real_
     )) |> 
     
     mutate(age_cat = age_categories(
          age_years,
          lower = 0,
          upper = 70,
          by = 10,
          above.char = "+"
     )) |> 
     
     mutate(moved = district_res != district_det) |> 
     
     mutate(district = coalesce(district_det, district_res)) |> 
     
     mutate(hospital = recode(hospital,
                              "Mitilary Hospital" = "Military Hospital",
                              "Port Hopital" = "Port Hospital",
                              "St. Mark's Maternity Hospital (SMMH)" = "SMMH",
                              "Port" = "Port Hospital"
                              # NA = 'Other'
     )) |> 
     filter(case_def == "Confirmed")

ggplot(data = surv, mapping = aes(x = district, y = age_years)) +
     # geom_point()
     # geom_jitter(color= "blue", size = 5, shape = 4) +
     # geom_boxplot(alpha = 0.5)
     geom_jitter() + 
     geom_violin(alpha = 0.5)

ggplot(data = surv, mapping = aes(x = gender, y = ht_cm)) +
     # geom_point()
     # geom_jitter(color= "blue", size = 5, shape = 4) +
     # geom_boxplot(alpha = 0.5)
     # geom_jitter() + 
     geom_violin(alpha = 0.5,szie =1)
