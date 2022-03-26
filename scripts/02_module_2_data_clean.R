# data clean
########################
# DH
# 3/25/2022
library(pacman)
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
head(surv_raw)
names(surv_raw)
surv_raw |> names()
# 可用于在指定列中快速制作值列表
# 告诉函数为列中的每个唯一值提供计数
surv_raw |> tabyl(gender)

# a histogram of the case ages 
ggplot(data = surv_raw, mapping = aes(x = age))+
     geom_histogram()
# a boxplot of the case age distribution
ggplot(data = surv_raw, mapping = aes(x = age))+
     geom_boxplot()

surv_raw |> tabyl("age unit")
class(surv_raw)
surv_raw$age
class(surv_raw$`onset date`)
class(surv_raw$"date of report")

ggplot(surv_raw, aes(x=`wt (kg)`)) + 
     geom_histogram()
# error ` ' "的区别
# ggplot(data = surv_raw, mapping = aes(x = 'wt (kg)'))+
#      geom_histogram()
ggplot(data=surv_raw, mapping = aes(x = `wt (kg)`)) +
     geom_histogram()

ggplot(data = surv_raw, mapping = aes(x = `ht (cm)`))+
     geom_histogram()

surv_raw %>% 
     tabyl(adm3_name_res, adm3_name_det)

surv <- surv_raw |> clean_names()
head(surv)
surv <- surv_raw |> 
     clean_names() |> 
     rename(
         date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det
     )
names(surv)
surv %>% 
     select(case_def, age, gender)
# keeps fever, vomit, and the columns in between them 
surv %>% 
     select(fever:vomit)

surv <- surv |> select(-row_num)
head(surv)

dim(surv)
surv <- surv |> distinct(date_onset)
dim(surv)
head(surv)

surv <- surv |> mutate(date_onset = mdy(date_onset))
surv <- surv |> mutate(date_onset = mdy(date_onset), date_report= mdy(date_report))
head(surv)

ggplot(data = surv, mapping = aes(x = date_onset))+
     geom_histogram()
class(surv$age)
surv <- surv |> mutate(age = as.numeric(age))
surv_raw %>% 
     tabyl(gender)
surv <- surv |> mutate(gender = na_if(gender, "")) |> tabyl(gender)
surv <- surv_raw |> mutate(across(.cols = where(is.character(), .fns=na_if, "")))
sum(is.na(surv$date_onset))
sum(is.na(surv$date_report))

head(surv)
surv <- surv |> mutate(diff = as.numeric(date_report - date_onset))
head(surv)

surv <- surv |> mutate(wt_kg = ifelse(wt_kg < 0, NA, wt_kg))
head(surv$wt_kg)
summary(surv$wt_kg)
tabyl(surv$wt_kg)

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

# Export cleaned file
rio::export(surv, here("data", "clean", "surveillance_linelist_clean_20141201.rds"))
