library(tidyverse)
library(janitor)
library(padr)
library(zoo)
library(readxl)
library(httr)
library(here)

palette = c('#264653',
            '#2a9d8f',
            '#e9c46a',
            '#f4a261',
            '#e76f51')

ablack = '#202020'

most_recent_weekday <- function(date){
  
  if(weekdays(date) == "Saturday"){
    return(date - 1)
  }else if(weekdays(date) == "Sunday"){
    return(date - 2)
  }else{
    return(date)
  }
  
}

national_df_collector <- function(xls_file){
  
  test_df <- read_excel(xls_file, sheet = "Summary Tests") %>%
    clean_names() %>%
    select(date = sample_date,
           cases = individuals_tested_positive,
           tests = all_individuals_tested,
           cases_per_100k = positivity_rate_per_100k_pop
    ) %>%
    pad('day') %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  death_df <- read_excel(xls_file, sheet = "Deaths") %>%
    clean_names() %>%
    select(date = date_of_death,
           deaths = number_of_deaths
    ) %>%
    pad('day') %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths))
  
  inpatients_df <- read_excel(xls_file, sheet = "Inpatients") %>%
    clean_names() %>%
    select(date = inpatients_at_midnight,
           covid_patients = number_of_confirmed_covid_inpatients,
           gender = sex
    ) %>%
    filter(gender == 'All') %>%
    select(date, covid_patients) %>%
    pad('day') %>%
    group_by(date) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    summarise(covid_patients = sum(covid_patients))
  
  icu_df <- read_excel(xls_file, sheet = "ICU") %>%
    clean_names()  %>%
    select(date,
           covid_ICU = confirmed_covid_occupied,
           non_covid_ICU = non_covid_occupied, 
           unoccupied_ICU_beds = unoccupied_beds
    ) %>%
    fill(covid_ICU:unoccupied_ICU_beds) %>%
    pad('day')
  
  ventilator_df <- read_excel(xls_file, sheet = "Ventilator Use") %>%
    clean_names() %>%
    select(date,
           covid_ventilator = confirmed_covid_ventilated,
           non_covid_ventilator = non_covid_ventilated, 
           total_ventilated
    ) %>%
    pad('day')
  
  beds_df <- read_excel(xls_file, sheet = "General Beds") %>%
    clean_names() %>%
    select(date,
           covid_beds = confirmed_covid_occupied,
           non_covid_beds = non_covid_occupied, 
           awaiting_admission,
           unoccupied_beds
    ) %>%
    pad('day') %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    filter(date != as.Date('2020-10-18')) %>%
    pad('day') 
  
  care_home_df <- read_excel(xls_file, sheet = "Care Homes") %>%
    clean_names() %>%
    select(date,
           care_homes_outbreaks = number_of_care_homes_with_covid_19_outbreaks
    ) %>%
    pad('day') %>%
    fill(care_homes_outbreaks)
  
  full_df <- list(test_df, death_df, inpatients_df, icu_df,
                  ventilator_df, beds_df, care_home_df) %>% 
    reduce(full_join, by = "date") %>%
    unique() %>%
    mutate(date = as.Date(date))
  
  return(full_df)
}

local_df_collector = function(xls_file){
  
  case_df <- read_excel(xls_file, sheet = "Tests") %>%
                clean_names() %>%
                select(date = date_of_specimen,
                     area = lgd2014name,
                     cases = individ_with_positive_lab_test,
                     tests = individ_with_lab_test,
                ) %>%
                pad(group="area") %>%
                mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
  max_date <- max(case_df$date)
  
  death_df <- read_excel(xls_file, sheet = "Deaths") %>%
                clean_names() %>%
                select(date = date_of_death,
                       deaths = number_of_deaths,
                       area = lgd
                ) %>%
                pad(group="area") %>%
                mutate(deaths = replace_na(deaths, 0)) %>%
                group_by(date, area) %>%
                summarise(deaths = sum(deaths)) %>%
                ungroup() %>%
                filter(date <= max_date)
  
  full_df = full_join(case_df, death_df,
                 by = c('date', 'area')
  ) %>%
    mutate(date = as.Date(date),
           deaths = replace_na(deaths, 0))
  
  return(full_df)
  
}

local_hospital_df_collector = function(xls_file){

  inpatient_data <- read_excel(xls_file, sheet = "Inpatients") %>%
    clean_names() %>%
    select(date = inpatients_at_midnight,
           covid_patients = number_of_confirmed_covid_inpatients,
           age_band,
           hsc_trust,
           gender = sex
    ) %>%
    pad('day') %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
    
  admission_data <- read_excel(xls_file, sheet = "Admissions",
                     col_types = c('date', rep('guess',2), 'date', rep('guess', 3))) %>%
            clean_names() %>%
            select(admission_date,
                   discharge_date,
                   hsc_trust,
                   hospital,
                   age_band,
                   gender = sex,
                   number_of_admissions
            ) %>%
            mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(admission_date = as.Date(admission_date),
           discharge_date = as.Date(discharge_date))
  
  return(admission_data)
}
