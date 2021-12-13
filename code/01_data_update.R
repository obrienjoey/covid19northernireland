source('code/00_source.R')

data_date = format(most_recent_weekday(Sys.Date()), "%d%m%y")

fname = paste0('https://www.health-ni.gov.uk/sites/default/files/publications/health/doh-dd-',
               data_date,'.xlsx')

GET(fname, write_disk(xls_file <- tempfile(fileext = ".xlsx")))

national_df <- national_df_collector(xls_file)
local_df <- local_df_collector(xls_file)
local_hospital_df <- local_hospital_df_collector(xls_file)
inpatient_df <- inpatient_df_collector(xls_file)

write_csv(national_df, 'data/ni_covid_national.csv')
write_csv(local_df, 'data/ni_covid_local.csv')
write_csv(local_hospital_df, 'data/ni_covid_hospital.csv')
write_csv(inpatient_df, 'data/ni_inpatients.csv')
