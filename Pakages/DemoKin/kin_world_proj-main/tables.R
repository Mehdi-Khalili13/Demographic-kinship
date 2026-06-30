library(tidyverse)
library(kableExtra)
library(data.table)
library(dtplyr)
library(purrr)
library(future)

# table 1 -----------------------------------------------------------------

# countries
regions <- read.csv("Data/locations_with_regions.csv")
countries_files <- list.files("Output/CIs_trajs_country", pattern = ".rda", full.names = T)
countries_table1 <- as.integer(substr(countries_files, 31, str_locate(countries_files, ".rda")[,"start"]-1))
countries_table1 <- countries_table1[!is.na(countries_table1)]

# table_data_agg: kin count by country and focal age
past_data_aggr <- map_df(countries_table1, function(c){
  print(c)
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
    kin_country_past %>% 
      summarise(living = sum(living), 
                .by = c(LocationId, year, age_focal, kin))}) %>% 
  rename(country = LocationId, count_living = living) %>% 
  mutate(Variant = "Estimate")
proj_data_aggr <- map_df(countries_table1, function(c){
  print(c)
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  df_plot2}) %>% 
  pivot_longer(median_living:ci_upp_living) %>%  
  rename(country = LocationId, Variant = name, count_living = value)

table_data_agg <- bind_rows(past_data_aggr, proj_data_aggr)
rm(past_data_aggr); rm(proj_data_aggr)
save(table_data_agg, file = "Tables/table_data_agg.rda")
rm(table_data_agg)

# Table 1: save a table for each focal age
load("Tables/table_data_agg.rda")
table1_data <- table_data_agg %>% filter(year %in% c(0, 14, 29))
rm(table_data_agg)
for(age in c(0, 7, 13)){
  this_age <- paste0(age * 5, "-", age * 5 + 4) 
  table_data_age <- table1_data %>% 
    left_join(fertestr::locs_avail() %>% 
                mutate(location_code = as.integer(location_code)) %>% 
                select(country = location_code, Country = location_code_iso3)) %>% 
    filter(age_focal %in% age) %>% 
    mutate(year = 1950 + year * 5,
           Year = paste0(year, "-", year + 5),
           Country = ifelse(Country == "United States of America", "USA", Country))
  
  # table 1
  table_data_age <- table_data_age %>% 
    filter(Variant %in% c("Estimate","median_living"),
           # kin %in% c("m", "s", "d", "gd", "gm")
           ) %>% 
    arrange(Country, year, kin) %>% 
    select(ISO = Country, Year, count_living, kin) %>% 
    pivot_wider(names_from = kin, values_from = count_living)
  
  # latex table 1. codigos en iso y todos los parientes
  table_latex <- table_data_age %>% 
    kbl(digits = 2, longtable = T, 
        # col.names = c("Country", "Year", "Offspring", "Grandchildern", "Granparents", "Parents", "Siblings"),
        booktabs = T
        , format = "latex"
        ) %>%
    collapse_rows(columns = 1, 
                  latex_hline = "major", valign = "middle") %>% 
    kable_styling(
                  latex_options = c("repeat_header"), 
                  font_size = 12)
  
  # write
  writeLines(table_latex, paste0("Tables/table_paper_",this_age,'.tex'))
}

# table aggr csv -----------------------------------------------------------------

load("Data/locs_codes.rda")
load("Tables/table_data_agg.rda")
table_agg_csv <- table_data_agg %>% 
  lazy_dt() %>% 
  mutate(year = paste0(1950 + year * 5, "-", 1950 + year * 5 + 5),
        age_focal = paste0(age_focal * 5, "-", age_focal * 5 + 4)) %>% 
  left_join(locs_codes %>% 
              mutate(location_code = as.integer(location_code)) %>% 
              select(country = location_code, Country = location_code_iso3)) %>%
  select(-country) %>% 
  pivot_wider(names_from = Country, values_from = count_living) %>% 
  as.data.frame()
fwrite(table_agg_csv, file = "Tables/table_data_agg.csv")  

# table desaggr csv -----------------------------------------------------------------

# countries
countries_files <- list.files("Output/CIs_trajs_country", pattern = ".rda", full.names = T)
countries_table <- as.integer(substr(countries_files, 31, str_locate(countries_files, ".rda")[,"start"]-1))
countries_table1 <- as.integer(substr(countries_files, 31, str_locate(countries_files, ".rda")[,"start"]-1))
countries_table1 <- countries_table1[!is.na(countries_table1)]
countries_table <- countries_table1[!is.na(countries_table1)]

# table_data_agg
table_data_desagg <- map_df(countries_table, function(c){
  print(c)
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  past_data_desaggr <- kin_country_past %>% 
                      rename(country = LocationId, count_living = living) %>% 
                      mutate(Variant = "Estimate")
  proj_data_desaggr <- df_plot4 %>% 
    pivot_longer(median_living:ci_upp_living) %>%  
    rename(country = LocationId, Variant = name, count_living = value)
  bind_rows(past_data_desaggr, proj_data_desaggr) %>% 
    mutate(year = paste0(1950 + year * 5, "-", 1950 + year * 5 + 5),
           age_focal = paste0(age_focal * 5, "-", age_focal * 5 + 4),
           age_kin = paste0(age_kin * 5, "-", age_kin * 5 + 4))
})
save(table_data_desagg, file = "Tables/table_data_desagg.rda")
rm(table_data_desagg)

# write csv
load("Tables/table_data_desagg.rda")
table_data_desagg_csv <- table_data_desagg %>% 
  lazy_dt() %>% 
  left_join(locs_codes %>% 
              mutate(location_code = as.integer(location_code)) %>% 
              select(country = location_code, Country = location_code_iso3)) %>%
  select(-country) %>% 
  pivot_wider(names_from = Country, values_from = count_living) %>% 
  as.data.table()

fwrite(table_data_desagg_csv, file = "Tables/table_data_desagg.csv")
