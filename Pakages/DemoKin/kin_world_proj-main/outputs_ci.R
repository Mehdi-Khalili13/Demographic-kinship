# generate a data frame with median and CI on living and dead for each kin, age_focal, age_kin, sex_kin
# save in Output folder

# aggregations
  # plot 1. country, year, age_focal. 
  # plot 2. country, year, age_focal, kin_grouped.
  # plot 3. regions, year, age_focal. kin_grouped_vert_horiz. 
  # plot 4. country, year, age_focal, kin_grouped, age_kin.
# measures: median, CI80% living

library(data.table)
library(tidyverse)
library(dtplyr)
library(furrr)

years <- trunc(seq(1952.5, 2097.5, 5))
past <- max(which(years<2022))

# country specific median and p80% -----------------------------------------------------

trajs_country <- list.files("trajs/kin_trajs_country/", pattern = "traj_", full.names = T)
plan(multisession, workers = 3)
tic <- Sys.time() # 11 hrs
future_map(trajs_country, function(i){
  
  # load
  load(i); print(i)

  # gral recode
  kin_country <- out %>% dtplyr::lazy_dt() %>% 
    mutate(kin = ifelse(kin %in% c("os","ys"), "s",
                        ifelse(kin %in% c("oa","ya"),"a",
                               ifelse(kin %in% c("coa","cya"), "c",
                                      ifelse(kin %in% c("nos","nys"), "n", kin)))))
  
  # past
  kin_country_past <- kin_country %>% 
    filter(year <= past, trajectory == 1)  %>% 
    summarise(living = sum(living),
              .by = c(LocationId, year, age_focal, kin, sex_kin, age_kin)) %>%
    as.data.table()
  
  # for plot: country, year, age_focal, kin_grouped, age_kin.
  kin_aggr <- kin_country %>% lazy_dt() %>% 
    filter(year > past) %>% 
    summarise(living = sum(living),
              .by = c(LocationId, year, age_focal, kin, sex_kin, age_kin, trajectory)) 
  rm(kin_country); rm(out)
  df_plot4 <- kin_aggr %>% 
    summarise(median_living = median(living, na.rm = T),
           ci_low_living = quantile(living, probs = .1, na.rm = T),
           ci_upp_living = quantile(living, probs = .9, na.rm = T),
           .by = c(LocationId, year, age_focal, kin, sex_kin, age_kin)) %>%
    as.data.table()
  trajs_plot4 <- kin_aggr %>% filter(trajectory<=100) %>% as.data.table()
  
  # for plot: country, year, age_focal, kin_grouped.
  kin_aggr <- kin_aggr %>%
    summarise(living = sum(living),
              .by = c(LocationId, year, age_focal, kin, trajectory))
  df_plot2 <- kin_aggr %>% 
    summarise(median_living = median(living, na.rm = T),
           ci_low_living = quantile(living, probs = .1, na.rm = T),
           ci_upp_living = quantile(living, probs = .9, na.rm = T),
           .by = c(LocationId, year, age_focal, kin)) %>%
    as.data.table()
  trajs_plot2 <- kin_aggr %>% filter(trajectory<=100) %>% as.data.table()
  
  # for plot: country, year, age_focal.
  kin_aggr <- kin_aggr %>%
    summarise(living = sum(living),
              .by = c(LocationId, year, age_focal, trajectory))
  df_plot1 <- kin_aggr %>% 
    summarise(median_living = median(living, na.rm = T),
           ci_low_living = quantile(living, probs = .1, na.rm = T),
           ci_upp_living = quantile(living, probs = .9, na.rm = T),
           .by = c(LocationId, year, age_focal)) %>%
    as.data.table()
  trajs_plot1 <- kin_aggr %>% filter(trajectory<=100) %>% as.data.table()
  rm(kin_aggr)

  # save results by country
  country <- substr(i, 11, str_locate(i, ".rdata")[,"start"]-1)
  save(kin_country_past,
       df_plot1, trajs_plot1,
       df_plot2, trajs_plot2,
       df_plot4, trajs_plot4,
       file = paste0("Output/CIs_", country, ".rda"), compress = TRUE)
})
toc <- Sys.time()
toc-tic

# regional plot --------------------------------------------------------------

# group kin type in horizontal and vertical

# load detailed projection for each country
countries <- list.files("Output/CIs_trajs_country", pattern = "traj", full.names = T)
df_plot3_countries <- map_df(countries, function(j){
  load(j); print(j)
  this_country <- substr(j, 31, str_locate(j, ".rda")[,"start"]-1)
  
  # sum of medians (not accurate but gives general sense)
  # vertical and horizontal
  df_plot3_country <- bind_rows(
    df_plot2 %>% 
      mutate(kin_vh = case_when(kin %in% c("s","c") ~ "h", TRUE ~ "v")) %>% 
      summarise(median_living = sum(median_living),
                ci_low_living = sum(ci_low_living),
                ci_upp_living = sum(ci_upp_living),
                .by = c(year, age_focal, kin_vh)),
    kin_country_past %>% 
      mutate(kin_vh = case_when(kin %in% c("s","c") ~ "h", TRUE ~ "v")) %>% 
      summarise(median_living = sum(living),
                .by = c(year, age_focal, kin_vh))
  ) %>% 
  mutate(country = this_country) %>% 
  arrange(year, age_focal, kin_vh)
  
  # add total kin
  df_plot3_country <- df_plot3_country %>% 
    bind_rows(
      df_plot3_country %>% 
      summarise(median_living = sum(median_living),
                ci_low_living = sum(ci_low_living),
                ci_upp_living = sum(ci_upp_living),
                .by = c(country, year, age_focal)) %>% 
      mutate(kin_vh = "all")  
    ) %>% 
    arrange(year, country, kin_vh, age_focal)
  df_plot3_country
})

# add pop weights by Focal to get regional values: "if I select at random a woman in the region..."
regions <- read.csv("Data/locations_with_regions.csv")
regions <- regions %>% 
  mutate(Region = case_when(Region %in% c("Europe", "Northern America") ~ "Europe & Northern America", T ~ Region))
pop_weights <- read.csv("Data/WPP2022_PopulationByAge5GroupSex_Medium.csv") %>% 
  select(country = LocID, year = Time, age_focal = AgeGrpStart, pop = PopTotal) %>% 
  filter(year %in% years) %>% 
  mutate(year = (year - 1952)/5,
         age_focal = age_focal/5) %>% 
  left_join(regions %>% rename(country = LocationId))

# group by regions with pop
df_plot3_regions <- df_plot3_countries %>% 
  mutate(country = as.integer(country)) %>% 
  left_join(pop_weights) %>% 
  mutate(median_living_region = sum(median_living*pop)/sum(pop),
         ci_low_living_region = sum(ci_low_living*pop)/sum(pop),
         ci_upp_living_region = sum(ci_upp_living*pop)/sum(pop),
         .by = c(Region, year, age_focal, kin_vh))

# world count
df_plot3_world <- df_plot3_countries %>% 
  mutate(country = as.integer(country)) %>% 
  left_join(pop_weights) %>% 
  mutate(median_living_region = sum(median_living*pop)/sum(pop),
         ci_low_living_region = sum(ci_low_living*pop)/sum(pop),
         ci_upp_living_region = sum(ci_upp_living*pop)/sum(pop),
         .by = c(year, age_focal, kin_vh)) %>% 
  mutate(Region = "World")

# bind and save
df_plot3_regions <- bind_rows(df_plot3_regions, df_plot3_world)
save(df_plot3_regions, file = "Output/CIs_regions.rda")


