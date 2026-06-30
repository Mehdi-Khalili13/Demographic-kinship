library(ggrepel)
library(tidyverse)
library(patchwork)
library(DemoKin)
library(scales)

# plot 1. Multi-country all kin projections ------------------------------------------------------------------

regions <- read.csv("Data/locations_with_regions.csv")
countries_plot1 <- c(716, 156, 356, 380, 840)
ybr <- seq(1950, 2100, 25)

d1_all_ages <- map_df(countries_plot1, function(c){
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  bind_rows(
    trajs_plot1,
    kin_country_past %>% 
      summarise(living = sum(living), 
                .by = c(LocationId, year, age_focal)) %>% 
      mutate(trajectory = 1))
  }) %>% 
  mutate(year = 1950 + year * 5 + 2.5) %>% 
  rename(country = LocationId, count_living = living, traj = trajectory) %>% 
  left_join(regions %>% rename(country = LocationId)) %>% 
  mutate(Country = ifelse(Country == "United States of America", "USA", Country))

d2_all_ages <- map_df(countries_plot1, function(c){
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  df_plot1}) %>% 
  mutate(year = 1950 + year * 5 + 2.5) %>% 
  pivot_longer(median_living:ci_upp_living) %>%  
  rename(country = LocationId, Variant = name, count_living = value) %>% 
  left_join(regions %>% rename(country = LocationId)) %>% 
  mutate(Country = ifelse(Country == "United States of America", "USA", Country))

for(age in c(0, 7, 13)){
  this_age <- paste0(age * 5) 
  this_age_out <- paste0(age * 5, "-", age * 5 + 4) 
  d1 <- d1_all_ages %>% filter(age_focal == age)
  d2 <- d2_all_ages %>% filter(age_focal == age)
  d2_max <- max(d2$count_living[d2$year>=2050])
  
  p1 <- ggplot() +
    geom_rect(aes(xmin=2022, xmax=Inf, ymin=-Inf, ymax=Inf), fill="grey", alpha = 0.2) +
    geom_vline(xintercept = 2022, col="grey", linetype = 2) +
    # TRAJS
    geom_line(
      data = d1 %>% filter(year > 2022.5),
      aes(x = year, y = count_living, group = interaction(traj, Country))
      , col="grey", linewidth = 0.002, alpha=.5) +
    # hist lines
    geom_line(
      data = d1 %>% filter(year < 2030),
      aes(x = year, y = count_living, group = interaction(traj, Country), col=Country)
      , linewidth = .8
      ) +
    # proj line median
    geom_line(
      data = d2,
      aes(x = year, y = count_living, group = interaction(Variant, Country), col=Country, linetype=Variant)
      , linewidth = .8
      ) +
    geom_label(
      data = d1 %>% filter(year == 1977.5)
      , aes(x = year, y = count_living, label = Country)
      , nudge_y = 0
      , size = 6
      , show.legend = F) +
    geom_rect(
      aes(xmin=2050, xmax=2097.5, ymin=10, ymax=d2_max)
      , colour = "black", fill = NA, linetype = "dashed"
    ) +
    scale_color_manual(values = 2:6) +
    scale_linetype_manual(values = c(2,2,1), labels = c("lower 80%", "upper 80%", "median")) +
    scale_x_continuous(breaks = ybr) +
    scale_y_continuous(breaks = scales::pretty_breaks(n=6)) +
    labs(y=paste0("Number of kin for a woman aged ",this_age), x= "Year") +
    guides(colour = "none", shape = "none", linetype = "none") +
    theme_classic(base_size = 15) +
    theme(legend.position = "bottom", 
          text = element_text(size = 20))
  d3 <-
    d2 %>% 
    filter(
      year >= 2050
      ) %>% 
    pivot_wider(names_from = Variant, values_from = count_living) %>% 
    select(Country, year, ymin = contains("low"), ymax = contains("upp"))
  
  p2 <-
    ggplot() +
    geom_ribbon(
      aes(x = year, ymin = ymin, ymax = ymax, group = Country, fill = Country)
      , alpha = 0.6
      , data = d3
    ) +
    geom_line(
      data = d2 %>% 
        filter(
          year >= 2050, Variant == "median_living"
          ),
      aes(x = year, y = count_living, group = interaction(Variant, Country), col = Country)
      , linewidth = 1
      , show.legend = F
      , colour = "black"
    ) +
    geom_label_repel(
      data = d2 %>% 
        filter(
          year == 2057.5, Variant == "median_living"
          )
      , aes(x = year, y = count_living, label = Country)
      , nudge_y = 0
      , size = 5
      , show.legend = F
    ) +
    scale_x_continuous(breaks = c(2050, 2075, 2100)) +
    scale_fill_manual(values = 2:6) +
    labs(y="", x= "") +
    guides(colour = "none", shape = "none", linetype = "none", fill = "none") +
    coord_cartesian(xlim = c(2050, 2100)) +
    theme_bw() %+replace% 
    theme(axis.ticks = element_blank()
          , legend.background = element_blank()
          , legend.key = element_blank()
          , panel.background = element_blank()
          , strip.background = element_blank()
          , plot.background = element_blank()
          , complete = TRUE,
          text = element_text(size = 20)
    )
  plot1 <- p1 + inset_element(p2, left = 0.55, bottom = 0.45, top = 1, right = 1)
  ggsave(filename = paste0("Plots/plot1_",this_age_out,".pdf"), plot = plot1, width = 9, height = 7, units = "in")
}

# plot 2 Country-level kin composition ------------------------------------------------------------------

countries_plot2 <- c(76, 716, 156, 356, 380, 840)
age_keep <- c(0, 7, 13)
ybr <- seq(1950, 2100, 25)
demokin_codes <- DemoKin::demokin_codes %>% 
  select(kin = DemoKin, kin_label = Labels_female) %>% 
  mutate(kin_label = case_when(kin_label == "Mother" ~ "Parents",
                               kin_label == "Grandmothers" ~ "Grandparents",
                               kin_label == "Great-grandmothers" ~ "Great-grandparents",
                               kin_label == "Grand-daughters" ~ "Grandchildren",
                               kin_label == "Great-grand-daughters" ~ "Great-grandchildren",
                               kin_label == "Daughters" ~ "Offspring",
                               kin_label == "Sisters" ~ "Siblings",
                               kin_label == "Aunts" ~ "Aunts/Uncles",
                               kin_label == "Nieces" ~ "Niblings",
                               T ~ kin_label))

traj_dat_all_countries <- map_df(countries_plot2, function(c){
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  bind_rows(
    trajs_plot2,
    kin_country_past %>% 
      summarise(living = sum(living), 
                .by = c(LocationId, year, age_focal, kin)) %>% 
      mutate(trajectory = 1))}) %>% 
  mutate(year = 1950 + year * 5 + 2.5) %>% 
  rename(country = LocationId, count_living = living, traj = trajectory) %>% 
  left_join(demokin_codes) %>% 
  filter(age_focal %in% age_keep)

var_dat_all_countries <- map_df(countries_plot2, function(c){
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  df_plot2}) %>% 
  mutate(year = 1950 + year * 5 + 2.5) %>% 
  pivot_longer(median_living:ci_upp_living) %>%  
  rename(country = LocationId, Variant = name, count_living = value) %>% 
  left_join(demokin_codes) %>% 
  filter(age_focal %in% age_keep)

# pick proper type for each age
traj_dat_all_countries <- traj_dat_all_countries %>% 
  filter((age_focal == 0 & kin_label %in% c("Grandparents", "Great-grandparents", "Siblings", "Aunts/Uncles", "Cousins")) |
         (age_focal == 7 & kin_label %in% c("Grandparents", "Parents", "Siblings", "Offspring")) |
         (age_focal == 13 & kin_label %in% c("Offspring", "Parents", "Siblings", "Grandchildren", "Niblings")))
var_dat_all_countries <- var_dat_all_countries %>% 
  filter((age_focal == 0 & kin_label %in% c("Grandparents", "Great-grandparents", "Siblings", "Aunts/Uncles", "Cousins")) |
           (age_focal == 7 & kin_label %in% c("Grandparents", "Parents", "Siblings", "Offspring")) |
           (age_focal == 13 & kin_label %in% c("Offspring", "Parents", "Siblings", "Siblings", "Grandchildren", "Niblings")))
colors_kin <- tibble(kin_label = unique(var_dat_all_countries$kin_label), color = 2:(length(kin_label)+1))

iter <- 1
for(c in countries_plot2){
  if(c == 156 & iter == 1){
    this_country <- regions$Country[regions$LocationId==c]
    traj_dat <- traj_dat_all_countries %>% filter(country == c, age_focal != 7)
    var_dat <- var_dat_all_countries %>% filter(country == c, age_focal != 7)
    size_font <- 18
    iter <- iter + 1 
    this_country <- paste0(this_country,"_paper")
    facet_labs <- as_labeller(c(`0` = paste0("A. Newborn Focal (aged 0)"), 
                                `13` =   paste0("B. Focal aged 65")))
    rect_data <- data.frame(xmin=2022, xmax=Inf, ymin=-Inf, ymax=Inf, age_focal = c(0, 13))
  }else{
    this_country <- regions$Country[regions$LocationId==c]
    traj_dat <- traj_dat_all_countries %>% filter(country == c)
    var_dat <- var_dat_all_countries %>% filter(country == c)
    size_font <- 12
    facet_labs <- as_labeller(c(`0` = paste0("A. Newborn Focal  (aged 0)"), 
                                `7` =   paste0("B. Focal aged 35"),
                                `13` =   paste0("C. Focal aged 65")))
    rect_data <- data.frame(xmin=2022, xmax=Inf, ymin=-Inf, ymax=Inf, age_focal = c(0, 7, 13))
  }
  
  p <- 
    ggplot() +
    geom_rect(
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
      , fill="grey", alpha = 0.2
      , data = rect_data 
    ) +
    geom_vline(xintercept = 2022, col="grey", linetype = 2) +
    # Trajectories
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Aunts/Uncles") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Siblings") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Parents") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Grandparents") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Great-grandparents") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Cousins") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Niblings") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Grandchildren") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% filter(kin_label == "Great-grandchildren") 
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj), col="grey", linewidth = 0.2, alpha=.2
      , data = traj_dat %>% 
        filter(kin_label == "Offspring") 
    ) +
    # previous to projec
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Aunts/Uncles", year<=2030), col = colors_kin$color[colors_kin$kin_label == "Aunts/Uncles"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Siblings", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Siblings"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Parents", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Parents"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj) 
      , data = traj_dat %>% filter(kin_label == "Grandparents", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Grandparents"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Great-grandparents", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Great-grandparents"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Cousins", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Cousins"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Niblings", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Niblings"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Grandchildren", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Grandchildren"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% filter(kin_label == "Great-grandchildren", year<=2030) , col = colors_kin$color[colors_kin$kin_label == "Great-grandchildren"]
    ) +
    geom_line(
      aes(x = year, y = count_living, group = traj)
      , data = traj_dat %>% 
        filter(kin_label == "Offspring", year<=2030) 
      , col = colors_kin$color[colors_kin$kin_label == "Offspring"]
    )+
    # Median values
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Aunts/Uncles"]
      , data = var_dat %>% filter(kin_label == "Aunts/Uncles")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Siblings"]
      , data = var_dat %>% filter(kin_label == "Siblings")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Parents"]
      , data = var_dat %>% filter(kin_label == "Parents")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Grandparents"]
      , data = var_dat %>% filter(kin_label == "Grandparents")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Great-grandparents"]
      , data = var_dat %>% filter(kin_label == "Great-grandparents")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Cousins"]
      , data = var_dat %>% filter(kin_label == "Cousins")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Niblings"]
      , data = var_dat %>% filter(kin_label == "Niblings")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Grandchildren"]
      , data = var_dat %>% filter(kin_label == "Grandchildren")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Great-grandchildren"]
      , data = var_dat %>% filter(kin_label == "Great-grandchildren")
    ) +
    geom_line(
      aes(x = year, y = count_living, linetype = Variant), col = colors_kin$color[colors_kin$kin_label == "Offspring"]
      , data = var_dat %>% filter(kin_label == "Offspring")
    ) +
    geom_label_repel(
      aes(x = year, y = count_living, label = kin_label)
      , data = traj_dat %>% 
        filter(traj == 1, year == 1977.5)  %>% 
        mutate(
          kin_label = ifelse(kin_label == "Offspring", "Children", kin_label)
          # , kin_label = ifelse(kin_label == "Great-grandparents", "G-grandparents", kin_label)
          )
    ) +
    scale_linetype_manual(values = c(2,2,1)) +
    scale_x_continuous(breaks = ybr, labels = ybr) +
    scale_y_continuous(breaks = scales::pretty_breaks(n=6)) +
    labs(y="Mean number of kin", x="Year") +
    facet_wrap(.~age_focal, scales = "free", labeller = facet_labs) +
    coord_cartesian() +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = size_font)) 
  ggsave(filename = paste0("Plots/plot2_",this_country,".pdf"),plot = p, width = 8, height = 5, units = "in")
}

# plot 3. Regional projections ------------------------------------------------------------------

load("Output/CIs_regions.rda")
d1 <- df_plot3_regions %>% 
  mutate(year = 1950 + year * 5 + 2.5,
         kin_vh = case_when(kin_vh == 'h' ~ 'Horizontal',
                            kin_vh == 'v' ~ 'Vertical',
                            T ~ 'All kin'),
         Regions = case_when(Region == "Africa" ~ "A. Africa",
                            Region == 'Asia' ~ 'B. Asia',
                            Region == 'Europe & Northern America' ~ 'C. Europe & Northern America',
                            Region == 'Latin America and the Caribbean' ~ 'D. Latin America and the Caribbean',
                            Region == 'Oceania' ~ 'E. Oceania',
                            Region == 'World' ~ 'F. World', T ~ "none"),
         Regions_label = case_when(Region == "Africa" ~ "A",
                            Region == 'Asia' ~ 'B',
                            Region == 'Europe & Northern America' ~ 'C',
                            Region == 'Latin America and the Caribbean' ~ 'D',
                            Region == 'Oceania' ~ 'E',
                            Region == 'World' ~ 'F', T ~ "none"))
size_font <- 14
ages_plot3 <- c(0, 7, 13)
for(age in ages_plot3){
  this_age <- paste0(age * 5)
  this_age_out <- paste0(age * 5, "-", age * 5 + 4)
  d1_all <- d1 %>% filter(age_focal == age, kin_vh == 'All kin')
  p_all <- ggplot()+
    geom_rect(
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
      , fill="grey", alpha = 0.1
      , data = rect_data ) +
    geom_vline(xintercept = 2022, col="grey", linetype = 2) +
    geom_line(data = d1_all,
              aes(year, median_living, group = Country), 
              col = 'lightblue', linewidth = 0.005, alpha = 0.7) +
    geom_line(data = d1_all %>% 
                distinct(year, Regions, kin_vh, median_living_region),
              aes(year, median_living_region)
              , linewidth = .8
              ) +
    geom_line(data = d1_all %>%
                distinct(year, Regions, kin_vh, ci_low_living_region),
              aes(year, ci_low_living_region)
              , col = 2, linetype = 2
              , linewidth = .8
              ) +
    geom_line(data = d1_all %>%
                distinct(year, Regions, kin_vh, ci_upp_living_region),
              aes(year, ci_upp_living_region)
              , linetype = 2, col = 2
              , linewidth = .8
              ) +
    ylab(paste0('Mean number of kin for a woman aged ',this_age)) + xlab('Year') +
    facet_wrap(~Regions) + 
    theme_bw() +
    scale_y_continuous(breaks = scales::pretty_breaks(n=5)) +
    scale_x_continuous(breaks = seq(1950, 2100, 25)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = size_font))
  
  d1_vh <- d1 %>% filter(age_focal == age, kin_vh != 'All kin')
  p_vh <- ggplot()+
    geom_rect(
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)
      , fill="grey", alpha = 0.1
      , data = rect_data ) +
    geom_vline(xintercept = 2022, col="grey", linetype = 2) +
    geom_line(data = d1_vh,
              aes(year, median_living, group = Country), 
              col = 'lightblue', linewidth = 0.005, alpha = 0.7) +
    geom_line(data = d1_vh %>% 
                distinct(year, Regions_label, kin_vh, median_living_region),
              aes(year, median_living_region)) +
    geom_line(data = d1_vh %>%
                distinct(year, Regions_label, kin_vh, ci_low_living_region),
              aes(year, ci_low_living_region), col = 2, linetype = 2) +
    geom_line(data = d1_vh %>%
                distinct(year, Regions_label, kin_vh, ci_upp_living_region),
              aes(year, ci_upp_living_region), linetype = 2, col = 2) +
    ylab(paste0('Mean number of kin for a woman aged ',this_age)) + xlab('Year') +
    scale_y_continuous(breaks = scales::pretty_breaks(n=4)) +
    facet_grid(rows = vars(Regions_label), cols=vars(kin_vh)) + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          text = element_text(size = size_font-2))

  ggsave(filename = paste0("Plots/plot3_",this_age_out,"_all_kin.pdf"), plot = p_all, width = 8.5, height = 5.5, units = "in")
  ggsave(filename = paste0("Plots/plot3_",this_age_out,"_vh_kin.pdf"), plot = p_vh, width = 8, height = 5, units = "in")
}

# plot 4. Kin age structures ------------------------------------------------------------------

countries_plot4 <- c(76, 716, 156, 356, 380, 840)
age_keep <- c(0, 7, 13)

d1_all_ages <- map_df(countries_plot4, function(c){
  load(paste0("Output/CIs_trajs_country/traj_",c,".rda"))
  bind_rows(
    df_plot4 %>% as.data.frame(),
    kin_country_past %>% as.data.frame() %>% rename(median_living = living)) %>% 
    filter((age_focal %in% 0 & kin %in%  c("m", "s", "a", "gm", "ggm")) |
           (age_focal %in% 7 & kin %in%  c("m", "d", "s", "gm")) |
           (age_focal %in% 13 & kin %in% c("m", "d", "s", "gd", "ggd")) 
           )
  }) %>% 
  mutate(year = 1950 + year * 5,
         age_kin = age_kin * 5 + 2.5) %>% 
  rename(country = LocationId) %>% 
  left_join(demokin_codes) %>% 
  left_join(regions %>% rename(country = LocationId)) %>% 
  filter(year %in% c(1950, 2095), !is.na(median_living)) %>% 
  # mutate(year_range = paste0(year,"-", year+5)) %>% 
  mutate(
    Country = ifelse(Country == "United States of America", "USA", Country)
    )

# mean ages
d2_all_ages <- d1_all_ages %>% 
  group_by(age_focal, year, country, Country, kin_label, sex_kin) %>% 
  summarise(age_mean = sum(median_living*age_kin)/sum(median_living)) %>% 
  ungroup() %>% 
  mutate(median_living = ifelse(sex_kin == "f", 1, -1)) %>% 
  mutate(kin_label = ifelse(kin_label == "Offspring", "Children", kin_label))

for(paper in c(T, F)){
  for(age in age_keep){
    
    if(paper){
      d1 <- d1_all_ages %>% 
        filter(age_focal == age, country %in% c(380, 716)) %>% 
        mutate(kin_label = ifelse(kin_label == "Offspring", "Children", kin_label))
      
      d2 <- d2_all_ages %>% filter(age_focal == age, country %in% c(380, 716))
      
      d_pan <- 
        d1 %>% 
        select(Country, year) %>% 
        distinct() %>% 
        mutate(
          label = c("D", "B", "C", "A")
          , x = 100, y = -0.9
        )
      
    }else{
      d1 <- d1_all_ages %>% 
        filter(age_focal == age) %>% 
        mutate(kin_label = ifelse(kin_label == "Offspring", "Children", kin_label))
      
      d2 <- d2_all_ages %>% filter(age_focal == age)
      
      d_pan <- 
        d1 %>% 
        select(Country, year) %>% 
        distinct() %>% 
        mutate(
          label = NA
          , x = 100, y = -0.9
        )
    }
    this_age <- paste0(age * 5)
    this_age_out <- paste0(age * 5, "-", age * 5 + 4) 

    p <- ggplot() +
      # ladies
      geom_line(
        aes(y = median_living, x = age_kin, colour=kin_label)
        , linewidth = .5
        , data = d1 %>% filter(sex_kin == "f")
        ) +
      geom_ribbon(
        aes(x = age_kin, ymin = ci_low_living, ymax = ci_upp_living, 
            fill = kin_label, group = kin_label)
        , alpha = 0.4
        , data = d1 %>% filter(sex_kin == "f")
      ) +
      geom_line(
        aes(y = -median_living, x = age_kin, colour=kin_label)
        , linewidth = .5
        , data = d1 %>% filter(sex_kin == "m")
        )+
      # mean age lines
      geom_segment(data = d2, aes(x = age_mean, y = 0, 
                                  xend = age_mean,  yend = median_living, colour=kin_label), 
                   linetype = 1, alpha = .6, linewidth = 0.05) +
      geom_ribbon(
        aes(x = age_kin, ymin = -ci_low_living, ymax = -ci_upp_living, 
            fill = kin_label, group = kin_label)
        , alpha = 0.4
        , data = d1 %>% filter(sex_kin == "m")
      ) +
      # Sex labels
      annotate(geom="text", x=95, y=-max(d1$median_living), label="M", color="black", size = 3) +
      annotate(geom="text", x=95, y=max(d1$median_living), label="F", color="black", size = 3) +
      # Panel labels
      geom_text(
        aes(x = x, y = y, label = label)
        , size =5
        , fontface = "bold"
        , data = d_pan
      ) +
      scale_x_continuous(breaks = seq(0,100,20), labels = seq(0,100,20)) +
      labs(x = "Age of kin", y = paste0("Distribution of kin for a Focal aged ",this_age)) +
      scale_y_continuous(labels = abs, breaks = scales::pretty_breaks(n=ifelse(paper, 5, 3))) +
      guides(fill = "none") +
      coord_flip() +
      facet_grid(year~Country) +
      theme_bw() +
      theme(
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.position = 'bottom',
        text = element_text(size = ifelse(paper,15, 12))) 
    
    ggsave(filename = paste0("Plots/plot4_",this_age_out,ifelse(paper,"_paper",''),".pdf"), 
           plot = p, width = 5, height = 5, units = "in")
  }
}
