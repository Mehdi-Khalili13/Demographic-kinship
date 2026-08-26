#install.packages("kinship2")
#library(kinship2)
library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(tidyr)
library(writexl)
library(gganimate)
library(devtools)
library(DemoKin)
library(fields)

#kinship2:::kinship()
data(package="DemoKin")

############## iran data 
females<-read_excel("/Users/mehdikhalili/Desktop/temporary/kinship/iranSx.xlsx", sheet = "f")
males<-read_excel("/Users/mehdikhalili/Desktop/temporary/kinship/iranSx.xlsx", sheet = "m")
fertf<-read_excel("/Users/mehdikhalili/Desktop/temporary/kinship/Fertility.xlsx", sheet = "2")
###########################
################## two-sex time varying 

females<-as.matrix(females)
males<-as.matrix(males)
fertf<-as.matrix(fertf)

females<-females[,-1]
males<-males[,-1]
fertf<-fertf[,-1]

system.time( h<-kin_time_variant_2sex(
  pf = females,
  pm = males,
  ff = fertf,
  fm = fertf,
  sex_focal = "f",
  birth_female = 1/2.04,
  pif = NULL,
  pim = NULL,
  nf = NULL,
  nm = NULL,
  output_cohort = NULL,
  output_period = c(1950:2100),
  output_kin = c("a","c","d","gd","ggd","ggm","gm","m","n","s"),
  list_output = FALSE
))


#####**********************************************************************
############### start analysing and preparying output 
#### plot ASFR for iran 1950 - 2100
#fertf<-read_excel("/Users/mehdikhalili/Desktop/temporary/kinship/Fertility.xlsx", sheet = "2")
fertf<-read_excel("C:/Users/m.khalili/Desktop/temporary/kinship/Fertility.xlsx", sheet = "2")
females<-read_excel("C:/Users/m.khalili/Desktop/temporary/kinship/iranSx.xlsx", sheet = "f")


fert<-fertf[,-c(77:152)]
fert<- fert[-c(1:10,52:101),]

age<- as.numeric(fert$age)
fert<- fert[,-1]

years <- as.numeric(colnames(fert))
age   <- 10:50

image.plot(
  x = years,
  y = age,
  z = t(fert),
  xlab = "سال",
  ylab = "سن",
  main = "میزان باروری ویژه سنی",
  xaxt = "n"
)

axis(
  side = 1,
  at = seq(
    ceiling(min(years) / 5) * 5,
    floor(max(years) / 5) * 5,
    by = 5
  )
)
layout(matrix(c(1, 2), nrow = 1), widths = c(1, 1))

par(mar = c(4, 4, 3, 1))

image.plot(
  x = as.numeric(colnames(s)),
  y = 0:nrow(s),
  z = t(as.matrix(s)),
  xlab = "سال",
  ylab = "سن",
  main = "نسبت بازماندگی"
)

par(mar = c(4, 4, 3, 1))




#####**********************************************************************

########## plot for kind of kins 
#casel<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "2")
casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "2")

casel <- casel %>%
  mutate(
    kin_label = case_when(
      kin == "a"   ~ "Aunts",
      kin == "c"   ~ "Cousins",
      kin == "d"   ~ "Daughters",
      kin == "gd"  ~ "Granddaughters",
      kin == "ggd" ~ "Great-granddaughters",
      kin == "ggm" ~ "Great-grandmothers",
      kin == "gm"  ~ "Grandmothers",
      kin == "m"   ~ "Mother",
      kin == "n"   ~ "Nieces",
      kin == "s"   ~ "Sisters",
      TRUE ~ kin
    )
    

  )

casel <- casel %>%
  mutate(
    kin_label2 = case_when(
      kin == "a"   ~ "عمه/خاله",
      kin == "c"   ~ "پسرعمود/دخترعمو/دخترخاله/پسرخاله",
      kin == "d"   ~ "دختران",
      kin == "gd"  ~ "نوه‌های دختری",
      kin == "ggd" ~ "نتیجه‌های دختری",
      kin == "ggm" ~ "مادربزرگ‌های بزرگ",
      kin == "gm"  ~ "مادربزرگ‌ها",
      kin == "m"   ~ "مادر",
      kin == "n"   ~ "خواهرزاده‌ها و برادرزاده‌های دختر",
      kin == "s"   ~ "خواهران",
      TRUE ~ kin
    )
  )
    
casel <- casel %>%
  mutate(
    family_type = case_when(
      kin == "a"   ~ "Aunts/Uncles",
      kin == "c"   ~ "Cousins",
      kin == "d"   ~ "Siblings",
      kin == "gd"  ~ "Grand-childrens",
      kin == "ggd" ~ "Great-grand-childrens",
      kin == "ggm" ~ "Great-grandfparents",
      kin == "gm"  ~ "Grandparents",
      kin == "m"   ~ "Parents",
      kin == "n"   ~ "Niblings",
      kin == "s"   ~ "Siblings",
      TRUE ~ kin
    )
  )



#### total number of kin based on age of focal

o1<-casel %>%
  group_by(year,kin_label2,age_focal) %>%
  summarise(
    count = sum(IRN),
    .groups = "drop"
  )

plot_data<- filter(o1, year == "1950-1955"| year == "2000-2005" | year == "2045-2050" | year == "2090-2095")

plot_data<- filter(plot_data, age_focal != "100-104")


scale_fill_manual(
  values = c(
    "#264653",
    "#2A9D8F",
    "#E9C46A",
    "#F4A261",
    "#E76F51",
    "#6A4C93",
    "#457B9D",
    "#8AB17D",
    "#C06C84",
    "#355070"
  )
)

age_levels <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85-89", "90-94", "95-99"
)

plot_data$age_focal <- factor(
  plot_data$age_focal,
  levels = age_levels,
  ordered = TRUE
)




 p<-ggplot(
   plot_data,
  aes(
    x = age_focal,
    y = count,
    fill = kin_label2,
    group = kin_label2
  )
) +
  
  geom_area(
    position = "stack",
    alpha = 0.90,
    colour = NA
  ) +
  
  facet_wrap(
    ~ year,
    ncol = 2
  ) +
  
  scale_fill_manual(
    values = c(
      "#264653",
      "#2A9D8F",
      "#E9C46A",
      "#F4A261",
      "#E76F51",
      "#6A4C93",
      "#457B9D",
      "#8AB17D",
      "#C06C84",
      "#355070"
    )
  ) +
  
  labs(
    x = "سن",
    y = "متوسط خویشاوندان در دسترس",
    fill = ""
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    panel.border = element_blank(),
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    axis.text.x = element_text(
      size = 9,
      colour = "black",
      angle = 45,
      hjust = 1
    ),
    
    axis.text.y = element_text(
      size = 10,
      colour = "black"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.4
    )
  ) +
  
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )


 ggsave(
   "C:/Users/m.khalili/Desktop/kinship_plot.png",
   plot = p,
   width = 12,
   height = 8,
   units = "in",
   dpi = 600
 )
 

#############################
 ################################# plot kins based on age of focal
 casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "2")
 

 
 casel <- casel %>%
   mutate(
     kin_label2 = case_when(
       kin == "a"   ~ "عمه/خاله",
       kin == "c"   ~ "پسرعمود/دخترعمو/دخترخاله/پسرخاله",
       kin == "d"   ~ "دختران",
       kin == "gd"  ~ "نوه‌های دختری",
       kin == "ggd" ~ "نتیجه‌های دختری",
       kin == "ggm" ~ "مادربزرگ‌های بزرگ",
       kin == "gm"  ~ "مادربزرگ‌ها",
       kin == "m"   ~ "مادر",
       kin == "n"   ~ "خواهرزاده‌ها و برادرزاده‌های دختر",
       kin == "s"   ~ "خواهران",
       TRUE ~ kin
     )
   )
 

 o1<-casel %>%
   group_by(year,kin_label2,age_focal) %>%
   summarise(
     count = sum(IRN),
     .groups = "drop"
   )
o2<- filter(o1, year =="1950-1955" | year =="2000-2005" | year == "2045-2050" | year== "2090-2095")
o2<- filter(o2, age_focal != "100-104")

age_order <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90-94"
)

p<-o2 %>%
  mutate(
    year = factor(year),
    age_focal = factor(
      age_focal,
      levels = age_order,
      ordered = TRUE
    )
  ) %>% 
  ggplot(
    aes(
      x = age_focal,
      y = count,
      color = year,
      group = year
    )
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ kin_label2,
    scales = "free_y"
  ) +
  scale_x_discrete(
    breaks = age_order[seq(1, length(age_order), by = 2)]
  ) +
  labs(
    x = "سن فرد کانونی",
    y = "تعداد خویشاوندان",
    color = "دوره"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 9
    ),
    strip.text = element_text(
      size = 11,
      face = "bold"
    )
  )
ggsave(
  "C:/Users/m.khalili/Desktop/kinship_plot.png",
  plot = p,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)


#####**********************************************************************

################# close and distant kin ###########
#casel<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "2")
casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "2")
casel <- casel %>%
  mutate(
    kin_label = case_when(
      kin == "a"   ~ "Aunts",
      kin == "c"   ~ "Cousins",
      kin == "d"   ~ "Daughters",
      kin == "gd"  ~ "Granddaughters",
      kin == "ggd" ~ "Great-granddaughters",
      kin == "ggm" ~ "Great-grandmothers",
      kin == "gm"  ~ "Grandmothers",
      kin == "m"   ~ "Mother",
      kin == "n"   ~ "Nieces",
      kin == "s"   ~ "Sisters",
      TRUE ~ kin
    )
  )
close_kin <- c(
  "Mother",
  "Daughters",
  "Sisters",
  "Grandmothers",
  "Granddaughters"
)

distant_kin <- c(
  "Aunts",
  "Cousins",
  "Nieces",
  "Great-grandmothers",
  "Great-granddaughters"
)

kin_close_distant <- casel %>%
  mutate(
    kin_distance = case_when(
      kin_label %in% close_kin ~ "Close kin",
      kin_label %in% distant_kin ~ "Distant kin",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(
    year,
    age_focal,
    kin_distance
  ) %>%
  summarise(
    count = sum(IRN, na.rm = TRUE),
    .groups = "drop"
  )



# -----------------------------
# Create close/distant groups
# -----------------------------

kin_close_distant <- casel %>%
  mutate(
    kin_distance = case_when(
      kin_label %in% close_kin ~ "Close kin",
      kin_label %in% distant_kin ~ "Distant kin",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(kin_distance),
    year %in% c(
      "1950-1955",
      "2000-2005",
      "2045-2050",
      "2095-2100"
    )
  ) %>%
  group_by(
    year,
    age_focal,
    kin_distance
  ) %>%
  summarise(
    count = sum(IRN, na.rm = TRUE),
    .groups = "drop"
  )


# -----------------------------
# Correct age order
# -----------------------------

age_order <- c(
  "0-4",
  "5-9",
  "10-14",
  "15-19",
  "20-24",
  "25-29",
  "30-34",
  "35-39",
  "40-44",
  "45-49",
  "50-54",
  "55-59",
  "60-64",
  "65-69",
  "70-74",
  "75-79",
  "80-84",
  "85-89",
  "90-94"
)

kin_close_distant <- kin_close_distant %>%
  mutate(
    age_focal = factor(
      age_focal,
      levels = age_order,
      ordered = TRUE
    ),
    
    year = factor(
      year,
      levels = c(
        "1950-1955",
        "2000-2005",
        "2045-2050",
        "2095-2100"
      )
    )
  )


# -----------------------------
# Plot
# -----------------------------

ggplot(
  kin_close_distant,
  aes(
    x = age_focal,
    y = count,
    color = kin_distance,
    group = kin_distance
  )
) +
  
  geom_line(linewidth = 1) +
  
  facet_wrap(
    ~ year,
    ncol = 2
  ) +
  
  labs(
    x = "Focal's age",
    y = "Mean number of available kin",
    color = NULL
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    panel.border = element_blank(),
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      size = 13,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 9
    ),
    
    legend.position = "bottom",
    
    panel.grid = element_blank()
  )

########***************************************************
########## cohort comparying


#####**********************************************************************

###### mean number of kinship for focal age 65- olders
#casel<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "3")
casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "3")
casel <- casel %>%
  filter(!kin %in% c("a", "ggm", "gm", "m"))

casel <- casel %>%
  mutate(
    kin_label2 = case_when(
      kin == "c"   ~ "پسرعموها، دخترعموها، پسردایی‌ها، دختردایی‌ها، پسرعمه‌ها، دخترعمه‌ها، پسرخاله‌ها و دخترخاله‌ها",
      kin == "d"   ~ "دختران",
      kin == "gd"  ~ "نوه‌های دختری",
      kin == "ggd" ~ "نتیجه‌های دختری",
      kin == "m"   ~ "مادر",
      kin == "n"   ~ "خواهرزاده‌ها و برادرزاده‌های دختر",
      kin == "s"   ~ "خواهران",
      TRUE ~ kin
    )
  )

h1<-casel %>%
  group_by(year,kin_label2,Variant) %>%
  summarise(
    count = sum(IRN),
    .groups = "drop"
  )

h2<-casel %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(IRN),
    .groups = "drop"
  )

df<-h1
# ---------------------------------------
# 1. Prepare data
# ---------------------------------------

plot_data <- df %>%
  filter(
    Variant %in% c(
      "ci_low_living",
      "ci_upp_living",
      "Estimate",
      "median_living"
    )
  ) %>%
  mutate(
    year_start = as.numeric(
      sub("-.*", "", as.character(year))
    )
  )


# ---------------------------------------
# 2. Reshape data
# ---------------------------------------

plot_wide <- plot_data %>%
  group_by(
    year_start,
    year,
    kin_label2,
    Variant
  ) %>%
  summarise(
    value = mean(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Variant,
    values_from = value
  )


# ---------------------------------------
# 3. Plot
# ---------------------------------------

p<-ggplot(
  plot_wide,
  aes(
    x = year_start,
    y = Estimate
  )
) +
  
  # Confidence interval: forecast period only
  geom_ribbon(
    data = plot_wide %>%
      filter(year_start >= 2025),
    aes(
      ymin = ci_low_living,
      ymax = ci_upp_living
    ),
    fill = "gray",
    alpha = 0.20
  ) +
  
  # Main trajectory
  geom_line(
    linewidth = 1.1,
    colour = "#333333"
  ) +
  
  # Forecast boundary
  geom_vline(
    xintercept = 2025,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "purple"
  ) +
  
  facet_wrap(
    ~ kin_label2,
    ncol = 2,
    scales = "free_y"
  ) +
  
  labs(
    x = "Year",
    y = "Mean number of available kin"
  ) +
  
  scale_x_continuous(
    breaks = seq(
      min(plot_wide$year_start, na.rm = TRUE),
      max(plot_wide$year_start, na.rm = TRUE),
      by = 10
    )
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    panel.border = element_blank(),
    
    strip.background = element_blank(),
    
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 13
    ),
    
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    
    panel.grid = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.4
    )
  )



ggsave(
  "C:/Users/m.khalili/Desktop/kinship_plot2.png",
  plot = p,
  width = 14,
  height = 8,
  units = "in",
  dpi = 600
)


#####**********************************************************************
############ comparing countries for under 15
iran<-read_excel("C:/Users/m.khalili/Desktop/under15.xlsx", sheet = "iran")

iran<-filter( iran ,age_focal=="0-4" |age_focal=="5-9" | age_focal=="10-14")
iran1<-iran %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(IRN),
    .groups = "drop"
  )


#TURKEY
#turkey<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "turkey")
turkey<-read_excel("C:/Users/m.khalili/Desktop/under15.xlsx", sheet = "turkey")

turkey<-filter( turkey ,age_focal=="0-4" |age_focal=="5-9" | age_focal=="10-14")

turkey1<-turkey %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(TUR),
    .groups = "drop"
  )
#JAPAN
#japan<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "japan")
japan<-read_excel("C:/Users/m.khalili/Desktop/under15.xlsx", sheet = "japan")

japan<-filter( japan ,age_focal=="0-4" |age_focal=="5-9" | age_focal=="10-14")

japan1<-japan %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(JPN),
    .groups = "drop")
#ITAKY
italy<-read_excel("C:/Users/m.khalili/Desktop/under15.xlsx", sheet = "italy")
italy<-filter( italy ,age_focal=="0-4" |age_focal=="5-9" | age_focal=="10-14")

italy1<-italy %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(ITA),
    .groups = "drop")


italy <- italy1 %>%
  dplyr::select(year, Variant, italy = count)

iran <- iran1 %>%
  dplyr::select(year, Variant, iran = count)
japan <- japan1 %>%
  dplyr::select(year, Variant, japan = count)
turkey <- turkey1 %>%
  dplyr::select(year, Variant, turkey = count)

final_data <- italy %>%
  full_join(iran, by = c("year", "Variant")) %>%
  full_join(japan, by = c("year", "Variant")) %>%
  full_join(turkey, by = c("year", "Variant"))
##### plot



df_long <- final_data %>%
  pivot_longer(
    cols = c(turkey, japan, italy, iran),
    names_to = "country",
    values_to = "value"
  ) %>%
  mutate(
    start_year = as.numeric(substr(year, 1, 4)),
    country = recode(
      country,
      turkey = "Turkey",
      japan = "Japan",
      italy = "Italy",
      iran = "Iran"
    )
  )


# ----------------------------------
# 2. Estimate + median forecast
#    برای ساخت خط پیوسته
# ----------------------------------

df_line <- df_long %>%
  filter(
    Variant %in% c("Estimate", "median_living")
  )


# ----------------------------------
# 3. CI
# ----------------------------------

df_ci <- df_long %>%
  dplyr::filter(
   Variant %in% c(
      "ci_low_living",
      "ci_upp_living"
    )
  ) %>%
  dplyr::select(
    start_year,
    country,
    Variant,
    value
  ) %>%
  tidyr::pivot_wider(
    names_from = Variant,
    values_from = value
  )


# ----------------------------------
# 4. نمودار
# ----------------------------------

P<-ggplot() +
  
  # ==================================
# پس‌زمینه Forecast
# ==================================

annotate(
  "rect",
  xmin = 2025,
  xmax = 2100,
  ymin = -Inf,
  ymax = Inf,
  fill = "grey92",
  colour = NA
) +
  
  # ==================================
# Confidence Interval
# ==================================

geom_ribbon(
  data = df_ci,
  aes(
    x = start_year,
    ymin = ci_low_living,
    ymax = ci_upp_living,
    fill = country
  ),
  alpha = 0.15,
  colour = NA
) +
  
  # ==================================
# خط پیوسته Estimate + Forecast
# ==================================

geom_line(
  data = df_line,
  aes(
    x = start_year,
    y = value,
    colour = country
  ),
  linewidth = 1.15
) +
  
  # ==================================
# محور X
# ==================================

scale_x_continuous(
  breaks = seq(1950, 2100, 10),
  limits = c(1950, 2100),
  expand = c(0, 0)
) +
  
  # ==================================
# رنگ کشورها
# ==================================

scale_colour_manual(
  values = c(
    "Japan" = "#1B4F72",
    "Italy" = "#922B21",
    "Iran" = "#117A65",
    "Turkey" = "#7D3C98"
  ),
  labels = c(
    "Japan" = "ژاپن",
    "Italy" = "ایتالیا",
    "Iran" = "ایران",
    "Turkey" = "ترکیه"
  )
) +
  
  scale_fill_manual(
    values = c(
      "Japan" = "#1B4F72",
      "Italy" = "#922B21",
      "Iran" = "#117A65",
      "Turkey" = "#7D3C98"
    ),
    labels = c(
      "Japan" = "ژاپن",
      "Italy" = "ایتالیا",
      "Iran" = "ایران",
      "Turkey" = "ترکیه"
    )
  ) +
  
  labs(
    title = "",
    subtitle = "",
    x = "سال",
    y = "تعداد خویشاوندان در دسترس",
    colour = NULL,
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    plot.subtitle = element_text(
      size = 11,
      colour = "grey40"
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      15, 20, 15, 15
    )
  )
ggsave(
  "C:/Users/m.khalili/Desktop/kinship_plot3.png",
  plot = P,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600









#####**********************************************************************
############ comparing countries for focal age 65
#####**********************************************************************

#IRAN
#iran<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "3")
iran<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "3")

iran<-filter( iran ,age_focal=="65-69")
iran1<-iran %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(IRN),
    .groups = "drop"
  )


#TURKEY
#turkey<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "turkey")
turkey<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "turkey")

turkey<-filter( turkey ,age_focal=="65-69")

turkey1<-turkey %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(TUR),
    .groups = "drop"
  )
#JAPAN
#japan<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "japan")
japan<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "japan")

japan<-filter( japan ,age_focal=="65-69")

japan1<-japan %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(JPN),
    .groups = "drop")
#ITAKY
italy<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "italy")
italy<-filter( italy ,age_focal=="65-69")

italy1<-italy %>%
  group_by(year,Variant) %>%
  summarise(
    count = sum(ITA),
    .groups = "drop")


#df<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "65")
df<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "65")

##### plot
df_long <- df %>%
  pivot_longer(
    cols = c(turkey, japan, italy, iran),
    names_to = "country",
    values_to = "value"
  ) %>%
  mutate(
    start_year = as.numeric(substr(year, 1, 4)),
    country = recode(
      country,
      turkey = "Turkey",
      japan = "Japan",
      italy = "Italy",
      iran = "Iran"
    )
  )


# ----------------------------------
# 2. Estimate + median forecast
#    برای ساخت خط پیوسته
# ----------------------------------

df_line <- df_long %>%
  filter(
    variant %in% c("Estimate", "median_living")
  )


# ----------------------------------
# 3. CI
# ----------------------------------

df_ci <- df_long %>%
  dplyr::filter(
    variant %in% c(
      "ci_low_living",
      "ci_upp_living"
    )
  ) %>%
  dplyr::select(
    start_year,
    country,
    variant,
    value
  ) %>%
  tidyr::pivot_wider(
    names_from = variant,
    values_from = value
  )


# ----------------------------------
# 4. نمودار
# ----------------------------------

P<-ggplot() +
  
  # ==================================
# پس‌زمینه Forecast
# ==================================

annotate(
  "rect",
  xmin = 2025,
  xmax = 2100,
  ymin = -Inf,
  ymax = Inf,
  fill = "grey92",
  colour = NA
) +
  
  # ==================================
# Confidence Interval
# ==================================

geom_ribbon(
  data = df_ci,
  aes(
    x = start_year,
    ymin = ci_low_living,
    ymax = ci_upp_living,
    fill = country
  ),
  alpha = 0.15,
  colour = NA
) +
  
  # ==================================
# خط پیوسته Estimate + Forecast
# ==================================

geom_line(
  data = df_line,
  aes(
    x = start_year,
    y = value,
    colour = country
  ),
  linewidth = 1.15
) +
  
  # ==================================
# محور X
# ==================================

scale_x_continuous(
  breaks = seq(1950, 2100, 10),
  limits = c(1950, 2100),
  expand = c(0, 0)
) +
  
  # ==================================
# رنگ کشورها
# ==================================

scale_colour_manual(
  values = c(
    "Japan" = "#1B4F72",
    "Italy" = "#922B21",
    "Iran" = "#117A65",
    "Turkey" = "#7D3C98"
  ),
  labels = c(
    "Japan" = "ژاپن",
    "Italy" = "ایتالیا",
    "Iran" = "ایران",
    "Turkey" = "ترکیه"
  )
) +
  
  scale_fill_manual(
    values = c(
      "Japan" = "#1B4F72",
      "Italy" = "#922B21",
      "Iran" = "#117A65",
      "Turkey" = "#7D3C98"
    ),
    labels = c(
      "Japan" = "ژاپن",
      "Italy" = "ایتالیا",
      "Iran" = "ایران",
      "Turkey" = "ترکیه"
    )
  ) +
  
  labs(
    title = "",
    subtitle = "",
    x = "سال",
    y = "تعداد خویشاوندان در دسترس",
    colour = NULL,
    fill = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    plot.subtitle = element_text(
      size = 11,
      colour = "grey40"
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      15, 20, 15, 15
    )
  )
ggsave(
  "C:/Users/m.khalili/Desktop/kinship_plot3.png",
  plot = P,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)
#####**********************************************************************
######### focal age distribution


#1) #################

##################### FOCAL AGE DISTRIBUTION FOR 65 YEARS OLD

casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "2")

df<-filter(casel, year =="1950-1955" | year =="2000-2005" | year =="2045-2050" | year =="2090-2095")

df1<-filter(df, age_focal =="65-69")
df1<- filter(df1, age_kin != "100-104")


df1 <- df1 %>%
  filter(kin %in% c("d", "gd", "ggd", "n"))

df2<- df1 %>% group_by(age_focal, year, kin,age_kin,) %>% 
  summarise(count = sum(IRN))

df2 <- df2 %>%
  mutate(
    kin_label2 = case_when(
      kin == "d"   ~ "دختران",
      kin == "gd"  ~ "نوه‌های دختری",
      kin == "ggd" ~ "نتیجه‌های دختری",
      kin == "n"   ~ "خواهرزاده‌ها و برادرزاده‌های دختر",
      TRUE ~ kin
    )
  )

age_kin_order <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24",
  "25-29", "30-34", "35-39", "40-44", "45-49",
  "50-54", "55-59", "60-64", "65-69", "70-74",
  "75-79", "80-84", "85-89", "90-94", "95-99"
)

df2 <- df2 %>%
  mutate(
    age_kin = factor(
      age_kin,
      levels = age_kin_order,
      ordered = TRUE
    ),
    
    year = factor(
      year,
      levels = c(
        "1950-1955",
        "2000-2005",
        "2045-2050",
        "2090-2095"
      )
    )
  )


p<-ggplot(
  df2,
  aes(
    x = age_kin,
    y = count,
    colour = year,
    group = year
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  facet_wrap(
    ~ kin_label2,
    scales = "free_y"
  ) +
  
  labs(
    x = "سن خویشاوندان",
    y = "تعداد خویشاوندان",
    colour = NULL
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 9
    ),
    
    legend.position = "bottom",
    
    strip.text = element_text(
      face = "bold",
      size = 12
    )
  )



ggsave(
  "C:/Users/m.khalili/Desktop/kinship_plot4.png",
  plot = p,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600
)


###############################################################
#casel<-read_excel("/Users/mehdikhalili/Desktop/k.xlsx", sheet = "2")
casel<-read_excel("C:/Users/m.khalili/Desktop/k.xlsx", sheet = "2")

table(casel$kin)
df<-filter(casel, year =="1970-1975")

df %>%
  mutate(
    age_focal = as.character(age_focal),
    age_kin = factor(
      age_kin,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85-89",
        "90-94",
        "95-99",
        "100-104"
      )
    )
  ) %>%
  filter(age_focal %in% c("0-4", "15-19", "30-34")) %>%
  filter(kin %in% c("m", "s","gm","a")) %>%
  rename_kin() %>%
  
  ggplot(
    aes(
      x = age_kin,
      y = IRN,
      colour = age_focal,
      group = age_focal
    )
  ) +
  
  geom_line(
    linewidth = 1
  ) +
  
  
  scale_color_discrete(
    name = "Focal's age"
  ) +
  
  labs(
    x = "Age of Focal's kin",
    y = "Age distribution"
  ) +
  
  theme_bw() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  ) +
  
  facet_wrap(~kin)