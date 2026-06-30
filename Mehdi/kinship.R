



############################################################
# 0) پکیج‌ها و تنظیمات
############################################################
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)
  library(readxl)
})

theme_set(theme_bw(base_size = 12))

make_age_labels <- function(a) ifelse(a < 100, sprintf("%d-%d", a, a+4), "100+")

############################################################
# 1) داده‌ها: واقعی یا مصنوعی
############################################################

USE_REAL_DATA <- FALSE   # ← اگر TRUE کنی، فایل‌های خودت لود می‌شوند

if (USE_REAL_DATA) {
  
  ### اینجا داده‌های خودت را جایگزین کن:
  
  # ASFR.xlsx : ستون year + ستون‌های سنین 15-19 ... 45-49
  asfr_raw <- read_excel("ASFR.xlsx")
  names(asfr_raw)[match(tolower(names(asfr_raw)), "year")] <- "year"
  ASFR_5y <- asfr_raw |>
    pivot_longer(-year, names_to="age_group", values_to="ASFR") |>
    mutate(age_lb = as.integer(gsub("([0-9]+).*", "\\1", age_group))) |>
    filter(age_lb >= 15 & age_lb <= 45) |>
    mutate(ASFR = ASFR / 1000) |>
    select(year, age_lb, ASFR)
  
  # lifetable.xlsx : ستون year، age_lb (0,5,...), nqx
  LT_5y <- read_excel("lifetable.xlsx") |>
    rename(year = 1, age_lb = 2, nqx = 3) |>
    mutate(age_lb = as.integer(age_lb))
  
  # population.xlsx : شیت women → سطر age_lb, ستون‌ها سال‌ها
  pop_w <- read_excel("population.xlsx", sheet="women")
  POPF_5y <- pop_w |>
    rename(age_lb = 1) |>
    pivot_longer(-age_lb, names_to="year", values_to="pop") |>
    mutate(year = as.integer(year),
           age_lb = as.integer(gsub("\\+","", age_lb))) |>
    filter(age_lb %% 5 == 0) |>
    select(year, age_lb, pop)
  
} else {
  
  ### داده مصنوعی واقع‌گرایانه (برای تست بدون فایل)
  set.seed(8)
  years <- 1360:1405
  ASFR_5y <- expand.grid(year=years, age_lb=seq(15,45,5)) |>
    mutate(ASFR = case_when(
      age_lb==15 ~ .04,
      age_lb==20 ~ .12,
      age_lb==25 ~ .14,
      age_lb==30 ~ .08,
      age_lb==35 ~ .035,
      age_lb==40 ~ .010,
      age_lb==45 ~ .003,
      TRUE ~ 0
    ) * exp(-(year - min(year))*0.003))
  
  LT_5y <- expand.grid(year=years, age_lb=seq(0,100,5)) |>
    mutate(nqx = case_when(
      age_lb==0 ~ .020,
      age_lb<=5 ~ .004,
      age_lb<=25 ~ .002,
      age_lb<=45 ~ .006,
      age_lb<=65 ~ .020,
      age_lb<=80 ~ .070,
      age_lb<=95 ~ .160,
      TRUE ~ .300
    ) * exp(-(year - min(year))*0.002))
  
  POPF_5y <- expand.grid(year=years, age_lb=seq(0,100,5)) |>
    mutate(pop = round(1e5 * exp(-(age_lb/50)^2) * (1 + 0.005*(year - min(year)))))
}

############################################################
# 2) تابع‌های ایمن برای گرفتن داده یک سال
############################################################

get_asfr_year_safe <- function(y){
  yrs <- sort(unique(ASFR_5y$year))
  use_year <- yrs[which.min(abs(yrs - y))]
  ASFR_5y %>% filter(year == use_year)
}

get_lt_year_safe <- function(y){
  yrs <- sort(unique(LT_5y$year))
  use_year <- yrs[which.min(abs(yrs - y))]
  LT_5y %>% filter(year == use_year)
}

############################################################
# 3) ساخت ماتریس لزلی 5 ساله (Female-only)
############################################################

SRB <- 105
p_female <- 100/(100+SRB)

build_leslie_5y <- function(asfr_year, lt_year, max_age = 100){
  ages <- seq(0, max_age, 5)
  n <- length(ages)
  
  px <- lt_year |>
    filter(age_lb %in% ages) |>
    arrange(age_lb) |>
    transmute(px = pmax(pmin(1-nqx,1),0)) |>
    pull(px)
  
  U <- matrix(0, n,n)
  for(i in 1:(n-1)) U[i+1,i] <- px[i]
  U[n,n] <- px[n]
  
  fert <- asfr_year |>
    filter(age_lb %in% seq(15,45,5)) |>
    arrange(age_lb) %>%
    pull(ASFR)
  
  # اگر نبود → صفر کن
  if(length(fert) == 0) fert <- rep(0, length(seq(15,45,5)))
  
  fert <- fert * 5 * p_female
  
  F <- matrix(0,n,n)
  idx <- match(seq(15,45,5), ages)
  F[1, idx] <- fert
  
  list(A = U+F, U=U, F=F, ages = ages)
}

############################################################
# 4) پیش‌بینی جمعیت (لزلی غیرایستا)
############################################################

project_population <- function(initial_pop, years){
  n <- length(initial_pop)
  Pops <- matrix(NA_real_, nrow=n, ncol=length(years))
  Pops[,1] <- initial_pop
  for(t in 2:length(years)){
    asfr_y <- get_asfr_year_safe(years[t-1])
    lt_y   <- get_lt_year_safe(years[t-1])
    L <- build_leslie_5y(asfr_y, lt_y)$A
    Pops[,t] <- L %*% Pops[,t-1]
  }
  dimnames(Pops) <- list(make_age_labels(seq(0,(n-1)*5,5)), years)
  Pops
}

base_year <- intersect(intersect(unique(ASFR_5y$year), unique(LT_5y$year)), unique(POPF_5y$year)) |> max()
ages5 <- seq(0,100,5)
N0_f <- POPF_5y %>% filter(year==base_year, age_lb %in% ages5) %>% arrange(age_lb) %>% pull(pop)

Pmat <- project_population(N0_f, base_year:(base_year+50))

############################################################
# 5) تعریف کوهورت‌ها و سناریوها
############################################################

cohorts <- list(
  list(name="1360–65", mid = 1362),
  list(name="1365–70", mid = 1367),
  list(name="1370–75", mid = 1372)
)

ASFR_base <- get_asfr_year_safe(base_year)

ASFR_high <- ASFR_base %>% mutate(ASFR = ASFR * 1.5)
ASFR_mid  <- ASFR_base %>% mutate(ASFR = ASFR * 1.0)
ASFR_low  <- ASFR_base %>% mutate(ASFR = ASFR * 0.7)

scenarios <- list(
  list(code="High", label="باروری بالا",   ASFR_m=ASFR_high, ASFR_d=ASFR_mid),
  list(code="Mid",  label="میانه",        ASFR_m=ASFR_mid,  ASFR_d=ASFR_mid),
  list(code="Low",  label="باروری پایین", ASFR_m=ASFR_low,  ASFR_d=ASFR_low)
)

report_ages <- c(65,80)

############################################################
# 6) شبیه‌سازی مونت‌کارلو خویشاوندی
############################################################

survive_to <- function(age, lt_year){
  if(age<=0) return(0)
  steps <- seq(0, age-age%%5, 5)
  pxs <- map_dbl(steps, ~{
    r <- lt_year %>% filter(age_lb==.x)
    if(nrow(r)==0) return(1)
    pmax(pmin(1-r$nqx,1),0)
  })
  prod(pxs)
}

simulate_one_cohort <- function(N, cohort_mid_birth, ASFR_m, ASFR_d, lt_table){
  fert_ages <- seq(15,45,5)
  kids <- daughters <- granddaughters <- list()
  
  for(A in report_ages){
    lt_rep <- get_lt_year_safe(cohort_mid_birth + A)
    
    # تعداد تولد مادر
    asfr_m <- ASFR_m %>% arrange(age_lb) %>% pull(ASFR)
    lam <- asfr_m * 5
    births <- matrix(rpois(N*length(lam), lam), nrow=N, byrow=TRUE)
    
    # دختر
    girls <- matrix(rbinom(N*length(lam), births, p_female), nrow=N)
    
    # بقا تا A
    alive_k <- matrix(0,nrow=N,ncol=length(lam))
    alive_g <- matrix(0,nrow=N,ncol=length(lam))
    
    for(j in seq_along(fert_ages)){
      child_age <- A - fert_ages[j]
      if(child_age>0){
        s <- survive_to(child_age, lt_rep)
        alive_k[,j] <- rbinom(N, births[,j], s)
        alive_g[,j] <- rbinom(N, girls[,j],  s)
      }
    }
    
    kids[[as.character(A)]] <- rowSums(alive_k)
    daughters[[as.character(A)]] <- rowSums(alive_g)
    
    # نوه‌ها
    grand_vec <- numeric(N)
    for(i in 1:N){
      g_count <- daughters[[as.character(A)]][i]
      if(g_count>0){
        # باروری نسل دختر
        asfr_d <- ASFR_d %>% arrange(age_lb) %>% pull(ASFR)
        lam_d <- asfr_d * 5
        birth_gd <- rpois(g_count*length(lam_d), lam_d)
        birth_gd <- matrix(birth_gd, nrow=g_count, byrow=TRUE)
        gd_f <- matrix(rbinom(length(birth_gd), birth_gd, p_female), nrow=g_count)
        # بقای نوه تا A (تقریب: متوسط فاصله نسلی = 25سال → نوه ~ A-25)
        child_age2 <- A - 25
        s2 <- survive_to(child_age2, lt_rep)
        grand_vec[i] <- sum(rbinom(length(gd_f), gd_f, s2))
      }
    }
    granddaughters[[as.character(A)]] <- grand_vec
  }
  
  # خروجی
  tibble(
    kids_65=kids$`65`, kids_80=kids$`80`,
    dau_65=daughters$`65`, dau_80=daughters$`80`,
    gd_65=granddaughters$`65`, gd_80=granddaughters$`80`
  )
}

run_all <- function(N=6000){
  out <- list()
  for(co in cohorts){
    for(sc in scenarios){
      sim <- simulate_one_cohort(N, co$mid, sc$ASFR_m, sc$ASFR_d, LT_5y)
      sim$cohort <- co$name
      sim$scenario <- sc$label
      out[[paste(co$name, sc$code)]] <- sim
    }
  }
  bind_rows(out)
}

sim_res <- run_all()

############################################################
# 7) خلاصه‌سازی و مصورسازی نهایی
############################################################


df_long <- sim_res |>
  pivot_longer(cols = starts_with(c("kids","dau","gd")),
               names_to="metric", values_to="value") |>
  mutate(report_age = case_when(
    grepl("_65", metric) ~ 65,
    grepl("_80", metric) ~ 80
  ),
  metric = case_when(
    grepl("^kids", metric) ~ "فرزندان زنده",
    grepl("^dau",  metric) ~ "دختران زنده",
    grepl("^gd",   metric) ~ "نوه‌دخترهای زنده"
  ))

summary_table <- df_long %>%
  group_by(cohort, scenario, metric, report_age) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    p10 = quantile(value, .10),
    p90 = quantile(value, .90),
    .groups="drop"
  )

print(summary_table)

ggplot(summary_table, aes(x=cohort, y=mean, fill=scenario)) +
  geom_col(position="dodge") +
  facet_grid(metric ~ report_age, scales="free_y") +
  labs(title="ساختار خویشاوندی در سنین سالمندی",
       x="کوهورت تولد", y="میانگین", fill="سناریوی باروری")

ggplot(summary_table, aes(x=cohort, y=mean, color=scenario, group=scenario)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=p10, ymax=p90), width=0.15) +
  facet_grid(metric ~ report_age, scales="free_y") +
  labs(title="عدم قطعیت ساختار خویشاوندی (نمودار دهک‌ها)",
       x="کوهورت تولد", y="میانگین ± دهک 10–90")





ggplot(df_long, aes(x=value, color=scenario)) +
  geom_density(size=1) +
  facet_grid(metric ~ cohort, scales="free") +
  labs(title="توزیع ساختار خویشاوندی به تفکیک کوهورت و نوع خویشاوند",
       x="تعداد خویشاوند", y="چگالی", color="سناریوی باروری")




ggplot(df_long, aes(x=cohort, y=value, fill=scenario)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_grid(metric ~ report_age, scales="free_y") +
  labs(title="مقایسه آماری ساختار خویشاوندی",
       x="کوهورت تولد", y="توزیع تعداد خویشاوندان")



ggplot(df_long, aes(x=scenario, y=value, fill=scenario)) +
  geom_violin(trim=FALSE, alpha=0.7) +
  facet_grid(metric ~ cohort, scales="free_y") +
  labs(title="پراکندگی کامل ساختار خویشاوندی",
       x="سناریوی باروری", y="تعداد خویشاوند")


df_freq <- df_long %>%
  group_by(cohort, scenario, metric, report_age, value) %>%
  summarise(n=n(), .groups="drop") %>%
  group_by(cohort, scenario, metric, report_age) %>%
  mutate(p=n/sum(n))

ggplot(df_freq, aes(x=value, y=scenario, fill=p)) +
  geom_tile() +
  facet_grid(metric ~ cohort) +
  scale_fill_viridis_c() +
  labs(title="Heatmap احتمال تعداد خویشاوندان",
       x="تعداد خویشاوند", y="سناریوی باروری", fill="احتمال")

library(ggridges)

ggplot(df_long, aes(x=value, y=scenario, fill=scenario)) +
  geom_density_ridges(alpha=0.8) +
  facet_grid(metric ~ cohort, scales="free") +
  labs(title="Ridge Distribution ساختار خویشاوندی",
       x="تعداد", y="سناریوی باروری")

for(co in unique(df_long$cohort)) {
  p <- ggplot(df_long %>% filter(cohort==co),
              aes(x=value, fill=scenario)) +
    geom_density(alpha=0.5) +
    facet_grid(metric ~ report_age, scales="free_y") +
    labs(title=paste("توزیع ساختار خویشاوندی - کوهورت", co),
         x="تعداد خویشاوند", y="چگالی") +
    theme(legend.position="bottom")
  print(p)
}



df_zero <- df_long %>%
  filter(metric=="فرزندان زنده") %>%
  group_by(cohort, scenario, report_age) %>%
  summarise(p_zero = mean(value == 0), .groups="drop")

ggplot(df_zero, aes(x=cohort, y=p_zero, fill=scenario)) +
  geom_col(position="dodge") +
  facet_wrap(~ report_age, labeller=label_both) +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="احتمال بی‌فرزندی در سالمندی",
       x="کوهورت تولد", y="درصد افراد بدون فرزند", fill="سناریوی باروری")


df_mean <- df_long %>%
  group_by(cohort, scenario, metric, report_age) %>%
  summarise(mean_rel = mean(value), .groups="drop")

ggplot(df_mean, aes(x=scenario, y=mean_rel, group=report_age, color=factor(report_age))) +
  geom_line(size=1.3) +
  geom_point(size=3) +
  facet_grid(metric ~ cohort, scales="free_y") +
  labs(title="تغییر ساختار خویشاوندی تحت سناریوهای باروری",
       x="سناریوی باروری", y="میانگین تعداد خویشاوند",
       color="سن گزارش (سال)")


ggplot(df_zero, aes(x=scenario, y=p_zero, color=cohort, group=cohort)) +
  geom_line(size=1.5) +
  geom_point(size=3) +
  facet_wrap(~ report_age) +
  scale_y_continuous(labels=scales::percent_format()) +
  labs(title="پیامد باروری نسل‌ها بر تنهایی سالمندی",
       x="سناریوی باروری", y="احتمال بی‌فرزندی در سنین سالمندی",
       color="کوهورت تولد")

