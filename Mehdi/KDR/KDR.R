# ============================================================
# COMPLETE SCRIPT: Kin Dependency Ratio, Iran, 1950–2100
# Historical period: Estimate
# Projection period: ci_low_living / median_living / ci_upp_living
# Outputs:
#   1) Age-specific ASKDR
#   2) Summary KDR by network
#   3) Historical + projected datasets
#   4) Young KDR plot
#   5) Old-age KDR plot
#   6) Combined young + old plot
# ============================================================

#install.packages("kinship2")
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
library(stringr)



# ============================================================
# 1. Paths and settings
# ============================================================

COUNTRY_CODE <- "JPN"

# Female population source (ages in rows, years in columns)
POP_SOURCE <- "C:/Users/m.khalili/Desktop/unpopfemale.xlsx"
POP_SHEET  <- "japan"

# Public kin data
KIN_FILE <- "C:/Users/m.khalili/Desktop/table_data_desagg.csv"

# Population file written here and then used by the analysis
POP_FILE                <- "C:/Users/m.khalili/Desktop/pop_iran_female_5year.csv"
POP_FILE_EXCEL_FRIENDLY <- "C:/Users/m.khalili/Desktop/pop_iran_female_5year_excel.csv"

df <- read_excel(POP_SOURCE, sheet = POP_SHEET)

female_population_5year <- df %>%
  
  pivot_longer(
    cols = -age,
    names_to = "year_numeric",
    values_to = "female_population"
  ) %>%
  
  mutate(
    year_numeric = as.numeric(year_numeric)
  ) %>%
  
  filter(
    year_numeric >= 1950,
    year_numeric < 2100
  ) %>%
  
  mutate(
    period_start = floor((year_numeric - 1950) / 5) * 5 + 1950,
    period_end   = period_start + 5
  ) %>%
  
  group_by(
    age,
    period_start,
    period_end
  ) %>%
  
  summarise(
    female_population = mean(
      female_population,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  mutate(
    year = paste0(period_start, "-", period_end),
    
    # استخراج عدد ابتدای گروه سنی
    age_start = as.numeric(
      str_extract(age, "^\\d+")
    )
  ) %>%
  
  arrange(
    period_start,
    age_start
  ) %>%
  
  rename(
    age_focal = age
  ) %>%
  
  select(
    age_focal,
    female_population,
    year
  )


# Aggregate single-year ages into the 5-year bands used by the kin data
female_population_5year <- female_population_5year %>%
  mutate(
    age_numeric = as.numeric(
      str_extract(age_focal, "^[0-9]+")
    ),
    age_focal_lower = floor(age_numeric / 5) * 5
  ) %>%
  group_by(
    year,
    age_focal_lower
  ) %>%
  summarise(
    female_population = sum(female_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    age_focal = paste0(age_focal_lower, "-", age_focal_lower + 4),
    country   = COUNTRY_CODE
  ) %>%
  arrange(
    as.numeric(str_extract(year, "^[0-9]+")),
    age_focal_lower
  ) %>%
  select(
    age_focal,
    female_population,
    year,
    country
  )

write_csv(
  female_population_5year,
  POP_FILE
)

# Excel-friendly copy (leading apostrophe keeps age labels as text)
write_csv(
  female_population_5year %>%
    mutate(
      age_focal = paste0("'", age_focal),
      year = as.character(year)
    ),
  POP_FILE_EXCEL_FRIENDLY
)





YOUNG_MAX <- 14
WORK_MIN  <- 15
WORK_MAX  <- 64
OLD_MIN   <- 65

# If you want only one kin network in the plots:
NETWORK_SELECTED <- "collateral"


# ============================================================
# 2. Helper functions for age groups
# ============================================================

parse_age_lower <- function(x) {
  
  # Remove quotes/apostrophes and surrounding spaces that Excel adds
  x <- str_remove(
    str_trim(as.character(x)),
    "^['\"]+"
  )
  
  out <- suppressWarnings(
    as.numeric(
      str_extract(x, "^[0-9]+")
    )
  )
  
  if (anyNA(out)) {
    
    stop(
      "Lower bound could not be extracted for: ",
      paste(
        unique(x[is.na(out)]),
        collapse = ", "
      )
    )
  }
  
  out
}


parse_age_upper <- function(x) {
  
  # Remove quotes/apostrophes and surrounding spaces that Excel adds
  x <- str_remove(
    str_trim(as.character(x)),
    "^['\"]+"
  )
  
  lower <- parse_age_lower(x)
  
  is_open <- str_detect(
    x,
    "\\+"
  )
  
  second_number <- str_extract(
    x,
    "(?<=-)[0-9]+"
  )
  
  upper <- suppressWarnings(
    as.numeric(second_number)
  )
  
  upper[is_open] <- Inf
  
  upper[
    is.na(upper) &
      !is_open
  ] <-
    lower[
      is.na(upper) &
        !is_open
    ]
  
  upper
}


# ============================================================
# 3. Normalize kinship codes
# ============================================================

normalize_kin_code <- function(x) {
  
  z <- str_to_lower(
    str_trim(
      as.character(x)
    )
  )
  
  case_when(
    
    z %in% c(
      "m",
      "mother",
      "father",
      "parent",
      "parents"
    ) ~ "parent",
    
    z %in% c(
      "s",
      "os",
      "ys",
      "sister",
      "brother",
      "sibling",
      "siblings"
    ) ~ "sibling",
    
    z %in% c(
      "d",
      "daughter",
      "son",
      "child",
      "children",
      "offspring"
    ) ~ "child",
    
    z %in% c(
      "gm",
      "grandmother",
      "grandfather",
      "grandparent",
      "grandparents"
    ) ~ "grandparent",
    
    z %in% c(
      "gd",
      "granddaughter",
      "grandson",
      "grandchild",
      "grandchildren"
    ) ~ "grandchild",
    
    z %in% c(
      "a",
      "oa",
      "ya",
      "aunt",
      "uncle",
      "aunt_uncle"
    ) ~ "aunt_uncle",
    
    z %in% c(
      "c",
      "coa",
      "cya",
      "cousin",
      "cousins"
    ) ~ "cousin",
    
    TRUE ~ NA_character_
  )
}


# ============================================================
# 4. Kin-network membership
# ============================================================

kin_network_membership <- tibble::tribble(
  
  ~kin_class,      ~network,
  
  "parent",        "nuclear",
  "sibling",       "nuclear",
  "child",         "nuclear",
  
  "parent",        "lineal",
  "sibling",       "lineal",
  "child",         "lineal",
  "grandparent",   "lineal",
  "grandchild",    "lineal",
  
  "parent",        "collateral",
  "sibling",       "collateral",
  "child",         "collateral",
  "grandparent",   "collateral",
  "grandchild",    "collateral",
  "aunt_uncle",    "collateral",
  "cousin",        "collateral"
)


# ============================================================
# 5. Read public kin data
# ============================================================

read_public_kin_projection <- function(
    path,
    country_code
) {
  
  required_id <- c(
    "year",
    "age_focal",
    "kin",
    "sex_kin",
    "age_kin",
    "Variant"
  )
  
  # Read only the header first (fast even for very large files)
  header_names <- names(
    read_csv(
      path,
      n_max = 0,
      show_col_types = FALSE
    )
  )
  
  missing <- setdiff(
    required_id,
    header_names
  )
  
  if (length(missing) > 0) {
    
    stop(
      "Missing columns in kin file: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  if (!country_code %in% header_names) {
    
    stop(
      "Country column ",
      country_code,
      " was not found."
    )
  }
  
  # Read only the columns that are actually needed
  raw <- read_csv(
    path,
    show_col_types = FALSE,
    col_select = all_of(
      c(
        required_id,
        country_code
      )
    )
  )
  
  raw |>
    transmute(
      
      country =
        country_code,
      
      year =
        as.character(year),
      
      variant =
        str_to_lower(
          as.character(Variant)
        ),
      
      age_focal =
        as.character(age_focal),
      
      age_focal_lower =
        parse_age_lower(age_focal),
      
      age_focal_upper =
        parse_age_upper(age_focal),
      
      age_kin =
        as.character(age_kin),
      
      age_kin_lower =
        parse_age_lower(age_kin),
      
      age_kin_upper =
        parse_age_upper(age_kin),
      
      kin =
        as.character(kin),
      
      sex_kin =
        as.character(sex_kin),
      
      n_living =
        as.numeric(
          .data[[country_code]]
        )
    )
}


# ============================================================
# 6. Read female population
# ============================================================

read_female_population <- function(path) {
  
  raw <- read_csv(
    path,
    show_col_types = FALSE
  )
  
  
  required <- c(
    "country",
    "year",
    "female_population"
  )
  
  missing <- setdiff(
    required,
    names(raw)
  )
  
  if (length(missing) > 0) {
    
    stop(
      "Missing columns in population file: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  if ("age_focal" %in% names(raw)) {
    
    raw |>
      transmute(
        
        country =
          as.character(country),
        
        year =
          as.character(year),
        
        age_focal =
          as.character(age_focal),
        
        age_focal_lower =
          parse_age_lower(age_focal),
        
        age_focal_upper =
          parse_age_upper(age_focal),
        
        female_population =
          as.numeric(
            female_population
          ),
        
        across(
          any_of("trajectory")
        )
      )
    
  } else {
    
    needed_age <- c(
      "age_focal_lower",
      "age_focal_upper"
    )
    
    missing_age <- setdiff(
      needed_age,
      names(raw)
    )
    
    if (length(missing_age) > 0) {
      
      stop(
        "Population data must contain age_focal ",
        "or age_focal_lower / age_focal_upper."
      )
    }
    
    
    raw |>
      mutate(
        
        country =
          as.character(country),
        
        year =
          as.character(year),
        
        age_focal_lower =
          as.numeric(age_focal_lower),
        
        age_focal_upper =
          as.numeric(age_focal_upper),
        
        female_population =
          as.numeric(female_population)
      ) |>
      select(
        country,
        year,
        age_focal_lower,
        age_focal_upper,
        female_population,
        any_of("trajectory")
      )
  }
}


# ============================================================
# 7. Validate kin data
# ============================================================

validate_kin_data <- function(kin_data) {
  
  required <- c(
    "country",
    "year",
    "age_focal_lower",
    "age_focal_upper",
    "age_kin_lower",
    "age_kin_upper",
    "kin",
    "n_living"
  )
  
  missing <- setdiff(
    required,
    names(kin_data)
  )
  
  if (length(missing) > 0) {
    
    stop(
      "Missing required kin columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  if (anyNA(kin_data$n_living)) {
    
    stop(
      "n_living contains missing values."
    )
  }
  
  
  if (any(
    kin_data$n_living < 0
  )) {
    
    stop(
      "n_living cannot be negative."
    )
  }
  
  
  crosses_cut <- with(
    kin_data,
    
    (
      age_kin_lower < WORK_MIN &
        age_kin_upper >= WORK_MIN
    ) |
      
      (
        age_kin_lower < OLD_MIN &
          age_kin_upper >= OLD_MIN
      ) |
      
      (
        age_focal_lower < WORK_MIN &
          age_focal_upper >= WORK_MIN
      ) |
      
      (
        age_focal_lower < OLD_MIN &
          age_focal_upper >= OLD_MIN
      )
  )
  
  
  if (any(crosses_cut)) {
    
    stop(
      "At least one age group crosses age 15 or age 65."
    )
  }
  
  
  invisible(TRUE)
}


# ============================================================
# 8. Calculate age-specific KDR
# ============================================================

calculate_askdr <- function(kin_data) {
  
  if (!"variant" %in% names(kin_data)) {
    
    kin_data <- kin_data |>
      mutate(
        variant = "estimate"
      )
  }
  
  
  validate_kin_data(
    kin_data
  )
  
  
  kin_prepared <- kin_data |>
    mutate(
      
      variant =
        str_to_lower(variant),
      
      kin_class =
        normalize_kin_code(kin),
      
      kin_age_band =
        case_when(
          
          age_kin_upper <= YOUNG_MAX ~
            "young",
          
          age_kin_lower >= WORK_MIN &
            age_kin_upper <= WORK_MAX ~
            "working",
          
          age_kin_lower >= OLD_MIN ~
            "old",
          
          TRUE ~
            NA_character_
        )
    )
  
  
  ignored_codes <- kin_prepared |>
    filter(
      is.na(kin_class)
    ) |>
    distinct(kin) |>
    pull(kin)
  
  
  if (length(
    ignored_codes
  ) > 0) {
    
    message(
      "Ignored kin codes: ",
      paste(
        ignored_codes,
        collapse = ", "
      )
    )
  }
  
  
  kin_expanded <- kin_prepared |>
    filter(
      !is.na(kin_class),
      !is.na(kin_age_band)
    ) |>
    inner_join(
      kin_network_membership,
      by = "kin_class",
      relationship = "many-to-many"
    )
  
  
  if (nrow(
    kin_expanded
  ) == 0) {
    
    stop(
      "No rows remain after kin mapping."
    )
  }
  
  
  scenario_cols <- intersect(
    
    c(
      "country",
      "year",
      "variant",
      "trajectory"
    ),
    
    names(
      kin_expanded
    )
  )
  
  
  group_cols <- c(
    
    scenario_cols,
    
    "network",
    
    "age_focal_lower",
    
    "age_focal_upper"
  )
  
  
  counts <- kin_expanded |>
    group_by(
      across(
        all_of(
          c(
            group_cols,
            "kin_age_band"
          )
        )
      )
    ) |>
    summarise(
      
      n_living =
        sum(
          n_living,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) |>
    pivot_wider(
      
      names_from =
        kin_age_band,
      
      values_from =
        n_living,
      
      values_fill = 0
    )
  
  
  for (
    nm in c(
      "young",
      "working",
      "old"
    )
  ) {
    
    if (
      !nm %in% names(counts)
    ) {
      
      counts[[nm]] <- 0
    }
  }
  
  
  askdr <- counts |>
    mutate(
      
      i_young =
        as.integer(
          age_focal_upper <=
            YOUNG_MAX
        ),
      
      i_working =
        as.integer(
          age_focal_lower >=
            WORK_MIN &
            age_focal_upper <=
            WORK_MAX
        ),
      
      i_old =
        as.integer(
          age_focal_lower >=
            OLD_MIN
        ),
      
      numerator_young =
        young +
        i_young,
      
      numerator_old =
        old +
        i_old,
      
      support_denominator =
        working +
        i_working,
      
      askdr_young =
        if_else(
          
          support_denominator > 0,
          
          numerator_young /
            support_denominator,
          
          NA_real_
        ),
      
      askdr_old =
        if_else(
          
          support_denominator > 0,
          
          numerator_old /
            support_denominator,
          
          NA_real_
        ),
      
      askdr_total =
        askdr_young +
        askdr_old
    ) |>
    
    arrange(
      
      across(
        all_of(
          c(
            scenario_cols,
            "network",
            "age_focal_lower"
          )
        )
      )
    )
  
  
  askdr
}


# ============================================================
# 9. Validate population data
# ============================================================

validate_population_data <- function(pop_data) {
  
  required <- c(
    "country",
    "year",
    "age_focal_lower",
    "age_focal_upper",
    "female_population"
  )
  
  
  missing <- setdiff(
    required,
    names(pop_data)
  )
  
  
  if (length(
    missing
  ) > 0) {
    
    stop(
      "Missing population columns: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  if (
    anyNA(
      pop_data$female_population
    ) ||
    any(
      pop_data$female_population <
      0
    )
  ) {
    
    stop(
      "female_population must be non-missing and non-negative."
    )
  }
  
  
  invisible(TRUE)
}


# ============================================================
# 10. Calculate summary KDR
# ============================================================

calculate_summary_kdr <- function(
    askdr,
    pop_data,
    focal_min = 15,
    focal_max = 64
) {
  
  validate_population_data(
    pop_data
  )
  
  
  weight_group_cols <- c(
    "country",
    "year"
  )
  
  
  if (
    "trajectory" %in%
    names(pop_data)
  ) {
    
    weight_group_cols <- c(
      weight_group_cols,
      "trajectory"
    )
  }
  
  
  pop_working <- pop_data |>
    
    filter(
      
      age_focal_lower >=
        focal_min,
      
      age_focal_upper <=
        focal_max
    ) |>
    
    group_by(
      
      across(
        all_of(
          weight_group_cols
        )
      )
    ) |>
    
    mutate(
      
      population_total_A =
        sum(
          female_population,
          na.rm = TRUE
        ),
      
      pi =
        female_population /
        population_total_A
    ) |>
    
    ungroup()
  
  
  if (
    nrow(pop_working) == 0
  ) {
    
    stop(
      "No population data exist for ages 15–64."
    )
  }
  
  
  join_cols <- c(
    "country",
    "year",
    "age_focal_lower"
  )
  
  
  if (
    "trajectory" %in%
    names(askdr)
  ) {
    
    join_cols <- c(
      join_cols,
      "trajectory"
    )
  }
  
  
  askdr_working <- askdr |>
    
    filter(
      
      age_focal_lower >=
        focal_min,
      
      age_focal_upper <=
        focal_max
    )
  
  
  weighted <- askdr_working |>
    
    left_join(
      
      pop_working |>
        
        select(
          
          all_of(
            join_cols
          ),
          
          female_population,
          
          pi
        ),
      
      by =
        join_cols
    )
  
  
  if (
    anyNA(
      weighted$pi
    )
  ) {
    
    missing_weights <- weighted |>
      
      filter(
        is.na(pi)
      ) |>
      
      distinct(
        country,
        year,
        age_focal_lower
      )
    
    
    print(
      missing_weights
    )
    
    
    stop(
      "Population weights are missing for some focal ages."
    )
  }
  
  
  scenario_cols <- intersect(
    
    c(
      "country",
      "year",
      "variant",
      "trajectory"
    ),
    
    names(weighted)
  )
  
  
  summary_kdr <- weighted |>
    
    group_by(
      
      across(
        all_of(
          c(
            scenario_cols,
            "network"
          )
        )
      )
    ) |>
    
    summarise(
      
      kdr_young =
        sum(
          askdr_young *
            pi,
          na.rm = TRUE
        ),
      
      kdr_old =
        sum(
          askdr_old *
            pi,
          na.rm = TRUE
        ),
      
      kdr_total =
        kdr_young +
        kdr_old,
      
      weight_sum =
        sum(pi),
      
      .groups =
        "drop"
    )
  
  
  bad_weights <- summary_kdr |>
    filter(
      abs(weight_sum - 1) > 1e-6
    )
  
  if (nrow(bad_weights) > 0) {
    
    warning(
      "Population weights do not sum to 1 for ",
      nrow(bad_weights),
      " group(s). Check that the population age groups ",
      "match the kin age groups."
    )
  }
  
  
  list(
    
    age_specific =
      weighted,
    
    summary =
      summary_kdr
  )
}


# ============================================================
# 11. Read files
# ============================================================

kin_data <- read_public_kin_projection(
  
  path =
    KIN_FILE,
  
  country_code =
    COUNTRY_CODE
)


pop_data <- read_female_population(
  POP_FILE
)


# ============================================================
# 12. Keep Iran and valid years
# ============================================================

kin_data <- kin_data |>
  
  filter(
    country ==
      COUNTRY_CODE
  )


pop_data <- pop_data |>
  
  filter(
    country ==
      COUNTRY_CODE
  )



# ============================================================
# 15. Keep only required variants by historical/projection period
# ============================================================

kin_data <- kin_data |>
  
  mutate(
    
    year_start =
      as.numeric(
        str_extract(
          year,
          "^[0-9]{4}"
        )
      )
  ) |>
  
  filter(
    
    (
      year_start <= 2020 &
        variant == "estimate"
    ) |
      
      (
        year_start >= 2025 &
          variant %in%
          c(
            "ci_low_living",
            "median_living",
            "ci_upp_living"
          )
      )
  ) |>
  
  select(
    -year_start
  )


# ============================================================
# 16. Calculate ASKDR for every period
# ============================================================

askdr_all <- calculate_askdr(
  kin_data
)


# ============================================================
# 17. Calculate summary KDR
# ============================================================

kdr_results <- calculate_summary_kdr(
  
  askdr =
    askdr_all,
  
  pop_data =
    pop_data,
  
  focal_min =
    15,
  
  focal_max =
    64
)


kdr_age_specific <-
  kdr_results$age_specific


kdr_all <-
  kdr_results$summary


# ============================================================
# 18. Add time variables
# ============================================================

kdr_all <- kdr_all |>
  
  mutate(
    
    year_start =
      as.numeric(
        str_extract(
          year,
          "^[0-9]{4}"
        )
      ),
    
    year_end =
      as.numeric(
        str_extract(
          year,
          "[0-9]{4}$"
        )
      ),
    
    year_mid =
      (
        year_start +
          year_end
      ) / 2,
    
    period_type =
      case_when(
        
        year_start <= 2020 ~
          "Historical",
        
        year_start >= 2025 ~
          "Projection",
        
        TRUE ~
          NA_character_
      )
  )







# ============================================================
# آماده‌سازی داده تاریخی و پیش‌بینی برای هر سه شبکه
# ============================================================

historical_kdr <- kdr_all |>
  filter(
    year_start <= 2020,
    variant == "estimate"
  )

future_kdr <- kdr_all |>
  filter(
    year_start >= 2025,
    variant %in% c(
      "ci_low_living",
      "median_living",
      "ci_upp_living"
    )
  )


# ============================================================
# تبدیل داده پیش‌بینی به wide
# برای هر سه شبکه
# ============================================================

future_kdr_wide <- future_kdr |>
  select(
    country,
    year,
    year_start,
    year_end,
    year_mid,
    network,
    variant,
    kdr_young,
    kdr_old,
    kdr_total
  ) |>
  pivot_wider(
    names_from = variant,
    values_from = c(
      kdr_young,
      kdr_old,
      kdr_total
    )
  )


# ============================================================
# مرتب‌سازی نام شبکه‌ها
# ============================================================

historical_kdr <- historical_kdr |>
  mutate(
    network = factor(
      network,
      levels = c(
        "nuclear",
        "lineal",
        "collateral"
      ),
      labels = c(
        "Nuclear",
        "Lineal",
        "Collateral"
      )
    )
  )


future_kdr_wide <- future_kdr_wide |>
  mutate(
    network = factor(
      network,
      levels = c(
        "nuclear",
        "lineal",
        "collateral"
      ),
      labels = c(
        "Nuclear",
        "Lineal",
        "Collateral"
      )
    )
  )


# ============================================================
# ساخت خط تاریخی + median آینده برای Young KDR
# ============================================================

young_line_data <- bind_rows(
  
  historical_kdr |>
    transmute(
      country,
      year,
      year_mid,
      network,
      kdr = kdr_young,
      period = "Historical"
    ),
  
  future_kdr_wide |>
    transmute(
      country,
      year,
      year_mid,
      network,
      kdr = kdr_young_median_living,
      period = "Projection"
    )
)


# ============================================================
# ساخت خط تاریخی + median آینده برای Old KDR
# ============================================================

old_line_data <- bind_rows(
  
  historical_kdr |>
    transmute(
      country,
      year,
      year_mid,
      network,
      kdr = kdr_old,
      period = "Historical"
    ),
  
  future_kdr_wide |>
    transmute(
      country,
      year,
      year_mid,
      network,
      kdr = kdr_old_median_living,
      period = "Projection"
    )
)


# ============================================================
# نمودار Young KDR برای هر سه شبکه
# ============================================================

p_young_all_networks <- ggplot() +
  
  geom_ribbon(
    data = future_kdr_wide,
    aes(
      x = year_mid,
      ymin = kdr_young_ci_low_living,
      ymax = kdr_young_ci_upp_living,
      fill = network,
      group = network
    ),
    alpha = 0.13,
    colour = NA
  ) +
  
  geom_line(
    data = young_line_data,
    aes(
      x = year_mid,
      y = kdr,
      color = network,
      group = network
    ),
    linewidth = 1.25
  ) +
  
  geom_point(
    data = historical_kdr,
    aes(
      x = year_mid,
      y = kdr_young,
      color = network
    ),
    size = 1.7
  ) +
  
  geom_vline(
    xintercept = 2025,
    linetype = "dashed",
    linewidth = 0.75,
    color = "grey40"
  ) +
  
  annotate(
    "text",
    x = 2027,
    y = Inf,
    label = "Projection",
    hjust = 0,
    vjust = 1.6,
    fontface = "bold",
    size = 4
  ) +
  
  scale_x_continuous(
    breaks = seq(1950, 2100, 10),
    limits = c(1950, 2100)
  ) +
  
  labs(
    title = "Young Kin Dependency Ratio in Iran",
    subtitle = "Nuclear, lineal and collateral kin networks, 1950–2100",
    x = NULL,
    y = "Young Kin Dependency Ratio",
    color = "Kin network",
    fill = "Kin network",
    caption = "Historical values are estimates; projected lines show medians and shaded bands show lower and upper bounds."
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "top",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )


p_young_all_networks


# ============================================================
# نمودار Old KDR برای هر سه شبکه
# ============================================================

p_old_all_networks <- ggplot() +
  
  geom_ribbon(
    data = future_kdr_wide,
    aes(
      x = year_mid,
      ymin = kdr_old_ci_low_living,
      ymax = kdr_old_ci_upp_living,
      fill = network,
      group = network
    ),
    alpha = 0.13,
    colour = NA
  ) +
  
  geom_line(
    data = old_line_data,
    aes(
      x = year_mid,
      y = kdr,
      color = network,
      group = network
    ),
    linewidth = 1.25
  ) +
  
  geom_point(
    data = historical_kdr,
    aes(
      x = year_mid,
      y = kdr_old,
      color = network
    ),
    size = 1.7
  ) +
  
  geom_vline(
    xintercept = 2025,
    linetype = "dashed",
    linewidth = 0.75,
    color = "grey40"
  ) +
  
  annotate(
    "text",
    x = 2027,
    y = Inf,
    label = "Projection",
    hjust = 0,
    vjust = 1.6,
    fontface = "bold",
    size = 4
  ) +
  
  scale_x_continuous(
    breaks = seq(1950, 2100, 10),
    limits = c(1950, 2100)
  ) +
  
  labs(
    title = "Old-age Kin Dependency Ratio in Iran",
    subtitle = "Nuclear, lineal and collateral kin networks, 1950–2100",
    x = NULL,
    y = "Old-age Kin Dependency Ratio",
    color = "Kin network",
    fill = "Kin network",
    caption = "Historical values are estimates; projected lines show medians and shaded bands show lower and upper bounds."
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    axis.text.y = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "top",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )


p_old_all_networks


# ============================================================
# یک گزینه خواناتر:
# هر شبکه در یک پنل جدا برای Young KDR
# ============================================================

p_young_facet <- ggplot() +
  
  geom_ribbon(
    data = future_kdr_wide,
    aes(
      x = year_mid,
      ymin = kdr_young_ci_low_living,
      ymax = kdr_young_ci_upp_living
    ),
    alpha = 0.18
  ) +
  
  geom_line(
    data = young_line_data,
    aes(
      x = year_mid,
      y = kdr
    ),
    linewidth = 1.2
  ) +
  
  geom_vline(
    xintercept = 2025,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  facet_wrap(
    ~ network,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    breaks = seq(1950, 2100, 20)
  ) +
  
  labs(
    title = "Young Kin Dependency Ratio in Iran",
    subtitle = "Comparison across three kin networks",
    x = NULL,
    y = "Young Kin Dependency Ratio"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )


p_young_facet


# ============================================================
# هر شبکه در یک پنل جدا برای Old KDR
# ============================================================

p_old_facet <- ggplot() +
  
  geom_ribbon(
    data = future_kdr_wide,
    aes(
      x = year_mid,
      ymin = kdr_old_ci_low_living,
      ymax = kdr_old_ci_upp_living
    ),
    alpha = 0.18
  ) +
  
  geom_line(
    data = old_line_data,
    aes(
      x = year_mid,
      y = kdr
    ),
    linewidth = 1.2
  ) +
  
  geom_vline(
    xintercept = 2025,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  facet_wrap(
    ~ network,
    ncol = 1,
    scales = "free_y"
  ) +
  
  scale_x_continuous(
    breaks = seq(1950, 2100, 20)
  ) +
  
  labs(
    title = "Old-age Kin Dependency Ratio in Iran",
    subtitle = "Comparison across three kin networks",
    x = NULL,
    y = "Old-age Kin Dependency Ratio"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank()
  )


p_old_facet


# ============================================================
# ذخیره نمودارها
# ============================================================

ggsave(
  "KDR_young_all_networks.png",
  p_young_all_networks,
  width = 13,
  height = 7,
  dpi = 400
)

ggsave(
  "KDR_old_all_networks.png",
  p_old_all_networks,
  width = 13,
  height = 7,
  dpi = 400
)

ggsave(
  "KDR_young_three_panels.png",
  p_young_facet,
  width = 11,
  height = 10,
  dpi = 400
)

ggsave(
  "KDR_old_three_panels.png",
  p_old_facet,
  width = 11,
  height = 10,
  dpi = 400
)


# ============================================================
# کنترل نهایی
# باید هر سه شبکه دیده شوند
# ============================================================

kdr_all |>
  count(network)

kdr_all |>
  distinct(network)

kdr_all |>
  arrange(
    year_start,
    network,
    variant
  ) |>
  print(n = 100)