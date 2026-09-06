library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(tibble)
library(readxl)

##########################################################################
# this function change single years population into 5 years
#######################################################################

df<-read_excel("C:/Users/m.khalili/Desktop/iranpop.xlsx", sheet = "1")

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


female_population_5year$country<- "IRN"
female_population_5year_save <- female_population_5year %>%
  mutate(
    age_focal = paste0("'", age_focal),
    year = as.character(year)
  )

write_csv(
  female_population_5year_save,
  "C:/Users/m.khalili/Desktop/j.csv"
)

write.csv(
  female_population_5year,
  "C:/Users/m.khalili/Desktop/japan.csv",
  row.names = FALSE
)





YOUNG_MAX <- 14
WORK_MIN  <- 15
WORK_MAX  <- 64
OLD_MIN   <- 65

COUNTRY_CODE <- "IRN"

KIN_FILE <- "C:/Users/m.khalili/Desktop/table_data_desagg.csv"
POP_FILE <- "C:/Users/m.khalili/Desktop/jiran.csv"


# ============================================================
# 2. AGE FUNCTIONS
# ============================================================

parse_age_lower <- function(x) {
  
  x <- as.character(x)
  
  out <- suppressWarnings(
    as.numeric(
      stringr::str_extract(
        x,
        "^[0-9]+"
      )
    )
  )
  
  if (anyNA(out)) {
    
    stop(
      "Lower age could not be extracted for: ",
      paste(
        unique(x[is.na(out)]),
        collapse = ", "
      )
    )
  }
  
  out
}



parse_age_upper <- function(x) {
  
  x <- as.character(x)
  
  lower <- parse_age_lower(x)
  
  is_open <- stringr::str_detect(
    x,
    "\\+"
  )
  
  second_number <- stringr::str_extract(
    x,
    "(?<=-)[0-9]+"
  )
  
  upper <- suppressWarnings(
    as.numeric(second_number)
  )
  
  upper[is_open] <- Inf
  
  idx <- is.na(upper) & !is_open
  
  upper[idx] <- lower[idx]
  
  upper
}


# ============================================================
# 3. NORMALIZE KIN CODES
# ============================================================

normalize_kin_code <- function(x) {
  
  z <- stringr::str_to_lower(
    stringr::str_trim(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    
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
# 4. DEFINE THREE KIN NETWORKS
# ============================================================

kin_network_membership <- tibble::tribble(
  
  ~kin_class,     ~network,
  
  "parent",       "nuclear",
  "sibling",      "nuclear",
  "child",        "nuclear",
  
  "parent",       "lineal",
  "sibling",      "lineal",
  "child",        "lineal",
  "grandparent",  "lineal",
  "grandchild",   "lineal",
  
  "parent",       "collateral",
  "sibling",      "collateral",
  "child",        "collateral",
  "grandparent",  "collateral",
  "grandchild",   "collateral",
  "aunt_uncle",   "collateral",
  "cousin",       "collateral"
)


# ============================================================
# 5. READ KIN DATA
# ============================================================

read_public_kin_projection <- function(
    path,
    country_code
) {
  
  raw <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  
  required_id <- c(
    "year",
    "age_focal",
    "kin",
    "sex_kin",
    "age_kin",
    "Variant"
  )
  
  missing_cols <- setdiff(
    required_id,
    names(raw)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Missing columns in kin file: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  if (!country_code %in% names(raw)) {
    
    stop(
      "Country column ",
      country_code,
      " does not exist in kin file."
    )
  }
  
  raw |>
    dplyr::transmute(
      
      country =
        country_code,
      
      year =
        as.character(.data$year),
      
      variant =
        stringr::str_to_lower(
          as.character(.data$Variant)
        ),
      
      age_focal =
        as.character(.data$age_focal),
      
      age_focal_lower =
        parse_age_lower(
          .data$age_focal
        ),
      
      age_focal_upper =
        parse_age_upper(
          .data$age_focal
        ),
      
      age_kin =
        as.character(.data$age_kin),
      
      age_kin_lower =
        parse_age_lower(
          .data$age_kin
        ),
      
      age_kin_upper =
        parse_age_upper(
          .data$age_kin
        ),
      
      kin =
        as.character(.data$kin),
      
      sex_kin =
        as.character(.data$sex_kin),
      
      n_living =
        as.numeric(
          .data[[country_code]]
        )
    )
}


# ============================================================
# 6. READ FEMALE POPULATION
# ============================================================

read_female_population <- function(path) {
  
  raw <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  
  required <- c(
    "country",
    "year",
    "female_population"
  )
  
  missing_cols <- setdiff(
    required,
    names(raw)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Missing columns in population file: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  if ("age_focal" %in% names(raw)) {
    
    out <- raw |>
      dplyr::transmute(
        
        country =
          as.character(.data$country),
        
        year =
          as.character(.data$year),
        
        age_focal =
          as.character(.data$age_focal),
        
        age_focal_lower =
          parse_age_lower(
            .data$age_focal
          ),
        
        age_focal_upper =
          parse_age_upper(
            .data$age_focal
          ),
        
        female_population =
          as.numeric(
            .data$female_population
          )
      )
    
  } else {
    
    required_age <- c(
      "age_focal_lower",
      "age_focal_upper"
    )
    
    missing_age <- setdiff(
      required_age,
      names(raw)
    )
    
    if (length(missing_age) > 0) {
      
      stop(
        "Population file must contain age_focal ",
        "or age_focal_lower and age_focal_upper."
      )
    }
    
    out <- raw |>
      dplyr::transmute(
        
        country =
          as.character(.data$country),
        
        year =
          as.character(.data$year),
        
        age_focal_lower =
          as.numeric(.data$age_focal_lower),
        
        age_focal_upper =
          as.numeric(.data$age_focal_upper),
        
        female_population =
          as.numeric(.data$female_population)
      )
  }
  
  out
}


# ============================================================
# 7. VALIDATE KIN DATA
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
  
  missing_cols <- setdiff(
    required,
    names(kin_data)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Missing required kin columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  if (anyNA(kin_data$n_living)) {
    
    stop(
      "n_living contains missing values."
    )
  }
  
  if (any(kin_data$n_living < 0)) {
    
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
      "At least one age group crosses age 15 or 65."
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 8. CALCULATE AGE-SPECIFIC KDR
# ============================================================

calculate_askdr <- function(kin_data) {
  
  if (!"variant" %in% names(kin_data)) {
    
    kin_data <- kin_data |>
      dplyr::mutate(
        variant = "estimate"
      )
  }
  
  validate_kin_data(
    kin_data
  )
  
  
  # ----------------------------------------------------------
  # Prepare kin data
  # ----------------------------------------------------------
  
  kin_prepared <- kin_data |>
    dplyr::mutate(
      
      variant =
        stringr::str_to_lower(
          as.character(.data$variant)
        ),
      
      kin_class =
        normalize_kin_code(
          .data$kin
        ),
      
      kin_age_band =
        dplyr::case_when(
          
          .data$age_kin_upper <=
            YOUNG_MAX ~
            "young",
          
          .data$age_kin_lower >=
            WORK_MIN &
            .data$age_kin_upper <=
            WORK_MAX ~
            "working",
          
          .data$age_kin_lower >=
            OLD_MIN ~
            "old",
          
          TRUE ~
            NA_character_
        )
    )
  
  
  # ----------------------------------------------------------
  # Unknown kin codes
  # ----------------------------------------------------------
  
  ignored_codes <- kin_prepared |>
    dplyr::filter(
      is.na(.data$kin_class)
    ) |>
    dplyr::distinct(
      .data$kin
    ) |>
    dplyr::pull(
      .data$kin
    )
  
  
  if (length(ignored_codes) > 0) {
    
    message(
      "Ignored kin codes: ",
      paste(
        ignored_codes,
        collapse = ", "
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Expand kin into networks
  # many-to-many is intentional
  # ----------------------------------------------------------
  
  kin_expanded <- kin_prepared |>
    
    dplyr::filter(
      !is.na(.data$kin_class),
      !is.na(.data$kin_age_band)
    ) |>
    
    dplyr::inner_join(
      
      kin_network_membership,
      
      by =
        "kin_class",
      
      relationship =
        "many-to-many"
    )
  
  
  if (nrow(kin_expanded) == 0) {
    
    stop(
      "No kin rows remain after network mapping."
    )
  }
  
  
  # ----------------------------------------------------------
  # Aggregate kin by age band
  # ----------------------------------------------------------
  
  counts_long <- kin_expanded |>
    
    dplyr::group_by(
      
      .data$country,
      .data$year,
      .data$variant,
      .data$network,
      .data$age_focal_lower,
      .data$age_focal_upper,
      .data$kin_age_band
    ) |>
    
    dplyr::summarise(
      
      n_living =
        sum(
          .data$n_living,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
    )
  
  
  # ----------------------------------------------------------
  # Long -> wide
  # ----------------------------------------------------------
  
  counts <- counts_long |>
    
    tidyr::pivot_wider(
      
      names_from =
        "kin_age_band",
      
      values_from =
        "n_living",
      
      values_fill =
        0
    )
  
  
  # ----------------------------------------------------------
  # Ensure all three age-band columns exist
  # ----------------------------------------------------------
  
  if (!"young" %in% names(counts)) {
    counts$young <- 0
  }
  
  if (!"working" %in% names(counts)) {
    counts$working <- 0
  }
  
  if (!"old" %in% names(counts)) {
    counts$old <- 0
  }
  
  
  # ----------------------------------------------------------
  # Calculate ASKDR
  # ----------------------------------------------------------
  
  askdr <- counts |>
    
    dplyr::mutate(
      
      i_young =
        as.integer(
          .data$age_focal_upper <=
            YOUNG_MAX
        ),
      
      i_working =
        as.integer(
          .data$age_focal_lower >=
            WORK_MIN &
            .data$age_focal_upper <=
            WORK_MAX
        ),
      
      i_old =
        as.integer(
          .data$age_focal_lower >=
            OLD_MIN
        ),
      
      numerator_young =
        .data$young +
        .data$i_young,
      
      numerator_old =
        .data$old +
        .data$i_old,
      
      support_denominator =
        .data$working +
        .data$i_working,
      
      askdr_young =
        dplyr::if_else(
          
          .data$support_denominator > 0,
          
          .data$numerator_young /
            .data$support_denominator,
          
          NA_real_
        ),
      
      askdr_old =
        dplyr::if_else(
          
          .data$support_denominator > 0,
          
          .data$numerator_old /
            .data$support_denominator,
          
          NA_real_
        ),
      
      askdr_total =
        .data$askdr_young +
        .data$askdr_old
    ) |>
    
    dplyr::arrange(
      
      .data$country,
      .data$year,
      .data$variant,
      .data$network,
      .data$age_focal_lower
    )
  
  
  askdr
}


# ============================================================
# 9. VALIDATE POPULATION DATA
# ============================================================

validate_population_data <- function(pop_data) {
  
  required <- c(
    "country",
    "year",
    "age_focal_lower",
    "age_focal_upper",
    "female_population"
  )
  
  missing_cols <- setdiff(
    required,
    names(pop_data)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Missing population columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  if (anyNA(pop_data$female_population)) {
    
    stop(
      "female_population contains missing values."
    )
  }
  
  if (any(pop_data$female_population < 0)) {
    
    stop(
      "female_population cannot be negative."
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 10. CALCULATE SUMMARY KDR
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
  
  
  # ----------------------------------------------------------
  # Population age weights
  # ----------------------------------------------------------
  
  pop_working <- pop_data |>
    
    dplyr::filter(
      
      .data$age_focal_lower >=
        focal_min,
      
      .data$age_focal_upper <=
        focal_max
    ) |>
    
    dplyr::group_by(
      .data$country,
      .data$year
    ) |>
    
    dplyr::mutate(
      
      population_total_A =
        sum(
          .data$female_population,
          na.rm = TRUE
        ),
      
      pi =
        .data$female_population /
        .data$population_total_A
    ) |>
    
    dplyr::ungroup()
  
  
  if (nrow(pop_working) == 0) {
    
    stop(
      "No population observations exist for ages 15-64."
    )
  }
  
  
  # ----------------------------------------------------------
  # Check duplicates
  # ----------------------------------------------------------
  
  duplicate_pop <- pop_working |>
    
    dplyr::count(
      .data$country,
      .data$year,
      .data$age_focal_lower
    ) |>
    
    dplyr::filter(
      .data$n > 1
    )
  
  
  if (nrow(duplicate_pop) > 0) {
    
    print(
      duplicate_pop
    )
    
    stop(
      "Duplicate population rows exist for country/year/age."
    )
  }
  
  
  # ----------------------------------------------------------
  # Restrict ASKDR to focal ages 15-64
  # ----------------------------------------------------------
  
  askdr_working <- askdr |>
    
    dplyr::filter(
      
      .data$age_focal_lower >=
        focal_min,
      
      .data$age_focal_upper <=
        focal_max
    )
  
  
  # ----------------------------------------------------------
  # Join population weights
  # ----------------------------------------------------------
  
  weighted <- askdr_working |>
    
    dplyr::left_join(
      
      pop_working |>
        
        dplyr::select(
          
          .data$country,
          .data$year,
          .data$age_focal_lower,
          .data$female_population,
          .data$pi
        ),
      
      by = c(
        "country",
        "year",
        "age_focal_lower"
      )
    )
  
  
  # ----------------------------------------------------------
  # Check missing weights
  # ----------------------------------------------------------
  
  if (anyNA(weighted$pi)) {
    
    missing_weights <- weighted |>
      
      dplyr::filter(
        is.na(.data$pi)
      ) |>
      
      dplyr::distinct(
        .data$country,
        .data$year,
        .data$age_focal_lower
      )
    
    print(
      missing_weights
    )
    
    stop(
      "Population weights are missing for some focal ages."
    )
  }
  
  
  # ----------------------------------------------------------
  # Summary KDR
  # ----------------------------------------------------------
  
  summary_kdr <- weighted |>
    
    dplyr::group_by(
      
      .data$country,
      .data$year,
      .data$variant,
      .data$network
    ) |>
    
    dplyr::summarise(
      
      kdr_young =
        sum(
          .data$askdr_young *
            .data$pi,
          na.rm = TRUE
        ),
      
      kdr_old =
        sum(
          .data$askdr_old *
            .data$pi,
          na.rm = TRUE
        ),
      
      kdr_total =
        .data$kdr_young +
        .data$kdr_old,
      
      weight_sum =
        sum(
          .data$pi
        ),
      
      .groups =
        "drop"
    )
  
  
  list(
    
    age_specific =
      weighted,
    
    summary =
      summary_kdr
  )
}


# ============================================================
# 11. READ DATA
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
# 12. KEEP SELECTED COUNTRY
# ============================================================

kin_data <- kin_data |>
  
  dplyr::filter(
    .data$country ==
      COUNTRY_CODE
  )


pop_data <- pop_data |>
  
  dplyr::filter(
    .data$country ==
      COUNTRY_CODE
  )




kin_data <- kin_data |>
  
  dplyr::mutate(
    
    year_start =
      as.numeric(
        stringr::str_extract(
          .data$year,
          "^[0-9]{4}"
        )
      )
  ) |>
  
  dplyr::filter(
    
    (
      .data$year_start <= 2020 &
        .data$variant ==
        "estimate"
    ) |
      
      (
        .data$year_start >= 2025 &
          .data$variant %in%
          c(
            "ci_low_living",
            "median_living",
            "ci_upp_living"
          )
      )
  ) |>
  
  dplyr::select(
    -.data$year_start
  )



# ============================================================
# 17. CALCULATE ASKDR
# ============================================================

askdr_all <- calculate_askdr(
  kin_data
)



# ============================================================
# 19. CALCULATE SUMMARY KDR
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
# 20. ADD TIME VARIABLES
# ============================================================

kdr_all <- kdr_all |>
  
  dplyr::mutate(
    
    year_start =
      as.numeric(
        stringr::str_extract(
          .data$year,
          "^[0-9]{4}"
        )
      ),
    
    year_end =
      as.numeric(
        stringr::str_extract(
          .data$year,
          "[0-9]{4}$"
        )
      ),
    
    year_mid =
      (
        .data$year_start +
          .data$year_end
      ) / 2,
    
    period_type =
      dplyr::case_when(
        
        .data$year_start <=
          2020 ~
          "Historical",
        
        .data$year_start >=
          2025 ~
          "Projection",
        
        TRUE ~
          NA_character_
      )
  )


# ============================================================
# 21. HISTORICAL DATA
# ============================================================

historical_kdr <- kdr_all |>
  
  dplyr::filter(
    
    .data$year_start <=
      2020,
    
    .data$variant ==
      "estimate"
  )


# ============================================================
# 22. FUTURE DATA
# ============================================================

future_kdr <- kdr_all |>
  
  dplyr::filter(
    
    .data$year_start >=
      2025,
    
    .data$variant %in%
      c(
        "ci_low_living",
        "median_living",
        "ci_upp_living"
      )
  )


# ============================================================
# 23. FUTURE LONG -> WIDE
# ============================================================

future_kdr_wide <- future_kdr |>
  
  dplyr::select(
    
    .data$country,
    .data$year,
    .data$year_start,
    .data$year_end,
    .data$year_mid,
    .data$network,
    .data$variant,
    .data$kdr_young,
    .data$kdr_old,
    .data$kdr_total
  ) |>
  
  tidyr::pivot_wider(
    
    names_from =
      "variant",
    
    values_from =
      c(
        "kdr_young",
        "kdr_old",
        "kdr_total"
      )
  )


# ============================================================
# 24. NETWORK LABELS
# ============================================================

historical_kdr <- historical_kdr |>
  
  dplyr::mutate(
    
    network =
      factor(
        
        .data$network,
        
        levels =
          c(
            "nuclear",
            "lineal",
            "collateral"
          ),
        
        labels =
          c(
            "Nuclear",
            "Lineal",
            "Collateral"
          )
      )
  )


future_kdr_wide <- future_kdr_wide |>
  
  dplyr::mutate(
    
    network =
      factor(
        
        .data$network,
        
        levels =
          c(
            "nuclear",
            "lineal",
            "collateral"
          ),
        
        labels =
          c(
            "Nuclear",
            "Lineal",
            "Collateral"
          )
      )
  )


# ============================================================
# 25. YOUNG CENTRAL LINES
# ============================================================

young_line_data <- dplyr::bind_rows(
  
  historical_kdr |>
    
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr =
        .data$kdr_young,
      
      period =
        "Historical"
    ),
  
  
  future_kdr_wide |>
    
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr =
        .data$kdr_young_median_living,
      
      period =
        "Projection"
    )
)


# ============================================================
# 26. OLD CENTRAL LINES
# ============================================================

old_line_data <- dplyr::bind_rows(
  
  historical_kdr |>
    
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr =
        .data$kdr_old,
      
      period =
        "Historical"
    ),
  
  
  future_kdr_wide |>
    
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr =
        .data$kdr_old_median_living,
      
      period =
        "Projection"
    )
)


# ============================================================
# 27. YOUNG KDR - ALL THREE NETWORKS
# ============================================================

p_young <- ggplot2::ggplot() +
  
  ggplot2::geom_ribbon(
    
    data =
      future_kdr_wide,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      ymin =
        .data$kdr_young_ci_low_living,
      
      ymax =
        .data$kdr_young_ci_upp_living,
      
      fill =
        .data$network,
      
      group =
        .data$network
    ),
    
    alpha =
      0.15,
    
    colour =
      NA
  ) +
  
  ggplot2::geom_line(
    
    data =
      young_line_data,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr,
      
      color =
        .data$network,
      
      group =
        .data$network
    ),
    
    linewidth =
      1.25
  ) +
  
  ggplot2::geom_vline(
    
    xintercept =
      2025,
    
    linetype =
      "dashed",
    
    linewidth =
      0.7
  ) +
  
  ggplot2::scale_x_continuous(
    
    breaks =
      seq(
        1950,
        2100,
        10
      ),
    
    limits =
      c(
        1950,
        2100
      )
  ) +
  
  ggplot2::labs(
    
    title =
      "Young Kin Dependency Ratio in Japan",
    
    subtitle =
      "Nuclear, lineal and collateral kin networks, 1950–2100",
    
    x =
      NULL,
    
    y =
      "Young Kin Dependency Ratio",
    
    color =
      "Kin network",
    
    fill =
      "Kin network"
  ) +
  
  ggplot2::theme_minimal(
    base_size =
      13
  ) +
  
  ggplot2::theme(
    
    plot.title =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.text.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    legend.position =
      "top",
    
    panel.grid.minor =
      ggplot2::element_blank()
  )


p_young


# ============================================================
# 28. OLD KDR - ALL THREE NETWORKS
# ============================================================

p_old <- ggplot2::ggplot() +
  
  ggplot2::geom_ribbon(
    
    data =
      future_kdr_wide,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      ymin =
        .data$kdr_old_ci_low_living,
      
      ymax =
        .data$kdr_old_ci_upp_living,
      
      fill =
        .data$network,
      
      group =
        .data$network
    ),
    
    alpha =
      0.15,
    
    colour =
      NA
  ) +
  
  ggplot2::geom_line(
    
    data =
      old_line_data,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr,
      
      color =
        .data$network,
      
      group =
        .data$network
    ),
    
    linewidth =
      1.25
  ) +
  
  ggplot2::geom_vline(
    
    xintercept =
      2025,
    
    linetype =
      "dashed",
    
    linewidth =
      0.7
  ) +
  
  ggplot2::scale_x_continuous(
    
    breaks =
      seq(
        1950,
        2100,
        10
      ),
    
    limits =
      c(
        1950,
        2100
      )
  ) +
  
  ggplot2::labs(
    
    title =
      "Old-age Kin Dependency Ratio in Japan",
    
    subtitle =
      "Nuclear, lineal and collateral kin networks, 1950–2100",
    
    x =
      NULL,
    
    y =
      "Old-age Kin Dependency Ratio",
    
    color =
      "Kin network",
    
    fill =
      "Kin network"
  ) +
  
  ggplot2::theme_minimal(
    base_size =
      13
  ) +
  
  ggplot2::theme(
    
    plot.title =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.text.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    legend.position =
      "top",
    
    panel.grid.minor =
      ggplot2::element_blank()
  )


p_old


# ============================================================
# 29. FACET YOUNG
# ============================================================

p_young_facet <- ggplot2::ggplot() +
  
  ggplot2::geom_ribbon(
    
    data =
      future_kdr_wide,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      ymin =
        .data$kdr_young_ci_low_living,
      
      ymax =
        .data$kdr_young_ci_upp_living
    ),
    
    alpha =
      0.20
  ) +
  
  ggplot2::geom_line(
    
    data =
      young_line_data,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr
    ),
    
    linewidth =
      1.2
  ) +
  
  ggplot2::geom_vline(
    
    xintercept =
      2025,
    
    linetype =
      "dashed"
  ) +
  
  ggplot2::facet_wrap(
    
    ~ network,
    
    ncol =
      1,
    
    scales =
      "free_y"
  ) +
  
  ggplot2::scale_x_continuous(
    
    breaks =
      seq(
        1950,
        2100,
        20
      )
  ) +
  
  ggplot2::labs(
    
    title =
      "Young Kin Dependency Ratio in Japan",
    
    x =
      NULL,
    
    y =
      "Young Kin Dependency Ratio"
  ) +
  
  ggplot2::theme_minimal(
    base_size =
      13
  ) +
  
  ggplot2::theme(
    
    strip.text =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    panel.grid.minor =
      ggplot2::element_blank()
  )


p_young_facet


# ============================================================
# 30. FACET OLD
# ============================================================

p_old_facet <- ggplot2::ggplot() +
  
  ggplot2::geom_ribbon(
    
    data =
      future_kdr_wide,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      ymin =
        .data$kdr_old_ci_low_living,
      
      ymax =
        .data$kdr_old_ci_upp_living
    ),
    
    alpha =
      0.20
  ) +
  
  ggplot2::geom_line(
    
    data =
      old_line_data,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr
    ),
    
    linewidth =
      1.2
  ) +
  
  ggplot2::geom_vline(
    
    xintercept =
      2025,
    
    linetype =
      "dashed"
  ) +
  
  ggplot2::facet_wrap(
    
    ~ network,
    
    ncol =
      1,
    
    scales =
      "free_y"
  ) +
  
  ggplot2::scale_x_continuous(
    
    breaks =
      seq(
        1950,
        2100,
        20
      )
  ) +
  
  ggplot2::labs(
    
    title =
      "Old-age Kin Dependency Ratio in Japan",
    
    x =
      NULL,
    
    y =
      "Old-age Kin Dependency Ratio"
  ) +
  
  ggplot2::theme_minimal(
    base_size =
      13
  ) +
  
  ggplot2::theme(
    
    strip.text =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    panel.grid.minor =
      ggplot2::element_blank()
  )


p_old_facet


# ============================================================
# 31. SAVE RESULTS
# ============================================================

readr::write_csv(
  
  kdr_all,
  
  "C:/Users/m.khalili/Desktop/KDR_IRN_1950_2100.csv"
)


readr::write_csv(
  
  kdr_age_specific,
  
  "C:/Users/m.khalili/Desktop/ASKDR_irN_1950_2100.csv"
)


readr::write_csv(
  
  future_kdr_wide,
  
  "C:/Users/m.khalili/Desktop/KDR_JPN_projection.csv"
)


# ============================================================
# 32. SAVE PLOTS
# ============================================================

ggplot2::ggsave(
  
  filename =
    "C:/Users/m.khalili/Desktop/KDR_old_JPN.png",
  
  plot =
    p_old,
  
  width =
    10,
  
  height =
    7,
  
  dpi =
    500
)



#**********************************************************************
#**************************************************************************
#************************************************************************
# ============================================================
# 31. TOTAL KDR
# KDR Total = KDR Young + KDR Old
# ============================================================


# ------------------------------------------------------------
# 31.1 کنترل اینکه KDR کل درست محاسبه شده
# ------------------------------------------------------------

kdr_all <- kdr_all |>
  dplyr::mutate(
    kdr_total_check =
      .data$kdr_young +
      .data$kdr_old
  )


# اختلاف باید صفر یا بسیار نزدیک صفر باشد
kdr_all |>
  dplyr::summarise(
    max_difference =
      max(
        abs(
          .data$kdr_total -
            .data$kdr_total_check
        ),
        na.rm = TRUE
      )
  )


# ============================================================
# 32. HISTORICAL TOTAL KDR
# ============================================================

total_historical <- historical_kdr |>
  dplyr::transmute(
    
    country =
      .data$country,
    
    year =
      .data$year,
    
    year_start =
      .data$year_start,
    
    year_end =
      .data$year_end,
    
    year_mid =
      .data$year_mid,
    
    network =
      .data$network,
    
    kdr_total =
      .data$kdr_young +
      .data$kdr_old,
    
    period =
      "Historical"
  )


# ============================================================
# 33. PROJECTED TOTAL KDR
# ============================================================

total_future <- future_kdr_wide |>
  dplyr::transmute(
    
    country =
      .data$country,
    
    year =
      .data$year,
    
    year_start =
      .data$year_start,
    
    year_end =
      .data$year_end,
    
    year_mid =
      .data$year_mid,
    
    network =
      .data$network,
    
    # lower projection bound
    kdr_total_low =
      .data$kdr_total_ci_low_living,
    
    # median projection
    kdr_total_median =
      .data$kdr_total_median_living,
    
    # upper projection bound
    kdr_total_high =
      .data$kdr_total_ci_upp_living,
    
    period =
      "Projection"
  )


# ============================================================
# 34. CENTRAL LINE:
# Historical Estimate + Future Median
# ============================================================

total_line_data <- dplyr::bind_rows(
  
  total_historical |>
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr_total =
        .data$kdr_total,
      
      period =
        "Historical"
    ),
  
  
  total_future |>
    dplyr::transmute(
      
      country =
        .data$country,
      
      year =
        .data$year,
      
      year_mid =
        .data$year_mid,
      
      network =
        .data$network,
      
      kdr_total =
        .data$kdr_total_median,
      
      period =
        "Projection"
    )
)


# مرتب‌سازی
total_line_data <- total_line_data |>
  dplyr::arrange(
    .data$network,
    .data$year_mid
  )



# ============================================================
# 36. TOTAL KDR PLOT
# ALL THREE NETWORKS IN ONE FIGURE
# ============================================================

p_total <- ggplot2::ggplot() +
  
  # ----------------------------------------------------------
# Projection uncertainty bands
# ----------------------------------------------------------

ggplot2::geom_ribbon(
  
  data =
    total_future,
  
  ggplot2::aes(
    
    x =
      .data$year_mid,
    
    ymin =
      .data$kdr_total_low,
    
    ymax =
      .data$kdr_total_high,
    
    fill =
      .data$network,
    
    group =
      .data$network
  ),
  
  alpha =
    0.14,
  
  colour =
    NA
) +
  
  
  # ----------------------------------------------------------
# Historical estimate + future median
# ----------------------------------------------------------

ggplot2::geom_line(
  
  data =
    total_line_data,
  
  ggplot2::aes(
    
    x =
      .data$year_mid,
    
    y =
      .data$kdr_total,
    
    color =
      .data$network,
    
    group =
      .data$network
  ),
  
  linewidth =
    1.35
) +
  
  
  # ----------------------------------------------------------
# Historical points
# ----------------------------------------------------------

ggplot2::geom_point(
  
  data =
    total_historical,
  
  ggplot2::aes(
    
    x =
      .data$year_mid,
    
    y =
      .data$kdr_total,
    
    color =
      .data$network
  ),
  
  size =
    1.8
) +
  
  
  # ----------------------------------------------------------
# Beginning of projection
# ----------------------------------------------------------

ggplot2::geom_vline(
  
  xintercept =
    2025,
  
  linetype =
    "dashed",
  
  linewidth =
    0.7,
  
  color =
    "grey40"
) +
  
  
  # ----------------------------------------------------------
# Projection label
# ----------------------------------------------------------

ggplot2::annotate(
  
  "text",
  
  x =
    2027,
  
  y =
    Inf,
  
  label =
    "Projection",
  
  hjust =
    0,
  
  vjust =
    1.5,
  
  fontface =
    "bold",
  
  size =
    4
) +
  
  
  # ----------------------------------------------------------
# X axis
# ----------------------------------------------------------

ggplot2::scale_x_continuous(
  
  breaks =
    seq(
      1950,
      2100,
      10
    ),
  
  limits =
    c(
      1950,
      2100
    ),
  
  expand =
    ggplot2::expansion(
      mult =
        c(
          0.01,
          0.01
        )
    )
) +
  
  
  # ----------------------------------------------------------
# Y axis
# ----------------------------------------------------------

ggplot2::scale_y_continuous(
  
  expand =
    ggplot2::expansion(
      mult =
        c(
          0.03,
          0.10
        )
    )
) +
  
  
  # ----------------------------------------------------------
# Titles
# ----------------------------------------------------------

ggplot2::labs(
  
  title =
    "Total Kin Dependency Ratio in Japan",
  
  subtitle =
    "Young and old-age kin dependency combined across three kin networks, 1950–2100",
  
  x =
    NULL,
  
  y =
    "Total Kin Dependency Ratio",
  
  color =
    "Kin network",
  
  fill =
    "Kin network",
  
  caption =
    "KDR Total = KDR Young + KDR Old. Historical values are estimates; projected lines represent medians and shaded bands represent lower and upper projection bounds."
) +
  
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

ggplot2::theme_minimal(
  base_size =
    13
) +
  
  ggplot2::theme(
    
    plot.title =
      ggplot2::element_text(
        face =
          "bold",
        size =
          17
      ),
    
    plot.subtitle =
      ggplot2::element_text(
        size =
          11
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold",
        size =
          12
      ),
    
    axis.text.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.text.x =
      ggplot2::element_text(
        angle =
          45,
        hjust =
          1
      ),
    
    legend.position =
      "top",
    
    legend.title =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    legend.text =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    panel.grid.minor =
      ggplot2::element_blank(),
    
    plot.caption =
      ggplot2::element_text(
        hjust =
          0,
        size =
          9
      )
  )


# نمایش نمودار
p_total


# ============================================================
# 37. TOTAL KDR - FACET VERSION
# Each kin network in separate panel
# ============================================================

p_total_facet <- ggplot2::ggplot() +
  
  ggplot2::geom_ribbon(
    
    data =
      total_future,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      ymin =
        .data$kdr_total_low,
      
      ymax =
        .data$kdr_total_high
    ),
    
    alpha =
      0.20
  ) +
  
  
  ggplot2::geom_line(
    
    data =
      total_line_data,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr_total
    ),
    
    linewidth =
      1.25
  ) +
  
  
  ggplot2::geom_point(
    
    data =
      total_historical,
    
    ggplot2::aes(
      
      x =
        .data$year_mid,
      
      y =
        .data$kdr_total
    ),
    
    size =
      1.5
  ) +
  
  
  ggplot2::geom_vline(
    
    xintercept =
      2025,
    
    linetype =
      "dashed",
    
    linewidth =
      0.7
  ) +
  
  
  ggplot2::facet_wrap(
    
    ~ network,
    
    ncol =
      1,
    
    scales =
      "free_y"
  ) +
  
  
  ggplot2::scale_x_continuous(
    
    breaks =
      seq(
        1950,
        2100,
        20
      )
  ) +
  
  
  ggplot2::labs(
    
    title =
      "Total Kin Dependency Ratio in Japan",
    
    subtitle =
      "Historical estimates and projected median with uncertainty bounds",
    
    x =
      NULL,
    
    y =
      "Total Kin Dependency Ratio",
    
    caption =
      "KDR Total = KDR Young + KDR Old"
  ) +
  
  
  ggplot2::theme_minimal(
    base_size =
      13
  ) +
  
  ggplot2::theme(
    
    plot.title =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.title.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    axis.text.y =
      ggplot2::element_text(
        face =
          "bold"
      ),
    
    strip.text =
      ggplot2::element_text(
        face =
          "bold",
        size =
          12
      ),
    
    panel.grid.minor =
      ggplot2::element_blank()
  )


p_total_facet


