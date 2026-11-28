
#install.packages()
 
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(rsocsim)
library(tidyverse) #For data wrangling
library(ggh4x) #for extended facet plotting
library(data.table)

#####################################################################################
################

# create a new folder for all the files related to a simulation.
# this will be in your home- or user-directory:
folder = rsocsim::create_simulation_folder()

# create a new supervisory-file. supervisory-files tell socsim what
# to simulate. create_sup_file will create a very basic supervisory file
# and it copies some rate-files that will also be needed into the 
# simulation folder:
supfile = rsocsim::create_sup_file(folder)

# Choose a random-number seed:
seed = 300

# Start the simulation:
rsocsim::socsim(folder,supfile,seed)
#############################


ASFR<- read_excel("C:/Users/m.khalili/Desktop/2.xlsx", sheet = "1")
ASFR$ASFR<- as.numeric(ASFR$ASFR)

ASFR <- 
  ASFR %>% 
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55", 
                         TRUE ~ Age),
         Age = as.numeric(Age), 
         Age_up = Age + 1, # SOCSIM uses the upper age bound
         Month = 0, 
         ASFR_mo = ASFR/12) %>% 
  select(-ASFR)


ASFR <- 
  ASFR %>% 
  group_by(Year) %>% 
  group_split() %>% 
  map_df(~ add_row(.x,
                   Year = unique(.x$Year), 
                   Age = 0, Age_up = 12,  Month = 0, ASFR_mo = 0.0, 
                   .before = 1)) %>% 
  group_by(Year) %>% 
  group_split() %>% 
  map_df(~ add_row(.x, 
                   Year = unique(.x$Year), 
                   Age = 56, Age_up = 111, Month = 0, ASFR_mo = 0.0, 
                   .after = 45)) %>% 
  ungroup() %>% 
  select(-Age)

# Extract the years available in HFD
years <- ASFR %>% pull(Year) %>% unique()
# Row numbers corresponding to sequence of years of age in ASFR
rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()



ltf<- read_excel("C:/Users/m.khalili/Desktop/2.xlsx", sheet = "f")
ltm<- read_excel("C:/Users/m.khalili/Desktop/2.xlsx", sheet = "m")



ltf$qx<- as.numeric(ltf$qx)
ltm$qx<- as.numeric(ltm$qx)


# Set size of initial population
size_opop <-  20000

# Create data.frame with 14 columns and nrows = size_opop
presim.opop <- setNames(data.frame(matrix(data = 0, ncol = 14, nrow = size_opop)), 
                        c("pid","fem","group","nev","dob","mom","pop",
                          "nesibm","nesibp","lborn","marid","mstat","dod","fmult"))

# Add pid 1:sizeopop
presim.opop$pid <- 1:size_opop

# Add sex randomly
presim.opop$fem <- sample(0:1, nrow(presim.opop), replace = T)

# Add group 1 for all individuals
presim.opop$group <- 1

# Add random dates of birth (max age around 50)
presim.opop$dob <- sample(600:1200, nrow(presim.opop), replace = T)

# Write initial population for pre-simulation (without fertility multiplier)
write.table(presim.opop, "presim.opop", row.names = F, col.names = F)


## Create an empty data frame for presim.omar
presim.omar <- data.frame()

# Write empty omar for pre-simulation
write.table(presim.omar, "presim.omar", row.names = F, col.names = F)



# Specify the folder where the supervisory and the rate files are. 
# If the R session is running through the project, you can use the following command. 
folder <- getwd()

# Type the name of the supervisory file  stored in the above folder:
supfile <- "socsim_USA.sup"

# Choose a seed number (today's date) for the simulation:
seed <- "120423"

# Run a single SOCSIM-simulation with a given folder and the provided supervisory file
# using the "future" process method
rsocsim::socsim(folder, supfile, seed, process_method = "future")



## Read the opop file using the read_opop function
opop <- rsocsim::read_opop(folder = getwd(), supfile = "socsim_USA.sup", 
                           seed = "120423", suffix = "",  fn = NULL)
## Read the omar file using the read_opop function
omar <- rsocsim::read_omar(folder = getwd(), supfile = "socsim_USA.sup", 
                           seed = "120423", suffix = "",  fn = NULL)

head(opop)
head(omar)



# Estimate age-specific fertility rates
asfr_sim <- rsocsim::estimate_fertility_rates(opop = opop,
                                              final_sim_year = 2020, #[Jan-Dec]
                                              year_min = 1935, # Closed [
                                              year_max = 2020, # Open )
                                              year_group = 5, 
                                              age_min_fert = 10, # Closed [
                                              age_max_fert = 55, # Open )
                                              age_group = 5) #[,)

# Estimate age-specific mortality rates
asmr_sim <- rsocsim::estimate_mortality_rates(opop = opop, 
                                              final_sim_year = 2020, # [Jan-Dec]
                                              year_min = 1935, # Closed
                                              year_max = 2020, # Open )
                                              year_group = 5,
                                              age_max_mort = 110, # Open )
                                              age_group = 5) #[,)
# Choose years to plot (in intervals).
yrs_plot <- c("[1935,1940)", "[1980,1985)", "[2015,2020)") 

# Get the age levels to define them before plotting and avoid wrong order
age_levels <- levels(asmr_sim$age)

bind_rows(asfr_sim %>%
            mutate(rate = "ASFR",                   
                   sex = "female"),
          asmr_sim %>% 
            mutate(rate = "ASMR") %>% 
            filter(sex == "female")) %>% 
  mutate(age = factor(as.character(age), levels = age_levels)) %>% 
  # Filter rates of 0, infinite (N_Deaths/0_Pop) and NaN (0_Deaths/0_Pop) values
  filter(socsim !=0 & !is.infinite(socsim) & !is.nan(socsim)) %>% 
  filter(year %in% yrs_plot) %>% 
  ggplot(aes(x = age, y = socsim, group = year, colour = year)) +
  geom_line(linewidth = 1) +
  facet_wrap(. ~ rate, scales = "free") + 
  facetted_pos_scales(y = list(ASFR = scale_y_continuous(),
                               ASMR =  scale_y_continuous(trans = "log10"))) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "Age-Specific Fertility and Mortality Rates for Women in the USA \n retrieved from a SOCSIM microsimulation, 1933-2020",
       x = "Age", y = "Estimate")
