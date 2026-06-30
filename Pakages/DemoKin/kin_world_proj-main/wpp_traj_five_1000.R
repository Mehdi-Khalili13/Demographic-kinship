### 2 sex projection 5-y (for 1,000 individual trajectories per country)

library(tidyverse)
library(data.table)
library(devtools)
library(DemoKin)
library(DemoTools)
library(furrr)


options(timeout = max(1000, getOption("timeout")))
options(tibble.print_min=10)
Sys.setenv(LANG = "en")

# 0. Parameters ---------------------------------------------------------------
base_url <- 'https://population.un.org/dataportalapi/api/v1'
options(timeout = max(1000, getOption("timeout")))
indicators <- read.csv(paste0(base_url,'/indicators/?format=csv'), sep='|', skip=1) 
# years: the first is to start with kin stable distribution, the next is to jump by 5 years
years_projection <- seq(1952.5, 2097.5, 5) # should be seq(1953, 2098, 5) but needs interpolation
# countries <- c(156, 356)
# countries <- get_locations() %>% pull(1)
# Some countries shouldnt be here and break the function (bc they have 
# no WPP traj files). Remove them:

countries <- c(4, 8, 1)

# IGNORE countries: 876, 336 
# (incomplete surv files)
# Individual labelled trajectories
# what number of trajectories per country
n_traj <- 1000

# 1. download past and median projections of fert, mort and pop by 5-age groups -----------------------------------------------------------
wpp22_variants_asfr <- map_df(countries, function(X){
  Sys.sleep(1)
  target <- paste0(base_url,'/data/indicators/',17,'/locations/',X,'/start/',1950,'/end/',2100,'/?format=csv')
  tryCatch({return(read.csv(target, sep='|', skip=1))},error = function(e){})})%>% 
  filter(TimeMid %in% years_projection, Variant %in% c("Median")) %>% 
  arrange(Location, Variant, Sex, AgeStart)
wpp22_variants_surv <- map_df(countries, function(X){
  Sys.sleep(1)
  target <- paste0(base_url,'/data/indicators/',77,'/locations/',X,'/start/',1950,'/end/',2100,'/?format=csv')
  tryCatch({return(read.csv(target, sep='|', skip=1))},error = function(e){})})%>% 
  filter(SexId!=3, TimeMid %in% years_projection, Variant %in% c("Median")) %>% 
  arrange(Location, Variant, Sex, AgeStart)
wpp22_variants_pop <- map_df(countries, function(X){
  Sys.sleep(1)
  target <- paste0(base_url,'/data/indicators/',46,'/locations/',X,'/start/',1950,'/end/',2100,'/?format=csv')
  tryCatch({return(read.csv(target, sep='|', skip=1))},error = function(e){})})%>% 
  filter(SexId!=3, TimeMid %in% years_projection, Variant %in% c("Median")) %>% 
  arrange(Location, Variant, Sex, AgeStart)

save(wpp22_variants_asfr, wpp22_variants_surv, wpp22_variants_pop,
     file = "trajs/wpp22_variants_data.rdata")

# 2. Prepare individual trajectory projections -----------------------------------------------------------
# prepare data of trajectories for each country-list

# for(x in countries){
options(future.globals.maxSize = 2000*1024^2)
plan(multisession, workers = 10)
future_map_dfr(countries, function(x){
  
  # remove space
  wpp22_traj_asfr <- wpp22_traj_surv <- wpp22_traj_pop <- NULL
  
  # read trajectories
  ax <- paste0("trajs/unwpp_trajectories/asfr/UN_PPP2022_Output_ASFR1x1_by_Year_", x, ".csv.gz")
  sx <- paste0("trajs/unwpp_trajectories/mx/UN_PPP2022_Output_Mx1x1_by_Year_", x, ".csv.gz")
  px <- paste0("trajs/unwpp_trajectories/pop/UN_PPP2022_Output_Pop1x1_by_Year_", x, ".csv.gz")
  asfr_x_traj <- fread(ax)
  surv_x_traj <- fread(sx)
  pop_x_traj  <- fread(px)
  
  # surv: needs to find lx and then make abridged. This cost time with DemoTools
  wpp22_traj_surv <- surv_x_traj %>% 
    filter(SexID != 3, as.integer(Trajectory) <= n_traj) %>% 
    select(c(1,2,3,4,seq(6, 81, 5))) %>% 
    split(list(.$SexID, .$Trajectory), drop = T) %>% 
    lapply(function(x){
      this_sex <- ifelse(unique(x$SexID)==1,"m","f")
      apply(x %>% select(-c(1:4)) %>% as.data.frame(), MARGIN = 2, function(y) {
        y <- lt_single_mx(nMx = y, Age = 0:100, Sex = this_sex)
        lt_abridged(lx = y$lx[y$Age %in% c(0,1,seq(5,100,5))], 
                    Age = c(0,1,seq(5,100,5)), Sex = this_sex)$Sx}) %>% 
        as.data.frame() %>%
        mutate(LocID = unique(x$LocID), SexID = unique(x$SexID), Trajectory = unique(x$Trajectory), 
               Age = c(0,1,seq(5,100,5)))}) %>% 
    rbindlist()
  
  # pop
  wpp22_traj_pop <- pop_x_traj%>% 
    filter(SexID != 3, as.integer(Trajectory) <= n_traj) %>% 
    select(-`2101`) %>% 
    pivot_longer(as.character(2022:2100), names_to = "year", values_to = "value") %>%
    filter(year %in% trunc(years_projection)) %>% 
    mutate(Age = trunc(Age/5)*5) %>% 
    group_by(LocID, Age, SexID, year, Trajectory) %>% 
    summarise(Value = sum(value))
  
  # asfr
  wpp22_traj_asfr <- asfr_x_traj %>% 
    filter(as.integer(Trajectory) <= n_traj) %>% 
    pivot_longer(as.character(2022:2100), names_to = "year", values_to = "value") %>%
    filter(year %in% trunc(years_projection)) %>% 
    mutate(Age = pmax(pmin(trunc(Age/5)*5, 50),10)) %>% 
    group_by(LocID, Age, year, Trajectory) %>% 
    summarise(Value = sum(value)/5)
  
  save(wpp22_traj_surv, file = paste0("trajs/demogr_trajs/wpp22_traj_surv_",x,".rdata"))
  save(wpp22_traj_pop, file =  paste0("trajs/demogr_trajs/wpp22_traj_pop_",x,".rdata"))
  save(wpp22_traj_asfr, file = paste0("trajs/demogr_trajs/wpp22_traj_asfr_",x,".rdata"))
  }
)

# 3. Apply model to 1000 traj ------------------------------------------------------

# load variants (median) data
load("trajs/wpp22_variants_data.rdata")

# survival matrix previous to 2022 (needed to complete full matrix in next loop)
wpp22_surv_matrix_pre2022 <- wpp22_variants_surv %>% 
  filter(TimeLabel<2022, Variant == "Median") %>% 
  select(LocationId, year = TimeMid, SexId, Age = AgeStart, Value) %>%
  split(list(.$LocationId, .$year, .$SexId), drop = T) %>% 
  lapply(function(x){
    this_sex <- ifelse(unique(x$Sex)=="Male","m","f")
    y <- lt_abridged(lx = x$Value, Age = c(0,1,seq(5,100,5)), Sex = this_sex)
    data.frame(Value = y$Sx, 
               Age = y$Age,
               LocationId = unique(x$LocationId), 
               year = unique(x$year),
               SexId = unique(x$SexId))
  }) %>% 
  rbindlist() %>%
  pivot_wider(names_from = year, values_from = Value) %>% 
  arrange(SexId, Age)

# what combinations
combinations <- expand.grid(LocationId = countries, trajectory = 1:n_traj)

# run in parallel (mutiple cores)
# For Hydra, adjust max size that can be passed to parallel clusters
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
options(future.globals.maxSize= 2000*1024^2)
plan(multisession, workers = 10)
print(Sys.time())
kin_out_traj <- future_map_dfr(1:nrow(combinations), function(i){
  
  tryCatch({
    
    # country and traj
    country <- combinations$LocationId[i]
    # trajectory = 500
    trajectory <- combinations$trajectory[i]
    
    load(paste0("trajs/demogr_trajs/wpp22_traj_asfr_",country,".rdata"))
    load(paste0("trajs/demogr_trajs/wpp22_traj_pop_",country,".rdata"))
    load(paste0("trajs/demogr_trajs/wpp22_traj_surv_",country,".rdata"))
    
    # asfr
    wpp22_asfr_matrix <- 
      wpp22_traj_asfr %>% 
      ungroup %>% filter(Trajectory == trajectory) %>%
      mutate(year = as.integer(year)+.5) %>% 
      select(-LocID, -Trajectory) %>% 
      bind_rows(wpp22_variants_asfr %>% 
                  filter(LocationId == country, TimeLabel<2022, Variant == "Median") %>% 
                  select(year = TimeMid, Age = AgeStart, Value)) %>%
      arrange(year, Age) %>% 
      pivot_wider(names_from = year, values_from = Value) %>% 
      right_join(data.frame(Age = seq(0,100,5)), by = "Age") %>%
      replace(is.na(.), 0) %>% 
      arrange(Age) %>% select(-Age) %>%
      as.matrix()/1000
    # plot(colSums(wpp22_asfr_matrix)*5, col=2)
    
    # surv
    wpp22_surv_matrix <- cbind(
      wpp22_surv_matrix_pre2022 %>% filter(LocationId == country),
      wpp22_traj_surv %>% 
        ungroup() %>% 
        filter(Trajectory == trajectory) %>% 
        setNames(c(as.character(seq(2022.5, 2097.5, 5)), "LocID", "SexID", "Trajectory", "Age"))) %>% 
      select(-SexID, -SexId, -LocID, -Trajectory, -Age, -LocationId)
    # plot(as.numeric(wpp22_surv_matrix[1,]))
    
    # pop
    wpp22_pop_matrix  <- 
      # wpp22_traj_pop[[as.character(country)]] %>% 
      wpp22_traj_pop %>% 
      ungroup %>% filter(Trajectory == trajectory) %>% 
      mutate(year = as.integer(year)+.5, Value = Value * 1000) %>% 
      bind_rows(wpp22_variants_pop %>% 
                  filter(LocationId == country, TimeLabel<2022, Variant == "Median") %>% 
                  select(year = TimeMid, SexID = SexId, Age = AgeStart, Value)) %>% 
      arrange(SexID, year, Age) %>% 
      select(Sex = SexID, year, Age, Value) %>% 
      pivot_wider(names_from = year, values_from = Value) %>% 
      arrange(Sex, Age) %>% 
      select(-Sex, -Age)  %>%
      as.matrix()
    # plot(wpp22_pop_matrix[1,])
    
    # split matrix by sex
    pf_matrix <- wpp22_surv_matrix[1:22,]
    pm_matrix <- wpp22_surv_matrix[23:44,]
    pf <- rbind(pf_matrix[2:21,], pf_matrix[21,]) %>% as.matrix()
    pm <- rbind(pm_matrix[2:21,], pm_matrix[21,]) %>% as.matrix()
    pbf <- matrix(rep(as.numeric(pf_matrix[1,]),21), nrow = 21, byrow = T)
    pbm <- matrix(rep(as.numeric(pm_matrix[1,]),21), nrow = 21, byrow = T)
    pb <- (pbf+pbm)/2 # here needs DemoKin modification differentiate survival by sex from born to age 0:4
    ff <- rbind(
      wpp22_asfr_matrix[1:3,],
      (wpp22_asfr_matrix[4:10,] + wpp22_asfr_matrix[5:11,] * pf[4:10,])/2,
      wpp22_asfr_matrix[11:21,]) * pb * 5
    fm <- rbind(
      wpp22_asfr_matrix[0:3,],
      (wpp22_asfr_matrix[4:10,] + wpp22_asfr_matrix[5:11,] * pm[4:10,])/2,
      wpp22_asfr_matrix[11:21,]) * pb * 5
    # this option gives high offspring to group 10-14, with big effect to younger patterns, making very different to single age modelling
    # ff2 <- rbind(wpp22_asfr_matrix[-21,] + wpp22_asfr_matrix[-1,] * pf[-21,],
    #             wpp22_asfr_matrix[21,])/2 * pb * 5
    # fm2 <- rbind(wpp22_asfr_matrix[-21,] + wpp22_asfr_matrix[-1,] * pm[-21,],
    #             wpp22_asfr_matrix[21,])/2 * pb * 5
    # plot(ff[,5]); lines(ff2[,5],col=2)
    nf <- wpp22_pop_matrix[1:21,]
    nm <- wpp22_pop_matrix[22:42,]
    pif <- t(t(nf * ff)/colSums(nf * ff))
    pim <- t(t(nm * fm)/colSums(nm * fm))
    
    # plot(colSums(fm))
    # dim(pf);dim(pm);dim(pbf);dim(pbm);dim(pif);dim(pim);dim(nf);dim(nm)
    colnames(pf) <- colnames(pm) <- colnames(ff) <- colnames(fm) <- colnames(nf) <- colnames(nm) <- 0:(ncol(pf)-1)
    
    # two sex model
    kin_two_sex_traj_i <- kin2sex(pf = pf, pm = pf, 
                                  ff = ff, fm = fm, 
                                  pif = pif, pim = pim, 
                                  time_invariant = F)
    # kin_two_sex_traj_i$kin_summary %>% filter(age_focal == 14, year == 0) %>% summarise(sum(count_living))
    
    # save kin_full
    kin_two_sex_traj_i <- data.frame(combinations[i,], 
                                     kin_two_sex_traj_i$kin_full, row.names = NULL)
    save(kin_two_sex_traj_i, file = paste0("trajs/kin_trajs/kin_two_sex_traj_", 
                                           combinations$LocationId[i],"_",
                                           combinations$trajectory[i],".rdata"))
    
  },
  error = function(e){print(paste0("Error in i=", i, ". Country: ", country, ". Traj: ", trajectory))}
  )
  
})
print(Sys.time())

# 4. Create one file per country with all kin_summary variants ===========

f <- list.files("trajs/kin_trajs/", pattern = "^kin_two_sex_traj", full.names = T)
# Get country
con <- gsub("_", "", str_extract(f, "_[0-9]+_"))
f_l <- split(f, con)

# not yet
already <- str_extract(list.files("trajs/kin_trajs_country"), "[0-9]+")
to_run <- unique(sort(con))[!unique(sort(con)) %in% already]

options(future.globals.maxSize= 2000*1024^2)
plan(multisession, workers = 10)
future_map(to_run, function(c){
# for(c in unique(sort(con))){
  print(c)
  
  out <- 
    lapply(f_l[[as.character(c)]], function(x){
      print(x)
      load(x)
      kin_two_sex_traj_i 
        # select(LocationId, trajectory, year, age_focal, age_kin, sex_kin, kin, living) 
      # return(o)
    }) %>% 
    bind_rows() 
  
  # data.table::fwrite(out, paste0("Data/traj_", c, ".csv"), row.names = F)
  save(out, file = paste0("trajs/kin_trajs_country/traj_", c, ".rdata"))
  
}
)
