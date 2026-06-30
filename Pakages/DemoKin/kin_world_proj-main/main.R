# steps for producing results in paper "Projections of human kinship for the entire world"
# Authors: Alburez-Gutierrez, Williams, Caswell.
# Last update: 22 june 2023

# download data, run kinship 1000 trajectories by country and merge in one file per country
# save results in "trajs/kin_trajs_country/"
source("wpp_traj_five_1000.R")

# get credible intervals for each country
# save results in "Output/CIs_trajs_country/"
source("outputs_ci.R")

# build paper plots
# save results in "Plots/"
source("plots.R")

# build paper tables
# save results in "Tables/"
source("tables.R")