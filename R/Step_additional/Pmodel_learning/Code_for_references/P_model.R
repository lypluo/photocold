##################################################
#p-model running
#-->adopt from https://stineb.github.io/rsofun/articles/example.html 
##################################################
#install the rsofun R package:
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("stineb/rsofun")
library(rsofun)
#-------------------------------
#site selection and meta data
#-------------------------------
# mysites <- c("BE-Vie", "DE-Tha", "DK-Sor", "FI-Hyy", "IT-Col", "NL-Loo", "US-MMS", "US-WCr", "US-UMB", "US-Syv", "FR-Pue")
mysites <- "FR-Pue"
#
library(ingestr)
# A small number of meta data variables have to be specified for each site specifically to define the simulation years. This information is also used for input, calibration, and evaluation data ingestion. Required meta information is specified for each site (in rows) and a number of variables:
#   
# lat for latitude (decimal degrees)
# lon for longitude (decimal degrees) - this is only used for data ingestion but not for the P-model simulation with rsofun.
# elv for elevation (m a.s.l.)
# year_start and year_end specifying years covered by the simulation
# whc for the soil water holding capacity
# koeppen_code to group sites for evaluation by Koeppen-Geiger climate zones.

# This information is provided by the data frame siteinfo_fluxnet2015 which is available as part of the ingestr package.
siteinfo <- ingestr::siteinfo_fluxnet2015 %>%
  dplyr::filter(sitename %in% mysites)

## take only year 2007 to 2014, corresponding to subset of data for site FR-Pue provided in this package as demo
siteinfo <- siteinfo %>%
  dplyr::mutate(year_start = 2007, year_end = 2014)

##step 1
siteinfo <- siteinfo %>%
  dplyr::mutate(date_start = lubridate::ymd(paste0(year_start, "-01-01"))) %>%
  dplyr::mutate(date_end = lubridate::ymd(paste0(year_end, "-12-31")))

#-----------------------------
#simulation setting
#-----------------------------
#step2
#specify additional simulation parameter that are identical for all site-scale simulation
params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = FALSE,
  tempstress         = FALSE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
)
# Run prepare_setup_sofun() to define the simulation settings that contain all the information 
# specified by the two steps above (meta info, and simulation parameters-->step1 and step2), global simulation parameters 
# are wrapped inside an additional column params_siml, added to the site meta info dataframe.

site_info <- prepare_setup_sofun(site_info = siteinfo, params_siml = params_siml)

#-----------------------------------
#Define model parameter
#-----------------------------------
# First, let's do it by hand (calibration of parameters is shown later).
params_modl <- list(
  kphio           = 0.05,
  soilm_par_a     = 1.0,
  soilm_par_b     = 0.0,
  vpdstress_par_a = 0.2,
  vpdstress_par_b = 0.2,
  vpdstress_par_m = 5
)

#-----------------------------------
#Define soil parameter
#-----------------------------------
#For now, this is implemented as an illustration. Should be made site-specific. Values entered here take no effect.

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

#-----------------------------------
#Get input
#----------------------------------
# Now, meteorological forcing data and fAPAR data for each time step have to be collected. 
# These steps are described in the vignette vignettes/prepare_inputs_rsofun.Rmd for FLUXNET 2015 simulations. 
# All forcing data, combined with model and simulations parameters are collected into one big nested data frame df_drivers, 
# which stores all required information by rows for each site. An example of how this looks like is provided as part of the rsofun package.

#add by YP-->in current package, there are two datasetsto run and validate pmodel output. 
# These files can be directly loaded into your workspace by typing:
p_model_drivers
df_drivers<-p_model_drivers
# p_model_validation

#------------------------------------
#Run the model
#------------------------------------
## run for a single site
mod <- run_pmodel_f_bysite(
  df_drivers$sitename[1],
  df_drivers$params_siml[[1]],
  df_drivers$site_info[[1]],
  df_drivers$forcing[[1]],
  df_drivers$params_soil[[1]],
  params_modl = params_modl,
  makecheck = TRUE
)

## Run for the full set of sites
ptm <- proc.time()
df_output <- runread_pmodel_f(
  df_drivers,
  par = params_modl,
  makecheck = TRUE,
  parallel = FALSE
)
print(ptm)
#
library(ggplot2)
df_output$data[[1]] %>%
  ggplot(aes(x=date, y=gpp)) +
  geom_line() +
  labs(title = df_output$sitename[[1]], subtitle = "SOFUN output")

#---------------------------
#Calibrate:
#---------------------------
settings_calib <- list(
  method              = "gensa",
  targetvars          = c("gpp"),
  timescale           = list( gpp = "d" ),
  maxit               = 5, # (5 for gensa) (30 for optimr)    #
  sitenames           = "FR-Pue",
  metric              = "rmse",
  dir_results         = "./",
  name                = "ORG",
  par                 = list( kphio = list( lower=0.03, upper=0.07, init=0.0496 ) )
)

#get stuck with the directory to Euler:
#Use the ingestr package once again, now for collecting calibration target data. I.e., GPP based on the nighttime flux decomposition method.
settings_ingestr_fluxnet <- list(
  # dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/HH/",
  dir_hh="/cluster/work/climate/bestocke/data/FLUXNET-2015_Tier1/20191024/HH/",
  getswc = FALSE,
  filter_ntdt = TRUE,
  threshold_GPP = 0.8,
  remove_neg = FALSE
)

ddf_fluxnet_gpp <- ingestr::ingest(
  siteinfo = dplyr::filter(siteinfo, sitename == "FR-Pue"),
  source    = "fluxnet",
  getvars = list(gpp = "GPP_NT_VUT_REF",
                 gpp_unc = "GPP_NT_VUT_SE"),
  dir = "/cluster/work/climate/bestocke/data/FLUXNET-2015_Tier1/20191024/DD/",
  settings = settings_ingestr_fluxnet,
  timescale = "d"
)
