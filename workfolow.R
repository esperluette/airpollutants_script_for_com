# Supporting script to the article: 
# Impacts of a climate change initiative on air pollutant emissions: insights from the Covenant of Mayors
# Authors: Emanuela Peduzzi, Marta Baldi, Enrico Pisoni, Fabio Monforti-Ferrario.
# Script to analyse the CoM data-set (or a single city) and obtain emissions of air pollution precursors
# These scripts should be adapted depending on the input file and the data available. 
# only minimum data is uploaded as input file for GAINS and EDGAR
# GAINS data can be downloaded through the on-line tool https://gains.iiasa.ac.at/models/index.html
# EDGAR data can be requested at https://edgar.jrc.ec.europa.eu/overview.php?v=432_AP&SECURE=123
# CoM data reports energy consumption in MWh and CO2 emissions in ton
# In the results file energy consumption is in MWh, emissions are in ton, emission factor are in ton/MWh

# contact: emanuela.peduzzi@gmail.com, enrico.pisoni@ec.europa.eu
library(lattice)
library(cowplot)
library(ggplot2)

source('auxiliary_functions.R')
source('gains_ef.R')
source('edgar_ef.R')

###############################################################################################################
# Step 1 - Calculate emissions from CoM and Emission inventory data 

## Initial set-up
setwd("Z:/peduzem/2-com_2.0/airpollutants_script_for_com")
tag <- "dummy_data"
tag_ef <- 'gains' # 
# tag_ef <- 'edgar' # 
path_e <- paste0('./EDGAR/example_edgar_data.xlsx')
path_g <- paste0('./GAINS/actLev_IEF/')

polls <- c('PM2.5', 'NOx') # selection of pollutants 

# create sub directory to store results
subDir <- paste0('./output_CoM_data_epe_',Sys.Date(),'_ComExtraction',tag, tag_ef) #saving also day of production in the filename
if (dir.exists(subDir)==FALSE){
  dir.create(file.path('./', subDir))
}

# create a log file
log_con <- file(paste0(subDir,"/logfile.log"))

print('Begin programme')
cat('Begin programme', file = log_con,  sep = "\n")
cat(paste0('CoM data from ', tag, ' and emission factors from ', tag_ef), file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

## Reading 
# read CoM sector and carrier legend
print('Reading Legends and Mapping')
cat('Reading Legends and Mapping', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
sectors <- read_legend(paste0('./legend_', tag_ef, '.xlsx'), 'com_sectors', 'A1:C25')  
carriers <- read_legend(paste0('./legend_', tag_ef, '.xlsx'), 'com_carriers', 'A1:C22')

# read CoM data
print('Reading CoM File')
cat('Reading CoM File', file = paste0(subDir,"/test.log"), append = TRUE, sep= "\n")
allcities_beimei <- read_data_com_all_cities(paste0('./CoM/CoM_',tag,'.xlsx'), 'dummy_bei', 'dummy_mei')

## Data Cleaning
print('Data cleaning')
cat('Data cleaning CoM File', file = paste0(subDir,"/test.log"), append = TRUE, sep= "\n")
beimei_f <- data_cleaning(allcities_beimei, tag, sectors, carriers, subDir)

## Matching and inventory carriers and sectors IDs
print('Matching CoM and EF inventories IDs')
cat('Matching  CoM and EF inventories IDs', file = paste0(subDir,"/test.log"), append = TRUE, sep= "\n")
beimei_f <- left_join(beimei_f, carriers, by='carrier_id') #add CoM and inventory labels
beimei_f <- left_join(beimei_f, sectors, by='sector_id') #add CoM and inventory labels

# GAINS EF analysis
if (tag_ef == 'gains'){
  years_or <- c(unique(beimei_f$BeiYear), unique(beimei_f$MeiYear))
  y_r <- sapply(years_or, round_any, accuracy=5) 
  y_f <- sapply(years_or, floor_any, accuracy=5)
  years <- unique(c(y_r,y_f))
  gains_df_or <- read_data_gains_years(subDir, path_g, polls, years)
  res_df_ms <- com_gains_merge(beimei_f, gains_df_or, subDir, polls)
}
# EDGAR EF analysis
if (tag_ef == 'edgar'){
  res_df_ms <- com_edgar_merge(beimei_f, subDir, path_e, polls)
}

on.exit(close(log_con))

###############################################################################################################
# Step 2 - Generate Figures

# add delta years
res_df_ms$d_year <- res_df_ms$MeiYear - res_df_ms$BeiYear
# consider ms as factor
res_df_ms$ms<-as.factor(res_df_ms$ms)
# add sector pollutant for display 
res_df_ms$secpoll <-  with(res_df_ms, paste0(ms, poll))


res_df_ms_pp <- res_df_ms %>% mutate(d_CO2_per_pp=(emi_mei_co2/pop_mei - emi_bei_co2/pop_bei)/(emi_bei_co2/pop_bei) * 100,
         d_AQ_per_pp=(emi_e_mei_ef_mei/pop_mei - emi_e_bei_ef_bei/pop_bei)/(emi_e_bei_ef_bei/pop_bei) * 100, 
         d_AQ_act_per_pp=(emi_e_mei_ef_bei/pop_mei - emi_e_bei_ef_bei/pop_bei)/(emi_e_bei_ef_bei/pop_bei) * 100)


# labels axis 
laby = (expression(paste(CO[2], " emissions change per person [%]")))
labx = "NOx, PM2.5 em. change per person [%]"

# make plots
qdrnt_plot <-ggplot(data = res_df_ms_pp, aes(x = d_AQ_per_pp, y = d_CO2_per_pp, group=secpoll, size=d_year)) +
  ggtitle("With technological improvement") +
  coord_cartesian(ylim = c(-100, 100), xlim = c(-100, 100)) + 
  geom_hline(yintercept=0, size=1, colour = "black") +
  geom_vline(xintercept=0, size=1, colour = "black") +
  geom_point(aes(col=secpoll, shape=secpoll)) + labs(y=laby, x=labx) +
  scale_shape_manual(labels=c("2nox"="NOx (DOM)", "2pm25"="PM2.5 (DOM)","7nox"="NOx (TRA)","7pm25"="PM2.5 (TRA)"), 
                     values=c("2nox"=17, "2pm25"=17,"7nox"=19,"7pm25"=19)) +
  scale_color_manual(labels=c("2nox"="NOx (DOM)", "2pm25"="PM2.5 (DOM)","7nox"="NOx (TRA)","7pm25"="PM2.5 (TRA)"), 
                     values=c("2nox"="darkgreen", "2pm25"="red","7nox"="green","7pm25"="darkorange")) + 
  scale_size_continuous(range = c(1,5)) +
  labs(size="BEI, MEI interval (years)", colour="Pollutant (Sector)", shape="Pollutant (Sector)") +
  theme(aspect.ratio=1, text=element_text(size=8), plot.title = element_text(face="bold", hjust = 0.5)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot(qdrnt_plot)
ggsave(paste0(subDir, '/qdrnt_plot.png'), dpi = 300)

qdrnt_plot_act <-ggplot(data = res_df_ms_pp, aes(x = d_AQ_act_per_pp, y = d_CO2_per_pp, group=secpoll, size=d_year)) +
  ggtitle("Without technological improvement") +
  coord_cartesian(ylim = c(-100, 100), xlim = c(-100, 100)) + 
  geom_hline(yintercept=0, size=1, colour = "black") +
  geom_vline(xintercept=0, size=1, colour = "black") +
  geom_point(aes(col=secpoll, shape=secpoll)) + labs(y=laby, x=labx) +
  scale_shape_manual(labels=c("2nox"="NOx (DOM)", "2pm25"="PM2.5 (DOM)","7nox"="NOx (TRA)","7pm25"="PM2.5 (TRA)"), 
                     values=c("2nox"=17, "2pm25"=17,"7nox"=19,"7pm25"=19)) +
  scale_color_manual(labels=c("2nox"="NOx (DOM)", "2pm25"="PM2.5 (DOM)","7nox"="NOx (TRA)","7pm25"="PM2.5 (TRA)"), 
                     values=c("2nox"="darkgreen", "2pm25"="red","7nox"="green","7pm25"="darkorange")) + 
  scale_size_continuous(range = c(1,5)) +
  labs(size="BEI, MEI interval (years)", colour="Pollutant (Sector)", shape="Pollutant (Sector)") +
  theme(aspect.ratio=1, text=element_text(size=8), plot.title = element_text(face="bold", hjust = 0.5)) + guides(fill=guide_legend(nrow=2,byrow=TRUE))
plot(qdrnt_plot_act)
ggsave(paste0(subDir, '/qdrnt_plot_act.png'), dpi = 300)

