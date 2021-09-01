# functions that allow to calculate emissions factors from EDGAR 
# EF in EDGAR are in kton/TJ
# https://edgar.jrc.ec.europa.eu/overview.php?v=432_AP&SECURE=123
# Crippa, M., Guizzardi, D., Muntean, M., Schaaf, E., Dentener, F.,
# van Aardenne, J. A., Monni, S., Doering, U., Olivier, J. G. J., 
# Pagliari, V., and Janssens-Maenhout, G.: Gridded Emissions of Air
# Pollutants for the period 1970–2012 within EDGAR v4.3.2, 
# Earth Syst. Sci. Data Discuss., https://doi.org/10.5194/essd-2018-31,
# in review, 2018.

# contact: emanuela.peduzzi@gmail.com, enrico.pisoni@ec.europa.eu

library(reshape)  

com_edgar_merge<- function(beimei_f, subDir, path_e, polls){
#read data edgar, activity levels, IEF and emissions
print('Reading EDGAR File')
cat('Reading EDGAR File', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
dataedgar <- read_excel(paste0(path_e), sheet='implied_EF')

# putting all com data in a data.frame and adding the pollutant column
df_tmp <-beimei_f %>% ungroup()  # CoM data
df_final <- data.frame() # final dataframe 
for (poll in polls){
  df_tmp$poll <- poll
  df_final<-rbind(df_final, df_tmp)}

# I included all countries, because EDGAR can potentially give us all world countries. 
n_list_newdataset_edgar <- (list(
  Spain = 'ESP', 
  Germany = 'DEU',
  Austria = 'AUT',
  Bulgaria = 'BGR',
  BosniaandHerzegovina = 'BIH',
  Belgium = 'BEL',
  Finland = 'FIN',
  France = 'FRA',
  Italy = 'ITA',
  Greece = 'GRC', 
  Luxembourg = 'LUX',
  Norway = 'NOR',
  Portugal = 'PRT',
  Ireland = 'IRL',
  Romania = 'ROU',
  Hungary = 'HUN',
  UnitedKingdom = 'GBR',
  Estonia = 'EST',
  Poland = 'POL',
  Croatia = 'HRV',
  TheNetherlands = 'NLD',
  Denmark = 'DNK',
  Latvia = 'LVA',
  Sweden = 'SWE',
  Iceland = 'ISL',
  Switzerland = 'CHE',
  Slovakia = 'SVK',
  Cyprus = 'CYP',
  Slovenia = 'SVN',
  CzechRepublic = 'CZE',
  Lithuania = 'LTU', 
  Georgia = 'GEO',
  Ukraine = 'UKR',
  Turkey = 'TUR'
))


df_final <- df_final %>% mutate(cou4 = gsub(" ", "", cou)) %>% mutate(cou5 = gsub("-", "", cou4)) %>%
  mutate(edgar_country = ifelse(cou5 %in% names(n_list_newdataset_edgar), as.character(n_list_newdataset_edgar[cou5]), cou5)) %>%
  select(-matches('cou4')) %>% select(-matches('cou5'))

df_final <- df_final %>% filter(BeiYear>=1990 & BeiYear<=2015) %>% filter(MeiYear>=1990 & MeiYear<=2015)

# if you do not have all the time series 
beiyears <- unique(df_final$BeiYear)
meiyears <- unique(df_final$MeiYear)
years <- c(beiyears, meiyears)

# if you do not have all the time series 
# years <- 1990:2015 
coliefnames <- character(length = length(years))
for (i in seq_along(years)){
  coliefnames[i] <- paste0('DER_',years[i])
}

names(dataedgar)
coltokeep<-c(names(dataedgar)[1:4], coliefnames)
wideedgar <- dataedgar %>% select(coltokeep)
longedgar <- melt(as.data.frame(wideedgar), id=names(wideedgar)[1:4]) 
longedgar <- longedgar %>% mutate(year = gsub("DER_", "", variable)) %>% select(-c(variable)) #%>% 
#dplyr::rename(ief=value, edgar_country=Country_code_A3,poll=substance, edgar_sector=SECTOR, edgar_carrier=category) 

print('Calculating emissions of air pollutants precursors')
cat('Calculating emissions of air pollutants precursors', file = paste0(subDir,"/test.log"), append = TRUE,  sep= "\n")

# merging with the original dataframe
df_m <- merge(df_final, longedgar, 
              by.x = c("edgar_country","edgar_sector", "edgar_carrier", "poll", "BeiYear"), 
              by.y = c("Country_code_A3", "SECTOR","category", "substance", "year"), all.x=TRUE) %>% dplyr::rename(ief_bei=value)
df_m <- merge(df_m, longedgar, 
              by.x = c("edgar_country","edgar_sector", "edgar_carrier", "poll", "MeiYear"), 
              by.y = c("Country_code_A3", "SECTOR","category", "substance", "year"), all.x=TRUE) %>% dplyr::rename(ief_mei=value)


n_poll <- (list(
  PM2.5 = 'pm25', 
  PM10 = 'pm10',
  NOx = 'nox'))


df_m <- df_m %>% mutate(poll = ifelse(poll %in% names(n_poll), as.character(n_poll[poll]), poll))


# VERY IMPORTANT: changing units! CAREFUL!
df_m_units <- df_m %>% mutate(ief_bei=ief_bei *3.6, ief_mei= ief_mei *3.6)# 1kton/TJ -> 3.6 ton/MWh

df_m_final <-  df_m_units %>% mutate(emi_e_bei_ef_bei=ener_bei*ief_bei)
df_m_final <-  df_m_final %>% mutate(emi_e_mei_ef_mei=ener_mei*ief_mei)
df_m_final <-  df_m_final %>% mutate(emi_e_mei_ef_bei=ener_mei*ief_bei)

write.csv(df_m_final, file = paste0(subDir,'/summary_edgar_Com.csv'))
df_m_final_grouped <- df_m_final %>% group_by(cou, name, poll, ms) %>% summarise(ener_bei=sum(ener_bei, na.rm = TRUE), 
                                                                                 ener_mei=sum(ener_mei, na.rm = TRUE),
                                                                                 emi_bei_co2=sum(emi_bei_co2, na.rm = TRUE),
                                                                                 emi_mei_co2=sum(emi_mei_co2, na.rm = TRUE),
                                                                                 emi_e_bei_ef_bei=sum(emi_e_bei_ef_bei, na.rm = TRUE),
                                                                                 emi_e_mei_ef_mei=sum(emi_e_mei_ef_mei, na.rm = TRUE),
                                                                                 emi_e_mei_ef_bei=sum(emi_e_mei_ef_bei, na.rm = TRUE),
                                                                                 BeiYear=min(BeiYear),
                                                                                 MeiYear=min(MeiYear),
                                                                                 pop_bei=min(pop_bei),
                                                                                 pop_mei=min(pop_mei))

write.csv(df_m_final_grouped, file = paste0(subDir,'/summary_edgar_Com_sel_ms.csv'))
return(df_m_final_grouped)
}
