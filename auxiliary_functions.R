library(readxl)
library(dplyr)
library(R.utils)
library(tidyr)

###############################################################################################################
# https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816
round_any <-  function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
floor_any <-  function(x, accuracy, f=floor){f(x/ accuracy) * accuracy}

###############################################################################################################
#read Com data
read_data_com_all_cities <- function(namefile, tab_bei, tab_mei) {
  #read CoM data, BEI and MEI
  allcities <- list()
  allcities$bei <- read_excel(namefile, sheet=tab_bei)
  allcities$bei <- allcities$bei[allcities$bei$inventory_year!=0,] #some versions of the database have 0 for non-matches subsectors. 
  allcities$mei <- read_excel(namefile, sheet=tab_mei)
  allcities$mei <- allcities$mei[allcities$mei$inventory_year!=0,]
  return(allcities)
}

###############################################################################################################
#read CoM legend and link with Gains or Edgar
read_legend <- function(nf, sheet, range) {
  out <- read_excel(nf, sheet=sheet, range = range)
}

###############################################################################################################
# DATA CLEANING 
data_cleaning <- function(allcities_beimei, tag, sectors, carriers, subDir){

# To be able to correctly match cities, put city name to lower case and then capitalize first letter
# For cities with more than one name, only the first one is capitalized 
allcities_beimei$bei$name <- capitalize(tolower(allcities_beimei$bei$name)) 
allcities_beimei$mei$name <- capitalize(tolower(allcities_beimei$mei$name))

# keeping only energy consumption rows as we are not looking at energy production for the moment
allcities_beimei$bei <- allcities_beimei$bei[(allcities_beimei$bei$row_type == "energy consumption"), ]
allcities_beimei$mei <- allcities_beimei$mei[(allcities_beimei$mei$row_type == "energy consumption"), ]

# substituting 0 to NA for energy and emission measures 
allcities_beimei$bei$energy_measure[is.na(allcities_beimei$bei$energy_measure)] <- 0
allcities_beimei$mei$energy_measure[is.na(allcities_beimei$mei$energy_measure)] <- 0
allcities_beimei$bei$emission_measure[is.na(allcities_beimei$bei$emission_measure)] <- 0
allcities_beimei$mei$emission_measure[is.na(allcities_beimei$mei$emission_measure)] <- 0

#cities with negative energy consumptions (29082019 this does not do anything anymore)
citiesbeineg <- unique(allcities_beimei$bei[(allcities_beimei$bei$energy_measure < 0), ]$name)
citiesmeineg <- unique(allcities_beimei$mei[(allcities_beimei$mei$energy_measure < 0), ]$name)
neg_cities <- unique(c(citiesbeineg, citiesmeineg))
cat('The following cities report negative energy consumptions', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(neg_cities, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

# removing all rows with negative energy consumption (29082019 this does not do anything anymore)
cat('Negative energy consumption lines are being removed', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

nrow(allcities_beimei$bei)
allcities_beimei$bei <- allcities_beimei$bei[(allcities_beimei$bei$energy_measure >= 0), ] 
nrow(allcities_beimei$bei)

nrow(allcities_beimei$mei)
allcities_beimei$mei <- allcities_beimei$mei[(allcities_beimei$mei$energy_measure >= 0), ] 
nrow(allcities_beimei$mei)

# keep only sectors and carriers we are considering (this can probably be written in a more concise way), EPI please check
c_id <- carriers %>% filter(UQ(as.symbol(paste0(tag_ef, '_carrier'))) != 'NA') %>% pull(carrier_id)
s_id <- sectors %>% filter(UQ(as.symbol(paste0(tag_ef, '_sector'))) != 'NA') %>% pull(sector_id)
allcities_beimei$bei <- allcities_beimei$bei[allcities_beimei$bei$energy_consumption_carrier_id %in% c_id,]
allcities_beimei$mei <- allcities_beimei$mei[allcities_beimei$mei$energy_consumption_carrier_id %in% c_id,]
allcities_beimei$bei <- allcities_beimei$bei[allcities_beimei$bei$energy_consumption_sector_id %in% s_id,]
allcities_beimei$mei <- allcities_beimei$mei[allcities_beimei$mei$energy_consumption_sector_id %in% s_id,]
cat('Keeping only sectors and carriers that are matched in the legend file', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

# keep only max inventory year for MEI # this does not seem to do anything ... EPI please check
nrow(allcities_beimei$mei)
allcities_beimei$mei <- allcities_beimei$mei %>% group_by(name) %>% filter(inventory_year==max(inventory_year)) %>% ungroup()
nrow(allcities_beimei$mei)

# keep only min inventory year for BEI # -> marta to check
nrow(allcities_beimei$bei)
allcities_beimei$bei <- allcities_beimei$bei %>% group_by(name) %>% filter(inventory_year==min(inventory_year)) %>% ungroup()
nrow(allcities_beimei$bei)


# in case entries are repeated keep only the first -> marta to check
nrow(allcities_beimei$bei)

repbeicities <- unique(allcities_beimei$bei[duplicated(allcities_beimei$bei[,c('name','inventory_year',
                                                                               'energy_consumption_carrier_id',
                                                                               'energy_consumption_sector_id')]),]$name)

cat('Cities with repeated lines in the bei are', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(repbeicities, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

allcities_beimei$bei <- allcities_beimei$bei[!duplicated(allcities_beimei$bei[,c('name','inventory_year',
                                                                                 'energy_consumption_carrier_id',
                                                                                 'energy_consumption_sector_id')]),]
nrow(allcities_beimei$bei)


nrow(allcities_beimei$mei)
repmeicities <- unique(allcities_beimei$mei[duplicated(allcities_beimei$mei[,c('name','inventory_year',
                                                                               'energy_consumption_carrier_id',
                                                                               'energy_consumption_sector_id')]),]$name)


cat('Cities with repeated lines in the mei are: ', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(repmeicities, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

allcities_beimei$mei <- allcities_beimei$mei[!duplicated(allcities_beimei$mei[,c('name','inventory_year',
                                                                                 'energy_consumption_carrier_id',
                                                                                 'energy_consumption_sector_id')]),]
nrow(allcities_beimei$mei)
cat('...repeated lines are being removed', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

# Some cities report an energy consumption but no emissions for fossil fuels. 
# missing emissions (apart for electricity consumption and biogenic fuels)
# in the BEI # no longer necessary
mis_emi_bei <- allcities_beimei$bei %>% filter(energy_measure > 0 & 
                                                 emission_measure <= 0 & 
                                                 !(energy_consumption_carrier_id %in% c(1, 11, 12, 13)))
c_mis_emi_bei <- unique(mis_emi_bei$name)
# err_cities <- append(err_cities, c_mis_emi_bei)
cat('The following cities report fossil fuel energy consumption and no CO2 emissions in their BEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep= "\n")
cat(c_mis_emi_bei, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
print('The following cities report fossil fuel energy consumption and no CO2 emissions in their BEI')
print(c_mis_emi_bei)
# and in the MEI
mis_emi_mei <- allcities_beimei$mei %>% filter(energy_measure > 0 & 
                                                 emission_measure <= 0 & 
                                                 !(energy_consumption_carrier_id %in% c(1, 11, 12, 13)))
c_mis_emi_mei <- unique(mis_emi_mei$name)
# err_cities <- unique(append(err_cities, c_mis_emi_mei))
cat('The following cities report fossil fuel energy consumption and no CO2 emissions in their MEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(c_mis_emi_mei, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")

if (nrow(mis_emi_mei) !=0 | nrow(mis_emi_bei) !=0){
  cat('Correcting using default emission factors...', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
  print('The following cities report fossil fuel energy consumption and no CO2 emissions in their MEI')
  print(c_mis_emi_mei)
  print('Correcting using default emission factors...')
  # the default emission factors in the CoM data (EPI to check, AK to check)
  co2ef <- read_excel(paste0('./input/CoM/CoM_data_',tag,'.xlsx'), n_max=22, sheet='energy_consumption_carrier_id')
  co2ef <- co2ef %>%  mutate(energy_consumption_carrier_id = as.numeric(energy_consumption_carrier_id))
  # we add the ef to the database only where they missing # 29082019 - this should not be necessary anymore
  mis_emi_mei <- left_join(mis_emi_mei, co2ef, by=c('energy_consumption_carrier_id'))
  mis_emi_bei <- left_join(mis_emi_bei, co2ef, by=c('energy_consumption_carrier_id'))
  # we calculate the emissions
  mis_emi_mei <- mis_emi_mei %>% mutate(emission_measure=energy_measure*EF) %>% select(-matches('EF'), -matches('item_text'))
  mis_emi_bei <- mis_emi_bei %>% mutate(emission_measure=energy_measure*EF) %>% select(-matches('EF'), -matches('item_text'))
  # we join the original inventory and clean it - for the BEI
  allcities_beimei$bei <- left_join(allcities_beimei$bei, mis_emi_bei, by=setdiff(names(allcities_beimei$bei),c("emission_measure")))
  allcities_beimei$bei <- allcities_beimei$bei %>% mutate(emission_measure.y=ifelse(is.na(emission_measure.y), emission_measure.x, emission_measure.y))
  allcities_beimei$bei <- allcities_beimei$bei %>%
    select(setdiff(names(allcities_beimei$bei),c("emission_measure.x")))  %>%
    dplyr::rename(emission_measure=emission_measure.y)  
  # trial <- allcities_beimei$bei
  # we join the original inventory and clean it - for the MEI
  allcities_beimei$mei <- left_join(allcities_beimei$mei, mis_emi_mei, by=setdiff(names(allcities_beimei$mei),c("emission_measure")))
  allcities_beimei$mei <- allcities_beimei$mei %>% mutate(emission_measure.y=ifelse(is.na(emission_measure.y), emission_measure.x, emission_measure.y))
  # trial <- allcities_beimei$mei
  allcities_beimei$mei <- allcities_beimei$mei %>%
    select(setdiff(names(allcities_beimei$mei),c("emission_measure.x")))  %>%
    dplyr::rename(emission_measure=emission_measure.y)
}  

# The opposite is also true, some cities report emissions for fossil fuels but no energy consumption:
# in the BEI
mis_ene_bei <- allcities_beimei$bei %>% filter(energy_measure <= 0 & 
                                                 emission_measure > 0 & 
                                                 !(energy_consumption_carrier_id %in% c(1, 11, 12, 13)))
c_mis_ene_bei <- unique(mis_ene_bei$name)
# err_cities <- unique(append(err_cities, c_mis_ene_bei))
cat('The following cities report CO2 emissions but not energy consumption in their BEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(c_mis_ene_bei, file = paste0(subDir,"/test.log"), append = TRUE, sep ="\n")
print('The following cities report CO2 emissions but not energy consumption in their BEI')
print(c_mis_ene_bei)
# and in the MEI
mis_ene_mei <- allcities_beimei$mei %>% filter(energy_measure <= 0 & 
                                                 emission_measure > 0 & 
                                                 !(energy_consumption_carrier_id %in% c(1, 11, 12, 13)))
c_mis_ene_mei <- unique(mis_ene_mei$name)
# err_cities <- unique(append(err_cities, c_mis_ene_mei))
cat('The following cities report CO2 emissions but not energy consumption in their MEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(c_mis_ene_mei, file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
print('The following cities report CO2 emissions but not energy consumption in their MEI')
print(c_mis_ene_mei)



if (nrow(mis_ene_mei) !=0 | nrow(mis_ene_bei) !=0){
  cat('Correcting using default emission factors...', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
  print('Correcting using default emission factors...')
  # we add the ef to the database only where it is missing
  mis_ene_mei <- left_join(mis_ene_mei, co2ef, by=c('energy_consumption_carrier_id'))
  mis_ene_bei <- left_join(mis_ene_bei, co2ef, by=c('energy_consumption_carrier_id'))
  # we calculate the energy consumption
  mis_ene_mei <- mis_ene_mei %>% mutate(energy_measure=emission_measure/EF) %>% select( -matches('EF'),-matches('item_text'))
  mis_ene_bei <- mis_ene_bei %>% mutate(energy_measure=emission_measure/EF) %>% select( -matches('EF'),-matches('item_text'))
  # we join the original inventory and clean it - for the BEI
  allcities_beimei$bei <- left_join(allcities_beimei$bei, mis_ene_bei, by=setdiff(names(allcities_beimei$bei),c("energy_measure")))
  allcities_beimei$bei <- allcities_beimei$bei %>% mutate(energy_measure.y=ifelse(is.na(energy_measure.y), energy_measure.x, energy_measure.y))
  allcities_beimei$bei <- allcities_beimei$bei %>%
    select(setdiff(names(allcities_beimei$bei),c("energy_measure.x")))  %>%
    dplyr::rename(energy_measure=energy_measure.y)  
  # we join the original inventory and clean it - for the MEI
  allcities_beimei$mei <- left_join(allcities_beimei$mei, mis_ene_mei, by=setdiff(names(allcities_beimei$mei),c("energy_measure")))
  allcities_beimei$mei <- allcities_beimei$mei %>% mutate(energy_measure.y=ifelse(is.na(energy_measure.y), energy_measure.x, energy_measure.y))
  allcities_beimei$mei <- allcities_beimei$mei %>%
    select(setdiff(names(allcities_beimei$mei),c("energy_measure.x")))  %>%
    dplyr::rename(energy_measure=energy_measure.y)
}

# Some cities only have a MEI, others only have a BEI - for now missing BEIs and MEIs for a city are copied from the exisitng ones.   
# cities with a BEI 
citiesbei <- unique(allcities_beimei$bei$name)
# cities with a MEI
citiesmei <- unique(allcities_beimei$mei$name)
# cities with both BEI and MEI
citiesbeimei <- intersect(citiesbei, citiesmei)
# err_cities <- unique(append(err_cities, setdiff(citiesbei, citiesbeimei)))
print('The following cities have a BEI but not a MEI, the BEI is copied to the MEI')
print(setdiff(citiesbei, citiesbeimei))
cat('The following cities have a BEI but not a MEI, the BEI is copied to the MEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(setdiff(citiesbei, citiesbeimei), file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
# err_cities <- unique(append(err_cities, setdiff(citiesmei, citiesbeimei)))
print('The following cities have a MEI but not a BEI, the MEI is copied to the BEI')
print(setdiff(citiesmei, citiesbeimei))
cat('The following cities have a MEI but not a BEI, the MEI is copied to the BEI', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(setdiff(citiesmei, citiesbeimei), file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
# copy BEIs and MEIS for the cities where either the bei or the mei is missing
meis <- allcities_beimei$mei
beis <- allcities_beimei$bei
meis2 <- rbind(meis, beis[beis$name %in% setdiff(citiesbei, citiesbeimei),])
beis2 <- rbind(beis, meis[meis$name %in% setdiff(citiesmei, citiesbeimei),])
# join the BEIs and MEIS
beimei <- full_join(beis2, meis2, by=c('name', 'country_name', 'energy_consumption_carrier_id','energy_consumption_sector_id'))
names(beimei)
beimei_f <- beimei %>%
  select(name, country_name, population.x, inventory_year.x, energy_consumption_carrier_id, 
         energy_consumption_sector_id, energy_measure.x, emission_measure.x, inventory_year.y,
         energy_measure.y, emission_measure.y, population.y) %>%
  dplyr::rename(cou=2, pop_bei=3, BeiYear=4, carrier_id=5, sector_id=6, ener_bei=7,
         emi_bei_co2=8, MeiYear=9, ener_mei=10, emi_mei_co2=11, pop_mei=12) #@todo - do the same elsewhere
# group and mutate
beimei_f <- beimei_f %>% group_by(name) %>% mutate(BeiYear=ifelse(is.na(BeiYear),
                                                                  mean(BeiYear, na.rm = TRUE),
                                                                  BeiYear), 
                                                   MeiYear=ifelse(is.na(MeiYear),
                                                                  mean(MeiYear, na.rm = TRUE),
                                                                  MeiYear),
                                                   ener_bei=ifelse(is.na(ener_bei),
                                                                   0,
                                                                   ener_bei),
                                                   ener_mei=ifelse(is.na(ener_mei),
                                                                   0,
                                                                   ener_mei), 
                                                   emi_bei_co2=ifelse(is.na(emi_bei_co2),
                                                                      0,
                                                                      emi_bei_co2),
                                                   emi_mei_co2=ifelse(is.na(emi_mei_co2),
                                                                      0,
                                                                      emi_mei_co2),
                                                   pop_bei=ifelse(is.na(pop_bei),
                                                                  mean(pop_bei, na.rm = TRUE),
                                                                  pop_bei),
                                                   pop_mei=ifelse(is.na(pop_mei),
                                                                  mean(pop_mei, na.rm = TRUE),
                                                                  pop_mei)
)

beimei_f <- beimei_f %>% mutate(ms=ifelse(sector_id %in% c(9, 10, 11, 12), 7, 2))
# cities that use coal or lignite in transport (maybe trains?) 
bad_cities <- distinct(beimei_f[beimei_f$ms==7 & (beimei_f$carrier_id %in% c(8,9)),] %>% select(name)) # heating oil should also be added. 

# # to remove the bad cities uncomment the following line
# #df_final_f <- df_final_f %>% filter(!(name %in% c(t(bad_cities))))
# # I decided to remove only the lines instead (so I keep Vienna)
print("The lines using coal for transport are being removed for the following cities" )
print(c(t(bad_cities)))
cat('The lines using coal for transport are being removed for the following cities', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
cat(c(t(bad_cities)), file = paste0(subDir,"/test.log"), append = TRUE,  sep= "\n")
beimei_f <- beimei_f[!(beimei_f$name %in% bad_cities), ]
beimei_f <- beimei_f[beimei_f$name %in% citiesbeimei, ]
return(beimei_f)
}