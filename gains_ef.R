# functions that allow to calculate emissions factors from GAINS
# emissons factors reported by gains are in kton/PJ
# International Institute for Applied Systems Analysis (IIASA). GAINS Model.
# Laxenburg, Austria. url: http://gains.iiasa.ac.at/models/index.
# html.

# contact: emanuela.peduzzi@gmail.com, enrico.pisoni@ec.europa.eu


library(readxl)
library(dplyr)
library(readr)
library(stringr)



###############################################################################################################
#read data GAINS, activity levels, IEF and emissions
read_data_gains <- function(path, polls) {
  res <- list()
  for (p in polls) {
    nf <- paste0(path,p,'_emi_impl_n(1).csv')
    datagains <- data.frame()
    datagains <- suppressMessages(suppressWarnings(read_csv(file=nf, skip = 6)))
    # remove useless rows
    # ind <- which(datagains$Region == "Scenario Definition")
    # abbreviate region
    datagains <- datagains %>% mutate(Region = substr(Region,1,4)) %>% select('Region', 'Sector', 'Activity', '[Unit]', 'activity', 'impl_ef', 'emiss')
    # datagains <- datagains[1:ind-1,]
    # summarize all DOM together (DOM, DOM_STOVE...), from GAINS  
    # substitute average emission factors ONLY for DOM 
    flagDom <- str_detect(datagains$Sector, 'DOM')
    datagains$Sector[flagDom] = 'DOM'
    datagainsdom <- datagains %>% filter(Sector=='DOM') %>% group_by(Region, Sector, Activity, `[Unit]`) %>%
      na.omit() %>% summarise(activity=sum(activity), impl_ef=sum(emiss)/sum(activity), emiss=sum(emiss))
    # join dom to the original dataframe
    dataj <- left_join(datagains, datagainsdom, by=c('Region', 'Sector', 'Activity', "[Unit]"))
    # substitued ief with the calculated ones
    dataj <- dataj %>% mutate(activity.y=ifelse(is.na(activity.y), activity.x, activity.y),
                              emiss.y=ifelse(is.na(emiss.y), emiss.x, emiss.y), 
                              impl_ef.y=ifelse(is.na(impl_ef.y), impl_ef.x, impl_ef.y))
    # rename dataframe and take only unique rows                       
    dataj_f <- dataj %>%
      select(Region, Sector, Activity, `[Unit]`, activity.y, impl_ef.y, emiss.y)  %>%
      dplyr::rename(activity=5, impl_ef=6, emiss=7)  
    dataj_f <- distinct(dataj_f)
    
    # in some rare cases, e.g. when activity and emissions are zero (very small) for the detailed DOM sectors, the calculated ef 
    # are NA while the original ones may be different than zero . Therefore when joining I get more than
    # one EF for the same poll, sector, activity, region combination (see CYPR, DOM, BC1, PM25). To avoid this problem
    # I take the average emission factor. 
    dataj_f <- dataj_f %>%  group_by(Region, Sector, Activity, `[Unit]`) %>% summarise(activity=mean(activity), impl_ef=mean(impl_ef), emiss=mean(emiss))  %>% ungroup()
    res[[p]] <- dataj_f
  }
  return(res)
}

read_data_gains_years<- function(subDir, path_g, polls, years){
print('Reading GAINS File')
cat('Reading GAINS File', file = paste0(subDir,"/test.log"), append = TRUE,  sep = "\n")
# read GAINS data - #loop from 1990 to 2030 or for the years you have available
polls <- str_replace_all(tolower(polls),'[.]', '')
gains_data <- list()
# gains_full_act <- list()
# gfa <- list()
# for (year in seq(1990,2030,5)) {
for (year in years){
  # print(year) #todelete
  yearC <- as.character(year)
  # output is a list with full GAINS IEF, act, emis...for nox,voc,nh3,pm,so2
  gains_data[[yearC]] <- read_data_gains(paste0(path_g,year,'/'), polls)
  # here I create a list containing for each year the full set of sec-act, binding all pollutants 
  # (to avoid missing some sec-act), for the domestic sector only the aggregated biogenic fuels are considered.
  # this is not used now but it may be used in the future to estimate percentage reduction of a sectors for SHERPA input
  # gains_full_act[[yearC]] <- rbind(gains_data[[yearC]]$nox, gains_data[[yearC]]$voc, 
  #                                  gains_data[[yearC]]$nh3, gains_data[[yearC]]$pm, gains_data[[yearC]]$so2) 
  # gfa[[yearC]] <- distinct(gains_full_act[[yearC]], Region, Sector, Activity, .keep_all= TRUE) %>% filter(`[Unit]`=='[PJ]', 
  #                                                                                                         !(Activity %in% c('FWD', 'BIOG', 'CHCOA')))
}

# create dataframe with all GAINS data:
# gainsAvailYear <- seq(1990,2030,5)
gainsAvailYear <- years
# polls <- c('pm10', 'pm25', 'nox') # selection of pollutants
gains_df <- data.frame()
for (poll in polls){
  for (year in gainsAvailYear){
    gains_tmp<- gains_data[[as.character(year)]][[poll]] %>% filter(str_detect(Sector, "DOM|TRA_RD"))
    gains_tmp$year <- year
    gains_tmp$poll <- poll
    gains_df<-rbind(gains_df, gains_tmp)
  }}

# remove duplicated rows (e.g. DOM...) 
gains_df_or<- distinct(gains_df) 

return(gains_df_or)}


com_gains_merge<- function(beimei_f, gains_df_or, subDir, polls){
# polls <- c('pm10', 'pm25', 'nox') # selection of pollutants
polls <- str_replace_all(tolower(polls),'[.]', '')
  
# putting all com data in a data.frame and adding the pollutant column
df_tmp <-beimei_f %>% ungroup()  # CoM data
df_final <- data.frame() # final dataframe 
for (poll in polls){
  df_tmp$poll <- poll
  df_final<-rbind(df_final, df_tmp)}

# matching country name between CoM and gains
n_list_newdataset <- (list(
  Spain = 'SPAI', ## should be sp
  Germany = 'GERM',
  Austria = 'AUST',
  Bulgaria = 'BULG',
  Belgium = 'BELG',
  Finland = 'FINL',
  France = 'FRAN',
  Italy = 'ITAL',
  Greece = 'GREE', #?? should be EL
  Luxemburg = 'LUXE',
  Norway = 'NORW',
  Portugal = 'PORT',
  Ireland = 'IREL',
  Romania = 'ROMA',
  Hungary = 'HUNG',
  UnitedKingdom = 'UNKI',
  Estonia = 'ESTO',
  Poland = 'POLA',
  Croatia = 'CROA',
  TheNetherlands = 'NETH',
  Denmark = 'DENM',
  Latvia = 'LATV',
  Sweden = 'SWED',
  Iceland = 'ICEL',
  Switzerland = 'SWIT',
  Slovakia = 'SKRE',
  Cyprus = 'CYPR',
  Slovenia = 'SLOV',
  CzechRepublic = 'CZRE',
  Lithuania = 'LITH')
)

df_final <- df_final %>% mutate(cou4 = gsub(" ", "", cou)) %>% mutate(cou5 = gsub("-", "", cou4)) %>%
  mutate(gains_country = ifelse(cou5 %in% names(n_list_newdataset), as.character(n_list_newdataset[cou5]), cou5)) %>%
  select(-matches('cou4')) %>% select(-matches('cou5'))



# all years considered (for BEI and MEI)
years <- unique(c(unique(df_final$BeiYear), unique(df_final$MeiYear)))
gains_sel_y <- data.frame()

# interpolate the ief, 
for (y in years){
  # y <- 2012
  lbY <- floor(y/5)*5
  ubY <- ceiling(y/5)*5
  df_y_gains <-gains_df_or %>% filter(year %in% c(lbY, ubY))
  # put the two years side by side
  df_y_gains <- inner_join(df_y_gains[df_y_gains$year==lbY,],
                           df_y_gains[df_y_gains$year==ubY,],
                           by=c('Region', 'Sector','Activity', 'poll', '[Unit]'))
  if(lbY!=ubY){
    # calculate ief
    df_y_gains <- df_y_gains %>% mutate(da=(activity.x-activity.y)/(year.x-year.y),
                                        activity=activity.x+da*(y-year.x),
                                        de=(emiss.x-emiss.y)/(year.x-year.y),
                                        emiss=emiss.x+de*(y-year.x),
                                        di=(impl_ef.x-impl_ef.y)/(year.x-year.y),
                                        impl_ef1= ifelse(is.infinite(emiss/activity) | is.na(emiss/activity), 0, emiss/activity), # found for CYPR emission 0, activity 0, ef != 0...
                                        impl_ef2= ifelse(is.infinite(impl_ef.x+di*(y-year.x)) | is.na(impl_ef.x+di*(y-year.x)), 0, impl_ef.x+di*(y-year.x)) # I don't think this can ever be inf
                                        
    ) %>%
      mutate(impl_eff=pmax(impl_ef1, impl_ef2)
      ) %>%
      select(Region, Sector, Activity, activity, poll, '[Unit]', impl_eff) %>%
      mutate(emiss=impl_eff*activity)
  } else {
    df_y_gains <-gains_df_or %>% filter(year %in% c(y)) %>%  dplyr::rename(impl_eff=impl_ef)
  }
  # substitute na and infinite (value for CYPRUS) with zeros 
  df_y_gains[is.na(df_y_gains)] <- 0
  df_y_gains[sapply(df_y_gains, simplify = 'matrix', is.infinite)] <- 0
  
  # calculate means over all countries to be substituted to the zeros (weighted mean) (is.na(impl_eff) probably not necessary)
  df_y_gains <- df_y_gains %>% group_by(Sector, Activity, poll)  %>%
    mutate(impl_ef_mean=weighted.mean(impl_eff[impl_eff!=0 & !is.na(impl_eff)], activity[impl_eff!=0 & !is.na(impl_eff)], rm.na=T))
  
  # this take care of the Germany case - DOM BC1 otherwise gets too high emission factors... could be done in a simpler way
  if (lbY >= 1995){
    y_m <- lbY-5
    tom_l <- gains_df_or[gains_df_or$year==y_m,] %>% select(c('Region', 'Sector', 'Activity', '[Unit]', 'poll', 'impl_ef')) %>% 
      dplyr::rename(impl_efl=impl_ef)
    gains_df_m <-full_join(df_y_gains, tom_l, by=c('Region', 'Sector', 'Activity', '[Unit]', 'poll'))
    df_y_gains<-gains_df_m %>% mutate(impl_eff=ifelse(isZero(impl_eff) & !is.na(impl_efl),impl_efl,impl_eff))
  }
  
  # if the IEF is ZERO take the average one of all the sector activity of all other countries
  df_y_gains <- df_y_gains %>% mutate(impl_ef=ifelse(isZero(impl_eff) & !is.na(impl_ef_mean),impl_ef_mean,impl_eff)) %>%
    select(Region, Sector, Activity, '[Unit]', poll, activity, impl_ef, emiss, impl_ef_mean) %>% ungroup()
  df_y_gains$year <- y
  
  gains_sel_y<- rbind(gains_sel_y, df_y_gains)
  
}

#------------------------------------------------------------------------------------------------------------
# calculate non-exhaust emissions for transport
# remove this if you do not want ne
# calculate total activities and total emissions
df_y_gains_ne <- gains_sel_y  %>% 
  filter(str_detect(Sector, "TRA_RD")) %>% filter(str_detect(poll, "pm")) %>%
  group_by(Region, Sector, poll, `[Unit]`, year) %>% 
  mutate(totactivity=ifelse(`[Unit]` %in% c('[Gvkm]'), mean(activity), sum(activity)), 
         totemissions=sum(emiss)) %>% select(Region, Sector, poll, `[Unit]`, year, totactivity, totemissions) %>% distinct()

df_y_gains_ne_en <- df_y_gains_ne[df_y_gains_ne$`[Unit]`=="[PJ]", ]
df_y_gains_ne_dist <- df_y_gains_ne[df_y_gains_ne$`[Unit]`=="[Gvkm]", ]
df_y_gains_ne_j <- inner_join(df_y_gains_ne_en, df_y_gains_ne_dist, by=c('Region', 'Sector', 'poll', 'year')) %>% 
  mutate(impl_ef_ne=totemissions.y/totactivity.x)
df_y_gains_ne_j[is.na(df_y_gains_ne_j)] <- 0
df_y_gains_ne_j <- df_y_gains_ne_j %>% group_by(Sector, poll, year)  %>%
  mutate(impl_ef_ne_mean=weighted.mean(impl_ef_ne[impl_ef_ne!=0], totactivity.x[impl_ef_ne!=0]))
df_y_gains_ne_j <- df_y_gains_ne_j %>% mutate(impl_ef_ne=ifelse(isZero(impl_ef_ne),impl_ef_ne_mean,impl_ef_ne)) %>%
  select(Region, Sector, poll, year, '[Unit].x',impl_ef_ne) %>%  dplyr::rename('[Unit]'='[Unit].x')

gains_sel_y <- merge(gains_sel_y, df_y_gains_ne_j, all.x=TRUE, by=c('Region', 'Sector', 'poll', '[Unit]', 'year')) %>%
  mutate(impl_ef=ifelse(!is.na(impl_ef_ne), (impl_ef+impl_ef_ne), impl_ef))  %>%  select(-c('impl_ef_ne', 'impl_ef_mean'))
#------------------------------------------------------------------------------------------------------------

df_final_f <- df_final
# correcting HO to MD to transport (it would not make sense)
df_final_f <-  df_final_f %>% transform(
  gains_carrier=ifelse(
    gains_carrier=='HO'& str_detect(gains_sector, "TRA_RD"),
    'MD',
    gains_carrier))

#adding all carrier names according to gains
df_final_f <- df_final_f %>% transform(gains_carrier_f=gains_carrier) %>% transform(
  gains_carrier_f=ifelse(
    gains_carrier_f=='BIO' & gains_sector=='DOM'& (poll=='pm10' | poll=='pm25' | poll=='voc'), 
    "FWD, BIOG, CHCOA",
    gains_carrier)) %>% transform(
      gains_carrier_f=ifelse(
        gains_carrier=='BIO' & gains_sector=='DOM'& !(poll=='pm10' | poll=='pm25' | poll=='voc'), 
        "OS1",
        gains_carrier_f)) %>% transform(
          gains_carrier_f=ifelse(
            gains_carrier=='BIO' &  str_detect(gains_sector, "TRA_RD"), 
            "MD, GSL, GAS",
            gains_carrier_f)) %>% transform(
              gains_carrier_f=ifelse(
                gains_carrier=='ALL_FF', 
                'MD, GSL, GAS, BC1, HC1, LPG, MD, DC, HF, HC2, BC2',
                gains_carrier_f)) %>% transform(
                  gains_carrier_f=ifelse(
                    gains_carrier=='HO'& gains_sector=='DOM', 
                    'MD, HF',
                    gains_carrier_f)) 


print('Calculating emissions of air pollutants precursors')
cat('Calculating emissions of air pollutants precursors', file = paste0(subDir,"/test.log"), append = TRUE,  sep= "\n")
# adding row ID for expansion 
df_final_f$ID <- seq.int(nrow(df_final_f))
sect_comb <- df_final_f

# expanding rows with multiple elements (e.g. fuels, sectors)
sect_comb_expand <- sect_comb %>%
  group_by(ID) %>%
  tidyr::expand(gains_sector=trimws(unlist(strsplit(as.character(gains_sector), ","))),
         gains_carrier_f=trimws(unlist(strsplit(as.character(gains_carrier_f), ","))), name, gains_country, gains_carrier, BeiYear, MeiYear, poll)

sect_comb_expand_m <- merge(sect_comb_expand, gains_sel_y, 
                            by.x = c("gains_country","gains_sector", "gains_carrier_f", "poll", "BeiYear"), 
                            by.y = c("Region", "Sector","Activity", "poll", "year")) %>%
  dplyr::rename(act_bei=activity, ief_bei=impl_ef,  emiss_bei=emiss)
sect_comb_expand_m <- merge(sect_comb_expand_m, gains_sel_y, 
                            by.x = c("gains_country","gains_sector", "gains_carrier_f", "poll", "MeiYear", "[Unit]"), 
                            by.y = c("Region", "Sector","Activity", "poll", "year", "[Unit]")) %>%
  dplyr::rename(act_mei=activity, ief_mei=impl_ef,  emiss_mei=emiss)

# calculating wm for each expanded ID 
sect_comb_expand_wm <- sect_comb_expand_m %>% group_by(ID) %>% 
  mutate(ief_bei = ifelse(sum(act_bei)!=0, weighted.mean(ief_bei, act_bei), mean(ief_bei)),
         ief_mei=ifelse(sum(act_mei)!=0,  weighted.mean(ief_mei, act_mei), mean(ief_mei))) 

# merging with the original dataframe
sect_comb <- distinct(merge(sect_comb, sect_comb_expand_wm, all.x=TRUE,  by=c("ID", "name", "gains_country", "gains_carrier", "BeiYear","MeiYear", "poll")) %>% 
                        select(-c("gains_sector.y", "gains_carrier_f.y", "act_bei", "act_mei", "emiss_bei", "emiss_mei")))

# saving results

print('Saving results')
cat('Saving results', file = paste0(subDir,"/test.log"), append = TRUE,  sep= "\n")

df_final_results <- sect_comb
df_final_results <- df_final_results %>% mutate(ief_bei=ief_bei *3.6 *10**(-3), ief_mei= ief_mei *3.6 *10**(-3))# kton/PJ to ton/MWh careful! 
df_final_results <-  df_final_results %>% mutate(emi_e_bei_ef_bei=ener_bei*ief_bei)
df_final_results <-  df_final_results %>% mutate(emi_e_mei_ef_mei=ener_mei*ief_mei)
df_final_results <-  df_final_results %>% mutate(emi_e_mei_ef_bei=ener_mei*ief_bei)

names(df_final_results)
write.csv(df_final_results, file = paste0(subDir,'/summary_gains_Com.csv'))

df_final_results[is.na(df_final_results)] <- 0
df_final_results2 <- df_final_results%>%  select(-c('ID', '[Unit]')) %>% group_by(cou, name, poll, ms) %>% summarise(ener_bei=sum(ener_bei, na.rm = TRUE),
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

write.csv(df_final_results2, file = paste0(subDir,'/summary_gains_Com_sel_ms.csv'))

return(df_final_results2)
}

