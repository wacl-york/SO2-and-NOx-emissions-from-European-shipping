library(here)
library(dplyr)
library(ggplot2)
library(openair)
library(lubridate)
library(tidyr)
library(dygraphs)
library(zoo)

assignPlumes = function(df,
                        peakPeaks = NULL,
                        forwards = 60,
                        backwards = 30,
                        background_duration = 15){
  
  if(is.null(peakPeaks)){
    peakPeaks = which(!is.na(df$Peak))
  }
  
  peakTable = tibble(
    Peak = df$Peak[peakPeaks],
    rangeStart = peakPeaks-(backwards+background_duration),
    rangeEnd = peakPeaks+(forwards+background_duration)) %>% 
    rowwise() %>% 
    mutate(data = df[rangeStart:rangeEnd,] %>% 
             mutate(index = 1:nrow(.)) %>% 
             mutate(isBackground = !between(index,background_duration,nrow(.)-background_duration),
                    isPeak = between(index,background_duration,nrow(.)-background_duration)
             ) %>% 
             list()
    ) %>% 
    dplyr::select(-rangeStart,-rangeEnd)
  
  peakTable
  
}


spanPlume = function(x,
                     forwards = 60,
                     backwards = 30){
  
  start = x-backwards
  end = x+forwards
  
  start:end
}

# calcPlumeBackground = function(peak,
#                                species = "so2",
#                                .f = mean,
#                                ...){
#   peak = peak %>% 
#     filter(isBackground)
#   
#   .f(peak[[species]],...)
# }

createPeakStages = function(peak,species){
  peak_rle = rle(peak$isBackground)$lengths
  
  stages = map2(.x = 1:length(peak_rle),
                .y = peak_rle,~rep(.x,.y)) %>% 
    unlist()
  
  peak %>% 
    mutate(stage = stages) %>% 
    group_by(stage) %>% 
    select(stage, !!species)
}

calcPlumeStats = function(peak, species = "so2"){
  
  peak %>% 
    createPeakStages(species = species) %>% 
    summarise_all(c("mean","median","sd"),na.rm = T) 
  
}

calcPlumeArea = function(data,stats,species){
  
  stage2 = data %>% 
    createPeakStages(species = species) %>% 
    filter(stage == 2) %>% 
    mutate(index = 1:nrow(.))
  
  if(is.na(stage2[[species]][1]) | is.na(stage2[[species]][nrow(stage2)])){
    
    species_nao = na.omit(stage2[[species]])
    
    if(is.na(stage2[[species]][1])){
      stage2[[species]][1] = species_nao[1]
    }
    
    if(is.na(stage2[[species]][nrow(stage2)])){
      stage2[[species]][nrow(stage2)] = species_nao[length(species_nao)]
    }
  }
  
  stage2[[species]] = zoo::na.approx(stage2[[species]])
  
  background = rep(NA,nrow(stage2))
  background[1] = stats$median[1]
  background[length(background)] = stats$median[2]
  
  stage2$background = zoo::na.approx(background)
  
  stage2$enhance = stage2[[species]]-stage2[["background"]]
  
  stats$area = c(NA,pracma::trapz(stage2$index,stage2$enhance),NA)
  
  #
  stats
}

calcEmissionRatio = function(peakDat){
  peakDat %>% 
    filter(!is.na(area)) %>% 
    select(Peak,species,area) %>% 
    pivot_wider(names_from = "species",values_from = "area") %>% 
    pivot_longer(-c(Peak,co2),names_to = "species",values_to = "area") %>% 
    mutate(ratio = area/co2)
} 

validatePlume = function(peak){
  # t-test 'n' shit
}


dat_raw = read.csv(here("C181_example_of_data.csv")) %>% 
  select(-X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  tibble() %>% 
  mutate(nox_smooth = na.approx(nox,na.rm = F))

peak_meta = read.csv(here("integrations_ACRUISE1_C179_C190_no_ships.csv")) %>% 
  mutate(date = dmy_hms(paste0(Date,Timestamp,sep = " "))) %>% 
  select(-Date,-Timestamp) %>% 
  tibble()

peakDat = peak_meta %>% 
  filter(Flight_C == 181) %>% 
  select(date,Peak) %>% 
  left_join(dat_raw,.,"date") %>% 
  assignPlumes(forwards = 60) %>% 
  mutate(so2 = list(calcPlumeStats(data)),
         co2 = list(calcPlumeStats(data, species = "co2")),
         nox = list(calcPlumeStats(data, species = "nox"))) %>% 
  pivot_longer(-c(Peak,data),
               names_to = "species",
               values_to = "stats") %>% 
  rowwise() %>% 
  mutate(stats = list(calcPlumeArea(data,stats,species))) %>% 
  unnest(stats)

ratios = calcEmissionRatio(peakDat)
