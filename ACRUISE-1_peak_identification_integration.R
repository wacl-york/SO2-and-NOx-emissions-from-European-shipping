################################################################################
### ACRUISE-1 peak identification and integration ###

#Contributions from: Will Drysdale, Dominika Pasternak.

################################################################################
### Loading packages ###

library(dplyr)
library(ggplot2)
library(ncdf4)
library(ggmap)
library(reshape2)
library(grid)
library(gridExtra)
library(Rmisc)
library(plotly)
library(tidyverse)
library(shonarrr)
library(wsdmiscr)
library(lubridate) 
library(viridis)
library(waclr)
library(data.table)
library(here)
library(openair)

library(pracma)



################################################################################
### Initial setup ###

# set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE_INTEGRATION3/merged")

# load files
dm1 <- readRDS("c181_merge.RDS")

# subset
dat_raw <- subset(dm1, select=c(date, nox, so2)) %>%
           timeAverage("5 sec")

# remove NAs
#dm[is.na(dm)] <- 0



################################################################################
### Find some peaks with Will's help ###

detect_some_peaks = function(dat,col,thresh = 0.03){
  
  col = enquo(col)
  
  dat %>% 
    tibble() %>% 
    mutate(
      norm = scales::rescale(!!col),
      rate = (!!col-lag(!!col))/lag(!!col),
      rate = case_when(is.infinite(rate) ~ NA_real_,
                       rate == -1 ~ NA_real_,
                       TRUE~rate),
      rate_norm = scales::rescale(rate),
      weight_rate_norm = rate_norm*norm,
      thresh = sqrt(weight_rate_norm) >= thresh
    )
}


detect_some_peaks(dat_raw,nox,thresh = 0.03) %>% 
  ggplot()+
  geom_point(aes(date,nox, colour = thresh))


detect_some_peaks(dat_raw,so2,thresh = 0.15) %>% # change the theshold value to change sensitivity 
  ggplot()+
  geom_point(aes(date,so2, colour = thresh))



################################################################################


################################################################################
### Manchester's code ###

# ### Reads data from the CSV
# ### Removes all recordings that are 0
# Defines each plume as beginning at a manually specified timepoint (to the nearest second) and lasting for 1 minute
# For each plume:
# Finds the recordings in the 15 timepoints before and after the plume
# Calculates the mean gas concentration in these 2 windows for each gas to use as the background concentration
# Finds the plume concentrations made during the plume period for each gas
# Subtracts the background concentration from the plume concentrations for each gas
# Sets negative values to missing (or 0 would have the same effect)
# Calculates the sum of these background-corrected plume concentrations (this is effectively calculating the area under the peak)
# Some additional conversions that we don't fully understand, like converting SO2 concentration to mass and calculating Emission Factors (EF) as the ratio of one concentration to another, normalised by 1000 and scaled by various 'magic' numbers  (lines 380 - 449)
# Runs a one-sided t-test for CO2 and SO2 concentrations between the plume and background concentrations
# Discards all concentrations and EFs if the CO2 t-test was significant (SO2 t-test results are unused)


# Briefly we define that the 15s before and 15s after the plume area is the 'plume baseline area'. As we cannot decide an exact starting time and ending time for the plume, the baseline will help us work out the 'net peak area'. 
# To validate our calculation, we apply a t-test to the non-decay gas (CO2 and NOx) for the two groups (the baseline area group and the plume area group). Because the peak area must be higher than the baseline we will apply a single tail t-test with 5% significance level. If a ship plume failed to reject the hypothesis, it will be filtered out.







################################################################################
### JUNK ###

### Find peaks ###

# find where peaks are
peaks <-  as.data.frame(findpeaks(dm$nox, 
                                  nups = 1, 
                                  ndowns = 1, 
                                  minpeakheight = 10, 
                                  minpeakdistance = 10, 
                                  threshold = 10)) %>%
  dplyr::rename(height = V1,
                date = V2,
                start = V3,
                end = V4)



ggplot()+
  geom_point(data=dm,
             aes(x=date, y=nox),
             size=2)+
  geom_point(data=peaks, 
             aes(x=date, y=height),
             colour="red") +
  theme_bw()









