
#extracting longnames, thanks Shona x #
### Loading packages ###

library(dplyr)
library(ggmap)
library(ggplot2)
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


#set working directory
setwd("G:/My Drive/ACRUISE/Masaru_info")

#load files
ncdf <- "faam-wcm2000_faam_20190712_r0_c181_prelim.nc"

#open ncdf
data_nc <-  ncdf4::nc_open(ncdf)

#gen short names
vars <- names(data_nc$var)

#get long names
#df_meta <- purrr::map_dfr(
#  vars, ~ncatt_get(data_nc, .x))
#longnames <- df_meta$long_name


# or
df_meta <- purrr::map(
  vars, ~ncatt_get(data_nc, .x)
)
my_vars <- c("long_name")
longnames <- purrr::map_dfr(df_meta, `[`, my_vars) 



#pu together short & long names
names <-  data.frame(vars,longnames)

#save
write.csv(names, "G:/My Drive/ACRUISE/Masaru_info/wcm2000.csv")

#close ncdf
nc_close(data_nc)
