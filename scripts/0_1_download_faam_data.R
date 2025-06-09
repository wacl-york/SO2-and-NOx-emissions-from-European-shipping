library(faamr)
library(keyring)

source(here::here('functions','flight_details.R'))

ceda_user = key_get("ceda_user")
ceda_pass = key_get("ceda", ceda_user)

flightDetails = flight_details()

dirOut = here::here('data','faam_raw')

if(!dir.exists(dirOut)){
  dir.create(dirOut, recursive = T)
}

flight_download(flight = flightDetails$flight,
                user = ceda_user,
                pass = ceda_pass,
                dirOut = dirOut,
                files = c("flight-sum.txt", "core_1hz.nc", "faam-fgga.na")
                )
