
tidy_ugga_canisters = function(file,
                               lowRate = 0.998,
                               highRate = 1.002,
                               co2Thresh = 500,
                               naThresh = 15, 
                               caseNum = c(rep(8, 16),rep(106,8)),
                               bottleNum = c(1:16,1:8),
                               returnAllData = TRUE,
                               statistic = c("median","sd")){
  
  rateData <-  file %>% 
    mutate(co2_rate = CO2_ppm/lag(CO2_ppm)) %>%
    mutate(CO2_ppm = ifelse(between(co2_rate, lowRate, highRate) &
                              CO2_ppm < co2Thresh, CO2_ppm, 999999)) %>% 
    select(date = Time,
           CH4_ppm, 
           CO2_ppm)
  
  
  rleData = rateData$CO2_ppm %>% 
    rle() %>% 
    wsdmiscr::tidy_rle() %>% 
    filter(values == 999999, lengths >= naThresh) %>% 
    mutate(timeStart = rateData$date[idxStart],
           timeEnd = rateData$date[idxEnd])
  
  canisterTimes = rleData %>% 
    select(date_start = timeEnd, 
           date_end = timeStart) %>% 
    mutate(date_end = lead(date_end)) %>% 
    filter(!is.na(date_end)) 
  
  rateDataLong = rateData %>% 
    pivot_longer(-date) %>% 
    mutate(value = ifelse(value == 999999, NA, value))
  
  avgData = purrr::map(
    statistic,
    ~waclr::aggregate_by_date_span(canisterTimes,
                                   rateDataLong,
                                   type = "name",
                                   statistic = .x) %>% 
      rename(!!.x := value) %>% 
      pivot_wider(names_from = "name",
                  names_glue = "{name}_{.value}",
                  values_from = all_of(.x))
  ) %>% 
    purrr::reduce(left_join, by = c("date_start","date_end")) %>%
    mutate(case = caseNum,
           bottle = bottleNum)
  
  # Return
  if(returnAllData){
    list(rateData = rateData, 
         rleData = rleData, 
         canisterTimes = canisterTimes,
         avgData = avgData)
  }else{
    avgData
  }
  
  
}
