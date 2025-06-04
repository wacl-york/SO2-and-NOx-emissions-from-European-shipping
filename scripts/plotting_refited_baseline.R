refit_onList <- detect_plumes_and_refit(dm$SO2_conc_scaled, bg_so2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 1,
                                     plume_buffer = 15)
bg_so2_new = refit_onList$bg_new

refit_on = refit_onList$out

refit_off <- acruiseR::detect_plumes(dm$SO2_conc_scaled, bg_so2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 1,
                                     plume_buffer = 15,
                                     refit = FALSE )

backgrounds = tibble(
  first = bg_so2$bg,
  second = bg_so2_new$bg
) 

bg_plot <-  bind_cols(dm,backgrounds)



#refitted vs not refitted
ggplot(data=bg_plot)+
  geom_line(aes(x=date, 
                 y=SO2_conc_scaled),
            size=1,
            alpha=1) +
  geom_line(aes(x=date,
                y=first),
            colour="#35b779",
            size=1) +
  geom_line(aes(x=date,
                y=second),
            colour="#fde725",
            size=1) +
  theme_bw()+
  theme(text = element_text(size=13))+
  labs(x= "Time", y=bquote(''~SO[2]~(ppb)~''))



# before refit plus st devs - so2

a=bg_plot$SO2_conc_scaled %>% 
  na.omit()%>%
  sd()



bg_plot %>%
  filter(between(date, 
                 ymd_hms("2021-10-01 14:04:45"),
                 ymd_hms("2021-10-01 14:05:45"))) %>%


ggplot()+
  geom_line(aes(x=date, 
                y=SO2_conc_scaled),
            #size=1,
            colour="grey50") +
  geom_line(aes(x=date,
                y=first),
            colour="#fcfdbf",
            size=1) +
  geom_line(aes(x=date,
                y=first+1*a),
            colour="#fc8961",
            size=1) +  
  geom_line(aes(x=date,
                y=first+3*a),
            colour="#b73779",
            size=1) +
  geom_point(aes(x=date, 
                y=SO2_conc_scaled),
            size=3,
            shape=4) +
  theme_bw()+
  theme(text = element_text(size=14))+
  labs(x= "Time", y=bquote(''~SO[2]~(ppb)~''))


# peaks integrated

acruiseR::plot_plumes(dm$SO2_conc_scaled, dm$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 1)+
  theme_bw()+
  theme(legend.position='none',
        text = element_text(size=14))+ 
  labs(x= "Time", y=bquote(''~SO[2]~(ppb)~''))+
  xlim(dmy_hms("01-10-2021 14:04:30"), dmy_hms("01-10-2021 14:06:00"))+
  ylim(-1,4)








# -------------------------------------------------------------------------



onPlot = acruiseR::plot_plumes(dm$SO2_conc_scaled, dm$time_nano, refit_on,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)

offPlot = acruiseR::plot_plumes(dm$SO2_conc_scaled, dm$time_nano, refit_off,
                               ylabel = "Concentration",
                               xlabel = "Time",
                               date_fmt = "%H:%M",
                               bg_alpha = 0.9)+
  theme(legend.position='none')

plume_data = bind_rows(
  onPlot$data %>%
    mutate(refit = T),
  offPlot$data %>%
    mutate(refit = F)
) %>%
  tibble()

baseline_refit_off = plume_data %>%
  filter(is.na(plume_id),
         !refit)

baseline_refit_on = plume_data %>%
  filter(is.na(plume_id),
         refit)

plumes = plume_data %>%
  filter(!is.na(plume_id),
         refit)

plume_data %>%
  ggplot()+
  geom_line(aes(time,concentration, colour = as.factor(plume_id)))+
  facet_wrap(~refit)





