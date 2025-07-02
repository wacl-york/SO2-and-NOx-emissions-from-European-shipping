detect_plumes_and_refit <- function(concentration,
                          background,
                          time,
                          plume_sd_threshold = 3,
                          plume_sd_starting = 1,
                          plume_buffer = 15) {
  require(nanotime)
  require(data.table)

  refit = TRUE # this is a massive bodge
  # Convert into nanotime (64-bit int) if in POSIX (32-bit double)
  bg <- background$bg
  time <- acruiseR:::posix_to_nanotime(time)
  residual_sd <- sd(concentration - bg, na.rm = T)
  dt <- data.table::data.table(time = time, concentration = concentration, background = bg)
  dt[, is_plume_starting := !is.na(concentration) & !is.na(background) & concentration > (background + plume_sd_starting * residual_sd)]
  dt[, is_plume := !is.na(concentration) & !is.na(background) & concentration > (background + plume_sd_threshold * residual_sd)]
  dt[, plume_group_starting := cumsum((is_plume_starting != data.table::shift(is_plume_starting, fill = F, type = "lag")))]
  
  plume_groups_dt <- dt[is_plume_starting == TRUE, list("has_plume" = sum(is_plume), start = min(time), end = max(time)), by = plume_group_starting][has_plume > 0]
  
  # Find overlapping plumes within the buffer
  setkey(plume_groups_dt, start, end)
  overlaps <- foverlaps(plume_groups_dt,
                        plume_groups_dt[, .(start, end = end + nanotime::nanoduration(hours = 0, minutes = 0, seconds = plume_buffer, nanoseconds = 0))],
                        type = "any",
                        which = TRUE
  )
  
  overlaps[, c("x_prev_seen", "y_prev_seen") := list(duplicated(xid), duplicated(yid))]
  overlaps[, combined_plume := cumsum(!(x_prev_seen | y_prev_seen))]
  # Find group for each row number
  new_plumes <- unique(overlaps, by = c("xid", "combined_plume"))
  # Join back into overlapping plumes then elongate plumes with new members
  plumes_final <- plume_groups_dt[, xid := 1:nrow(plume_groups_dt)][new_plumes, on = "xid"][, list(
    start = min(start),
    end = max(end)
  ),
  by = combined_plume
  ]
  # Then add on baseline and convert back to datetime
  plumes_final[, combined_plume := NULL]
  setorder(plumes_final, start)
  out <- as.data.frame(plumes_final)
  
  if (refit) {
    # Remove plumes and reidentify background
    plumes_final[, in_plume := TRUE]
    plumes_removed <- plumes_final[dt, on = c("start <= time", "end >= time"), .(time, concentration, background, in_plume)]
    plumes_removed[in_plume == TRUE, concentration := NA]
    args <- c(list(concentration = plumes_removed$concentration), background$call)
    bg_new <- do.call(identify_background, args)
    
    # Redetect plumes with new background
    out <- detect_plumes(concentration,
                         bg_new,
                         time,
                         plume_sd_threshold = plume_sd_threshold,
                         plume_sd_starting = plume_sd_starting,
                         plume_buffer = plume_buffer,
                         refit = FALSE
    )
    
    out = list(out = out, 
               bg_new = bg_new)
    
  }
  out
}
