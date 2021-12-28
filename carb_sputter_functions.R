# Functions for carbonate sputter data


# add consensus values to carb sputter data
add_consenus <- function(df) {
  # get rec_num
  db <- conNOSAMS()
  query <- glue::glue_sql("SELECT tp_num, rec_num
             FROM target
             WHERE tp_num IN ({tpnums*})",
             tpnums = df[['tp_num']],
             .con = db
  )
  
  recs <- odbc::dbSendQuery(db, query)
  recnums <- odbc::dbFetch(recs)
  odbc::dbClearResult(recs)
  df <- left_join(df, recnums)
  # get cons table
  std <- amstools::getStdTable()
  # join on rec_num
  df %>%  
    left_join(select(std, rec_num, fm_consensus), by = "rec_num") %>% 
    mutate(fm_consensus = case_when(rec_num == 101730 ~ 1.0398,
                                    rec_num == 72446 ~ 0.0013,
                                    TRUE ~ fm_consensus))
}

# Blank correct carbonate sputter data
blank_cor_carb <- function(data, blanks = NULL, fmstd = 1.0398) {
  if (!is.null(blanks)) {
    data <- data %>% 
      mutate(sample_type = case_when(wheel_pos %in% blanks ~ "B",
                                     sample_type == "B" ~ "U",
                                     TRUE ~ sample_type ))
  }
  
  blanks <- data %>% 
    filter(sample_type == "B")
  
  data['sig_norm_ratio'] <- pmax(data['int_err'], data['ext_err'])
  
  meanblank <- mean(blanks$norm_ratio)
  
  # get blank error using SNICSer error floor
  blankerr <- amstools::blankErr(blanks$norm_ratio, blanks$sig_norm_ratio) # uses SNICSer error floor method
  # apply blank correction and propagate error
  data %>% 
    mutate(fm_corr = amstools::doLBC(norm_ratio, meanblank, fmstd),
           sig_fm_corr = amstools::doLBCerr(norm_ratio, meanblank, fmstd, sig_norm_ratio, blankerr)
    )
}