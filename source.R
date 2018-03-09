payment_sched_months <- function(balance, int, payoff_months){
  
  discount_factor = ((1 + int/12)^payoff_months - 1)/(int/12*(1 + int/12)^payoff_months)
  mo_payment <- balance/discount_factor
  
  df_full <- data.frame(month = 0, int_pay = 0, prin_pay = 0, total_pay = 0, balance = balance)
  
  for(month in 1:payoff_months){
    
    cur_balance <- df_full$balance[df_full$month == month - 1]
    
    int_pay <- cur_balance*int/12
    prin_pay <- mo_payment - int_pay
    
    new_balance <- cur_balance - prin_pay
    
    df <- data.frame(month = month, int_pay = int_pay, prin_pay = prin_pay, 
                     total_pay = mo_payment, balance = new_balance)
    
    df_full <- rbind(df_full, df)
    
  }
  
  df_full
  
}

payment_sched_mo_pay <- function(balance, int, mo_payment){
  
  df_full <- data.frame(month = 0, int_pay = 0, prin_pay = 0, 
                        total_pay = 0, balance = balance)

  month <- 1
  
  new_balance <- balance
    
  while(new_balance > 0.01){

    cur_balance <- df_full$balance[df_full$month == month - 1]

    int_pay <- cur_balance*int/12
    
    if(cur_balance + int_pay < mo_payment){
      mo_payment <- cur_balance + int_pay
    }
  
    prin_pay <- mo_payment - int_pay
    
    new_balance <- cur_balance - prin_pay
    
    df <- data.frame(month = month, int_pay = int_pay, prin_pay = prin_pay, 
                     total_pay = mo_payment, balance = new_balance)
    
    df_full <- rbind(df_full, df)
    
    month <- month + 1
    
  }
  
  df_full
  
}

calculate_schedule <- function(loan_info_df, loan_name, payoff_months = NA, mo_payment = NA){
  if(all(is.na(c(payoff_months, mo_payment)))) stop("Must specify payoff months or monthly payment")
  else if(sum(is.na(c(payoff_months, mo_payment))) !=1) stop("Can't specify both payoff months and monthly payment")
  
  balance <- loan_info_df %>%
    filter(name == loan_name) %>%
    pull(bal)
  
  int = loan_info_df %>%
    filter(name == loan_name) %>%
    pull(int)
  
  if(!is.na(mo_payment)){
    payment_sched_mo_pay(balance = balance, int = int, mo_payment = mo_payment)
  } else {
    payment_sched_months(balance = balance, int = int, payoff_months = payoff_months)
  }
}


conduct_schedule_analysis <- function(loan_info_list, max_mo_pay, int_tiebreak){
  
  loan_info_df <- purrr::map_df(loan_info_list, function(sublist){ 
    data.frame(sublist, stringsAsFactors = F)
  })
  
  months_til_payoff <- floor(difftime(payoff_date, Sys.time(), units = "days") %>% 
                               as.numeric/30.42)
  
  if(int_tiebreak == "higher balance"){
    loan_df_sort <- loan_info_df %>%
      arrange(desc(int), desc(bal))
  } else if(int_tiebreak == "lower balance"){
    loan_df_sort <- loan_info_df %>%
      arrange(desc(int), bal)
  } else {
    loan_df_sort <- loan_info_df %>%
      arrange(desc(int), desc(bal))
    warning("No proper tiebreaker defined")
  }

  orig_order <- loan_df_sort$name
  
  all_extra_pay_sched <- NULL
  all_others_pay_sched <- NULL
  
  for(i in 1:nrow(loan_df_sort)){
    min_pay <- sum(loan_df_sort$min_pay)
    extra_pay_loan <- max_mo_pay - min_pay
    min_pay_loan <- loan_df_sort$min_pay[1]
  
    loan_pay_name <- loan_df_sort[1, "name"]
    
    extra_pay_sched <- calculate_schedule(loan_df_sort, 
                                          loan_name = loan_pay_name, 
                                          mo_payment = extra_pay_loan + min_pay_loan) %>%
      mutate(name = loan_pay_name, 
             extra_pay = extra_pay_loan)
    
    all_extra_pay_sched <- bind_rows(all_extra_pay_sched, extra_pay_sched)
    
    payoff_mos <- max(extra_pay_sched$month)
    
    if(nrow(loan_df_sort) > 1){
      others <- loan_df_sort[2:nrow(loan_df_sort), "name"]
      others_pay_sched <- purrr::map_df(others, function(loan_name){
        min_pay <- loan_df_sort[loan_df_sort$name == loan_name, "min_pay"]
        sched <- calculate_schedule(loan_df_sort, 
                                    loan_name = loan_name, 
                                    mo_payment = min_pay)
        sched <- sched[sched$month %in% 1:payoff_mos, ]
        sched$name = loan_name
        sched
      })
      
      all_others_pay_sched <- bind_rows(all_others_pay_sched, others_pay_sched)
      
      others_summ <- others_pay_sched %>%
        group_by(name) %>%
        summarize(final_bal = min(balance))
      
      loan_df_sort <- loan_df_sort[-1, ]
      
      loan_df_sort <- merge(loan_df_sort, others_summ, by = "name") %>%
        mutate(bal = final_bal) %>%
        arrange(desc(int), desc(bal)) %>%
        select(name, bal, int, min_pay)
    }
  }
  
  extra_pay_df <- all_extra_pay_sched %>%
    filter(month != 0) %>%
    mutate(month_count = 1:n())
  
  if(length(orig_order)>1){
    min_pay_df <- all_others_pay_sched %>%
      filter(month != 0) %>%
      group_by(name) %>%
      arrange(name, desc(balance)) %>%
      mutate(month_count = 1:n())
    
    all_payments <- bind_rows(extra_pay_df, min_pay_df) %>%
      mutate(name = factor(name, levels = orig_order)) 
    
  } else {
    all_payments <- extra_pay_df %>%
      mutate(name = factor(name, levels = orig_order)) 
  }
  
  print(sprintf("Total interest to be paid = $%0.2f", sum(all_payments$int_pay)))

}


plot_schedules <- function(payment_sched){
  p1 <- ggplot(payment_sched, aes(x = month_count, y = balance)) +
    geom_line() + 
    facet_wrap(~name, ncol = 1) 
  
  plot_dat <- payment_sched %>%
    rename(`interest payment` = int_pay, 
           `principle payment` = prin_pay) %>%
    gather(comp, value, `interest payment`, `principle payment`)
  
  p2 <- ggplot(payment_sched, aes(x = month_count, y = value, col = comp)) +
    geom_line() +
    facet_wrap(~name, ncol = 1) 
  
  gridExtra::grid.arrange(p1, p2, ncol=2)
}

calculate_mo_payments <- function(payment_sched){
  payment_sched %>%
    group_by(month_count) %>%
    summarize(monthly_payment = sum(total_pay))
  
}
