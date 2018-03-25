payment_sched_months <- function(balance, int, payoff_months){
  
  discount_factor = ((1 + int/12)^payoff_months - 1)/(int/12*(1 + int/12)^payoff_months)
  mo_payment <- balance/discount_factor
  
  df_full <- data.frame(month = 0, start_balance = NA, int_pay = 0, prin_pay = 0, total_pay = 0, balance = balance)
  
  for(month in 1:payoff_months){
    
    start_balance <- df_full$balance[df_full$month == month - 1]
    
    int_pay <- start_balance*int/12
    prin_pay <- mo_payment - int_pay
    
    print(sprintf("new_balance = %s", paste(new_balance, collapse = ", ")))
    
    new_balance <- start_balance - prin_pay
    
    df <- data.frame(month = month, start_balance = start_balance, int_pay = int_pay, prin_pay = prin_pay, 
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
  
  while(new_balance > 0.0001){

    start_balance <- df_full$balance[df_full$month == month - 1]

    int_pay <- start_balance*int/12
    
    if(start_balance + int_pay < mo_payment){
      mo_payment <- start_balance + int_pay
    }
  
    prin_pay <- mo_payment - int_pay
    
    new_balance <- start_balance - prin_pay
    
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
  
  if(length(balance) > 1) stop("Loans have same names, maybe?")
  
  int = loan_info_df %>%
    filter(name == loan_name) %>%
    pull(int)
  
  if(!is.na(mo_payment)){
    payment_sched_mo_pay(balance = balance, int = int, mo_payment = mo_payment)
  } else {
    payment_sched_months(balance = balance, int = int, payoff_months = payoff_months)
  }
}


conduct_schedule_analysis <- function(loan_info_list, max_mo_pay, int_tiebreak = "higher balance"){

  loan_info_df <- purrr::map_df(loan_info_list, function(sublist){ 
    data.frame(sublist, stringsAsFactors = F)
  })
  
  print(loan_info_df)
  
  #print(sprintf("minimum monthly payments: %0.2f", sum(loan_info_df$min_pay)))
  
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
    new_order <- loan_df_sort$name
    
    min_pay <- sum(loan_df_sort$min_pay)
    
    if(!is.na(max_mo_pay)){
      extra_pay_loan <- round(max_mo_pay - min_pay, 2)

      if(extra_pay_loan < 0) stop("you have entered a maximum monthly payment that is lower than the total minimum loan payments")
    } else {
      extra_pay_loan <- 0  
    }
    
    min_pay_loan <- loan_df_sort$min_pay[1]
    
    loan_pay_name <- loan_df_sort[1, "name"]
    
    extra_pay_sched <- calculate_schedule(loan_df_sort, 
                                          loan_name = loan_pay_name, 
                                          mo_payment = extra_pay_loan + min_pay_loan) %>%
      mutate(name = loan_pay_name, 
             extra_pay = extra_pay_loan,
             df_orig = "extra_pay")
    
    last_extra_pay <- extra_pay_sched$total_pay[nrow(extra_pay_sched)] - min_pay_loan
    
    #print("last_extra_pay") ; print(last_extra_pay)
    
    if(last_extra_pay < 0) last_extra_pay <- 0
    extra_pay_sched$extra_pay[nrow(extra_pay_sched)] = last_extra_pay
    
    all_extra_pay_sched <- bind_rows(all_extra_pay_sched, extra_pay_sched)
    
    payoff_mos <- max(extra_pay_sched$month)
  
    leftover_last_pay <- min_pay_loan + extra_pay_loan - tail(all_extra_pay_sched$total_pay, 1)
    
   # print("left_over_last_pay") ; print(leftover_last_pay)
        
    if(nrow(loan_df_sort) > 1){
      others <- loan_df_sort[2:nrow(loan_df_sort), "name"]
      others_pay_sched <- purrr::map_df(others, function(loan_name){
        min_pay <- loan_df_sort[loan_df_sort$name == loan_name, "min_pay"]
        sched <- calculate_schedule(loan_df_sort, 
                                    loan_name = loan_name, 
                                    mo_payment = min_pay)
        sched <- sched[sched$month %in% 1:payoff_mos, ]
        sched$name <- loan_name
        sched$extra_pay <- 0
        sched$df_orig <- "other"
        sched
      })
      
      if(!is.na(max_mo_pay)){
      
        # row_grab <- 0
        # 
        # next_month <- others_pay_sched %>%
        #   filter(name == orig_order[i + row_grab + 1],
        #          month >= max(extra_pay_sched$month))
        # 
        # while(nrow(next_month) == 0){
        #   
        #   print("row_grab") ; print(row_grab)
        #   
        #   next_month <- others_pay_sched %>%
        #     filter(name == orig_order[i + row_grab + 1],
        #            month >= max(extra_pay_sched$month))
        #   
        #   print(nrow(next_month))
        #   
        #   row_grab <- row_grab + 1
        # }        
        # 
        # print(orig_order[i + row_grab + 1])
        # print(max(others_pay_sched$month))
        # 
        # print("row_grab use") ; print(i + row_grab + 1)
        # 
        # print(orig_order[i + row_grab + 1])
        # 
        which_row_leftover <- which(others_pay_sched$name == orig_order[i + 1] &
                         others_pay_sched$month == max(others_pay_sched$month))
        # 
        # if(length(which_row_leftover) == 0) return(others_pay_sched)
        # 
        # 
        # print(which_row_leftover)
        # 
        #print("before add leftover") ; print(others_pay_sched[which_row_leftover, ])
        
        orig_prin <- others_pay_sched$prin_pay[which_row_leftover]
        last_balance <- others_pay_sched$balance[which_row_leftover - 1]
        int_pay <- others_pay_sched$int_pay[which_row_leftover]
        
       # print("sum") ; print(leftover_last_pay + orig_prin + int_pay)
        
      #  print("orig_prin") ; print(orig_prin)
      #  print('int_pay') ; print(int_pay)
        
        if(leftover_last_pay + orig_prin + int_pay > last_balance)
          leftover_last_pay <- max(0, last_balance - orig_prin - int_pay)
        
        others_pay_sched$extra_pay[which_row_leftover] <- leftover_last_pay
        others_pay_sched$prin_pay[which_row_leftover] <- leftover_last_pay + orig_prin
        others_pay_sched$total_pay[which_row_leftover] <- leftover_last_pay + orig_prin + int_pay
        
        #print('total pay') ; print(others_pay_sched$total_pay[which_row_leftover])
        
        others_pay_sched$balance[which_row_leftover] <- last_balance - others_pay_sched$total_pay[which_row_leftover]
        
        #print("after add leftover") ; print(others_pay_sched[which_row_leftover,])
      } 
      
      
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
  
  #print(sprintf("Total interest to be paid = $%0.2f", sum(all_payments$int_pay)))

  sched <- all_payments %>%
    arrange(name, month_count)
  
  sched
  
}

# HA!
user_friendly_schedule <- function(payment_sched){
   payment_sched %>%
    select(month_count, name, total_pay, int_pay)
}


plot_schedules <- function(payment_sched){
  p1 <- ggplot(payment_sched, aes(x = month_count, y = balance)) +
    geom_line() + 
    facet_wrap(~name, ncol = 1) 
  
  plot_dat <- payment_sched %>%
    rename(`interest payment` = int_pay, 
           `principle payment` = prin_pay, 
           `total payment` = total_pay) %>%
    gather(comp, value, `interest payment`, `principle payment`, `total payment`) %>%
    mutate(comp = factor(comp, levels = c("interest payment", "principle payment", "total payment")))
  
  p2 <- ggplot(plot_dat, aes(x = month_count, y = value, col = comp)) +
    geom_line() +
    facet_wrap(~name, ncol = 1) 
  
  gridExtra::grid.arrange(p1, p2, ncol=2, widths = c(1, 1.4))
}

plot_mo_payments <- function(payment_sched){

  min_pay <- payment_sched %>% 
    mutate(min = total_pay - extra_pay) %>% 
    group_by(month_count) %>% 
    summarize(min_pay_mo = sum(min))
  
  print(min_pay)
  
  cols <- c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
            "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq <- seq(1, 8, by = floor(8/length(unique(payment_sched$name))))
  
  cols_use <- cols[cols_use_seq]
  
  ggplot(payment_sched, aes(x = month_count, y = total_pay)) +
    geom_bar(stat = "identity", color = "white", aes(fill = name)) +
    geom_line(data = min_pay, aes(y = min_pay_mo, color = "Minimum monthly\n payments"), size = 1) +
    ylab("Total Monthly Payment") +
    labs(x = "Month from now", y = "Monthly Payment ($)") +
    scale_fill_manual("", values = cols_use) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(12, max(payment_sched$month_count), by = 12))
  
}


get_payoff_options <- function(loan_info_list){ 

  min_pay <- lapply(loan_info_list, function(sub_list){
    sub_list[["min_pay"]]
  }) %>% unlist %>% sum

  start_50 <- min_pay - min_pay %% 50 + 50

  mo_pay_try <- c(NA, min_pay, seq(start_50, ceiling(start_50+500), by = 50))
  
  all_payoff_options <- purrr::map_df(mo_pay_try, function(mo_pay){
    
    sched_df <- conduct_schedule_analysis(loan_info_list = loan_info_list, 
                              max_mo_pay = mo_pay, 
                              int_tiebreak = "higher balance")
    
    if(is.na(mo_pay)) mo_pay <- NA

    sched_df %>%
      summarize(`Months to pay off` = max(month_count),
                `Total interest paid` = sum(int_pay)) %>%
      mutate(mo_pay = mo_pay)
  })
  
}

plot_payoff_options <- function(payoff_options){
  
  all_gather <- payoff_options %>% 
    filter(!is.na(mo_pay)) %>%
    gather(group, value, -mo_pay) %>%
    mutate(value_lab = case_when(group == "Months to pay off" ~ as.character(value),
                                 TRUE ~ sprintf("$%0.2f", value)))
  
  start_x <- payoff_options$mo_pay[2]
  put_text <- start_x + .5*(1000 - start_x)
  
  baremin_gather <- payoff_options %>%
    filter(is.na(mo_pay)) %>%
    gather(group, value, -mo_pay) %>%
    mutate(value_lab = round(value),
           padding = .025*value,
           nudge_x = .05*(1000 - start_x))
  
  ggplot(all_gather, aes(x = mo_pay, y = value)) + 
    facet_wrap(~group, scales = "free_y", ncol = 2) +
    geom_line() +
    geom_point() +
   # geom_text(aes(label = value_lab, vjust = 0, hjust = .5), size = 4) +
    geom_text(data = baremin_gather, position = "dodge",
              aes(x = put_text, y = value + padding, 
                  label = "Making no overpayments"), col = "red") +
    geom_hline(data = baremin_gather, aes(yintercept = value), col = "red") +
    geom_hline(aes(yintercept = 0), col = NA) +
    labs(x = "Total monthly payments", y = "") +
    lims(x = c(start_x, 1000))
}




