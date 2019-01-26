calc_mo_pay = function(start_balance, int, payment){
  
  int_pay = start_balance*int/12
  prin_pay = payment - int_pay
  total_pay = int_pay + prin_pay
  
  new_balance = start_balance - prin_pay
  
  if(new_balance < 0){
    payment = start_balance + int_pay
    new_balance = 0
  }
  
  data.frame("payment" = payment, "old_balance" = start_balance, 
             "new_balance" = new_balance, "int_pay" = int_pay)
  
}

conduct_schedule_analysis = function(loan_info_list, max_mo_pay = NA, 
                                     int_tiebreak = "higher balance"){
  
  loan_info_df = purrr::map_df(loan_info_list, function(sublist){ 
    data.frame(sublist, stringsAsFactors = F)
  })
  
  if(int_tiebreak == "higher balance"){
    loan_df_sort = loan_info_df %>%
      arrange(desc(int), desc(balance))
  } else if(int_tiebreak == "lower balance"){
    loan_df_sort = loan_info_df %>%
      arrange(desc(int), balance)
  } else {
    loan_df_sort = loan_info_df %>%
      arrange(desc(int), desc(balance))
    warning("No proper tiebreaker defined")
  }
  
  if(!is.na(max_mo_pay) && max_mo_pay < sum(loan_df_sort$min_pay)){
    stop("you have entered a maximum monthly payment that is ",
         "lower than the total minimum loan payments")
  }
  
  orig_order <- loan_df_sort$name
  
  month = 1
  
  total_balance = sum(loan_df_sort$balance)
  
  all_payment_df = NULL
  
  while(total_balance > 0){
    
    if(!is.na(max_mo_pay)){
      overpay_loan = loan_df_sort$name[1]
      if(nrow(loan_df_sort) > 1){
        non_overpay_loans = loan_df_sort$name[-1]
        next_overpay = loan_df_sort$name[2]
      } else {
        non_overpay_loans = NULL
        next_overpay = NULL
      }
    } else {
      overpay_loan = NULL
      next_overpay = NULL
      non_overpay_loans = loan_df_sort$name
    }
    
    if(length(non_overpay_loans)>0){
      min_pay_df = purrr::map_df(non_overpay_loans, function(loan_name){
        
        loan_row = loan_df_sort %>% filter(name == loan_name)
        
        calc_mo_pay(start_balance = loan_row$balance, 
                    int = loan_row$int, payment = loan_row$min_pay) %>%
          mutate(name = loan_name, type = "min_payment",
                 min_pay = min(loan_row$min_pay, payment),
                 month = month)
      })
      
    } else {
      
      min_pay_df <- NULL
      
    }
    
    total_min_pay = sum(min_pay_df$payment)
    
    redirect_overpay = NULL
    
    if(length(overpay_loan)>0){
      overpay = max_mo_pay - total_min_pay 
      
      if(overpay < 0){
        overpay_df = NULL
      } else {
        loan_row = loan_df_sort %>% filter(name == overpay_loan)
        overpay_df = calc_mo_pay(start_balance = loan_row$balance, 
                                 int = loan_row$int, payment = overpay) %>%
          mutate(month = month,
                 name = overpay_loan, 
                 min_pay = min(loan_row$min_pay, payment),
                 type = "overpay")
        
        if(overpay_df$payment < overpay){
          redirect_overpay = overpay - overpay_df$payment
        }
      }
      
      payment_df = bind_rows(min_pay_df, overpay_df) %>% 
        mutate(month = month)
      
      # if leftover extra pay from overpay loan 
      if(length(redirect_overpay)>0 & length(next_overpay)>0){
        orig_payment = payment_df %>%
          filter(name == next_overpay) %>%
          pull(payment)
        
        
        loan_row = loan_df_sort %>% filter(name == next_overpay)
        
        redo_pay = calc_mo_pay(start_balance = loan_row$balance, 
                               int = loan_row$int, payment = orig_payment + redirect_overpay) %>%
          mutate(name = next_overpay, type = "min_payment",
                 min_pay = min(orig_payment + redirect_overpay, loan_row$min_pay),
                 month = month)
        
        payment_df = payment_df %>%
          filter(name != next_overpay) %>%
          bind_rows(redo_pay)
        
      }
      
    } else {
      payment_df = min_pay_df  
    }
    
    all_payment_df = bind_rows(all_payment_df, payment_df)
    
    update_loan_df = left_join(loan_df_sort, payment_df %>% select(name, new_balance), by = "name")
    
    loan_df_sort = update_loan_df %>% 
      mutate(balance = new_balance) %>%
      select(-new_balance) %>%
      filter(balance > 0)
    
    total_balance = sum(loan_df_sort$balance)
    
    month = month + 1
    
  }
  
  all_payment_df %>%
    mutate(name = factor(name, levels = orig_order)) 
  
}



get_payoff_options <- function(loan_info_list){ 
  
  min_pay <- lapply(loan_info_list, function(sub_list){
    sub_list[["min_pay"]]
  }) %>% unlist %>% sum
  
  start_50 <- min_pay - min_pay %% 50 + 50
  
  if(abs(min_pay - start_50) < 20) start_50 <- start_50 + 50
  
  mo_pay_try <- c(NA, min_pay, seq(start_50, ceiling(start_50+500), by = 50))
  
  all_payoff_options <- purrr::map_df(mo_pay_try, function(mo_pay){
    
    sched_df <- conduct_schedule_analysis(loan_info_list = loan_info_list, 
                                          max_mo_pay = mo_pay, 
                                          int_tiebreak = "higher balance")
    
    sched_df %>%
      summarize(`Months to pay off` = max(month),
                `Total interest to be paid` = sum(int_pay)) %>%
      mutate(mo_pay = mo_pay)
  })
  
}

plot_payoff_options <- function(payoff_options){
  
  maxval <- max(payoff_options$mo_pay, na.rm = T)
  
  curr_pay <- payoff_options$mo_pay[2]
  
  all_gather <- payoff_options %>% 
    filter(!is.na(mo_pay)) %>%
    gather(group, value, -mo_pay) %>%
    mutate(value_lab = case_when(group == "Months to pay off" ~ as.character(value),
                                 TRUE ~ sprintf("$%0.2f", value)))
  
  start_x <- payoff_options$mo_pay[2]
  put_text <- start_x + .5*(maxval - start_x)
  
  baremin_gather <- payoff_options %>%
    filter(is.na(mo_pay)) %>%
    gather(group, value, -mo_pay) %>%
    mutate(value_lab = round(value),
           padding = .08*value,
           nudge_x = .05*(maxval - start_x))
  
  ggplot(all_gather, aes(x = mo_pay, y = value)) + 
    facet_wrap(~group, scales = "free_y", ncol = 2) +
    geom_line() +
    geom_point() +
    geom_text(data = baremin_gather, position = "dodge",
              aes(x = put_text, y = value + padding, 
                  label = "Making no overpayments"), size = 5, col = "red") +
    geom_text(data = baremin_gather, position = "dodge",
              aes(x = put_text, y = value + padding/3), 
                  label = sprintf("Currently $%0.2f", curr_pay), size = 4, col = "red") +
    geom_hline(data = baremin_gather, aes(yintercept = value), col = "red") +
    geom_hline(aes(yintercept = 0), col = NA) +
    labs(title = "Select a point to see payment plan for selected monthly payment",
         x = "Total monthly payments", y = "") +
    scale_x_continuous(labels = scales::dollar_format(prefix="$"), lim = c(start_x, maxval)) +
  #  scale_y_continuous(breaks = all_gather$value, labels = all_gather$value_lab) + 
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
          strip.text.x = element_text(size = 15, face = "bold"), 
          title = element_text(size = 15))
}

plot_mo_payments = function(payment_sched){
  
  min_pay = payment_sched %>% 
    group_by(month) %>% 
    summarize(min_pay_mo = sum(min_pay))
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(payment_sched$name))))
  
  cols_use = cols[cols_use_seq]
  
  ggplot(payment_sched, aes(x = month, y = payment)) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6) +
    geom_line(data = min_pay, aes(y = min_pay_mo, color = "Minimum monthly\n payments"), size = 1) +
    labs(y = "Total Monthly Payment", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = seq(12, max(payment_sched$month), by = 12)) +
    ggthemes::scale_color_gdocs() +
    scale_color_manual("", values = "red") +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$"))
  
}

plot_balance_over_time <- function(payment_sched){
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(payment_sched$name))))
  
  cols_use = cols[cols_use_seq]
  
  ggplot(payment_sched, aes(x = month, y = new_balance, fill = name)) + 
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6) +
    scale_fill_manual("", values = cols_use) +
    labs(y = "Total loan balance") +
    ggthemes::scale_color_gdocs() +
    scale_x_continuous("Month from now", breaks = seq(12, max(payment_sched$month), by = 12)) +
    theme_linedraw() +
    
    scale_y_continuous(labels = scales::dollar_format(prefix="$"))
}



