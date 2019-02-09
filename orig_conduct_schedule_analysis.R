
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
    
    # if not overpaying, determine which loan to overpay on,
    #    which is next in line
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
    
    # for all 'nonoverpay' loans, calculate their mo payments
    if(length(non_overpay_loans) > 0){
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
    
    # how much of your allotment is going to minimum payments?
    total_min_pay = sum(min_pay_df$payment)
    
    redirect_overpay = NULL
    
    # append overpayment row to min_pay_df 
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
          # how much is left after making the maximum overpayment?
          redirect_overpay = overpay - overpay_df$payment
        }
      }
      
      payment_df = bind_rows(min_pay_df, overpay_df) %>% 
        mutate(month = month)
      
      # if any leftover extra pay from overpay loan 
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
