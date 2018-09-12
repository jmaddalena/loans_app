conduct_schedule_analysis <- function(loan_info_list, max_mo_pay, int_tiebreak = "higher balance"){
  
  loan_info_df <- purrr::map_df(loan_info_list, function(sublist){ 
    data.frame(sublist, stringsAsFactors = F)
  })
  
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
  
  #for(i in 1:nrow(loan_df_sort)){
  
  loans_remain <- loan_df_sort$name
  
  while(length(loans_remain) > 1){
    
    print(loans_remain)
    
    #i <- loan_df_sort[1]
    
    print("1"); print(loan_df_sort)
    
    #print(sprintf("analyzing loan %s", orig_order[i]))
    new_order <- loan_df_sort$name
    
    min_pay <- sum(loan_df_sort$min_pay)
    #print(sprintf("minimum payments on all loans: $%0.2f", min_pay))
    
    if(!is.na(max_mo_pay)){
      extra_pay_loan <- round(max_mo_pay - min_pay, 2)
      
      if(extra_pay_loan < 0) stop("you have entered a maximum monthly payment that is lower than the total minimum loan payments")
    } else {
      extra_pay_loan <- 0  
    }
    
    min_pay_loan <- loan_df_sort$min_pay[1]
    # print(sprintf("minimum payments on this loan: $%0.2f", min_pay_loan))
    
    loan_pay_name <- loan_df_sort[1, "name"]
    
    # print("calculating schedule")
    
    extra_pay_sched <- calculate_schedule(loan_df_sort, 
                                          loan_name = loan_pay_name, 
                                          mo_payment = extra_pay_loan + min_pay_loan) %>%
      mutate(name = loan_pay_name, 
             extra_pay = extra_pay_loan,
             df_orig = "extra_pay")
    
    #print(head(extra_pay_sched))
    
    last_extra_pay <- extra_pay_sched$total_pay[nrow(extra_pay_sched)] - min_pay_loan
    
    #print(sprintf("Last extra payment: $%0.2f", last_extra_pay))
    
    if(last_extra_pay < 0) last_extra_pay <- 0
    extra_pay_sched$extra_pay[nrow(extra_pay_sched)] = last_extra_pay
    
    all_extra_pay_sched <- bind_rows(all_extra_pay_sched, extra_pay_sched)
    
    payoff_mos <- max(extra_pay_sched$month)
    
    #print(sprintf("payoff_mos: %s", payoff_mos))
    
    leftover_last_pay <- min_pay_loan + extra_pay_loan - tail(all_extra_pay_sched$total_pay, 1)
    
    #print(sprintf("left_over_last_pay: %s", leftover_last_pay))
    
    if(nrow(loan_df_sort) > 1){
      others_df <- loan_df_sort[2:nrow(loan_df_sort), ]
      
      others <- others_df$name
      
      print(sprintf("now calculating concurrent payments for %s", paste(others, collapse = ", ")))
      
      if(sum(others_df$bal > 0)){
        
        others_pay_sched <- purrr::map_df(others, function(loan_name){
          
          print(loan_name)
          
          loan_row <- loan_df_sort[loan_df_sort$name == loan_name, , drop = F]
          
          print(loan_row)
          
          if(loan_row$bal == 0) return(NULL)
          
          min_pay <- loan_row$min_pay
          
          #print(sprintf("min pay for loan %s: %s", loan_name, min_pay))
          
          sched <- calculate_schedule(loan_df_sort, 
                                      loan_name = loan_name, 
                                      mo_payment = min_pay)
          
          #print(sprintf("min pay for loan %s: %s", loan_name, min_pay))
          
          sched <- sched[sched$month %in% 1:payoff_mos, ]
          sched$name <- loan_name
          sched$extra_pay <- 0
          sched$df_orig <- "other"
          sched
        })
        
        ## condition if leftover overpayment necessary 
        if(!is.na(max_mo_pay) & max(extra_pay_sched$month) %in% others_pay_sched$month){ #unverified
          
          ### I need to first determine if the balance is 0, no more payments needed
          
          #print("calculating leftover extra payment")
          
          print(sprintf("next loan: %s",  loan_df_sort[2, "name"] ))
          print(sprintf("max month: %s", max(others_pay_sched$month)))
          print(sprintf("max month (alt): %s", max(extra_pay_sched$month)))
          
          print(extra_pay_sched)
          print(others_pay_sched)
          
          # choosing which loan to apply leftover overpayment to 
          
          loans_at_start <- loan_df_sort$name 
          
          others_pay_still_around <- others_pay_sched %>%
            filter(month == max(extra_pay_sched$month)) %>%
            pull(name) %>% 
            unique()
          
          loans_remain <- loans_at_start[loans_at_start %in% others_pay_still_around]
          
          loan_overpay <- loans_remain[1]
          
          which_row_leftover <- which(others_pay_sched$name == loan_overpay &
                                        others_pay_sched$month == max(others_pay_sched$month))
          
          #print(sprintf("leftover row from others_pay: %s", which_row_leftover))
          
          #print(others_pay_sched[which_row_leftover,])
          
          print(table(others_pay_sched$name))
          print(which_row_leftover)
          
          orig_prin <- others_pay_sched$prin_pay[which_row_leftover]
          last_balance <- others_pay_sched$start_balance[which_row_leftover]
          int_pay <- others_pay_sched$int_pay[which_row_leftover]
          
          ## temporary fix, if others_pay_sched is only one row to cover small last payment
          #if(length(last_balance) == 0) last_balance <- others_pay_sched$total_pay[which_row_leftover]
          
          print(leftover_last_pay)
          print(orig_prin)
          print(int_pay)
          print(last_balance)
          
          if(leftover_last_pay + orig_prin + int_pay > last_balance)
            leftover_last_pay <- max(0, last_balance - orig_prin - int_pay)
          
          others_pay_sched$extra_pay[which_row_leftover] <- leftover_last_pay
          others_pay_sched$prin_pay[which_row_leftover] <- leftover_last_pay + orig_prin
          others_pay_sched$total_pay[which_row_leftover] <- leftover_last_pay + orig_prin + int_pay
          
          others_pay_sched$balance[which_row_leftover] <- last_balance - others_pay_sched$total_pay[which_row_leftover]
          
        } 
        
        others_months <- others_pay_sched %>%
          group_by(name) %>%
          summarize(max_mo = max(month))
        
        # if any concurrent loans paid off during extra pay, add their min payments to overpayment
        if(any(others_months < extra_pay_sched$month)){
          print(others_months)
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
        
      } #  if(sum(others_df$bal > 0))
      
      loan_df_sort <- loan_df_sort %>% filter(bal > 0) ## unverified
      loans_remain <- loan_df_sort$name
      
    } #  if(nrow(loan_df_sort) > 1)
  } # while(length(loans_remain) > 1)
  
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
  
  sched <- all_payments %>%
    arrange(name, month_count)
  
  sched
  
}