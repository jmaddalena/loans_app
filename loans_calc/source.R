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
                                     int_tiebreak = "lower balance"){
  
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
       # next_overpay = loan_df_sort$name[2]
        next_overpays = loan_df_sort$name[2:nrow(loan_df_sort)]
      } else {
        non_overpay_loans = NULL
        next_overpays = NULL
      }
    } else {
      overpay_loan = NULL
      next_overpays = NULL
      non_overpay_loans = loan_df_sort$name
    }
    
    # for all 'nonoverpay' loans, calculate their mo payments
    if(length(non_overpay_loans) > 0){
      min_pay_df = purrr::map_df(non_overpay_loans, function(loan_name){
        
        loan_row = loan_df_sort %>% filter(name == loan_name)
        
        calc_mo_pay(start_balance = loan_row$balance, 
                    int = loan_row$int/100, payment = loan_row$min_pay) %>%
          mutate(name = loan_name, type = "min_payment",
                 min_pay = min(loan_row$min_pay, payment),
                 month = month)
      })
      
    } else {
      min_pay_df <- NULL
    }
    
    # how much of your allotment is going to minimum payments?
    total_min_pay = sum(min_pay_df$payment)
    
    redirect_overpay = 0
    
    # append overpayment row to min_pay_df 
    if(length(overpay_loan)>0){
      # overpay is desired total payment on loan being overpaid (confusing, I know)
      overpay = max_mo_pay - total_min_pay 
      
      if(overpay < 0){
        overpay_df = NULL
      } else {
        loan_row = loan_df_sort %>% filter(name == overpay_loan)
        overpay_df = calc_mo_pay(start_balance = loan_row$balance, 
                                 int = loan_row$int/100, payment = overpay) %>%
          mutate(month = month,
                 name = overpay_loan, 
                 min_pay = min(loan_row$min_pay, payment),
                 type = "overpay")
        
        if(overpay_df$payment < overpay){
          # how much is left after making the maximum overpayment?
          # redirect_overpay is desired EXTRA payment for next loan (diff from overpay)
          redirect_overpay = overpay - overpay_df$payment
        }
      }
      
      payment_df = bind_rows(min_pay_df, overpay_df) %>% 
        mutate(month = month)
      
      count <- 1
      
      # if any leftover extra pay from overpay loan 
      while(!is.null(next_overpays) &
            length(redirect_overpay)>0 & 
            count <= length(next_overpays) &
            sum(payment_df$new_balance) > max_mo_pay){
        
        next_overpay <- next_overpays[count]
        
        orig_payment = payment_df %>%
          filter(name == next_overpay) %>%
          pull(payment)
      
        loan_row = loan_df_sort %>% filter(name == next_overpay)
      
        redo_pay = calc_mo_pay(start_balance = loan_row$balance, 
                               int = loan_row$int/100, payment = orig_payment + redirect_overpay) %>%
          mutate(name = next_overpay, type = "min_pay",
                 min_pay = min(payment, loan_row$min_pay),
                 month = month)
        
        payment_df = payment_df %>%
          filter(name != next_overpay) %>%
          bind_rows(redo_pay)
        
        total_pay = sum(payment_df$payment)
        redirect_overpay = max_mo_pay - total_pay
        
        count <- count + 1

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


get_all_schedules <- function(loan_list){
  
  min_pay <- lapply(loan_list, function(sub_list){
    sub_list[["min_pay"]]
  }) %>% unlist %>% sum

  start_50 <- min_pay - min_pay %% 50 + 50
  if(abs(min_pay - start_50) < 20) start_50 <- start_50 + 50
  
  mo_pay_try <- c(NA, min_pay, seq(start_50, ceiling(start_50+500), by = 50))

  n <- length(mo_pay_try)
  
  all_sched_df <- data.frame()
  
  factor_levels <- c()      
  
  for(mo_pay in mo_pay_try){
    
    lab <- ifelse(is.na(mo_pay), "Minimum", sprintf("$%0.2f", mo_pay))
    factor_levels <- c(factor_levels, lab)
    
    mo_pay_display <- ifelse(is.na(mo_pay), min_pay, mo_pay)
    
    sched_df <- conduct_schedule_analysis(loan_info_list = loan_list, 
                                          max_mo_pay = mo_pay, 
                                          int_tiebreak = "lower balance") %>%
      mutate(mo_pay = mo_pay,
             mo_pay_lab = lab)
    
    all_sched_df <- bind_rows(all_sched_df, sched_df)
  }
  
  all_sched_df$mo_pay_lab <- factor(all_sched_df$mo_pay_lab, levels = factor_levels)
  
  all_sched_df
  
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
  
  max_y <- baremin_gather %>% 
    filter(group == "Months to pay off") %>%
    mutate(max_y = value + padding) %>%
    pull(max_y)

  if(max_y > 100){ breaks <- seq(0, max_y, by = 20)
  } else breaks <- seq(0, max_y, by = 10)
  
  x_vals <- unique(payoff_options$mo_pay)
  x_vals <- x_vals[-which(is.na(x_vals))]
  
  p1_str <- "Months to pay off"
  p1 <- ggplot(all_gather %>% filter(group == p1_str), 
               aes(x = mo_pay, y = value)) + 
    geom_line() +
    geom_point() +
    geom_text(data = baremin_gather %>% filter(group == p1_str), position = "dodge",
              aes(x = put_text, y = value + padding, 
                  label = "Making no overpayments"), size = 5, col = "red") +
    geom_text(data = baremin_gather %>% filter(group == p1_str), position = "dodge",
              aes(x = put_text, y = value + padding/3), 
              label = sprintf("(Current minimum payments = $%0.2f)", curr_pay), size = 4, col = "red") +
    geom_hline(data = baremin_gather %>% filter(group == p1_str), aes(yintercept = value), col = "red") +
    geom_hline(aes(yintercept = 0), col = NA) +
    labs(title = "Months to pay off all loans", x = "Monthly payments", y = "") +
    scale_x_continuous(breaks = x_vals, minor_breaks = x_vals[1], labels = scales::dollar_format(prefix="$"), lim = c(start_x, maxval)) +
    scale_y_continuous(breaks = breaks) +
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
          strip.text.x = element_text(size = 15, face = "bold"), 
          title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  
  max_y <- baremin_gather %>% 
    filter(group == "Total interest to be paid") %>%
    mutate(max_y = value + padding) %>%
    pull(max_y)
  
  if(max_y > 300000){ breaks <- seq(0, max_y, by = 50000)
  } else if(max_y > 100000){ breaks <- seq(0, max_y, by = 10000)
  } else if(max_y > 10000){ breaks <- seq(0, max_y, by = 2000)
  } else if(max_y > 3000){ breaks <- seq(0, max_y, by = 500)
  } else breaks <- seq(0, max_y, by = 200)
  
  p2_str <- "Total interest to be paid"
  p2 <- ggplot(all_gather %>% filter(group == p2_str), 
               aes(x = mo_pay, y = value)) + 
    geom_line() +
    geom_point() +
    geom_text(data = baremin_gather %>% filter(group == p2_str), position = "dodge",
              aes(x = put_text, y = value + padding, 
                  label = "Making no overpayments"), size = 5, col = "red") +
    geom_text(data = baremin_gather %>% filter(group == p2_str), position = "dodge",
              aes(x = put_text, y = value + padding/3), 
              label = sprintf("(Current minimum payments = $%0.2f)", curr_pay), size = 4, col = "red") +
    geom_hline(data = baremin_gather %>% filter(group == p2_str), aes(yintercept = value), col = "red") +
    geom_hline(aes(yintercept = 0), col = NA) +
    labs(title = "Total interest to be paid", x = "Monthly payments", y = "") +
    scale_x_continuous(breaks = x_vals, minor_breaks = x_vals[1], labels = scales::dollar_format(prefix="$"), lim = c(start_x, maxval)) +
    scale_y_continuous(labels = scales::dollar_format(prefix="$"),
                       breaks = breaks) +
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
          strip.text.x = element_text(size = 15, face = "bold"), 
          title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  
  gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(9.2, 10))
}


plot_mo_payments = function(payment_sched){
  
  min_pay = payment_sched %>% 
    group_by(month) %>% 
    summarize(min_pay_mo = sum(min_pay))
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(payment_sched$name))))
  
  cols_use = cols[cols_use_seq]
  
  max_mo = max(payment_sched$month)
  if(max_mo < 200) xbreaks = seq(12, max_mo, by = 12)
  else xbreaks = seq(12, max_mo, by = 24)
  
  ggplot(payment_sched, aes(x = month, y = payment)) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6) +
    geom_line(data = min_pay, aes(y = min_pay_mo, color = "Minimum monthly\n payments"), size = 1) +
    labs(y = "Total Monthly Payment", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = xbreaks) +
    ggthemes::scale_color_gdocs() +
    #scale_color_manual("", values = "red") +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$"))
  
}

plot_balance_over_time <- function(payment_sched){
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(payment_sched$name))))
  
  cols_use = cols[cols_use_seq]
  
  max_mo = max(payment_sched$month)
  if(max_mo < 200) xbreaks = seq(12, max_mo, by = 12)
  else xbreaks = seq(0, max_mo, by = 24)
  
  ggplot(payment_sched, aes(x = month, y = new_balance, fill = name)) + 
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6) +
    scale_fill_manual("", values = cols_use) +
    labs(y = "Total loan balance") +
    ggthemes::scale_color_gdocs() +
    scale_x_continuous("Month from now", breaks = xbreaks) +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$"))
}

plot_bar_all_options <- function(all_sched){
  
  vals <- unique(all_sched$mo_pay)
  vals_use <- c(vals[1:2], vals[which(vals %% 100 == 0)])
  vals_use <- vals_use[!duplicated(vals_use)]
  
  all_sched <- all_sched %>%
    filter(mo_pay %in% vals_use)
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(all_sched$name))))
  
  cols_use = cols[cols_use_seq]

  p1 <- ggplot(all_sched, aes(x = month, y = payment)) +
    facet_wrap(~ mo_pay_lab, ncol = 1) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6, size = .1) +
    labs(title = "Payments over time", y = "Monthly Payment", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = seq(12, max(all_sched$month), by = 12)) +
    ggthemes::scale_color_gdocs() +
    #scale_color_manual("", values = "red") +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
    guides(fill=FALSE) +
    theme(
      axis.title.y=element_blank(),
      axis.text.y = element_text(size = 6),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  p2 <- ggplot(all_sched, aes(x = month, y = new_balance)) +
    facet_wrap(~ mo_pay_lab, ncol = 1) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6, size = .1) +
    labs(title = "Balance over time", y = "Remaining Balance", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = seq(12, max(all_sched$month), by = 12)) +
    ggthemes::scale_color_gdocs() +
    #scale_color_manual("", values = "red") +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
    guides(fill=FALSE) +
    theme(
      axis.title.y=element_blank(),
      axis.text.y = element_text(size = 6),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  factor_levels <- c()
  
  empty_dat <- purrr::map_df(vals_use, function(mo_pay){
    lab <- ifelse(is.na(mo_pay), "Minimum\npayments", sprintf("$%0.2f/mo", mo_pay))
    factor_levels <<- c(factor_levels, lab)
    
    data.frame(x = 0, y = 0, mo_pay = lab, stringsAsFactors = F)
  })
  
  empty_dat$mo_pay <- factor(empty_dat$mo_pay, levels = factor_levels)
  
  p0 <- ggplot(empty_dat, aes(x = x, y = y)) +
    geom_blank() +
    facet_wrap(~mo_pay, ncol = 1) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x=element_text(color = "white"),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    labs(x = "", y = "", title = "") +
    geom_text(aes(label = mo_pay), size = 4)
  
  gridExtra::grid.arrange(p0, p1, p2, ncol = 3, widths = c(4,8,8))
  
}

plot_bar_one_option <- function(all_sched, mo_pay_use){
  
  filter_sched <- all_sched %>%
    filter(mo_pay_lab == mo_pay_use)
  
  month_range <- range(all_sched$month)
  max_payment <- max(all_sched$mo_pay, na.rm = T)
  max_balance <- max(all_sched$new_balance)
  
  cols = c("#11CEC4", "#218ED2", "#3250D6", "#6A43DA", 
           "#B455DE", "#E267D2", "#E67AA9", "#EB8EBE")
  
  cols_use_seq = seq(1, 8, by = floor(8/length(unique(filter_sched$name))))
  
  cols_use = cols[cols_use_seq]
  
  max_mo = max(all_sched$month)
  if(max_mo < 180) xbreaks = seq(0, max_mo, by = 12)
  else xbreaks = seq(0, max_mo, by = 24)
  
  p1 <- ggplot(filter_sched, aes(x = month, y = payment)) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6, size = .1) +
    labs(title = "Payments over time", y = "Monthly Payment", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = xbreaks, limits = month_range) +
    ggthemes::scale_color_gdocs() +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$"),
                       limits = c(0, max_payment)) +
    theme(
      axis.title.y=element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  p2 <- ggplot(filter_sched, aes(x = month, y = new_balance)) +
    geom_bar(stat = "identity", color = "white", aes(fill = name), alpha = .6, size = .1) +
    labs(title = "Balance over time", y = "Remaining Balance", color = "") +
    scale_fill_manual("", values = cols_use) +
    scale_x_continuous("Month from now", breaks = xbreaks,
                       limits = month_range) +
    ggthemes::scale_color_gdocs() +
    theme_linedraw() +
    scale_y_continuous(labels = scales::dollar_format(prefix="$")) +
    theme(legend.position="bottom", legend.direction="horizontal") +
    theme(
      axis.title.y=element_blank(),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
  
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend <- g_legend(p2)
  
  gridExtra::grid.arrange(
    gridExtra::arrangeGrob(p1 + theme(legend.position="none"),
                           p2 + theme(legend.position="none"),
                           nrow=1),
            mylegend, nrow=2, heights=c(10, 1))
    
}

