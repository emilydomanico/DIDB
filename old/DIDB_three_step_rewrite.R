
  #user inputs
  # percentage threshold set by slider
  dim1 <- 10*0.01
  dim2 <- 5*0.01
  dim3 <- 5*0.01
  

  
  #prep for non-map related tasks
  #read metric result data
  alldata <- read_csv("data/data.csv")
  data <- alldata 
  #clean data so no hyphens
  data[data=="no-build"] <- "no_build"
  data[data=="Low-income"] <- "Low_income"
  data[data=="Non-low-income"] <- "Non_low_income"
  data[data=="Non-minority"] <- "Non_minority"
  
  data <- data %>%
    spread( scenario, model_result, drop = TRUE)
  data <- data %>%
    mutate(delta = build - no_build)
  
  cat <- "Accessibility"
  
  data <- data %>%
    filter(category==cat)%>%
    #slider1 sets confidence interval, use this to control error
    mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
    #mutate(delta = build - no_build) %>%
    mutate(error_b = build*error_aug) %>%
    mutate(error_nb =no_build*error_aug) %>%
    mutate(LB_b = build-error_b) %>%
    mutate(UB_b = build+error_b) %>%
    mutate(LB_nb = no_build - error_nb)%>%
    mutate(UB_nb = no_build + error_nb)%>%
    # check if b range distinct from nb
    mutate(real_change = case_when(
      (UB_b < LB_nb) | (UB_nb < LB_b) ~ TRUE,
      TRUE ~ FALSE
    ))%>%
    mutate(change_label = case_when(
      real_change == TRUE ~ "Exceeds baseline \nuncertainty",
      real_change == FALSE ~ "No likely impact"
    )) %>%
    #slider2 sets percent amount to consider from no build model result to establish if impact is large enough to consider
    #im_th_amt "impact threshold amount"
    mutate(im_th_amt = no_build*dim2) %>%
    #compares delta to impact threshold amount
    mutate(impact = case_when (abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta > 0 ) ~ "Benefit",
                               abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta < 0 ) ~ "Benefit",
                               abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta < 0 ) ~ "Burden",
                               abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta > 0 ) ~ "Burden",
                               abs(delta) < abs(im_th_amt) & (category == "Accessibility" & delta > 0 ) ~ "Benefit within threshold",
                               abs(delta) < abs(im_th_amt) & (category != "Accessibility" & delta < 0 ) ~ "Benefit within threshold",
                               abs(delta) < abs(im_th_amt) & (category == "Accessibility" & delta < 0 ) ~ "Burden within threshold",
                               abs(delta) < abs(im_th_amt) & (category != "Accessibility" & delta > 0 ) ~ "Burden within threshold",
                               TRUE ~ "Error"))
  
  change_results <- data %>%
    select( metric, population,real_change)%>%
    mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                 str_detect(population, ".ncome") ~ "i",
                                 TRUE ~ "NA")) %>%
    arrange(factor(poptype)) %>%
    spread(population, real_change) %>%
    mutate(change_type = case_when( 
      (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Exceeds uncertainty for both",
      (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No likely impact for either",
      (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only exceeds uncertainty for protected population",
      (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only exceeds uncertainty for non-protected population",
      TRUE ~ "something else happened")) %>%
    mutate(change_test = case_when(
      (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Yes",
      (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No",
      (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Yes",
      (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Yes",
    ))%>%
    select(metric, poptype, change_type, change_test)
  
  #make an impact table, bring together all impact options by population in a table
  impact_table <- data %>%
    select( metric,population,delta, no_build,real_change, impact)%>%
    mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                 str_detect(population, ".ncome") ~ "i",
                                 TRUE ~ "NA")) %>%
    #find percent change between no_build and build
    mutate( per_change = delta/no_build) %>%
    select(metric, poptype, population, per_change, real_change,impact) %>%
    # control order of entries
    arrange(factor(population, levels = c("Low_income","Non_low_income", "Minority","Non_minority")))
  
  diff <- impact_table %>%
    select(metric, poptype, population, per_change) %>%
    arrange(factor(poptype)) %>%
    spread(population, per_change) %>%
    mutate(difference = case_when(
      (is.na(Low_income) & is.na(Non_low_income)) ~ Minority - Non_minority,
      (is.na(Minority) & is.na(Non_minority)) ~ Low_income - Non_low_income,
      # must be a numeric error
      TRUE ~ NA_real_)) %>%
    mutate(ratio = case_when(
      (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
      (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
      # must be a numeric error
      TRUE ~ NA_real_)) %>%
    select(metric, poptype, difference, ratio)
  
  # investigate what kind of impact
  impact_results <- impact_table %>%
    select( metric, poptype, population, impact) %>%
    arrange(factor(poptype)) %>%
    spread(population, impact) %>%
    mutate(impact_type = case_when( 
      (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Benefit for both",
      (Minority == "Benefit" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit" & Non_low_income == "Benefit within threshold") ~ "Only benefits protected population, benefits non-protected population within threshold",
      (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected population, burdens non-protected population",
      (Minority == "Benefit" & Non_minority == "Burden within threshold") | (Low_income == "Benefit" & Non_low_income == "Burden within threshold") ~ "Only benefits protected population, burdens non-protected population within threshold",
      (Minority == "Benefit within threshold" & Non_minority == "Benefit") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population, benefits protected population within the threshold",
      (Minority == "Burden within threshold" & Non_minority == "Benefit") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population, burdens protected population within threshold",
      (Minority == "Benefit within threshold" & Non_minority == "Burden") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population, benefits protected population within threshold",
      (Minority == "Burden within threshold" & Non_minority == "Burden") | (Low_income == "Burden within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population, burdens protected population within the threshold",
      (Minority == "Benefit within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit within threshold") ~ "Benefit within threshold for both",
      (Minority == "Benefit within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden within threshold") ~ "Protected population benefits within threshold, non-protected burdened within threshold",
      (Minority == "Burden within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit within threshold") ~ "Protected population burdened within threshold, non-protected benefits within threshold",
      (Minority == "Burden within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Burden within threshold") ~ "Burden within threshold for both",
      (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected population, benefits non-protected population",
      (Minority == "Burden" & Non_minority == "Benefit within threshold") | (Low_income == "Burden" & Non_low_income == "Benefit within threshold") ~ "Only burdens protected population, benefits non-protected population within threshold",
      (Minority == "Burden" & Non_minority == "Burden within threshold") | (Low_income == "Burden" & Non_low_income == "Burden within threshold") ~ "Only burdens protected population, burdens non-protected population within threshold",
      (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
      TRUE ~ "something else happend")) %>%
    mutate(impact_test = case_when(
      (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Yes",
      (Minority == "Benefit" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit" & Non_low_income == "Benefit within threshold") ~ "Yes",
      (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Yes",
      (Minority == "Benefit" & Non_minority == "Burden within threshold") | (Low_income == "Benefit" & Non_low_income == "Burden within threshold") ~ "Yes",
      (Minority == "Benefit within threshold" & Non_minority == "Benefit") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit") ~"Yes",
      (Minority == "Burden within threshold" & Non_minority == "Benefit") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit") ~"Yes",
      (Minority == "Benefit within threshold" & Non_minority == "Burden") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden") ~"Yes",
      (Minority == "Burden within threshold" & Non_minority == "Burden") | (Low_income == "Burden within threshold" & Non_low_income == "Burden") ~"Yes",
      (Minority == "Benefit within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit within threshold") ~ "No",
      (Minority == "Benefit within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden within threshold") ~ "No",
      (Minority == "Burden within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit within threshold") ~ "No",
      (Minority == "Burden within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Burden within threshold") ~ "No",
      (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Yes",
      (Minority == "Burden" & Non_minority == "Benefit within threshold") | (Low_income == "Burden" & Non_low_income == "Benefit within threshold") ~ "Yes",
      (Minority == "Burden" & Non_minority == "Burden within threshold") | (Low_income == "Burden" & Non_low_income == "Burden within threshold") ~ "Yes",
      (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Yes",
      TRUE ~ "something else happend")) %>%
    mutate(impact_class= case_when(
      (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Benefit",
      (Minority == "Benefit" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit" & Non_low_income == "Benefit within threshold") ~ "Benefit",
      (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Mixed",
      (Minority == "Benefit" & Non_minority == "Burden within threshold") | (Low_income == "Benefit" & Non_low_income == "Burden within threshold") ~ "Mixed",
      (Minority == "Benefit within threshold" & Non_minority == "Benefit") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit") ~"Benefit",
      (Minority == "Burden within threshold" & Non_minority == "Benefit") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit") ~"Mixed",
      (Minority == "Benefit within threshold" & Non_minority == "Burden") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden") ~"Mixed",
      (Minority == "Burden within threshold" & Non_minority == "Burden") | (Low_income == "Burden within threshold" & Non_low_income == "Burden") ~"Burden",
      (Minority == "Benefit within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit within threshold") ~ "Benefit",
      (Minority == "Benefit within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden within threshold") ~ "Mixed",
      (Minority == "Burden within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit within threshold") ~ "Mixed",
      (Minority == "Burden within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Burden within threshold") ~ "Burden",
      (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Mixed",
      (Minority == "Burden" & Non_minority == "Benefit within threshold") | (Low_income == "Burden" & Non_low_income == "Benefit within threshold") ~ "Mixed",
      (Minority == "Burden" & Non_minority == "Burden within threshold") | (Low_income == "Burden" & Non_low_income == "Burden within threshold") ~ "Burden",
      (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden",
      TRUE ~ "something else happend"
    ))%>%
    select(metric, poptype, impact_type, impact_test, impact_class)
  
  dispro <- diff %>%
    left_join(change_results) %>%
    left_join(impact_results) %>%
    mutate(DB = case_when(
      impact_class== "Burden" & ratio >= (1 + dim3) ~ "Protected population burdened more",
      impact_class== "Benefit" & ratio >= (1+dim3 ) ~ "Protected population benefits more",
      impact_class== "Mixed" & ratio >= (1 + dim3 ) ~ "Protected population affected more",
      impact_class== "Burden" & ratio <= (1-dim3) ~ "Non-protected population burdened more",
      impact_class== "Benefit" & ratio <= (1-dim3) ~ "Non-protected population benefits more",
      impact_class== "Mixed" & ratio <= (1-dim3) ~ "Non-protected population affected more",
      ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
      TRUE ~ "Something else happened. problem!"
    )) 
  
    
  
  DIDB <- dispro %>%
    select(metric, poptype, ratio, change_type,change_test, impact_type, impact_test,impact_class, DB) %>%
    mutate(instance = case_when (
      (change_type == "No likely impact for either") ~ "No",
      
      (impact_type == "Benefit for both") & (DB == "Protected population benefits more") ~ "No",
      (impact_type == "Benefit for both") & (DB == "Non-protected population benefits more") ~ "Yes",
      (impact_type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Protected population benefits more") ~ "No",
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Non-protected population benefits more") ~ "Yes",
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Benefits protected population, burdens non-protected population") ~ "No",
      
      (impact_type == "Only benefits protected population, burdens non-protected population within threshold") ~ "No",
      
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Protected population benefits more") ~ "Yes",
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Non-protected population benefits more") ~ "Yes",
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Only burdens non-protected population, benefits protected population within threshold") ~ "No",
      
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Protected population burdened more") ~ "No",
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Non-protected population burdened more") ~ "No",
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Benefit within threshold for both") ~ "No",
      (impact_type == "Protected population benefits within threshold, non-protected burdened within threshold") ~ "No",
      (impact_type == "Protected population burdened within threshold, non-protected benefits within threshold") ~ "No",
      (impact_type == "Burden within threshold for both") ~ "No",
      
      (impact_type == "Burdens protected population, benefits non-protected population") ~ "Yes",
      
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Protected population burdened more") ~ "Yes",
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Non-protected population burdened more") ~ "Yes",
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
      
      (impact_type == "Burden for both") & (DB == "Protected population burdened more") ~ "Yes",
      (impact_type == "Burden for both") & (DB == "Non-protected population burdened more") ~ "No",
      (impact_type == "Burden for both") & (DB == "Disproportionality within threshold") ~ "No",
      
      TRUE ~ "something elese happend, problem!"))%>%
    mutate(DB_reason = case_when (
      (change_type == "No likely impact for either") ~ "No likely impact for either population",
      
      (impact_type == "Benefit for both") & (DB == "Protected population benefits more") ~ "Exceeds the disproportionality threshold, but no adverse impact for protected population",
      (impact_type == "Benefit for both") & (DB == "Non-protected population benefits more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Protected population benefits more") ~ "Exceeds the disproportionality threshold, but no adverse impact for protected population",
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Non-protected population benefits more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Benefits protected population, burdens non-protected population") ~ "No adverse impact for protected population",
      
      (impact_type == "Only benefits protected population, burdens non-protected population within threshold") ~ "No adverse impact for protected population",
      
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Protected population benefits more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Non-protected population benefits more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Only burdens non-protected population, benefits protected population within threshold") ~ "No adverse impact for protected population",
      
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Protected population burdened more") ~ "Exceeds the disproportionality threshold, but no adverse impact for protected population",
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Non-protected population burdened more") ~ "Exceeds the disproportionality threshold, but no adverse impact for protected population",
      (impact_type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Benefit within threshold for both") ~ "Does not exceed impact threshold",
      (impact_type == "Protected population benefits within threshold, non-protected burdened within threshold") ~ "Does not exceed impact threshold",
      (impact_type == "Protected population burdened within threshold, non-protected benefits within threshold") ~ "Does not exceed impact threshold",
      (impact_type == "Burden within threshold for both") ~ "Does not exceed impact threshold",
      
      (impact_type == "Burdens protected population, benefits non-protected population") ~ "Adverse impact for protected population",
      
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Protected population burdened more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Non-protected population burdened more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      (impact_type == "Burden for both") & (DB == "Protected population burdened more") ~ "Exceeds the disproportionality threshold",
      (impact_type == "Burden for both") & (DB == "Non-protected population burdened more") ~ "Exceeds the disproportionality threshold, but no adverse impact for protected population",
      (impact_type == "Burden for both") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
      
      TRUE ~ "something elese happend, problem!"))
  
  DIDB[DIDB== "i"] <- "I"
  DIDB[DIDB== "m"] <- "M"
  
  DIDB_clean <- DIDB%>%
    mutate(Metric = metric)%>%
    select(Metric, poptype, change_test, impact_test, DB, instance, DB_reason)%>%
    rename("Population Group" = poptype)%>%
    rename("Uncertainty Test" = change_test)%>%
    rename("Adverse Impact Test"= impact_test)%>%
    rename("Disproportionality Test"= DB)%>%
    rename("DIDB" = instance) %>%
    rename("Reason"= DB_reason)
  
  
  