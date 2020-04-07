library(tidyverse)
library(hrbrthemes)
library(readr)
library(gridExtra)
library(DT)

# percentage threshold set by slider
#dim1<- input$Dim1*0.01
#dim2<- input$Dim2*0.01
#dim3<- input$Dim3*0.01
dim1 <- .1
dim2 <- .01
dim3 <- .8


alldata <- read_csv("data.csv")

data <- alldata
#clean data so no hyphens
data[data=="no-build"] <- "no_build"
data[data=="Low-income"] <- "Low_income"
data[data=="Non-low-income"] <- "Non_low_income"
data[data=="Non-minority"] <- "Non_minority"


data <- data %>%
  spread( scenario, model_result, drop = TRUE)
data <- data %>%
  #slider1 sets confidence interval, use this to control error
  mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
  mutate(delta = build - no_build) %>%
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
    real_change == TRUE ~ "Real change",
    real_change == FALSE ~ "No real change"
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

change_type <- data %>%
  select( metric, population,real_change)%>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  arrange(factor(poptype)) %>%
  spread(population, real_change) %>%
  #mutate values for reactive text
  #mutate(change_type = case_when( 
  #  (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Real change for both populations",
  #  (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No real change for both populations",
  #  (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only real change for the protected population",
  #  (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only real change for the non-protected population",
  #  TRUE ~ "something else happend")) %>%
  mutate(change_type = case_when( 
    (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Real change for both",
    (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No real change for both",
    (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only real change for protected population",
    (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only real change for non-protected population",
    TRUE ~ "something else happend")) %>%
  select(metric, poptype, change_type)

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
impact_type <- impact_table %>%
  select( metric, poptype, population, impact) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact) %>%
  mutate(type = case_when( 
    (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Benefit for both",
    (Minority == "Benefit" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit" & Non_low_income == "Benefit within threshold") ~ "Only benefits protected population, benefits non-protected population within threshold",
    (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected population, burdens non-protected population",
    (Minority == "Benefit" & Non_minority == "Burden within threshold") | (Low_income == "Benefit" & Non_low_income == "Burden within threshold") ~ "Only benefits protected population, burdens non-protected population within threshold",
    (Minority == "Benefit within threshold" & Non_minority == "Benefit") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population, benefits protected population within the threshold",
    (Minority == "Burden within threshold" & Non_minority == "Benefit") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population, burdens protected population within threshold",
    (Minority == "Benefit within threshold" & Non_minority == "Burden") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population, benefits protected population within threshold",
    (Minority == "Burden within threshold" & Non_minority == "Burden") | (Low_income == "Burden within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population, burdens protected population within the threshold",
    (Minority == "Benefit within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Benefit within threshold") ~ "Impact within threshold for both",
    (Minority == "Benefit within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Benefit within threshold" & Non_low_income == "Burden within threshold") ~ "Impact within threshold for both",
    (Minority == "Burden within threshold" & Non_minority  == "Benefit within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Benefit within threshold") ~ "Impact within threshold for both",
    (Minority == "Burden within threshold" & Non_minority  == "Burden within threshold") | (Low_income == "Burden within threshold" & Non_low_income == "Burden within threshold") ~ "Impact within threshold for both",
    (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected population, benefits non-protected population",
    (Minority == "Burden" & Non_minority == "Benefit within threshold") | (Low_income == "Burden" & Non_low_income == "Benefit within threshold") ~ "Only burdens protected population, benefits non-protected population within threshold",
    (Minority == "Burden" & Non_minority == "Burden within threshold") | (Low_income == "Burden" & Non_low_income == "Burden within threshold") ~ "Only burdens protected population, burdens non-protected population within threshold",
    (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
    TRUE ~ "something else happend")) %>%
  select(metric, poptype, type)

dispro <- diff %>%
  left_join(impact_type) %>%
  left_join(change_type) %>%
  mutate(DB = case_when(
    ratio >= (1 + dim3) ~ "Protected population affected more",
    ratio <= (1 - dim3) ~ "Non-protected population affected more",
    ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
    TRUE ~ "Something else happened. problem!"
  )) 

DIDB <- dispro %>%
  select(metric, poptype, ratio, change_type, type, DB) %>%
  mutate(instance = case_when (
    (change_type == "No real change for both") ~ "No: No significant change for either population",
    # Note, might need to add change_type != "No real change for both" to all further conditions...
    
    (type == "Benefit for both") & (DB == "Protected population affected more") ~ "No",
    (type == "Benefit for both") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "No",
    (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Benefits protected population, burdens non-protected population") ~ "No: No adverse impact for protected population",
    
    (type == "Only benefits protected population, burdens non-protected population within threshold") ~ "No: No adverse impact for protected population",
    
    (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Protected population affected more") ~ "Yes",
    (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
    (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Only burdens non-protected population, benefits protected population within threshold") ~ "No: No adverse impact for protected population",
    
    (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Protected population affected more") ~ "No",
    (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Non-protected population affected more") ~ "No",
    (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Impact within threshold for both") ~ "No: Does not exceed impact threshold",
    
    (type == "Burdens protected population, benefits non-protected population") ~ "Yes: Adverse impact for protected population",
    
    (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
    (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
    (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
    (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
    
    (type == "Burden for both") & (DB == "Protected population affected more") ~ "Yes",
    (type == "Burden for both") & (DB == "Non-protected population affected more") ~ "No",
    (type == "Burden for both") & (DB == "Disproportionality within threshold") ~ "No",
    
    TRUE ~ "something elese happend, problem!"))%>%
  select(metric, poptype, instance)

DIDB_clean <- DIDB%>%
  mutate(Metric = metric)%>%
  select(Metric, poptype, instance)%>%
  rename("Population Type" = poptype)%>%
  rename("Disperate Impact or Disproportionate Burden" = instance)
  
#######################


dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))

#change underscores back to hyphens
data[data=="Low_income"] <- "Low-income"
data[data=="Non_low_income"] <- "Non-low-income"
data[data=="Non_minority"] <- "Non-minority"

#factor population with levels to control display order in plot
data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  





DIDB_count <- DIDB %>%
  select(poptype, instance)

DIDB_count[DIDB_count == "No"] <- 0
DIDB_count[DIDB_count == "Yes"] <- 1

function (x) {
  
}

# DIDB count for metric_filter
all_metrics <- c("Retail amenities", "Higher education","Healthcare facilities", "Jobs by transit","Congested VMT","Carbon monoxide emissions", "Average attraction - highway", "Average production - highway","Average attraction - transit",
                 "Average production - transit")
poptypes <- rep(c("i", "m"), 10)

count_table <- data.frame (metric = rep(all_metrics, each = 2)) %>%
  mutate(poptypes) %>%
  mutate(count = 
           
           
           
           
           
#############################

#DIDB count for all metrtics and all thresholds

##using code that runs for all metrics. Then loop through by new slider inputs
##bind resulting rows to dataframe to collect output





metric <- c("Retail amenities", "Higher education","Healthcare facilities", "Jobs by transit","Congested VMT","Carbon monoxide emissions", "Average attraction - highway", "Average production - highway","Average attraction - transit",
            "Average production - transit")
poptypes <- c("i", "m")

#101
Dim1 <- seq(0, 100, by = 1)

#201
Dim2 <- seq(0, 20, by = .1)

#31
Dim3 <- seq(0, 30, by = 1)


all_metrics <- c("Retail amenities", "Higher education","Healthcare facilities", "Jobs by transit","Congested VMT","Carbon monoxide emissions", "Average attraction - highway", "Average production - highway","Average attraction - transit",
                 "Average production - transit")
poptypes <- rep(c("i", "m"), 10)

df <- data.frame (metric = rep(all_metrics, each = 2)) %>%
  mutate(poptypes) %>%
  mutate(count = NA)


df <- 
  
           
           
           