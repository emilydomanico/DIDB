library(tidyverse)
library(hrbrthemes)
library(readr)
library(gridExtra)


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
  #step 1 from spreadsheet
  mutate(delta = build - no_build) %>%
  mutate(error_b = build*error_aug) %>%
  mutate(error_nb =no_build*error_aug) %>%
  mutate(LB_b = build-error_b) %>%
  mutate(UB_b = build+error_b) %>%
  mutate(LB_nb = no_build - error_nb)%>%
  mutate(UB_nb = no_build + error_nb) %>%
  #slider2 sets percent amount to consider from no build model result to establish if impact is large enough to consider
  #im_th_amt "impact threshold amount"
  mutate(im_th_amt = no_build*dim2) %>%
  #compares delta to impact threshold amount
  mutate(impact = case_when (abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta > 0 ) ~ "Benefit",
                             abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta < 0 ) ~ "Benefit",
                             abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta < 0 ) ~ "Burden",
                             abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta > 0 ) ~ "Burden",
                             TRUE ~ "No Impact"))

#make an impact table, bring together all impact options by population in a table
impact_table <- data %>%
  select( metric,population,delta, no_build, impact)%>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  #find percent change between no_build and build
  mutate( per_change = delta/no_build) %>%
  select(metric, poptype, population, per_change, impact) %>%
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
    TRUE ~ 999999)) %>%
  mutate(ratio = case_when(
    (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
    (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
    # must be a numeric error
    TRUE ~ 999999)) %>%
  select(metric, poptype, difference, ratio)

# investigate what kind of impact
impact_type <- impact_table %>%
  select( metric, poptype, population, impact) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact) %>%
  mutate(type = case_when( 
    (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Benefit for both",
    (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
    (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected, burdens non-protected",
    (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected, benefits non-protected",
    (Minority == "Benefit" & Non_minority == "No Impact") | (Low_income == "Benefit" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Burden" & Non_minority == "No Impact") | (Low_income == "Burden" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Benefit") | (Low_income == "No Impact" & Non_low_income == "Benefit") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Burden") | (Low_income == "No Impact" & Non_low_income == "Burden") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    TRUE ~ "something else happend")) %>%
  select(metric,poptype, type)

dispro <- diff %>%
  left_join(impact_type) %>%
  mutate(DB = case_when(
    #note, check spelling....
    #note, more conditions to bring in 
    ratio > 1 + dim3 ~ "Protected population changes more",
    ratio < 1 - dim3 ~ "Non-protected population changes more",
    1- dim3 < ratio | ratio < 1 +dim3 ~ "Disproportionality within threshold",
    TRUE ~ "Something else happened. problem!"
  )) 

DIDB <- dispro %>%
  select(metric, poptype, ratio, type, DB) %>%
  mutate(instance = case_when (
    # right now, takes first two cases regardless of whether dispropotionality is within the threshold
    type == "Burdens protected, benefits non-protected" ~ "Yes",
    type == "Benefits protected, burdens non-protected" ~ "No",
    type == "Benefit for both" & ratio < 1-dim3 ~ "Yes",
    type == "Benefit for both" & ratio > 1-dim3 ~ "No",
    type == "Burden for both" & ratio > 1+dim3 ~ "Yes",
    type == "Burden for both" & ratio < 1+dim3 ~ "No",
    #questions about following cases
    type == "Only benefits protected population" & ratio < 1-dim3 ~ "?",
    type == "Only burdens protected population" & ratio > 1+dim3 ~ "?",
    type == "Only benefits non-protected population" & ratio < 1-dim3  ~ "?",
    type == "Only burdens non-protected population" & ratio > 1+dim3 ~ "?",
    #assuming if no impact, then no DIDB
    #type == "Impacts Neither" & (1- dim3 < ratio | ratio < 1 +dim3) ~ "No",
    # DB == "Disproportionality within threshold" ~ "No",
    TRUE ~ "something else happend"
  ))%>%
  select(metric, poptype, instance) %>%
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
           
           
           
           