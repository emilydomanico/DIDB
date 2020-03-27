library(tidyverse)
library(hrbrthemes)
library(readr)

alldata <- read_csv("data.csv")

#metric selected from selectInput dropdown
#metric_filter <- input$metric
#metric_filter <- c("Congested VMT")
metric_filter <- c("Retail amenities")

# percentage threshold set by slider
#dim1<- input$Dim1*0.01
#dim2<- input$Dim2*0.01
#dim3<- input$Dim3*0.01
dim1 <- .47
dim2 <- .92
dim3 <- .8

data <- alldata %>%
  filter(metric == metric_filter)
#clean data so no hyphens
data[data=="no-build"] <- "no_build"
data[data=="Low-income"] <- "Low_income"
data[data=="Non-low-income"] <- "Non_low_income"
data[data=="Non-minority"] <- "Non_minority"

##Calculate error and scale by dim1. Use result to calculate upper and lower bounds for build & no-build scenarios
##Calculate impact and scale by dim2 for each population

## note: error_aug from alldata used as error, includes 10% confidence interval and zscore

## To find if building will trigger an impact
## filter by population, calculate differences between build and no build (delta), determine impact from scaled delta (by dim2), and bring into common dataframe


data <- data %>%
  spread( scenario, model_result, drop = TRUE)
data <- data %>%
  #step 1 from spreadsheet
  mutate(delta = build - no_build) %>%
  #scaling delta by dim2 slider
  #note: this use of dim2 could be wrong/ not useful
  mutate(delta_scaled = delta*dim2) %>%
  mutate(error_b = build*error_aug) %>%
  mutate(error_nb =no_build*error_aug) %>%
  #scaling error by dim1 slider
  mutate(error_scaled_b = error_b*dim1) %>%
  mutate(error_scaled_nb =error_nb*dim1) %>%
  mutate(LB_b = build-error_scaled_b) %>%
  mutate(UB_b = build+error_scaled_b) %>%
  mutate(LB_nb = no_build - error_scaled_nb)%>%
  mutate(UB_nb = no_build + error_scaled_nb) %>%
  
  #step 2 from spreadsheet
  #Impact option 1a: impact as calculated in the spreadsheet
  mutate(impact_original = case_when (abs(delta) > abs(error_nb) & (category == "Accessibility" & delta > 0 ) ~ "Positive Impact",
                                      abs(delta) > abs(error_nb) & (category != "Accessibility" & delta < 0) ~ "Positive Impact",
                                      abs(delta) > abs(error_nb) & (category == "Accessibility" & delta < 0 ) ~ "Negative Impact",
                                      abs(delta) > abs(error_nb) & (category != "Accessibility" & delta > 0 )~ "Negative Impact",
                                      TRUE ~ "No Impact")) %>%
  #Impact option 1b: impact as calculated in speadsheet + accounting for threshold sliders
  mutate(impact_scaled = case_when (abs(delta_scaled) > abs(error_scaled_nb) & (category == "Accessibility" & delta_scaled > 0 ) ~ "Positive Impact",
                                    abs(delta_scaled) > abs(error_scaled_nb) & (category != "Accessibility" & delta_scaled < 0 ) ~ "Positive Impact",
                                    abs(delta_scaled) > abs(error_scaled_nb) & (category == "Accessibility" & delta_scaled < 0 ) ~ "Negative Impact",
                                    abs(delta_scaled) > abs(error_scaled_nb) & (category != "Accessibility" & delta_scaled > 0 ) ~ "Negative Impact",
                                    TRUE ~ "No Imapact")) %>%
  
  #Impact option 2: impact as absolute change (note 0 to 100 slider not appropriate for all metrics)
  mutate(impact_absolute = case_when ( abs(delta) > dim2/.01 & (category == "Accessibility" & delta > 0 ) ~ "Positive Impact",
                                       abs(delta) > dim2/.01 & (category != "Accessibility" & delta < 0) ~ "Positive Impact",
                                       abs(delta) > dim2/.01 & (category == "Accessibility" & delta < 0 ) ~ "Negative Impact",
                                       abs(delta) > dim2/.01 & (category != "Accessibility" & delta > 0 )~ "Negative Impact",
                                       TRUE ~ "No Impact")) %>%
  #Impact option 3: impact as percentage point change with threshold set by slider 2
  mutate(percent_change= round(delta/no_build*100, digits = 1)) %>%
  mutate(impact_perctdiff = case_when (abs(percent_change) > dim2/.01 & (category == "Accessibility" & delta > 0 ) ~ "Positive Impact",
                                       abs(percent_change) > dim2/.01 & (category != "Accessibility" & delta < 0) ~ "Positive Impact",
                                       abs(percent_change) > dim2/.01 & (category == "Accessibility" & delta < 0 ) ~ "Negative Impact",
                                       abs(percent_change) > dim2/.01 & (category != "Accessibility" & delta > 0 )~ "Negative Impact",
                                       TRUE ~ "No Impact"))

#make an impact table, bring together all impact option by population in a table
impact_table <- data %>%
  select( population,delta,percent_change, starts_with("impact"))%>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, delta, percent_change, impact_original, impact_scaled, impact_absolute, impact_perctdiff) %>%
  # control order of entries
  arrange(factor(population, levels = c("Low_income","Non_low_income", "Minority","Non_minority"))) #%>%

# investigate by specific impact method

# impact_original
impact_method <- "impact_original"

impact_original <- impact_table %>%
  select( population, impact_method) %>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, impact_method) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact_method) %>%
  mutate(impact_type = case_when( 
    (Minority == "Positive Impact" & Non_minority  == "Positive Impact") | (Low_income == "Positive Impact" & Non_low_income == "Positive Impact") ~ "Benefit for both",
    (Minority == "Negative Impact" & Non_minority  == "Negative Impact") | (Low_income == "Negative Impact" & Non_low_income == "Negative Impact") ~ "Burden for both",
    (Minority == "Positive Impact" & Non_minority == "No Impact") | (Low_income == "Positive Impact" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Negative Impact" & Non_minority == "No Impact") | (Low_income == "Negative Impact" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Positive Impact") | (Low_income == "No Impact" & Non_low_income == "Positive Impact") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Negative Impact") | (Low_income == "No Impact" & Non_low_income == "Negative Impact") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    
    TRUE ~ "something elese happend"))

# impact_scaled
impact_method <- "impact_scaled"

impact_scaled <- impact_table %>%
  select( population, impact_method) %>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, impact_method) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact_method) %>%
  mutate(impact_type = case_when( 
    (Minority == "Positive Impact" & Non_minority  == "Positive Impact") | (Low_income == "Positive Impact" & Non_low_income == "Positive Impact") ~ "Benefit for both",
    (Minority == "Negative Impact" & Non_minority  == "Negative Impact") | (Low_income == "Negative Impact" & Non_low_income == "Negative Impact") ~ "Burden for both",
    (Minority == "Positive Impact" & Non_minority == "No Impact") | (Low_income == "Positive Impact" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Negative Impact" & Non_minority == "No Impact") | (Low_income == "Negative Impact" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Positive Impact") | (Low_income == "No Impact" & Non_low_income == "Positive Impact") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Negative Impact") | (Low_income == "No Impact" & Non_low_income == "Negative Impact") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    
    TRUE ~ "something elese happend"))


# impact_absolute
impact_method <- "impact_absolute"

impact_absolute <- impact_table %>%
  select( population, impact_method) %>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, impact_method) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact_method) %>%
  mutate(impact_type = case_when( 
    (Minority == "Positive Impact" & Non_minority  == "Positive Impact") | (Low_income == "Positive Impact" & Non_low_income == "Positive Impact") ~ "Benefit for both",
    (Minority == "Negative Impact" & Non_minority  == "Negative Impact") | (Low_income == "Negative Impact" & Non_low_income == "Negative Impact") ~ "Burden for both",
    (Minority == "Positive Impact" & Non_minority == "No Impact") | (Low_income == "Positive Impact" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Negative Impact" & Non_minority == "No Impact") | (Low_income == "Negative Impact" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Positive Impact") | (Low_income == "No Impact" & Non_low_income == "Positive Impact") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Negative Impact") | (Low_income == "No Impact" & Non_low_income == "Negative Impact") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    
    TRUE ~ "something elese happend"))

# impact_perctdiff
impact_method <- "impact_perctdiff"

impact_perctdiff <- impact_table %>%
  select( population, impact_method) %>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, impact_method) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact_method) %>%
  mutate(impact_type = case_when( 
    (Minority == "Positive Impact" & Non_minority  == "Positive Impact") | (Low_income == "Positive Impact" & Non_low_income == "Positive Impact") ~ "Benefit for both",
    (Minority == "Negative Impact" & Non_minority  == "Negative Impact") | (Low_income == "Negative Impact" & Non_low_income == "Negative Impact") ~ "Burden for both",
    (Minority == "Positive Impact" & Non_minority == "No Impact") | (Low_income == "Positive Impact" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Negative Impact" & Non_minority == "No Impact") | (Low_income == "Negative Impact" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Positive Impact") | (Low_income == "No Impact" & Non_low_income == "Positive Impact") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Negative Impact") | (Low_income == "No Impact" & Non_low_income == "Negative Impact") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    
    TRUE ~ "something elese happend"))


#Benefit or burden

#Disproportionality option 1:
  mutate(abs_diff = ) %>%
#Disproportionality option 2:
  mutate(percent_diff = ) %>%
#Disproportionality option 3:
  mutate(ratio = ) %>%



#factor population with levels to control display order in plot
data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
#extract unit for horizontal axis label
metric_unit <- data$metric_unit[1]

##DRAW THE PLOT

metric_plot<- ggplot(data, aes(x=population, y= UB_nb)) +
  geom_segment( aes(x=population, xend=population, y=LB_b, yend=UB_b, ), color= "#E69F00", alpha=.65, size= 10, show.legend = TRUE) +
  geom_segment( aes(x=population, xend=population, y=LB_nb, yend=UB_nb),color= "#56B4E9", alpha=.5, size= 10, show.legend = TRUE) +
  geom_point( aes(x=population, y=build),color= "#E69F00", shape="square", size=4, show.legend = TRUE) +
  geom_point( aes(x=population, y=no_build), color = "#56B4E9", shape="square", size=4, show.legend = TRUE) +
  geom_point( aes(x=population, y=build), shape=20, size=1, show.legend = TRUE)+
  geom_point( aes(x=population, y=no_build), shape=20, size=1, show.legend = TRUE)+
  #render delta
  geom_segment( aes(x=as.numeric(population) +.2, xend=as.numeric(population)+.2, y=no_build, yend= build)) +
  #render delta-scaled
  #Impact text, note: should depend on selected impact method
  geom_text(aes(x=as.numeric(population) +.3, y= no_build ,label= paste(impact_perctdiff, ", ", percent_change, "%")),hjust="inward", size= 4)+
  coord_flip()+
  theme_minimal() +
  theme(
    #legend.position = "none",
    axis.ticks.y=element_blank(),
    #axis.text.y= element_blank()
  )+
  labs(title = paste(metric_filter, "by population"))+
  ylab(paste(metric_filter, " (", metric_unit, ")"))+
  xlab("Population")
print(metric_plot)

print(impact_table)



# Plot the percentage change introduced by building, and how that changes with the impact threshold


impact_plot <- ggplot(data, aes(x= population))+
  geom_segment( aes(x=population, xend= population, y= 0,yend=percent_change), shape=20, size=1, show.legend = TRUE)+
  geom_text(aes(x=as.numeric(population) +.3, y= percent_change ,label= paste(impact_perctdiff, ", ", percent_change, "%")),hjust="inward", size= 4)+
  coord_flip()+
  theme_minimal()+
  labs(title = paste(metric_filter, "by population"))+
  ylab("")+
  xlab("Population")
  #ylim(-(dim2/0.01),dim2/0.01)
print(impact_plot)
  
u <- renderTable(impact_table)
  



#burden_plot <- 


