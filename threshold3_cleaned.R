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
dim1 <- .1
dim2 <- .01
dim3 <- .8


#dispro_method <- input$dis_method


data <- alldata %>%
  filter(metric == metric_filter)
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
  mutate(im_th_amt = no_build*dim2) %>%
  
  #step 2 from spreadsheet

  #Impact option 1b: impact as calculated in speadsheet + accounting for threshold sliders
  mutate(impact = case_when (abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta > 0 ) ~ "Benefit",
                             abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta < 0 ) ~ "Benefit",
                             abs(delta) > abs(im_th_amt) & (category == "Accessibility" & delta < 0 ) ~ "Burden",
                             abs(delta) > abs(im_th_amt) & (category != "Accessibility" & delta > 0 ) ~ "Burden",
                             TRUE ~ "No Impact"))
  
#make an impact table, bring together all impact option by population in a table
impact_table <- data %>%
  select( population,delta, impact)%>%
  mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                               str_detect(population, ".ncome") ~ "i",
                               TRUE ~ "NA")) %>%
  select(poptype, population, delta, impact) %>%
  # control order of entries
  arrange(factor(population, levels = c("Low_income","Non_low_income", "Minority","Non_minority"))) #%>%

# investigate what kind of impact
impact_type <- impact_table %>%
  select( poptype, population, impact) %>%
  arrange(factor(poptype)) %>%
  spread(population, impact) %>%
  mutate(type = case_when( 
    (Minority == "Benefit" & Non_minority  == "Benefit") | (Low_income == "Benefit" & Non_low_income == "Benefit") ~ "Benefit for both",
    (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
    (Minority == "Benefit" & Non_minority == "No Impact") | (Low_income == "Benefit" & Non_low_income == "No Impact") ~ "Only benefits protected population",
    (Minority == "Burden" & Non_minority == "No Impact") | (Low_income == "Burden" & Non_low_income == "No Impact") ~ "Only burdens protected population",
    (Minority == "No Impact" & Non_minority == "Benefit") | (Low_income == "No Impact" & Non_low_income == "Benefit") ~"Only benefits non-protected population",
    (Minority == "No Impact" & Non_minority == "Burden") | (Low_income == "No Impact" & Non_low_income == "Burden") ~"Only burdens non-protected population",
    (Minority == "No Impact" & Non_minority  == "No Impact") | (Low_income == "No Impact" & Non_low_income == "No Impact") ~ "Impacts Neither",
    TRUE ~ "something elese happend"))


#Disproportionality option 1:
#  mutate(percent_diff = ) %>%
#Disproportionality option 2:
#  mutate(ratio = ) %>%



#factor population with levels to control display order in plot
data$population <- factor(data$population, levels = c("Non_minority", "Minority","Non_low_income", "Low_income"))  
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
  geom_segment( aes(x=as.numeric(population) +.2, xend=as.numeric(population)+.2, y=no_build, yend= build), color = "black") +
  #Impact text, note: should depend on selected impact method
  geom_text(aes(x=as.numeric(population) +.3, y= no_build , label=impact),hjust="inward", size= 4)+
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




# Plot the percentage change introduced by building, and how that changes with the impact threshold


impact_plot <- ggplot(data, aes(x= population))+
  geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build), shape=20, size=4, show.legend = TRUE)+
  geom_hline( aes(yintercept = dim2), color = "red")+
  geom_hline( aes(yintercept = -dim2), color = "red")+
  geom_hline(aes(yintercept = 0), alpha=.5, color = "black")+
  geom_text(aes(x=as.numeric(population) +.3, y= delta/no_build ,label= impact),hjust="inward", size= 4)+
  coord_flip()+
  theme_minimal()+
  labs(title = paste("Percent change", "by population"))+
  ylab("")+
  xlab("Population")
print(impact_plot)
  
  



#burden_plot <- 


