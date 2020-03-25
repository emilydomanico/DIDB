library(tidyverse)
library(hrbrthemes)
library(readr)


setwd("C:/Users/Emily/Desktop/CTPS_DIDB")
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
#the hyphen causes some problems when if becomes a column name, so replace the value with an underscore to clean up code further down
data[data=="no-build"] <- "no_build"

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
  mutate(impact_original = case_when (abs(delta)> abs(error_nb) ~ "Impact",
                                      TRUE ~ "No Impact")) %>%
  #Impact option 1b: impact as calculated in speadsheet + accounting for threshold sliders
  mutate(impact_scaled = case_when (abs(delta_scaled) > abs(error_scaled_nb) ~ "Impact",
                                    TRUE ~ "No Imapact")) %>%
  
  #Impact option 2: impact as absolute change (note 0 to 100 slider not appropriate for all metrics)
  mutate(impact_absolute = case_when ( abs(delta) > dim2/.01 ~"Impact",
                                       TRUE ~ "No Impact")) %>%
  #Impact option 3: impact as percentage point change with threshold set by slider 2
  mutate(percent_change= round(delta/no_build*100, digits = 1)) %>%
  mutate(impact_perctdiff = case_when (abs(percent_change) > dim2/.01 ~ "Impact",
                                       TRUE ~ "No Impact"))
#Set impact and burden methods
##not in use yet
#impact_option <- "impact_percentdiff" # impact_absolute, impact_scaled
#burden_option <- "burden_1" #burden2, burden_3 

#make an impact table
impact_table <- data %>%
  select( population,delta, starts_with("impact"))


#Benefit or burden
y <- impact_table %>%
  filter(str_detect(population, ".inority"))
ty <- t(y)

colnames(ty)
rownames(ty)

  mutate(impact_effect = case_when( ~ "Benefit for both",
                                           ~ "Min/low burden, non benefit",
                                           ~ "Non burden, min/low benefit",
                                           ~ "Burden for both"
                                           ~ NA
  )) %>%
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
  
impact_table <-
  



#burden_plot <- 


