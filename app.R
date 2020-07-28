# Packages #############################################
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyBS)
#library(sf)
library(ggthemes)
library(kableExtra)



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



#prep for map tasks
mrs <- read_csv("data/pct_change_by_TAZ.csv")
mrs_list <- mrs %>%
  select(TAZ_ID)

mrs_pop <- read_csv("data/pct_change_by_TAZ_pop.csv")

mrs_diff<- read_csv("data/mr_diff2.csv")

#prep for metric filtering
met_list <- read_csv("data/metric_category.csv")

taz_pop <- read_csv("data/pop_by_TAZ_2040.csv")
pop_mpo <- taz_pop %>%
  right_join(mrs_list)

# UI ####################################################

ui <- fluidPage(
#css #######################################
  tags$head(
    tags$style(HTML("
    p {
    font-size: 13px;
    }
    h5 {
    font-weight: bold;
    }
    ")
               
    )
  ),
  withMathJax(),
  #titlePanel("DI/DB Thresholds"),
  setSliderColor(c(rep("DimGray",6)), c(1,2,3,4,5,6)),
  chooseSliderSkin("Flat"),
# SideBar Panel UI ######################################
  sidebarLayout(
    sidebarPanel(width= 3,
      h2("DI/DB Thresholds"),
      br(),
      br(),
      h5("Baseline Uncertainty Threshold"),
      radioButtons("Dim1", "I want to set the confidence level to:",
                   c("No uncertainty"=0,
                     "Low uncertainty"=10,
                     "Moderate uncertainty"= 50,
                     #"90 %"=90,
                     "High uncertainty"= 95),
                   selected= 10,
                   inline= TRUE),
      p("This threshold sets the sensitivity for detecting the likelihood of the model outputs for the build and no-build scenarios. The radio buttons represent confidence levels - how confident we feel that the model outputs represent the potential reality. For example, moderate uncertainty means that we are somewhat confident that the model results represent the impacts of the transportation system in 2040. Higher uncertainty would result in a greater range of likely values - as indicated by the blue and orange bars - and less of a change of identifying a potential impact."),
      br(),
      sliderInput("Dim2", label = "Practical Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
     #h5("Impact Threshold"), 
     p("This threshold sets the sensitivity for determining if the impacts of implementing the build scenario would be practically significant. The slider represents a percent change. At 0%, any change between the build and no-build scenario would be considered a practically significant impact. As the threshold increases, the likelihood of identifying an adverse effect decreases. The impact is calculated as the percent change between scenarios:"),
      withMathJax("$$\\scriptsize\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build}} \\cdot 100$$"),
      p(" "),
      p("If an impact is found, it is categorized as a benefit or a burden based on the directionality of the metric. (For example, an increase in carbon monoxide emissions is a burden, while an increase in access to jobs is a benefit.)"),
      br(),
     #h5("Disproportionality Threshold"), 
      sliderInput("Dim3", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
      p("This threshold determines if the impacts found in the previous step would disproportionately affect the minority or low-income population compared to the nonminority or non-low-income population."),
      p("Disproportionality is calculated as a ratio, comparing the absolute value of the percent change for the protected population (from the second step) to the absolute value of the percent change for the non-protected population."),
     br()
     ),
# Main Panel UI ######################################################    
    mainPanel(
      tabsetPanel( type = "tabs",
                   tabPanel("Introduction",
                            br(),
                            h5("Purpose of the DI/DB Threshold Application:"),
                            p("The Boston Region Metropolitan Planning Organization (MPO) staff have developed this application
                              to help staff and interested stakeholders better visualize how setting the three different thresholds
                              within the MPO’s Disparate Impact and Disproportionate Burden (DI/DB) Policy would affect the likelihood
                              of identifying potential future disparate impacts and disproportionate burdens. "),
                            p("The DI/DB Policy helps the MPO meet the federal requirement to identify potential, future disparate
                              impacts on minority populations, and disproportionate burdens on low-income populations that may result from
                              MPO investments, in the aggregate, that are funded in its Long-Range Transportation Plan (LRTP).
                              (One of the roles of the LRTP is to dedicate funding over the next twenty years to regionally significant
                              transportation projects.) To do so, the MPO uses a regional travel demand model to identify probable impacts on minority, low-income,
                            nonminority, and non-low-income populations. In ", em("Destination 2040"), ", these impacts were analyzed for
                            a suite of ten metrics, which are also used in this application. To see the results from the analysis of the MPO’s latest LRTP, ", em("Destination 2040"),", ",
                            a("click here", href= "https://www.bostonmpo.org/data/pdf/plans/LRTP/destination/Destination-2040-LRTP-20191030.pdf#page=243", target="_blank"),
                            ". "),
                            p("This application uses the model’s results for each metric analyzed in ", em("Destination 2040")," to demonstrate the
                              role each of the three thresholds within the DI/DB Policy, help stakeholders better understand and provide input into
                              the DI/DB Policy, and assist MPO staff with setting these thresholds. To be considered a disparate impact or
                              disproportionate burden, any expected impact must pass each of the three thresholds—or ", em("tests"), "—in order, as described below."),
                            br(),
                            h5("Three-Test DI/DB Investigation:"),
                            p("The investigation takes the form of three tests, 
                              which build on one another to indicate whether there is a potential DI or DB for each metric. 
                              The first test asks whether an impact is likely, which is necessary since the model inputs 
                              contain some uncertainty and because it is projecting impacts twenty years into the future. 
                              For each population group, the first test determines if the impact for each metric exceeds the 
                              model’s baseline uncertainty.  The second test asks if the impact is practically significant for each of the four population groups. 
                              The third test asks whether the minority or low-income populations would be disproportionately 
                              affected compared to the nonminority and non-low-income populations, respectively. 
                              The DI/DB Threshold Application allows users to pair a tolerance threshold with each test in 
                              order to explore how different tolerances affect the sensitivity to DI/DB outcomes."),
                            br(),
                            h5("Definitions:"),
                            p("A disparate impact is a facially neutral policy or practice that disproportionately affects
members of a group identified by race, color, or national origin, where the policy or
practice lacks a substantial legitimate justification, and where there exists one or more
alternatives that would serve the same legitimate objectives but with a less
disproportionate effect. A minority person is one who identifies as Black or African
American; American Indian or Alaskan Native; Asian; Native Hawaiian or other Pacific
Islander; and/or Hispanic or Latino/a/x."),
                            p("A disproportionate burden is a neutral policy or practice that disproportionately affects
low-income populations more than non-low income populations. The MPO considers a
person whose family income is at or below 200% of the poverty
level for their family size as low income."),
                   ),
                   #UI by Acc #######################################
                   tabPanel("Accessibility Metrics",
                            br(),
                            column(width=12,
                                     selectInput("metricAcc", "Metric:",
                                                 choices = list(
                                                   Accessibility= c("Access to retail opportunities by transit", 
                                                                    "Access to higher education by transit",
                                                                    "Access to healthcare facilities by transit", 
                                                                    "Access to jobs by transit")),
                                                  selected = "Access to retail opportunities by transit"),
                            ),
                            
                            column(width = 12,
                                   h5("Baseline Uncertainty Threshold"),
                                   p("If there is a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Practical Impact Threshold."),
                                   plotOutput(outputId = "metric_plotAcc"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Practical Impact Threshold"),
                                   p("Where a potential impact is indicated, is the impact practically significant for each population?"),
                                   plotOutput("impact_plotAcc"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Disproportionality Threshold"),
                                   p("Where there is a potential practically significant impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotAcc"),
                                   br(),
                                   ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBAcc_met"),
                                   p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair."),
                                   br(),
                                   )
                            ),
                   #UI by Env ###############################################
                   tabPanel("Environmental Metrics",
                            br(),
                            column(width=12,
                                     selectInput("metricEnv", "Metric:",
                                                 choices = list(
                                                   Environmental= c("Congested vehicle miles traveled","Carbon monoxide emissions")),
                                                 selected = "Carbon monoxide emissions"),
                            ),
                            column(width = 12,
                                   h5("Baseline Uncertainty Threshold"),
                                   p("If there is a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Practical Impact Threshold."),
                                   plotOutput(outputId = "metric_plotEnv"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Practical Impact Threshold"),
                                   p("Where a potential impact is indicated, is the impact practically significant for each population?"),
                                   plotOutput("impact_plotEnv"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Disproportionality Threshold"),
                                   p("Where there is a potential practically significant impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotEnv"),
                                   br(),
                            ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBEnv_met"),
                                   p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair."),
                                   br(),
                            )
                   ),
                   #UI by Mob ###########################
                   tabPanel("Mobility Metrics",
                            br(),
                            column(width=12,
                                     selectInput("metricMob", "Metric:",
                                                 choices = list(Mobility= c("Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
                                                                            "Average production - transit travel time")),
                                                 selected = "Average production - transit travel time"),
                            ),
                            column(width = 12,
                                   h5("Baseline Uncertainty Threshold"),
                                   p("If there is a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Practical Impact Threshold."),
                                 
                                   plotOutput(outputId = "metric_plotMob"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Practical Impact Threshold"),
                                   p("Where a potential impact is indicated, is the impact practically significant for each population?"),
                                   plotOutput("impact_plotMob"),
                                   br(),
                            ),
                            column(width = 6,
                                   h5("Disproportionality Threshold"),
                                   p("Where there is a potential practically significant impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotMob"),
                                   br(),
                            ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBMob_met"),
                                   p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair."),
                                   br(),
                            )
                   ),
                   # Results for all metrics ##############################
                   tabPanel("Results for all Metrics",
                            br(),
                            p("The table below will show instances of DI/DB for the current threshold settings accross all metrics."),
                            htmlOutput("DIDBAcc"),
                            htmlOutput("DIDBEnv"),
                            htmlOutput("DIDBMob"),
                            p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair."),
                            
                            )
                
      ) #close tabsetPanel()
    ) # close mainpanel()
  ) # close sidebarlayout()
) # close fluidpage() 

# Server##########################################

server <- function(input, output) {

  # Metric Plot Acc #######################################################
  output$metric_plotAcc <- renderPlot({
    
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricAcc 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
      mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    data_metric <- data %>%
      mutate(subtitle= case_when(
        category == "Accessibility" ~ "An increase represents a benefit. A decrease represents a burden.",
        category != "Accessibility" ~ "An increase represents a burden. A decrease represents a benefit.",
        TRUE ~ "Error!"
      ))
    subtitle <- data_metric$subtitle[1]
    
    ##DRAW THE PLOT
    
    metric_plot<- ggplot(data, aes(x=population, y= UB_nb)) +
      geom_segment( aes(x=population, xend=population, y=LB_b, yend=UB_b, color= "Build"), alpha=.65, size= 10) +
      geom_segment( aes(x=population, xend=population, y=LB_nb, yend=UB_nb, color = "No-build"), alpha=.5, size= 10) +
      geom_point( aes(x=population, y=build, color = "Build"), shape="square", size=4) +
      geom_point( aes(x=population, y=no_build, color= "No-build"), shape="square", size=4) +
      geom_point( aes(x=population, y=build), shape=20, size=1, show.legend = TRUE)+
      geom_point( aes(x=population, y=no_build), shape=20, size=1, show.legend = TRUE)+
      geom_text(aes(x=as.numeric(population) +.3, y= no_build , label=change_label),hjust="inward", size= 4)+
      scale_color_manual(name= "Scenario", values= c("Build" = "#E69F00", "No-build" = "#56B4E9"))+
      coord_flip()+
      theme_minimal() +
      theme(
        axis.ticks.y=element_blank(),
        #axis.text.y= element_blank()
        plot.title = element_text(face= "bold"))+
      labs(title = paste(metric_filter, "by population"),
           subtitle = str_wrap(subtitle, width = 33))+
      ylab(paste(metric_filter, " (", metric_unit, ")"))+
      xlab("Population")
    
    print(metric_plot)
    
  })
  # Metric Plot Env #############################################
  output$metric_plotEnv <- renderPlot({
    
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricEnv
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
      mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    data_metric <- data %>%
      mutate(subtitle= case_when(
        category == "Accessibility" ~ "An increase represents a benefit. A decrease represents a burden.",
        category != "Accessibility" ~ "An increase represents a burden. A decrease represents a benefit.",
        TRUE ~ "Error!"
      ))
    subtitle <- data_metric$subtitle[1]
    
    ##DRAW THE PLOT
    
    metric_plot<- ggplot(data, aes(x=population, y= UB_nb)) +
      geom_segment( aes(x=population, xend=population, y=LB_b, yend=UB_b, color= "Build"), alpha=.65, size= 10) +
      geom_segment( aes(x=population, xend=population, y=LB_nb, yend=UB_nb, color = "No-build"), alpha=.5, size= 10) +
      geom_point( aes(x=population, y=build, color = "Build"), shape="square", size=4) +
      geom_point( aes(x=population, y=no_build, color= "No-build"), shape="square", size=4) +
      geom_point( aes(x=population, y=build), shape=20, size=1, show.legend = TRUE)+
      geom_point( aes(x=population, y=no_build), shape=20, size=1, show.legend = TRUE)+
      geom_text(aes(x=as.numeric(population) +.3, y= no_build , label=change_label),hjust="inward", size= 4)+
      scale_color_manual(name= "Scenario", values= c("Build" = "#E69F00", "No-build" = "#56B4E9"))+
      coord_flip()+
      theme_minimal() +
      theme(
        axis.ticks.y=element_blank(),
        #axis.text.y= element_blank()
        plot.title = element_text(face= "bold"))+
      labs(title = paste(metric_filter, "by population"),
           subtitle = str_wrap(subtitle, width = 33))+
      ylab(paste(metric_filter, " (", metric_unit, ")"))+
      xlab("Population")
    
    print(metric_plot)
    
  })
  # Metric Plot Mob #########################################################
  output$metric_plotMob <- renderPlot({
    
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricMob 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
      mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    data_metric <- data %>%
      mutate(subtitle= case_when(
        category == "Accessibility" ~ "An increase represents a benefit. A decrease represents a burden.",
        category != "Accessibility" ~ "An increase represents a burden. A decrease represents a benefit.",
        TRUE ~ "Error!"
      ))
    subtitle <- data_metric$subtitle[1]
    
    ##DRAW THE PLOT
    
    metric_plot<- ggplot(data, aes(x=population, y= UB_nb)) +
      geom_segment( aes(x=population, xend=population, y=LB_b, yend=UB_b, color= "Build"), alpha=.65, size= 10) +
      geom_segment( aes(x=population, xend=population, y=LB_nb, yend=UB_nb, color = "No-build"), alpha=.5, size= 10) +
      geom_point( aes(x=population, y=build, color = "Build"), shape="square", size=4) +
      geom_point( aes(x=population, y=no_build, color= "No-build"), shape="square", size=4) +
      geom_point( aes(x=population, y=build), shape=20, size=1, show.legend = TRUE)+
      geom_point( aes(x=population, y=no_build), shape=20, size=1, show.legend = TRUE)+
      geom_text(aes(x=as.numeric(population) +.3, y= no_build , label=change_label),hjust="inward", size= 4)+
      scale_color_manual(name= "Scenario", values= c("Build" = "#E69F00", "No-build" = "#56B4E9"))+
      coord_flip()+
      theme_minimal() +
      theme(
        axis.ticks.y=element_blank(),
        #axis.text.y= element_blank()
        plot.title = element_text(face= "bold"))+
      labs(title = paste(metric_filter, "by population"),
           subtitle = str_wrap(subtitle, width = 33))+
      ylab(paste(metric_filter, " (", metric_unit, ")"))+
      xlab("Population")
    
    print(metric_plot)
    
  })

# Impact plot #####################################################  
  # Impact plot Acc #########################################################
  output$impact_plotAcc <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricAcc 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    
    # Plot the percentage change introduced by building, and how that changes with the impact threshold
    impact_plot <- ggplot(data, aes(x= population))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= -dim2, ymax= dim2, alpha= 0.2, color ="#ededed")+
      #geom_bracket(xmin= -dim2, xmax= -dim2, y.position= )+
      #geom_rect( aes(xmin = , xmax = , ymin= -dim2, ymax= dim2), alpha= 0.08)+
      #geom_curve( aes(x= as.numeric(population) +.1, xend= as.numeric(population)+.1, y= -dim2, yend= dim2), color= "black", label= "Impact Threshold")+
      #geom_hline( aes(yintercept = dim2), size= .75,color = "#6e6e6e")+
      #geom_hline( aes(yintercept = -dim2), size=.75, color = "#6e6e6e")+
      #annotate(rect, aes(x= "Low-income", xend= "Low-income", y= -dim2 ,yend= dim2))+
      #geom_segment( aes(x= 4.5, xend= 4.5, y= -dim2, yend= +dim2 ),color= "navy", size= 10)+
      geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build, color= impact), shape=20, size=4, show.legend = FALSE)+
      scale_colour_manual(values = c("Benefit within threshold" = "#858585","Burden within threshold"= "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
      geom_text(aes(x=as.numeric(population) +.2, y= delta/no_build ,label= impact),hjust="inward", size= 4)+
      geom_hline(aes(yintercept = 0), size= 1, color = "black")+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title = paste("Percent change", "by population"))+
      ylab("")+
      xlab("Population")
    print(impact_plot)
    
  })
  # Impact Plot Env ################################################
  output$impact_plotEnv <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricEnv 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    
    # Plot the percentage change introduced by building, and how that changes with the impact threshold
    impact_plot <- ggplot(data, aes(x= population))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= -dim2, ymax= dim2, alpha= 0.2, color ="#ededed")+
      #geom_bracket(xmin= -dim2, xmax= -dim2, y.position= )+
      #geom_rect( aes(xmin = , xmax = , ymin= -dim2, ymax= dim2), alpha= 0.08)+
      #geom_curve( aes(x= as.numeric(population) +.1, xend= as.numeric(population)+.1, y= -dim2, yend= dim2), color= "black", label= "Impact Threshold")+
      #geom_hline( aes(yintercept = dim2), size= .75,color = "#6e6e6e")+
      #geom_hline( aes(yintercept = -dim2), size=.75, color = "#6e6e6e")+
      #annotate(rect, aes(x= "Low-income", xend= "Low-income", y= -dim2 ,yend= dim2))+
      #geom_segment( aes(x= 4.5, xend= 4.5, y= -dim2, yend= +dim2 ),color= "navy", size= 10)+
      geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build, color= impact), shape=20, size=4, show.legend = FALSE)+
      scale_colour_manual(values = c("Benefit within threshold" = "#858585","Burden within threshold"= "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
      geom_text(aes(x=as.numeric(population) +.2, y= delta/no_build ,label= impact),hjust="inward", size= 4)+
      geom_hline(aes(yintercept = 0), size= 1, color = "black")+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title = paste("Percent change", "by population"))+
      ylab("")+
      xlab("Population")
    print(impact_plot)
    
  })
  # Impact Plot Mob ###################################################
  output$impact_plotMob <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricMob 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    data <- data %>%
      filter(metric == metric_filter)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    
    # Plot the percentage change introduced by building, and how that changes with the impact threshold
    impact_plot <- ggplot(data, aes(x= population))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= -dim2, ymax= dim2, alpha= 0.2, color ="#ededed")+
      #geom_bracket(xmin= -dim2, xmax= -dim2, y.position= )+
      #geom_rect( aes(xmin = , xmax = , ymin= -dim2, ymax= dim2), alpha= 0.08)+
      #geom_curve( aes(x= as.numeric(population) +.1, xend= as.numeric(population)+.1, y= -dim2, yend= dim2), color= "black", label= "Impact Threshold")+
      #geom_hline( aes(yintercept = dim2), size= .75,color = "#6e6e6e")+
      #geom_hline( aes(yintercept = -dim2), size=.75, color = "#6e6e6e")+
      #annotate(rect, aes(x= "Low-income", xend= "Low-income", y= -dim2 ,yend= dim2))+
      #geom_segment( aes(x= 4.5, xend= 4.5, y= -dim2, yend= +dim2 ),color= "navy", size= 10)+
      geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build, color= impact), shape=20, size=4, show.legend = FALSE)+
      scale_colour_manual(values = c("Benefit within threshold" = "#858585","Burden within threshold"= "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
      geom_text(aes(x=as.numeric(population) +.2, y= delta/no_build ,label= impact),hjust="inward", size= 4)+
      geom_hline(aes(yintercept = 0), size= 1, color = "black")+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title = paste("Percent change", "by population"))+
      ylab("")+
      xlab("Population")
    print(impact_plot)
    
  })
  # Dispro plot Acc ##############################
  output$burden_plotAcc <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricAcc 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    data <- data %>%
      filter(metric== metric_filter) %>% 
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact",
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
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
    
    burden_plot <- ggplot(dispro, aes(x = poptype))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= 1-dim3, ymax= 1+dim3, alpha= 0.2, color ="#ededed")+
      #geom_hline(aes(yintercept = 1+dim3), size= .75,color = "#6e6e6e")+
      #geom_hline(aes(yintercept = 1-dim3), size= .75,color = "#6e6e6e")+
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DB), size = 4, show.legend = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", 
                                    "Protected population affected more"= "#ff6666", 
                                    "Non-protected population affected more"= "#ff6666",
                                    "Protected population burdened more" = "#ff6666",
                                    "Protected population benefits more" = "#4a4a4a",
                                    "Non-protected population burdened more" = "#4a4a4a",
                                    "Non-protected population benefits more" = "#ff6666"
                                    ))+
      geom_hline(aes(yintercept = 1), size= 1, color = "#5e5e5e")+
      geom_text( aes(x=as.numeric(poptype)+.2, y= ratio, label = str_wrap(DB, width = 20)), hjust= "inward", size = 4)+
      scale_x_discrete(labels= c("i"= str_wrap("Low-income / Non-low-income", width = 15), "m"= str_wrap("Minority / Non-minority",width = 12)))+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title= "Ratio by population group")+
      ylab(str_wrap("Ratio,  (% change protected population) / (% change non-protected population)", width = 40))+
      xlab("Population group")
    print(burden_plot)
    
  })
  # Dispro plot Env #########################################################
  output$burden_plotEnv <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricEnv 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    #dispro_method <- input$dis_method
    
    
    data <- data %>%
      filter(metric== metric_filter) %>% 
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact",
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
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
    
    burden_plot <- ggplot(dispro, aes(x = poptype))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= 1-dim3, ymax= 1+dim3, alpha= 0.2, color ="#ededed")+
      #geom_hline(aes(yintercept = 1+dim3), size= .75,color = "#6e6e6e")+
      #geom_hline(aes(yintercept = 1-dim3), size= .75,color = "#6e6e6e")+
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DB), size = 4, show.legend = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", 
                                    "Protected population affected more"= "#ff6666", 
                                    "Non-protected population affected more"= "#ff6666",
                                    "Protected population burdened more" = "#ff6666",
                                    "Protected population benefits more" = "#4a4a4a",
                                    "Non-protected population burdened more" = "#4a4a4a",
                                    "Non-protected population benefits more" = "#ff6666"
      ))+
      geom_hline(aes(yintercept = 1), size= 1, color = "#5e5e5e")+
      geom_text( aes(x=as.numeric(poptype)+.2, y= ratio, label = str_wrap(DB, width = 20)), hjust= "inward", size = 4)+
      scale_x_discrete(labels= c("i"= str_wrap("Low-income / Non-low-income", width = 15), "m"= str_wrap("Minority / Non-minority",width = 12)))+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title= "Ratio by population group")+
      ylab(str_wrap("Ratio,  (% change protected population) / (% change non-protected population)", width = 40))+
      xlab("Population group")
    print(burden_plot)
    
  })
  #Dispro Plot Mob #############################################
  output$burden_plotMob <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricMob 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    #dispro_method <- input$dis_method
    
    
    data <- data %>%
      filter(metric== metric_filter) %>% 
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact",
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
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
    
    burden_plot <- ggplot(dispro, aes(x = poptype))+
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= 1-dim3, ymax= 1+dim3, alpha= 0.2, color ="#ededed")+
      #geom_hline(aes(yintercept = 1+dim3), size= .75,color = "#6e6e6e")+
      #geom_hline(aes(yintercept = 1-dim3), size= .75,color = "#6e6e6e")+
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DB), size = 4, show.legend = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", 
                                    "Protected population affected more"= "#ff6666", 
                                    "Non-protected population affected more"= "#ff6666",
                                    "Protected population burdened more" = "#ff6666",
                                    "Protected population benefits more" = "#4a4a4a",
                                    "Non-protected population burdened more" = "#4a4a4a",
                                    "Non-protected population benefits more" = "#ff6666"
      ))+
      geom_hline(aes(yintercept = 1), size= 1, color = "#5e5e5e")+
      geom_text( aes(x=as.numeric(poptype)+.2, y= ratio, label = str_wrap(DB, width = 20)), hjust= "inward", size = 4)+
      scale_x_discrete(labels= c("i"= str_wrap("Low-income / Non-low-income", width = 15), "m"= str_wrap("Minority / Non-minority",width = 12)))+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title= "Ratio by population group")+
      ylab(str_wrap("Ratio,  (% change protected population) / (% change non-protected population)", width = 40))+
      xlab("Population group")
    print(burden_plot)
    
  })
  # DIDB count table Acc_metric filter###########################################
  output$DIDBAcc_met <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    metric_filter <- input$metricAcc
    
    
    
    data <- data %>%
      #filter(category==cat)%>%
      filter(metric== metric_filter)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test))%>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype)%>%
      rename("Passes Uncertainty Test?" = change_test)%>%
      rename("Passes Practical Impact Test?"= impact_test)%>%
      rename("Disproportionality Test Result"= DB)%>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE)%>%
      column_spec(1,width= "20em")%>%
      column_spec(2:4, width= "5em")%>%
      column_spec(5, width= "20em")%>%
      column_spec(6, width= "5em")%>%
      column_spec(7, width= "20em")%>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
# DIDB count table Acc###########################################  
  output$DIDBAcc  <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
  
    cat <- "Accessibility"
    
    data <- data %>%
      filter(category==cat)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test))%>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
             ) %>%
      rename("Population Group" = poptype)%>%
      rename("Passes Uncertainty Test?" = change_test)%>%
      rename("Passes Practical Impact Test?"= impact_test)%>%
      rename("Disproportionality Test Result"= DB)%>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE)%>%
      column_spec(1,width= "20em")%>%
      column_spec(2:4, width= "5em")%>%
      column_spec(5, width= "20em")%>%
      column_spec(6, width= "5em")%>%
      column_spec(7, width= "20em")%>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
  # DIDB count table Env_metric filter###########################################
  output$DIDBEnv_met <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    metric_filter <- input$metricEnv
    
    
    
    data <- data %>%
      #filter(category==cat)%>%
      filter(metric== metric_filter)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
    
    DIDB_clean <- DIDB %>%
      mutate(Metric = metric) %>%
      select(Metric, poptype, change_test, impact_test, DB, instance, DB_reason) %>%
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test)) %>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color = ifelse(instance == "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype) %>%
      rename("Passes Uncertainty Test?" = change_test) %>%
      rename("Passes Practical Impact Test?"= impact_test) %>%
      rename("Disproportionality Test Result"= DB) %>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE) %>%
      column_spec(1,width= "20em") %>%
      column_spec(2:4, width= "5em") %>%
      column_spec(5, width= "20em") %>%
      column_spec(6, width= "5em") %>%
      column_spec(7, width= "20em") %>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
  # DIDB count table Env###########################################  
  output$DIDBEnv <-  renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    cat <- "Environmental"
    
    data <- data %>%
      filter(category==cat) %>%
      #slider1 sets confidence level, use this to control error
      mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
      #mutate(delta = build - no_build) %>%
      mutate(error_b = build*error_aug) %>%
      mutate(error_nb = no_build*error_aug) %>%
      mutate(LB_b = build-error_b) %>%
      mutate(UB_b = build+error_b) %>%
      mutate(LB_nb = no_build - error_nb) %>%
      mutate(UB_nb = no_build + error_nb) %>%
      # check if b range distinct from nb
      mutate(real_change = case_when(
        (UB_b < LB_nb) | (UB_nb < LB_b) ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      mutate(change_label = case_when(
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
      select( metric, population,real_change) %>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
      arrange(factor(poptype)) %>%
      spread(population, real_change) %>%
      mutate(change_type = case_when( 
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
        (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only exceeds uncertainty for protected population",
        (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only exceeds uncertainty for non-protected population",
        TRUE ~ "something else happened")) %>%
      mutate(change_test = case_when(
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Yes",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No",
        (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Yes",
        (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Yes",
      )) %>%
      select(metric, poptype, change_type, change_test)
    
    #make an impact table, bring together all impact options by population in a table
    impact_table <- data %>%
      select( metric,population,delta, no_build,real_change, impact) %>%
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
      )) %>%
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
    
    DIDB_clean <- DIDB %>%
      mutate(Metric = metric) %>%
      select(Metric, poptype, change_test, impact_test, DB, instance, DB_reason) %>%
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test)) %>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", 
                                  color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype) %>%
      rename("Passes Uncertainty Test?" = change_test) %>%
      rename("Passes Practical Impact Test?"= impact_test) %>%
      rename("Disproportionality Test Result"= DB) %>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE) %>%
      column_spec(1,width= "20em") %>%
      column_spec(2:4, width= "5em") %>%
      column_spec(5, width= "20em") %>%
      column_spec(6, width= "5em") %>%
      column_spec(7, width= "20em") %>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
  # DIDB count table Mob_metric filter###########################################
  output$DIDBMob_met <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    metric_filter <- input$metricMob
    
    
    
    data <- data %>%
      #filter(category==cat)%>%
      filter(metric== metric_filter) %>%
      #slider1 sets confidence level, use this to control error
      mutate(error_aug = for_error/ 1.96*qnorm(1-(1-dim1)/2)) %>%
      #mutate(delta = build - no_build) %>%
      mutate(error_b = build*error_aug) %>%
      mutate(error_nb =no_build*error_aug) %>%
      mutate(LB_b = build-error_b) %>%
      mutate(UB_b = build+error_b) %>%
      mutate(LB_nb = no_build - error_nb) %>%
      mutate(UB_nb = no_build + error_nb) %>%
      # check if b range distinct from nb
      mutate(real_change = case_when(
        (UB_b < LB_nb) | (UB_nb < LB_b) ~ TRUE,
        TRUE ~ FALSE
      )) %>%
      mutate(change_label = case_when(
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
        (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only exceeds uncertainty for protected population",
        (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only exceeds uncertainty for non-protected population",
        TRUE ~ "something else happened")) %>%
      mutate(change_test = case_when(
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Yes",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No",
        (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Yes",
        (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Yes",
      )) %>%
      select(metric, poptype, change_type, change_test)
    
    #make an impact table, bring together all impact options by population in a table
    impact_table <- data %>%
      select( metric,population,delta, no_build,real_change, impact) %>%
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
      )) %>%
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        
        TRUE ~ "something elese happend, problem!")) %>%
      mutate(DB_reason = case_when (
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
    
    DIDB_clean <- DIDB %>%
      mutate(Metric = metric) %>%
      select(Metric, poptype, change_test, impact_test, DB, instance, DB_reason) %>%
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test)) %>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype) %>%
      rename("Passes Uncertainty Test?" = change_test) %>%
      rename("Passes Practical Impact Test?"= impact_test) %>%
      rename("Disproportionality Test Result"= DB) %>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE) %>%
      column_spec(1,width= "20em") %>%
      column_spec(2:4, width= "5em") %>%
      column_spec(5, width= "20em") %>%
      column_spec(6, width= "5em") %>%
      column_spec(7, width= "20em") %>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
  # DIDB count table Mob###########################################  
  output$DIDBMob <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1)*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    cat <- "Mobility"
    
    data <- data %>%
      filter(category==cat)%>%
      #slider1 sets confidence level, use this to control error
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
        real_change == TRUE ~ "Potential impact",
        real_change == FALSE ~ "No potential impact"
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
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Potential impact for both",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No potential impact for either",
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
        (change_type == "No potential impact for either") ~ "No",
        
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
        (change_type == "No potential impact for either") ~ "No potential impact for either population",
        
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
      #arrange(desc(instance), DB, desc(impact_test), desc(change_test))%>%
      mutate(change_test = cell_spec(change_test, "html", 
                                     #color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", 
                                     #color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", 
                            #color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype)%>%
      rename("Passes Uncertainty Test?" = change_test)%>%
      rename("Passes Practical Impact Test?"= impact_test)%>%
      rename("Disproportionality Test Result"= DB)%>%
      rename("DI or DB?" = instance) %>%
      rename("Reason for DI/DB Result"= DB_reason)
    
    
    kable(DIDB_clean, format = "html", escape= FALSE)%>%
      column_spec(1,width= "20em")%>%
      column_spec(2:4, width= "5em")%>%
      column_spec(5, width= "20em")%>%
      column_spec(6, width= "5em")%>%
      column_spec(7, width= "20em")%>%
      kable_styling(font_size = 12,
                    bootstrap_options = c( "hover", "condensed")
      )
  })
 
}

shinyApp(ui, server)