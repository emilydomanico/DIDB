library(shiny)
library(shinyWidgets)
library(tidyverse)
library(hrbrthemes)
library(readr)
library(shinyBS)
library(formattable)

alldata <- read_csv("data.csv")



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    p {
    font-size: 11px;
    }
    h5 {
    font-weight: bold;
    }
    ")
               
    )
  ),
  withMathJax(),
  titlePanel("DI DB Thresholds"),
  setSliderColor(c(rep("DimGray",3)), c(1,2,3)),
  chooseSliderSkin("Flat"),
  sidebarLayout(
    sidebarPanel(
      #to fix side panel position
      #style = "position: fixed; width:inherit;",
      selectInput("metric", "Metric:",
                  choices = c("Access to retail amenities by transit", "Access to higher education by transit","Access to healthcare facilities by transit", "Access to jobs by transit","Congested vehicle miles traveled","Carbon monoxide emissions", "Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
                              "Average production - transit travel time"), selected = "Carbon monoxide emissions"),
      
      hr(),
      
      #Sliders to toggle sensitivity
      
      sliderInput("Dim1", label = "Step 1: Forecasting Error Confidence Interval Threshold", min = 0, max = 100, post= " %", value = 10, step = 1),
      p("This threshold sets the sensitivity for detecting if a real change exists. The slider represents the confidence interval — a higher percentage produces a greater range of likely values, and a less of a chance of identifying a real change."),
      #bsPopover("Dim1", "Confidence Interval Threshold:", "This threshold sets the sensitivity for detecting if a real change exists. The slider represents the confidence interval — a higher percentage produces a greater range of likely values, and a less of a chance of identifying a real change.", "bottom"),
      br(),
      
      sliderInput("Dim2", label = "Step 2: Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
      p("This threshold sets the sensitivity for determining if an impact is between the build and no-build scenarios is meaningful. The slider represents a percent change tolerance.. At zero, any change between the build and no-build scenarios is considered an impact. As the threshold increases, the likelihood of identifying an adverse effect decreases. Change between scenarios is calculated as percent change:"),
      withMathJax("$$\\scriptsize\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build}} \\cdot 100$$"),
      #withMathJax("\\(\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build} \\cdot 100}\\)"),
      #p("((Build - (No-build) ) / (No-build))*100."),
      p(" "),
      p("If an impact is found, we indicate whether it is a benefit or a burden based on the directionality of the metric (i.e., whether an increase is a benefit or a burden)."),
      #can fix names of options later on.
      br(),
      
      sliderInput("Dim3", label = "Step 3: Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
      #selectInput("dis_method", label = "Disproportionality method to visualize:", choices = c("Percent Difference", "Ratio"), selected = "Ratio")
      p("This threshold determines if the impacts found in the previous step disproportionately affect the minority or low-income population more than the nonminority or non-low-income population."),
      p("Disproportionality is calculated as a ratio, comparing the absolute value of the percent change for the protected population (from the second step) to the absolute value of the percent change non-protected population.")
    ),
    
    mainPanel(
      tabsetPanel( type = "tabs",
                   tabPanel("Investigate by Metric",
                            br(),
                            h5("Step 1:"),
                            p("We need to test if there is a real change present. If there is a real change for either the protected population or the non-protected population, we will proceed to step 2."),
                            plotOutput(outputId = "metric_plot"),
                            #textOutput(), note:
                            tableOutput("change_result"),
                            br(),
                            textOutput("change"),
                            #p("PLACEHOLDER : Reactive text indicating if there is a real change for one or more metrics. If so proceed to next step. Or default DIDB."),
                            hr(),
                            column(width = 6,
                                   h5("Step 2:"),
                                   p("Where there is a real change indicated, what kind of impact is projected between the Build and No-build scenarios? How does the No-build scenario impact each population?"),
                                   plotOutput("impact_plot"),
                                   br(),
                                   #p("Reactive text indicating if there is a impact that exceeds the threshold set. If so proceed to next step. Or default DIDB.")
                            ),
                            column(width = 6,
                                   h5("Step 3:"),
                                   p("How much of a benefit or burden would the protected population receive compared to the non-protected population? Does that difference constitute a disparate impact or disproportionate burden?"),
                                   plotOutput("burden_plot"),
                                   br(),
                                   #p("Reactive text indicating if there is a disproportionate burden. Prompt to see how slider inputs work accross all metric in the next tab.")
                            )),
                   tabPanel("Results for all Metrics",
                            br(),
                            p("The table below will show instances of DIDB for the current threshold settings accross all metrics."),
                            formattableOutput("DIDB"),
                            p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair.")) #,
#                    tabPanel("DIDB Rules",
#                             br(),
#                             p("Definitions:"),
#                             p("A disparate impact is a facially neutral policy or practice that disproportionately affects
# members of a group identified by race, color, or national origin, where the policy or
# practice lacks a substantial legitimate justification, and where there exists one or more
# alternatives that would serve the same legitimate objectives but with a less
# disproportionate effect. A minority person is one who identifies as Black or African
# American; American Indian or Alaskan Native; Asian; Native Hawaiian or other Pacific
# Islander; and/or Hispanic or Latino/a/x."),
#                             p("A disproportionate burden is a neutral policy or practice that disproportionately affects
# low-income populations more than non-low income populations. The MPO considers a
# person as low income as one whose family income is at or below 200% of the poverty
# level for their family size."), 
#                             p("Step 1:"),
#                             p("Step 2:"),
#                             p("Step 3:"))
      ) #close tabsetPanel()
    ) # close mainpanel()
  ) # close sidebarlayout()
) # close fluidpage()


server <- function(input, output, session) {
  
  output$metric_plot <- renderPlot({
    
    #user imputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    
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
        category == "Accessibility" ~ "An increase introduced by the build scenario is a benefit. A decrease introduced by the build scenario is a burden.",
        category != "Accessibility" ~ "An increase introduced by the build scenario is a burden. A decrease introduced by the build scenario is a benefit.",
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
           subtitle = str_wrap(subtitle, width = 58))+
      ylab(paste(metric_filter, " (", metric_unit, ")"))+
      xlab("Population")
    
    print(metric_plot)
    
  })
  output$change <- renderText({
    
    #user imputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    
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
      select( population,real_change)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
      arrange(factor(poptype)) %>%
      spread(population, real_change) %>%
      mutate(change_type = case_when( 
        (Minority == TRUE & Non_minority  == TRUE) | (Low_income == TRUE & Non_low_income == TRUE) ~ "Real change for both populations",
        (Minority == FALSE & Non_minority  == FALSE) | (Low_income == FALSE & Non_low_income == FALSE) ~ "No real change for both populations",
        (Minority == TRUE & Non_minority == FALSE) | (Low_income == TRUE & Non_low_income == FALSE) ~ "Only real change for the protected population",
        (Minority == FALSE & Non_minority == TRUE) | (Low_income == FALSE & Non_low_income == TRUE) ~ "Only real change for the non-protected population",
        TRUE ~ "something else happend")) %>%
      select(poptype, change_type)
    
    change <- change_type
    
    income_change <- change$change_type[ change$poptype=="i"]
    min_change <- change$change_type [ change$poptype=="m"]
    
    paste("For the metric ", tolower(metric_filter), "at the confidence interval of ", input$Dim1, "%, ", "there is projected to be ", tolower(income_change), " in the income population group, and there is projected to be ", tolower(min_change), " in the minority population group." )
    
  })
  
  output$impact_plot <- renderPlot({
    #user imputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
    
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
      geom_rect( aes(xmin = -Inf, xmax = Inf, ymin= -dim2, ymax= dim2), alpha= 0.08, color ="#f2f2f2")+
      #geom_hline( aes(yintercept = dim2), size= .75,color = "#6e6e6e")+
      #geom_hline( aes(yintercept = -dim2), size=.75, color = "#6e6e6e")+
      geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build, color= impact), shape=20, size=4, show.legend = FALSE)+
      scale_colour_manual(values = c("Benefit within threshold" = "#858585", "Burden within threshold"= "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
      geom_hline(aes(yintercept = 0), size= 1, color = "#5e5e5e")+
      geom_text(aes(x=as.numeric(population) +.25, y= delta/no_build ,label= str_wrap(impact, width= 14)),hjust="inward", size= 4)+
      scale_y_continuous(labels = scales::percent)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title = paste("Percent change", "by population"))+
      ylab("")+
      xlab("Population")
    print(impact_plot)
    
    
  })
  
  output$burden_plot <- renderPlot({
    #user imputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
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
      select( population,real_change)%>%
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
      select(poptype, change_type)
    
    change <- change_type
    
    income_change <- change$change_type[ change$poptype=="i"]
    min_change <- change$change_type [ change$poptype=="m"]
    
    #change text to render (reactive version)
    
    paste("For the metric ", tolower(metric_filter), "at the confidence interval of ", input$Dim1, "%, ", "there is ", tolower(income_change), " in the income population group, and there is", tolower(min_change), " in the minority population group." )
    
    
    #make an impact table, bring together all impact options by population in a table
    impact_table <- data %>%
      select( population,delta, no_build,real_change, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
      #find percent change between no_build and build
      mutate( per_change = delta/no_build) %>%
      select(poptype, population, real_change, per_change, impact) %>%
      # control order of entries
      arrange(factor(population, levels = c("Low_income","Non_low_income", "Minority","Non_minority")))
    
    diff <- impact_table %>%
      select(poptype, population, per_change) %>%
      arrange(factor(poptype)) %>%
      spread(population, per_change) %>%
      mutate(difference = case_when(
        (is.na(Low_income) & is.na(Non_low_income)) ~ Minority - Non_minority,
        (is.na(Minority) & is.na(Non_minority)) ~ Low_income - Non_low_income,
        # must be a numeric error
        #na real
        TRUE ~ NA_real_)) %>%
      mutate(ratio = case_when(
        (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
        (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
        # must be a numeric error
        TRUE ~ NA_real_)) %>%
      select(poptype, difference, ratio)
    
    # investigate what kind of impact
    impact_type <- impact_table %>%
      select( poptype, population, impact) %>%
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
      select(poptype, type)
    
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      left_join(change_type) %>%
      mutate(DB = case_when(
        ratio >= (1 + dim3) ~ "Protected population affected more",
        ratio <= (1 - dim3) ~ "Non-protected population affected more",
        ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
        TRUE ~ "Something else happened. problem!"
      )) 
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
    
    burden_plot <- ggplot(dispro, aes(x = poptype))+
      geom_rect( aes(xmin = -Inf, xmax = Inf, ymin= 1-dim3, ymax= 1+dim3), alpha= 0.08, color ="#ededed")+
      #geom_hline(aes(yintercept = 1+dim3), size= .75,color = "#6e6e6e")+
      #geom_hline(aes(yintercept = 1-dim3), size= .75,color = "#6e6e6e")+
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DB), shape = 20, size = 4, show.legned = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", "Protected population affected more"= "#ff6666", "Non-protected population affected more"= "#ff6666"))+
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
  
  output$DIDB <- renderFormattable({
    #user imputs
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    
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
        (change_type == "No real change for both") ~ "No",
        # Note, might need to add change_type != "No real change for both" to all further conditions...
        
        (type == "Benefit for both") & (DB == "Protected population affected more") ~ "No",
        (type == "Benefit for both") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "No",
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Benefits protected population, burdens non-protected population") ~ "No",
        
        (type == "Only benefits protected population, burdens non-protected population within threshold") ~ "No",
        
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only burdens non-protected population, benefits protected population within threshold") ~ "No",
        
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Protected population affected more") ~ "No",
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Impact within threshold for both") ~ "No",
        
        (type == "Burdens protected population, benefits non-protected population") ~ "Yes",
        
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
      mutate(DB_reason = case_when (
        (change_type == "No real change for both") ~ "No real change present for either population",
        # Note, might need to add change_type != "No real change for both" to all further conditions...
        
        (type == "Benefit for both") & (DB == "Protected population affected more") ~ "Does not exceed the disproportionality threshold",
        (type == "Benefit for both") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "Does not exceed the disproportionality threshold",
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only benefits protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Benefits protected population, burdens non-protected population") ~ "No adverse impact for protected population",
        
        (type == "Only benefits protected population, burdens non-protected population within threshold") ~ "No adverse impact for protected population",
        
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only benefits non-protected population, benefits protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only benefits non-protected population, burdens protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Only burdens non-protected population, benefits protected population within threshold") ~ "No adverse impact for protected population",
        
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Protected population affected more") ~ "Does not exceed the disproportionality threshold",
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Non-protected population affected more") ~ "Does not exceed the disproportionality threshold",
        (type == "Only burdens non-protected population, burdens protected population within the threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Impact within threshold for both") ~ "Does not exceed impact threshold",
        
        (type == "Burdens protected population, benefits non-protected population") ~ "Adverse impact for protected population",
        
        (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only burdens protected population, benefits non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Non-protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Only burdens protected population, burdens non-protected population within threshold") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        (type == "Burden for both") & (DB == "Protected population affected more") ~ "Exceeds the disproportionality threshold",
        (type == "Burden for both") & (DB == "Non-protected population affected more") ~ "Does not exceed the disproportionality threshold",
        (type == "Burden for both") & (DB == "Disproportionality within threshold") ~ "Does not exceed the disproportionality threshold",
        
        TRUE ~ "something elese happend, problem!"))%>%
      select(metric, poptype, instance, DB_reason)
    
    DIDB[DIDB== "i"] <- "I"
    DIDB[DIDB== "m"] <- "M"
    
    DIDB_clean <- DIDB%>%
      mutate(Metric = metric)%>%
      select(Metric, poptype, instance, DB_reason)%>%
      rename("Population Group" = poptype)%>%
      rename("DIDB" = instance) %>%
      rename("Reason"= DB_reason)
    
    
    formattable(DIDB_clean,
                align= c("l", "c", "c", "l"),
                list(
                  `DIDB`= formatter("span", 
                                    style = x ~ ifelse(x == "Yes", style(color= "red", font.weight= "bold"), style(color="green"))
                  ) 
                  
                ))
  })
  #addPopover(session, "metric_plot", "Change confidence interval:", "To change the confidence interval applied to the forecasting error, use the Forecasting Error Confidence Inverval threshold slider to the right.", trigger = "hover")
  #addPopover(session, "impact_plot", "Change impact threshold:", "To change the impact threshold tolerance (gray polygon), use the Impact Threshold slider to the right.", trigger = "hover")
  #addPopover(session, "burden_plot", "Change disproportionality threshold:", "To change the disproportionality tolerance (gray polygon), use the Disproportionality Threshold slider to the right.", trigger = "hover")
}

shinyApp(ui, server)