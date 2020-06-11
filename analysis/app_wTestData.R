library(shiny)
library(shinyWidgets)
library(tidyverse)
library(hrbrthemes)
library(readr)
library(DT)

alldata <- read_csv("data_wtest.csv")



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    p {
    font-size: 10px;
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
      style = "position: fixed; width:inherit;",
      selectInput("metric", "Metric:",
                  choices = c("Retail amenities", "Higher education","Healthcare facilities", "Jobs by transit","Congested VMT","Carbon monoxide emissions", "Average attraction - highway", "Average production - highway","Average attraction - transit",
                              "Average production - transit", "Controversial Barricade"), selected = "Controversial Barricade"),
      
      hr(),
      
      #Sliders to toggle sensitivity
      
      sliderInput("Dim1", label = "Confidence Interval:", min = 0, max = 100, value = 10, step = 1),
      p("This threshold sets the sensitivity for detecting if a real change exists, given the amount of forecasting error in the model. This slider sets the confidence interval."),
      hr(),
      
      sliderInput("Dim2", label = "Impact Threshold:", min = 0, max = 20, value = 2, step = .1),
      p("This threshold sets the sensitivity to determining if a meaningful impact is introduced between the build and no-build scenarios. At zero, any change introduced by building is considered impactful. As the threshold increases, we decrease sensitivity to indicating whether a change is impactful. Change between scenarios is calculated as a percent: "),
      withMathJax("$$\\scriptsize\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build}} \\cdot 100$$"),
      #withMathJax("\\(\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build} \\cdot 100}\\)"),
      #p("((Build - (No-build) ) / (No-build))*100."),
      p(" "),
      p("If impact is reported, we indicate whether it is a benefit or a burden based on the metric."),
      #can fix names of options later on.
      hr(),
      
      sliderInput("Dim3", label = "Disproportionality Threshold:", min = 0, max = 30, value = 5, step = 1),
      #selectInput("dis_method", label = "Disproportionality method to visualize:", choices = c("Percent Difference", "Ratio"), selected = "Ratio")
      p("Set the sensitivity to determine if there is a disproportionate change introduced between populations. 
        Disproportionality is calculated as a ratio comparing percent change in the protected population to the percent change in the non-protected population. 
        At a ratio of 1, both protected and non-protected populations experience the same percent of change.")
    ),
    
    mainPanel(
      column( width = 12,
              plotOutput(outputId = "metric_plot")
      ),
      column(width = 6,
             plotOutput("impact_plot")
      ),
      column(width = 6,
             plotOutput("burden_plot")),
      column(width = 12,
             tableOutput("DIDB"))
    )
  ) # close sidebarlayout()
) # close fluidpage()


server <- function(input, output) {
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
      #step 1 from spreadsheet
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
                                 TRUE ~ "Impact within threshold"))
    
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
        category == "Accessibility" ~ "An increase introduced by building is a benefit. A decrease introduced by buiding is a burden.",
        category != "Accessibility" ~ "An increase introduced by building is a burden. A decrease introduced by building is a benefit.",
        TRUE ~ "Error!"
      ))
    subtitle <- data_metric$subtitle[1]
    
    ##DRAW THE PLOT
    ## Note, opportunitiy to draw metric_plot and impact_plot together with shared horizontal axis 
    ## Similar to :https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
    
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
           subtitle = str_wrap(subtitle, width = 48))+
      ylab(paste(metric_filter, " (", metric_unit, ")"))+
      xlab("Population")
    print(metric_plot)
    
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
      #step 1 from spreadsheet
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
                                 TRUE ~ "Impact within threshold"))
    
    #make an impact table, bring together all impact options by population in a table
    impact_table <- data %>%
      select( population,delta, no_build, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
      #find percent change between no_build and build
      mutate( per_change = delta/no_build) %>%
      select(poptype, population, per_change, impact) %>%
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
        (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
        (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected, burdens non-protected",
        (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected, benefits non-protected",
        (Minority == "Benefit" & Non_minority == "Impact within threshold") | (Low_income == "Benefit" & Non_low_income == "Impact within threshold") ~ "Only benefits protected population",
        (Minority == "Burden" & Non_minority == "Impact within threshold") | (Low_income == "Burden" & Non_low_income == "Impact within threshold") ~ "Only burdens protected population",
        (Minority == "Impact within threshold" & Non_minority == "Benefit") | (Low_income == "Impact within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population",
        (Minority == "Impact within threshold" & Non_minority == "Burden") | (Low_income == "Impact within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population",
        (Minority == "Impact within threshold" & Non_minority  == "Impact within threshold") | (Low_income == "Impact within threshold" & Non_low_income == "Impact within threshold") ~ "Impact within threshold for both",
        TRUE ~ "something else happend")) %>%
      select(poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DB = case_when(
        ratio >= (1 + dim3) ~ "Protected population affected more",
        ratio <= (1 - dim3) ~ "Non-protected population affected more",
        ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
        TRUE ~ "Something else happened. problem!"
      )) 
    
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
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
      geom_rect( aes(xmin = -Inf, xmax = Inf, ymin= -dim2, ymax= dim2), alpha= 0.08, color ="#ededed")+
      #geom_hline( aes(yintercept = dim2), size= .75,color = "#6e6e6e")+
      #geom_hline( aes(yintercept = -dim2), size=.75, color = "#6e6e6e")+
      geom_segment( aes(x=population, xend= population, y= 0,yend=delta/no_build, color= impact), shape=20, size=4, show.legend = FALSE)+
      scale_colour_manual(values = c("Impact within threshold" = "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
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
      #step 1 from spreadsheet
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
                                 TRUE ~ "Impact within threshold"))
    
    #make an impact table, bring together all impact options by population in a table
    impact_table <- data %>%
      select( population,delta, no_build, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
      #find percent change between no_build and build
      mutate( per_change = delta/no_build) %>%
      select(poptype, population, per_change, impact) %>%
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
        (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
        (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected, burdens non-protected",
        (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected, benefits non-protected",
        (Minority == "Benefit" & Non_minority == "Impact within threshold") | (Low_income == "Benefit" & Non_low_income == "Impact within threshold") ~ "Only benefits protected population",
        (Minority == "Burden" & Non_minority == "Impact within threshold") | (Low_income == "Burden" & Non_low_income == "Impact within threshold") ~ "Only burdens protected population",
        (Minority == "Impact within threshold" & Non_minority == "Benefit") | (Low_income == "Impact within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population",
        (Minority == "Impact within threshold" & Non_minority == "Burden") | (Low_income == "Impact within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population",
        (Minority == "Impact within threshold" & Non_minority  == "Impact within threshold") | (Low_income == "Impact within threshold" & Non_low_income == "Impact within threshold") ~ "Impact within threshold for both",
        TRUE ~ "something else happend")) %>%
      select(poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DB = case_when(
        ratio >= (1 + dim3) ~ "Protected population affected more",
        ratio <= (1 - dim3) ~ "Non-protected population affected more",
        ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
        TRUE ~ "Something else happened. problem!"
      )) 
    
    dispro$poptype <- factor(dispro$poptype, levels = c("m","i"))
    
    #change underscores back to hyphens
    data[data=="Low_income"] <- "Low-income"
    data[data=="Non_low_income"] <- "Non-low-income"
    data[data=="Non_minority"] <- "Non-minority"
    
    #factor population with levels to control display order in plot
    data$population <- factor(data$population, levels = c("Non-minority", "Minority","Non-low-income", "Low-income"))  
    #extract unit for horizontal axis label
    metric_unit <- data$metric_unit[1]
    
    
    burden_plot <- ggplot(dispro, aes(x = poptype))+
      geom_rect( aes(xmin = -Inf, xmax = Inf, ymin= 1-dim3, ymax= 1+dim3), alpha= 0.08, color ="#ededed")+
      #geom_hline(aes(yintercept = 1+dim3), size= .75,color = "#6e6e6e")+
      #geom_hline(aes(yintercept = 1-dim3), size= .75,color = "#6e6e6e")+
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DB), shape = 20, size = 4, show.legned = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", "Protected population affected more"= "#ff6666", "Non-protected population affected more"= "#ff6666"))+
      geom_hline(aes(yintercept = 1), size= 1, color = "black")+
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
  
  output$DIDB <- renderTable({
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
                                 TRUE ~ "Impact within threshold"))
    
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
        (Minority == "Burden" & Non_minority  == "Burden") | (Low_income == "Burden" & Non_low_income == "Burden") ~ "Burden for both",
        (Minority == "Benefit" & Non_minority == "Burden") | (Low_income == "Benefit" & Non_low_income == "Burden") ~ "Benefits protected, burdens non-protected",
        (Minority == "Burden" & Non_minority == "Benefit") | (Low_income == "Burden" & Non_low_income == "Benefit") ~ "Burdens protected, benefits non-protected",
        (Minority == "Benefit" & Non_minority == "Impact within threshold") | (Low_income == "Benefit" & Non_low_income == "Impact within threshold") ~ "Only benefits protected population",
        (Minority == "Burden" & Non_minority == "Impact within threshold") | (Low_income == "Burden" & Non_low_income == "Impact within threshold") ~ "Only burdens protected population",
        (Minority == "Impact within threshold" & Non_minority == "Benefit") | (Low_income == "Impact within threshold" & Non_low_income == "Benefit") ~"Only benefits non-protected population",
        (Minority == "Impact within threshold" & Non_minority == "Burden") | (Low_income == "Impact within threshold" & Non_low_income == "Burden") ~"Only burdens non-protected population",
        (Minority == "Impact within threshold" & Non_minority  == "Impact within threshold") | (Low_income == "Impact within threshold" & Non_low_income == "Impact within threshold") ~ "Impact within threshold for both",
        TRUE ~ "something else happend")) %>%
      select(metric,poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DB = case_when(
        ratio >= (1 + dim3) ~ "Protected population affected more",
        ratio <= (1 - dim3) ~ "Non-protected population affected more",
        ((1- dim3) < ratio) | (ratio < (1 +dim3)) ~ "Disproportionality within threshold",
        TRUE ~ "Something else happened. problem!"
      )) 
    
    DIDB <- dispro %>%
      select(metric, poptype, ratio, type, DB) %>%
      mutate(instance = case_when (
        # right now, takes first two cases regardless of whether dispropotionality is within the threshold
        (type == "Benefit for both") & (DB == "Protected population affected more") ~ "No",
        (type == "Benefit for both") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Benefit for both") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only benefits protected population") & (DB == "Protected population affected more") ~ "No",
        (type == "Only benefits protected population") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Only benefits protected population") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Benefits protected, burdens non-protected") & (DB == "Protected population affected more") ~ "No",
        (type == "Benefits protected, burdens non-protected") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Benefits protected, burdens non-protected") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only benefits non-protected population") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Only benefits non-protected population") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Only burdens non-protected population") & (DB == "Protected population affected more") ~ "No",
        (type == "Only burdens non-protected population") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Only burdens non-protected population") & (DB == "Disproportionality within threshold") ~ "No disproportionality tested",
        
        (type == "Impact within threshold for both") & (DB == "Protected population affected more") ~ "No disproportionality tested",
        (type == "Impact within threshold for both") & (DB == "Non-protected population affected more") ~ "No disproportionality tested",
        (type == "Impact within threshold for both") & (DB == "Disproportionality within threshold") ~ "No disproportionality tested",
        
        (type == "Burdens protected, benefits non-protected") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Burdens protected, benefits non-protected") & (DB == "Non-protected population affected more") ~ "Yes",
        (type == "Burdens protected, benefits non-protected") & (DB == "Disproportionality within threshold") ~ "No disproportionality tested",
        
        (type == "Only burdens protected population") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Only burdens protected population") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Only burdens protected population") & (DB == "Disproportionality within threshold") ~ "No",
        
        (type == "Burden for both") & (DB == "Protected population affected more") ~ "Yes",
        (type == "Burden for both") & (DB == "Non-protected population affected more") ~ "No",
        (type == "Burden for both") & (DB == "Disproportionality within threshold") ~ "No",
        
        TRUE ~ "something elese happend, problem!"))
    
    DIDB_clean <- DIDB%>%
      mutate(Metric = metric)%>%
      select(Metric, poptype, instance)%>%
      rename("Population Group" = poptype)%>%
      rename("Disperate Impact or Disproportionate Burden" = instance)
    
    
    DIDB_clean
    
  })
  
}

shinyApp(ui, server)
