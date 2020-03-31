library(shiny)
library(shinyWidgets)
library(tidyverse)
library(hrbrthemes)
library(readr)

alldata <- read_csv("data.csv")

ui <- fluidPage(
  titlePanel("DI DB Thresholds"),
  setSliderColor(c(rep("DimGray",3)), c(1,2,3)),
  chooseSliderSkin("Flat"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Metric:",
                  choices = c("Retail amenities", "Higher education","Healthcare facilities", "Jobs by transit","Congested VMT","Carbon monoxide emissions", "Average attraction - highway", "Average production - highway","Average attraction - transit",
                              "Average production - transit"), selected = "Carbon monoxide emissions"),
      
      hr(),
      
      #Sliders to toggle sensitivity
    
      sliderInput("Dim1", label = "Forecasting Error Threshold:", min = 0, max = 100, value = 10, step = .5),
      p(""),
      hr(),
  
      sliderInput("Dim2", label = "Impact Threshold:", min = 0, max = 100, value = 1, step = .5),
      p("How much impact is meaningful?"),
      #can fix names of options later on.
      hr(),
      
      sliderInput("Dim3", label = "Burden Threshold:", min = 0, max = 100, value = 5, step = .5),
      #selectInput("dis_method", label = "Disproportionality method to visualize:", choices = c("Percent Difference", "Ratio"), selected = "Ratio")
    ),
  
  mainPanel(
    column( width = 12,
    plotOutput(outputId = "metric_plot")
    ),
    column(width = 6,
   plotOutput("impact_plot")
   ),
    column(width = 6,
    plotOutput("burden_plot"))
  )
))


server <- function(input, output) {
  output$metric_plot <- renderPlot({
    
    #user imputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
    # percentage threshold set by slider
    dim1 <- input$Dim1*0.01
    dim2 <- input$Dim2*0.01
    dim3 <- input$Dim3*0.01
    
    dispro_method <- input$dis_method

    
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
      select( population,delta, no_build, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
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
        TRUE ~ 999999)) %>%
      mutate(ratio = case_when(
        #potentially take the absolute value in the outputs
        (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
        (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
        # must be a numeric error
        TRUE ~ 999999)) %>%
      select(poptype, difference, ratio)
    
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
        TRUE ~ "something else happend")) %>%
      select(poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DIDB = case_when(
        #note, check spelling....
        #note, more conditions to bring in 
        ratio > 1 + dim3 ~ "Protected population changes more",
        ratio < 1 - dim3 ~ "Non-protected population changes more",
        1- dim3 < ratio | ratio < 1 +dim3 ~ "Disproportionality within threshold",
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
    
    
    ##DRAW THE PLOT
    
    metric_plot<- ggplot(data, aes(x=population, y= UB_nb)) +
      geom_segment( aes(x=population, xend=population, y=LB_b, yend=UB_b, ), color= "#E69F00", alpha=.65, size= 10, show.legend = TRUE) +
      geom_segment( aes(x=population, xend=population, y=LB_nb, yend=UB_nb),color= "#56B4E9", alpha=.5, size= 10, show.legend = TRUE) +
      geom_point( aes(x=population, y=build),color= "#E69F00", shape="square", size=4, show.legend = TRUE) +
      geom_point( aes(x=population, y=no_build), color = "#56B4E9", shape="square", size=4, show.legend = TRUE) +
      geom_point( aes(x=population, y=build), shape=20, size=1, show.legend = TRUE)+
      geom_point( aes(x=population, y=no_build), shape=20, size=1, show.legend = TRUE)+
      geom_text(aes(x=as.numeric(population) +.3, y= no_build , label=impact),hjust="inward", size= 4)+
      coord_flip()+
      theme_minimal() +
      theme(
        axis.ticks.y=element_blank(),
        #axis.text.y= element_blank()
        plot.title = element_text(face= "bold"))+
      labs(title = paste(metric_filter, "by population"))+
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
    
    dispro_method <- input$dis_method
    
    
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
      select( population,delta, no_build, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
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
        TRUE ~ 999999)) %>%
      mutate(ratio = case_when(
        #potentially take the absolute value in the outputs
        (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
        (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
        # must be a numeric error
        TRUE ~ 999999)) %>%
      select(poptype, difference, ratio)
    
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
        TRUE ~ "something else happend")) %>%
      select(poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DIDB = case_when(
        #note, check spelling....
        #note, more conditions to bring in 
        ratio > 1 + dim3 ~ "Protected population changes more",
        ratio < 1 - dim3 ~ "Non-protected population changes more",
        1- dim3 < ratio | ratio < 1 +dim3 ~ "Disproportionality within threshold",
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
      scale_colour_manual(values = c("No Impact" = "#858585", "Benefit" = "#4a4a4a","Burden" = "#ff6666"))+
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
    
    dispro_method <- input$dis_method
    
    
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
      select( population,delta, no_build, impact)%>%
      mutate( poptype = case_when (str_detect(population, ".inority") ~ "m",
                                   str_detect(population, ".ncome") ~ "i",
                                   TRUE ~ "NA")) %>%
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
        TRUE ~ 999999)) %>%
      mutate(ratio = case_when(
        #potentially take the absolute value in the outputs
        (is.na(Low_income) & is.na(Non_low_income)) ~ abs(Minority)/abs(Non_minority),
        (is.na(Minority) & is.na(Non_minority)) ~ abs(Low_income)/abs(Non_low_income),
        # must be a numeric error
        TRUE ~ 999999)) %>%
      select(poptype, difference, ratio)
    
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
        TRUE ~ "something else happend")) %>%
      select(poptype, type)
    
    dispro <- diff %>%
      left_join(impact_type) %>%
      mutate(DIDB = case_when(
        #note, check spelling....
        #note, more conditions to bring in 
        ratio > 1 + dim3 ~ "Protected population changes more",
        ratio < 1 - dim3 ~ "Non-protected population changes more",
        1- dim3 < ratio | ratio < 1 +dim3 ~ "Disproportionality within threshold",
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
      geom_segment (aes(x= poptype, xend= poptype, y= 1, yend = ratio, color = DIDB), shape = 20, size = 4, show.legned = FALSE)+
      scale_color_manual(values = c("Disproportionality within threshold"= "#858585", "Protected population changes more"= "#ff6666", "Non-protected population changes more"= "#ff6666"))+
      geom_hline(aes(yintercept = 1), size= 1, color = "black")+
      geom_text( aes(x=as.numeric(poptype)+.2, y= ratio, label = str_wrap(DIDB, width = 20)), hjust= "inward", size = 4)+
      coord_flip()+
      theme_minimal()+
      theme(legend.position = "None", plot.title = element_text(face= "bold"))+
      labs(title= "Ratio by population type")+
      ylab(str_wrap("Ratio,  (% change protected population) / (% change non-protected population)", width = 40))+
      xlab("Population type")
    print(burden_plot)
    
    
  })  

}

shinyApp(ui, server)
