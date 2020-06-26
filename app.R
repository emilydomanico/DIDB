# Packages #############################################
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(hrbrthemes)
library(readr)
library(shinyBS)
library(sf)
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
mrs_list <- mrs%>%
  select(TAZ_ID)

mrs_pop <- read_csv("data/pct_change_by_TAZ_pop.csv")

mrs_diff<- read_csv("data/mr_diff2.csv")

#prep for metric filtering
met_list <- read_csv("data/metric_category.csv")

taz_pop <- read_csv("data/pop_by_TAZ_2040.csv")
pop_mpo <- taz_pop%>%
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
      h5("Threshold Summary"),
      h6("Accessibility Metrics:"),
      htmlOutput("AccText"), 
      br(),
      h6("Enviornmental Metrics:"),
      htmlOutput("EnvText"),
      br(),
      h6("Mobility Metrics:"),
      htmlOutput("MobText"),
      br(),
      #to fix side panel position
      #style = "position: fixed; width:inherit;",
      # selectInput("metric", "Metric:",
      #             choices = list(
      #               Accessibility= c("Access to retail amenities by transit", "Access to higher education by transit","Access to healthcare facilities by transit", "Access to jobs by transit"),
      #               Environmental= c("Congested vehicle miles traveled","Carbon monoxide emissions"),
      #               Mobility= c("Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
      #                           "Average production - transit travel time")
      #             ), selected = "Carbon monoxide emissions"),
      
      #hr(),
      
      #Sliders to toggle sensitivity
      #confidence level input option 1
      # radioButtons("Dim1", "Forecasting Error confidence level Threshold",
      #              c("0 %"=0,
      #                "10 %"= 10,
      #                "90 %"=90,
      #                "95 %"= 95),
      #              selected= 10,
      #              inline= TRUE),
      #confidence level input option 2
      # selectInput("Dim1", "Forecasting Error confidence level Threshold",
      #           choices = c(0, 10, 90, 95),
      #           selected = 10
      #             ),
      #confidence level input option 3
      # shinyWidgets::sliderTextInput(inputId = "Dim1", 
      #                               label = "Forecasting Error confidence level Threshold", 
      #                               choices = c(0,10,90,95,100),grid=TRUE,width=110, dragRange = TRUE, post = " %"),
      #sliderInput("Dim1", label = "Step 1: Forecasting Error confidence level Threshold", min = 0, max = 100, post= " %", value = 10, step = 1),
    
      #br(),
      
     # sliderInput("Dim2", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
     h5("Impact Threshold"), 
     p("This threshold sets the sensitivity for determining if the impacts of implementing the build-scenario would be meaningful. The slider represents a percent change. At 0%, any change between the build and no-build scenario would be considered a meaningful impact. As the threshold increase, the likelihood of identifying an adverse effect decreases. The impact is calculated as the percent change between scenarios:"),
      withMathJax("$$\\scriptsize\\frac{\\text{Build} - \\text{No-build} } {\\text{No-build}} \\cdot 100$$"),
      p(" "),
      p("If an impact is found, it is categorized as a benefit or a burden based on the directionality of the metric. (For example, an increase in carbon monoxide emissions is a burden, while an increase in access to jobs is a benefit."),
      br(),
     h5("Disproportionality Threshold"), 
      #sliderInput("Dim3", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
      #selectInput("dis_method", label = "Disproportionality method to visualize:", choices = c("Percent Difference", "Ratio"), selected = "Ratio")
      p("This threshold determines if the impacts found in the previous step disproportionately affect the minority or low-income population more than the nonminority or non-low-income population."),
      p("Disproportionality is calculated as a ratio, comparing the absolute value of the percent change for the protected population (from the second step) to the absolute value of the percent change non-protected population."),
     br(), 
     h5("Baseline Uncertainty Confidence Level"),
      p("This threshold sets set the sensitivity for detecting the likelihood of the model outputs for the build and no-build scenarios. The radio buttons represent confidence levels - a higher percentage would result in a greater range of likely values — as indicated by the blue and red bars — and less of a chance of identifying a likely impact to the population groups.")
     
     ),
# Main Panel UI ######################################################    
    mainPanel(
      tabsetPanel( type = "tabs",
                   # tabPanel("Change by TAZ", 
                   #          br(),
                   #          selectInput("metric_map", "Metric:",
                   #                      choices = list(
                   #                        Accessibility= c("Access to retail amenities by transit", "Access to higher education by transit","Access to healthcare facilities by transit", "Access to jobs by transit"),
                   #                        Environmental= c("Congested vehicle miles traveled","Carbon monoxide emissions"),
                   #                        Mobility= c("Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
                   #                                    "Average production - transit travel time")
                   #                      ), selected = "Carbon monoxide emissions"),
                   #          column(width = 6,
                   #          h5("Filter with histogram") ,
                   #          br(),
                   #          br(),
                   #          plotOutput("TAZ_hist", brush= brushOpts(id= "plot_brush", fill= "#c47d47", direction= "x"),
                   #                     height= 350), 
                   #          #verbatimTextOutput("plotbrush_raw"),
                   #          htmlOutput("plotbrush_txt")
                   #          ),
                   #          column(width= 6, 
                   #          # plotOutput("TAZ_hist_diff", brush= brushOpts(id= "plot_brush_dif", fill="#88bfaf", direction= "x"),
                   #          #            height= 350),
                   #          radioButtons("pct_taz", "Percent of TAZ's included in selection:",
                   #                       c("50 %"= 50,
                   #                         "60 %"= 60,
                   #                         "70 %"= 70,
                   #                         "80 %"= 80,
                   #                         "90 %"= 90,
                   #                         "95 %"= 95,
                   #                         "97 %"= 97,
                   #                         "98 %"= 98,
                   #                         "99 %"= 99),
                   #                       selected= 98,
                   #                       inline= TRUE),
                   #          # selectInput("pct_taz", "Percent of TAZ's included in selection:",
                   #          #             choices= c(50, 60, 70, 80, 95,97,98, 99),
                   #          #             
                   #          #             selected= 98),
                   #          plotOutput("TAZ_hist_diff",
                   #                     height= 350),
                   #          #NOTE: plotbrust_txt_dif used as select input reactive text for pct_taz selection
                   #          htmlOutput("plotbrush_txt_dif"),
                   #          p("Note: Selected range is inclusive.")
                   #          ),
                   #          #textOutput("plotbrush_txt"),
                   #          #textOutput("plotbrush_txt_dif"),
                   #          br(),
                   #          column(width=8,
                   #                 h5("Traffic Analysis Zones"),
                   #                 h6("To zoom, select a region on the map below"),
                   #                 p("Purple regions have a model output of 0 for the No-Build scenario and cannot be calculated as a percent difference"),
                   #                 plotOutput(outputId = "TAZ_map",
                   #                            height = 550,
                   #                            brush= brushOpts(
                   #                              id="plot2_brush",
                   #                              resetOnNew = TRUE
                   #                            ))
                   #                 ),
                   #          column(width= 4, 
                   #                 h5(""),
                   #                 plotOutput(outputId = "TAZ_map2",
                   #                            height = 600)
                   #                 )
                   #          ),
                   # tabPanel("Map by Population Type", 
                   #          br(),
                   #          selectInput("metric_mappop", "Metric:",
                   #                      choices = list(
                   #                        Accessibility= c("Access to retail amenities by transit", "Access to higher education by transit","Access to healthcare facilities by transit", "Access to jobs by transit"),
                   #                        Environmental= c("Congested vehicle miles traveled","Carbon monoxide emissions"),
                   #                        Mobility= c("Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
                   #                                    "Average production - transit travel time")
                   #                      ), selected = "Carbon monoxide emissions"),
                   #          h5("Filter with histogram") ,
                   #          plotOutput("TAZ_histpop", brush= brushOpts(id= "plot_brushpop", fill= "#c47d47", direction= "x"),
                   #                     height= 500),
                   #          br(),
                   #          column(width=8,
                   #                 h5("Traffic Analysis Zones"),
                   #                 h6("To zoom, select a region on the map below"),
                   #                 p("Purple regions have a model output of 0 for the No-Build scenario and cannot be calculated as a percent difference"),
                   #                 plotOutput(outputId = "TAZ_mappop",
                   #                            height = 750,
                   #                            brush= brushOpts(
                   #                              id="plot2_brushpop",
                   #                              resetOnNew = TRUE
                   #                            ))
                   #          ),
                   #          column(width= 4, 
                   #                 h5(""),
                   #                 plotOutput(outputId = "TAZ_map2pop",
                   #                            height = 750)
                   #          )
                   # ),
#                    tabPanel("DI/DB Rules",
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
# level for their family size.")
#                    ),
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
                                     #sliderInput("Dim2Acc", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                     #sliderInput("Dim3Acc", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1)
                            ),
                            # column(width = 6,
                            #        #h5("Baseline Uncertainty Test"),
                            #        radioButtons("Dim1Acc", "Baseline Uncertainty Level:",
                            #                     c("No uncertainty"=0,
                            #                       "Low"=10,
                            #                       "Moderate"= 50,
                            #                       #"90 %"=90,
                            #                       "High uncertainty"= 95),
                            #                     selected= 10,
                            #                     inline= TRUE),
                            #        p("If a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Impact Threshold."),
                            #        plotOutput(outputId = "metric_plotAcc"),
                            #        #textOutput(), note:
                            #        #tableOutput("change_result"),
                            #        br(),
                            #        #formattableOutput("DIDBAcc"),
                            #        #textOutput("change"),
                            # ),
                            column(width = 6,
                                   #h5("Impact Threshold"),
                                   sliderInput("Dim2Acc", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                   p("Where a likely impact is indicated, is the impact meaningful for each population?"),
                                   plotOutput("impact_plotAcc"),
                                   br(),
                                   #p("Reactive text indicating if there is a impact that exceeds the threshold set. If so proceed to next step. Or default DI/DB.")
                            ),
                            column(width = 6,
                                   #h5("Disproportionality Threshold"),
                                   sliderInput("Dim3Acc", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
                                   p("Where there is a likely meaningful impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotAcc"),
                                   br(),
                                   #p("Reactive text indicating if there is a disproportionate burden. Prompt to see how slider inputs work across all metric in the next tab.")
                            
                                   ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBAcc"),
                                   br(),
                                   ),
                            column(width = 12,
                            h5("Baseline Uncertainty Test"),
                            p("If a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Impact Threshold."),
                            radioButtons("Dim1Acc", "I want to set the confidence level to:",
                                         c("No uncertainty"=0,
                                           "Low uncertainty"=10,
                                           "Moderate uncertainty"= 50,
                                           #"90 %"=90,
                                           "High uncertainty"= 95),
                                         selected= 10,
                                         inline= TRUE),
                            plotOutput(outputId = "metric_plotAcc"),
                            #textOutput(), note:
                            #tableOutput("change_result"),
                            br(),
                            #formattableOutput("DIDBAcc"),
                            #textOutput("change"),
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
                                     #sliderInput("Dim2Env", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                     #sliderInput("Dim3Env", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1)
                            ),
                            column(width = 6,
                                   #h5("Impact Threshold"),
                                   sliderInput("Dim2Env", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                   p("Where a likely impact is indicated, is the impact meaningful for each population?"),
                                   plotOutput("impact_plotEnv"),
                                   br(),
                                   #p("Reactive text indicating if there is a impact that exceeds the threshold set. If so proceed to next step. Or default DI/DB.")
                            ),
                            column(width = 6,
                                   #h5("Disproportionality Threshold"),
                                   sliderInput("Dim3Env", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
                                   p("Where there is a likely meaningful impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotEnv"),
                                   br(),
                                   #p("Reactive text indicating if there is a disproportionate burden. Prompt to see how slider inputs work accross all metric in the next tab.")
                                   
                            ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBEnv"),
                                   br(),
                            ),
                            column(width = 12,
                                   h5("Baseline Uncertainty Test"),
                                   p("If a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Impact Threshold."),
                                   radioButtons("Dim1Env", "I want to set the confidence level to:",
                                                c("No uncertainty"=0,
                                                  "Low uncertainty"=10,
                                                  "Moderate uncertainty"= 50,
                                                  #"90 %"=90,
                                                  "High uncertainty"= 95),
                                                selected= 10,
                                                inline= TRUE),
                                   plotOutput(outputId = "metric_plotEnv"),
                                   #textOutput(), note:
                                   #tableOutput("change_result"),
                                   br(),
                                   #formattableOutput("DIDBEnv")
                                   #textOutput("change"),
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
                                     #sliderInput("Dim2Mob", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                     #sliderInput("Dim3Mob", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1)
                            ),
                            column(width = 6,
                                   #h5("Impact Threshold"),
                                   sliderInput("Dim2Mob", label = "Impact Threshold", min = 0, max = 20, post= " %", value = 2, step = .1),
                                   p("Where a likely impact is indicated, is the impact meaningful for each population?"),
                                   plotOutput("impact_plotMob"),
                                   br(),
                                   #p("Reactive text indicating if there is a impact that exceeds the threshold set. If so proceed to next step. Or default DI/DB.")
                            ),
                            column(width = 6,
                                   #h5("Disproportionality Threshold"),
                                   sliderInput("Dim3Mob", label = "Disproportionality Threshold", min = 0, max = 30, value = 5, post= " %", step = 1),
                                   p("Where there is a likely meaningful impact, would the minority or low-income populations be disproportionately affected?"),
                                   plotOutput("burden_plotMob"),
                                   br(),
                                   #p("Reactive text indicating if there is a disproportionate burden. Prompt to see how slider inputs work accross all metric in the next tab.")
                                   
                            ),
                            column(width = 12,
                                   htmlOutput(outputId = "DIDBMob"),
                                   br(),
                            ),
                            column(width = 12,
                                   h5("Baseline Uncertainty Test"),
                                   p("If a change between the build and no-build scenario for either the protected or non-protected populations, proceed to the Impact Threshold."),
                                   radioButtons("Dim1Mob", "I want to set the confidence level to:",
                                                c("No uncertainty"=0,
                                                  "Low uncertainty"=10,
                                                  "Moderate uncertainty"= 50,
                                                  #"90 %"=90,
                                                  "High uncertainty"= 95),
                                                selected= 10,
                                                inline= TRUE),
                                   plotOutput(outputId = "metric_plotMob"),
                                   #textOutput(), note:
                                   #tableOutput("change_result"),
                                   br(),
                                   #formattableOutput("DIDBMob")
                                   #textOutput("change"),
                            )
                   ),
                   # Results for all metrics ##############################
                   tabPanel("Results for all Metrics",
                            br(),
                            #textOutput("DIDB_count"),
                            p("The table below will show instances of DI/DB for the current threshold settings accross all metrics."),
                            htmlOutput("DIDBAcc2"),
                            htmlOutput("DIDBEnv2"),
                            htmlOutput("DIDBMob2"),
                            p("Note: I = Low-income and Non-low-income pair. M = Minority and Non-minority pair."),
                            
                            )
#,
                   # tabPanel("Change by TAZ", 
                   #          br(),
                   #          selectInput("metric_map", "Metric:",
                   #                      choices = list(
                   #                        Accessibility= c("Access to retail amenities by transit", "Access to higher education by transit","Access to healthcare facilities by transit", "Access to jobs by transit"),
                   #                        Environmental= c("Congested vehicle miles traveled","Carbon monoxide emissions"),
                   #                        Mobility= c("Average attraction - highway travel time", "Average production - highway travel time","Average attraction - transit travel time",
                   #                                    "Average production - transit travel time")
                   #                      ), selected = "Carbon monoxide emissions"),
                   #          column(width = 6,
                   #                 h5("Filter with histogram") ,
                   #                 br(),
                   #                 br(),
                   #                 plotOutput("TAZ_hist", brush= brushOpts(id= "plot_brush", fill= "#c47d47", direction= "x"),
                   #                            height= 350), 
                   #                 #verbatimTextOutput("plotbrush_raw"),
                   #                 htmlOutput("plotbrush_txt")
                   #          ),
                   #          column(width= 6, 
                   #                 # plotOutput("TAZ_hist_diff", brush= brushOpts(id= "plot_brush_dif", fill="#88bfaf", direction= "x"),
                   #                 #            height= 350),
                   #                 radioButtons("pct_taz", "Percent of TAZ's included in selection:",
                   #                              c("50 %"= 50,
                   #                                "60 %"= 60,
                   #                                "70 %"= 70,
                   #                                "80 %"= 80,
                   #                                "90 %"= 90,
                   #                                "95 %"= 95,
                   #                                "97 %"= 97,
                   #                                "98 %"= 98,
                   #                                "99 %"= 99),
                   #                              selected= 98,
                   #                              inline= TRUE),
                   #                 # selectInput("pct_taz", "Percent of TAZ's included in selection:",
                   #                 #             choices= c(50, 60, 70, 80, 95,97,98, 99),
                   #                 #             
                   #                 #             selected= 98),
                   #                 plotOutput("TAZ_hist_diff",
                   #                            height= 350),
                   #                 #NOTE: plotbrust_txt_dif used as select input reactive text for pct_taz selection
                   #                 htmlOutput("plotbrush_txt_dif"),
                   #                 p("Note: Selected range is inclusive.")
                   #          ),
                   #          #textOutput("plotbrush_txt"),
                   #          #textOutput("plotbrush_txt_dif"),
                   #          br(),
                   #          column(width=8,
                   #                 h5("Traffic Analysis Zones"),
                   #                 h6("To zoom, select a region on the map below"),
                   #                 p("Purple regions have a model output of 0 for the No-Build scenario and cannot be calculated as a percent difference"),
                   #                 plotOutput(outputId = "TAZ_map",
                   #                            height = 550,
                   #                            brush= brushOpts(
                   #                              id="plot2_brush",
                   #                              resetOnNew = TRUE
                   #                            ))
                   #          ),
                   #          column(width= 4, 
                   #                 h5(""),
                   #                 plotOutput(outputId = "TAZ_map2",
                   #                            height = 600)
                   #          )
                   # )
                   #,
#                    tabPanel("DI/DB Rules",
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
#                             # p("Step 1:"),
#                             # p("Step 2:"),
#                             # p("Step 3:"),
#                             # grVizOutput('case_chart')
#                             )
      ) #close tabsetPanel()
    ) # close mainpanel()
  ) # close sidebarlayout()
) # close fluidpage() 

# Server##########################################

server <- function(input, output) {
# Histogram of TAZ mr ##################################
#   output$TAZ_hist <- renderPlot({
#     
#     metric_filter <- input$metric_map
#     met_int <- met_list %>%
#       filter(metric_name== metric_filter)
#     met <- met_int$metric
#     pct_met <- paste0("pct_", met)
#     mrs_filtered <- mrs%>%
#       select(TAZ_ID, the_metric= pct_met)
#     
#     mrs_filtered <- mrs_filtered %>%
#       filter(is.infinite(the_metric)== FALSE)
#     
#     sd <- sd(mrs_filtered$the_metric, na.rm = TRUE)
#     mean <- mean(mrs_filtered$the_metric, na.rm = TRUE)
#     median <- median(mrs_filtered$the_metric, na.rm= TRUE)
#     
#     sd_rnd <- round(sd, digits = 3)
#     mean_rnd <- round(mean, digits = 3)
#     med_rnd <- round(median, digits = 3)
#     
#     
#     ggplot(mrs_filtered, aes(x= the_metric))+
#       geom_point(aes(y=1), shape= 20, alpha= .5)+
#       geom_histogram(alpha= .5)+
#       geom_vline(aes(xintercept = mean, color="mean", linetype= "mean"), alpha= .75)+
#       geom_vline(aes(xintercept = mean-sd, color= "std_dev",linetype= "std_dev"), alpha= .5)+
#       geom_vline(aes(xintercept = mean+sd, color= "std_dev",linetype= "std_dev"), alpha= .5)+
#       geom_vline(aes(xintercept = mean-2*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       geom_vline(aes(xintercept = mean+2*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       geom_vline(aes(xintercept = mean-3*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       geom_vline(aes(xintercept = mean+3*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       geom_vline(aes(xintercept = median, color= "median", linetype= "median"), alpha= .75)+
#       scale_color_manual(name= "Statistics", values= c(mean= "black", std_dev= "black", median= "black"),
#                          labels= c(mean= paste0("Mean: ", mean_rnd),
#                                    std_dev= paste0("Standard \nDeviation: +/-", sd_rnd),
#                                    median= paste0("Median: ", med_rnd)))+
#       scale_linetype_manual(name= "Statistics", values= c(mean= 1, std_dev= 2, median= 3),
#                             labels= c(mean= paste0("Mean: ", mean_rnd),
#                                       std_dev= paste0("Standard \nDeviation: +/-", sd_rnd),
#                                       median= paste0("Median: ", med_rnd)))+
#       scale_x_continuous(labels = (function(x) (paste0(format(x*100, big.mark= ","), " %"))))+
#       scale_y_continuous(trans= "log10")+ #  minor_breaks = mb   labels = scales::percent
#       theme_minimal()+
#       labs(x= paste("% change", str_to_lower(met_int$metric_name)),
#            y= "Count")+
#       theme(legend.text = element_text(size= 12),
#             legend.position = c(.95, .95),
#             legend.justification = c(1,1),
#             axis.text.x = element_text(size=11.5))
#   })
#   
#   output$TAZ_hist_diff <- renderPlot({
#     metric_filter <- input$metric_map
#     pct_in_selection <- as.numeric(input$pct_taz)/100
#     
#     met_int <- met_list %>%
#       filter(metric_name== metric_filter)
#     
#     met <- met_int$metric
#     
#     dif_met <- paste0("dif_", met)
#     
#     mrs_filtered_dif <- mrs_diff%>%
#       select(TAZ_ID, the_metric=dif_met)
#     
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     
#     mrs_filtered_dif <- mrs_filtered_dif %>%
#       filter(is.infinite(the_metric)== FALSE)
#     
#     sd <- sd(mrs_filtered_dif$the_metric, na.rm = TRUE)
#     mean <- mean(mrs_filtered_dif$the_metric, na.rm = TRUE)
#     median <- median(mrs_filtered_dif$the_metric, na.rm = TRUE)
#     
#     sd_rnd <- round(sd, digits = 3)
#     mean_rnd <- round(mean, digits = 3)
#     med_rnd <- round(median, digits = 3)
#     
#     quants <- quantile(mrs_filtered_dif$the_metric, probs= c(.005,.01,.015, .025, .05, .1, .15, .2, .25,.75,.8, .85,.9, .95, .975,.985, .99,.995))
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     
#     
#     q_up<- paste0(((.5 + pct_in_selection/2) *100), "%")
#     q_low <- paste0(((.5 - pct_in_selection/2) *100), "%")
#     
#     # filter_q <-mrs_filtered_dif%>%
#     #   filter(the_metric > quants[[q_low]] & the_metric < quants[[q_up]])
#     # 
#     
#     #mb <- as.numeric(1:10 %o% 10 ^ (0:4))
#     
#     
#     xmin <- quants[[q_low]]
#     xmax <- quants[[q_up]]
#     
#     ggplot(mrs_filtered_dif, aes(x= the_metric))+
#       #geom_rug(alpha=.5, color= "dark gray")+
#       geom_histogram(alpha= .5)+
#       geom_vline(aes(xintercept = mean, color="mean", linetype= "mean"), alpha= .75)+
#       geom_vline(aes(xintercept = mean-sd, color= "std_dev",linetype= "std_dev"), alpha= .5)+
#       #geom_vline(aes(xintercept = xmin), color= "pink")+
#       #geom_vline(aes(xintercept = xmax), color = "purple")+
#       geom_vline(aes(xintercept = mean+sd, color= "std_dev",linetype= "std_dev"), alpha= .5)+
#       # geom_vline(aes(xintercept = mean-2*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       # geom_vline(aes(xintercept = mean+2*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       # geom_vline(aes(xintercept = mean-3*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       # geom_vline(aes(xintercept = mean+3*sd, color= "std_dev",linetype= "std_dev"), alpha= .25)+
#       geom_vline(aes(xintercept = median, color= "median", linetype= "median"), alpha= .75)+
#       annotate("rect", xmin=xmin , xmax=xmax, ymin = 1, ymax = Inf, alpha= 0.35, color ="#88bfaf", fill= "#88bfaf")+
#       geom_point(aes(y=1), shape= 20, alpha= .5)+
#       scale_color_manual(name= "Statistics", values= c(mean= "black", std_dev= "black", median= "black"),
#                          labels= c(mean= paste0("Mean: ", mean_rnd),
#                                    std_dev= paste0("Standard \nDeviation: +/-", sd_rnd),
#                                    median= paste0("Median: ", med_rnd)
#                          ))+
#       scale_linetype_manual(name= "Statistics", values= c(mean= 1, std_dev= 2, median= 3),
#                             labels= c(mean= paste0("Mean: ", mean_rnd),
#                                       std_dev= paste0("Standard \nDeviation: +/-", sd_rnd),
#                                       median= paste0("Median: ", med_rnd)
#                             ))+
#       #scale_x_log10()+
#       #scale_x_continuous(trans= "log10", labels= scales::percent, minor_breaks = mb)+
#       scale_x_continuous(labels = (function(x) format(x,big.mark= ",")))+
#       scale_y_continuous(trans= "log10")+ #  minor_breaks = mb   labels = scales::percent
#       theme_minimal()+
#       #coord_flip()+
#       labs(x= paste("Absolute change", str_to_lower(met_int$metric_name)),
#            y= "Count")+
#       theme(legend.text = element_text(size= 12),
#             legend.position = c(.35, .95),
#             legend.justification = c(1,1),
#             axis.text.x = element_text(size=11.5))
#   })
# 
#   output$plotbrush_txt <- renderText({
#     metric_filter <- input$metric_map
#     met_int <- met_list %>%
#       filter(metric_name== metric_filter)
#     met <- met_int$metric
#     pct_met <- paste0("pct_", met)
#     mrs_filtered <- mrs%>%
#       select(TAZ_ID, the_metric= pct_met)
#     
#     mrs_filtered <- mrs_filtered %>%
#       filter(is.infinite(the_metric)== FALSE)
#     
#     filter <- brushedPoints(mrs_filtered, input$plot_brush)
#     
#     pop_total <-  pop_mpo%>%
#       summarise(sum(Population))
#     pop_total_num <- pop_total$`sum(Population)`[1]
# 
#     pop_selected <- pop_mpo%>%
#       right_join(filter)%>%
#       summarise(sum(Population))
#     pop_selected_num <- pop_selected$`sum(Population)`[1]
# 
#     pop_selected_pct <- round(pop_selected_num/ pop_total_num *100, 2)
# 
#     pop_notselected_num <- pop_total_num - pop_selected_num
#     pop_notselected_pct <- round(pop_notselected_num/pop_total_num *100, 2)
#     
#     taz_selected <-  if(is.null(input$plot_brush$xmin) ==TRUE){NA} else{nrow(filter)}
#     pct_taz_selected <- if(is.null(input$plot_brush$xmin) ==TRUE){NA} else{round(nrow(filter)/1901*100, 2)}
#     pct_taz_unselected <- if(is.null(input$plot_brush$xmin) ==TRUE){NA} else{round((1901-nrow(filter))/1901*100, 2)}
# 
#     pop_txt<- paste("MPO Population Included in Selection:", pop_selected_pct, "%", "<br>MPO Popluation Outside of Selection:", pop_notselected_pct, "%")
#     brushtext_min <- if(is.null(input$plot_brush$xmin) ==TRUE){NA} else{round(input$plot_brush$xmin,digits = 3)}
#     brushtext_max <- if(is.null(input$plot_brush$xmax) == TRUE) {NA} else{round(input$plot_brush$xmax,digits = 3)}
#     range_txt<- paste("Selected Range:", brushtext_min, "% to", brushtext_max, "%")
#     taz_count <- paste("Count of selected TAZ's: ", taz_selected)
#     taz_pct <- paste("TAZ's Included in Selection:", pct_taz_selected, "%", "<br>", "TAZ's Outside of Selection:", pct_taz_unselected, "%")
#     print(paste(range_txt,"<br>",pop_txt, "<br>", taz_count, "<br>", taz_pct))
# 
#   })
#   
#   
#   # output$plotbrush_raw <- renderPrint({
#   #   metric_filter <- input$metric_map
#   #   met_int <- met_list %>%
#   #     filter(metric_name== metric_filter)
#   #   met <- met_int$metric
#   #   pct_met <- paste0("pct_", met)
#   #   mrs_filtered <- mrs%>%
#   #     select(TAZ_ID, the_metric= pct_met)
#   #   
#   #   mrs_filtered <- mrs_filtered %>%
#   #     filter(is.infinite(the_metric)== FALSE)
#   #   
#   #   brushedPoints(mrs_filtered, input$plot_brush)
#   #   
#   # })
# 
#   output$plotbrush_txt_dif <- renderText({
#     metric_filter <- input$metric_map
#     pct_in_selection <- as.numeric(input$pct_taz)/100
#     
#     met_int <- met_list %>%
#       filter(metric_name== metric_filter)
#     met <- met_int$metric
#     dif_met <- paste0("dif_", met)
#     mrs_filtered_dif <- mrs_diff%>%
#       select(TAZ_ID, the_metric=dif_met)
#     
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     #pct_in_selection <- as.numeric(input$pct_taz)/100
#     quants <- quantile(mrs_filtered_dif$the_metric, probs= c(.005,.01,.015, .025, .05, .1, .15, .2, .25,.75,.8, .85,.9, .95, .975,.985, .99,.995))
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     
#     #pct_in_selection <- as.numeric(input$pct_pop)/100
#     q_up<- paste0(((.5 + pct_in_selection/2) *100), "%")
#     q_low <- paste0(((.5 - pct_in_selection/2) *100), "%")
#     
#     filter_dif <-mrs_filtered_dif%>%
#       filter(the_metric >= quants[[q_low]] & the_metric <= quants[[q_up]])
#     
#     
#     pop_total <-  pop_mpo%>%
#       summarise(sum(Population))
#     pop_total_num <- pop_total$`sum(Population)`[1]
#     
#     pop_selected <- pop_mpo%>%
#       right_join(filter_dif)%>%
#       summarise(sum(Population))
#     pop_selected_num <- pop_selected$`sum(Population)`[1]
#     
#     pop_selected_pct <- round(pop_selected_num/ pop_total_num *100, 2)
#     
#     pop_notselected_num <- pop_total_num - pop_selected_num
#     pop_notselected_pct <- round(pop_notselected_num/pop_total_num *100, 2)
#     
#     # taz_selected <-  if(is.null(input$plot_brush_dif$xmin) ==TRUE){NA} else{nrow(filter_dif)}
#     # pct_taz_selected <- if(is.null(input$plot_brush_dif$xmin) ==TRUE){NA} else{round(nrow(filter_dif)/1901*100, 2)}
#     # pct_taz_unselected <- if(is.null(input$plot_brush_dif$xmin) ==TRUE){NA} else{round((1901-nrow(filter_dif))/1901*100, 2)}
#     taz_selected <-  nrow(filter_dif)
#     pct_taz_selected <- round(nrow(filter_dif)/1901*100, 2)
#     pct_taz_unselected <- round((1901-nrow(filter_dif))/1901*100, 2)
#     
#     pop_txt<- paste("MPO Population Included in Selection:", pop_selected_pct, "%", "<br>MPO Popluation Outside of Selection:", pop_notselected_pct, "%")
#     
#     pop_txt<- paste("MPO Population Included in Selection:", pop_selected_pct, "%", "<br>MPO Popluation Outside of Selection:", pop_notselected_pct, "%")
#     # brushtext_min <- if(is.null(input$plot_brush_dif$xmin) ==TRUE){NA} else{round(input$plot_brush_dif$xmin,digits = 3)}
#     # brushtext_max <- if(is.null(input$plot_brush_dif$xmax) == TRUE) {NA} else{round(input$plot_brush_dif$xmax,digits = 3)}
#     # brushtext_min <-round(input$plot_brush_dif$xmin,digits = 3)
#     # brushtext_max <- round(input$plot_brush_dif$xmax,digits = 3)
#     # range_txt<- paste("Selected Range:", brushtext_min, " to", brushtext_max)
#     selected_min <- round(quants[[q_low]], 3)
#     selected_max <- round(quants[[q_up]], 3)
#     range_txt<- paste("Selected Range:", selected_min, " to", selected_max)
#     taz_count <- paste("Count of selected TAZ's: ", taz_selected)
#     taz_pct <- paste("TAZ's Included in Selection:", pct_taz_selected, "%", "<br>", "TAZ's Outside of Selection:", pct_taz_unselected, "%")
#     print(paste(range_txt,"<br>",pop_txt, "<br>", taz_count, "<br>", taz_pct))
#   })
# 
# # Taz map ############################################## 
# #testing to print out brush output
# 
#   
#   output$TAZ_map <- renderPlot({
#     metric_filter <- input$metric_map
#     met_int <- met_list %>%
#       filter(metric_name== metric_filter)
#     met <- met_int$metric
#     pct_met <- paste0("pct_", met)
#     dif_met <- paste0("dif_", met)
#     mrs_filtered <- mrs%>%
#       select(TAZ_ID, the_metric= pct_met)
#     mrs_na <- mrs_filtered %>%
#       filter(is.na(the_metric)==TRUE | is.infinite(the_metric)== TRUE)
#     mrs_filtered_dif <- mrs_diff%>%
#       select(TAZ_ID, the_metric=dif_met)
#     
#     
#     filter <- brushedPoints(mrs_filtered, input$plot_brush)
#     
#     pct_in_selection <- as.numeric(input$pct_taz)/100
#     quants <- quantile(mrs_filtered_dif$the_metric, probs= c(.005,.01,.015, .025, .05, .1, .15, .2, .25,.75,.8, .85,.9, .95, .975,.985, .99,.995))
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     
#     #pct_in_selection <- as.numeric(input$pct_pop)/100
#     q_up<- paste0(((.5 + pct_in_selection/2) *100), "%")
#     q_low <- paste0(((.5 - pct_in_selection/2) *100), "%")
#     
#     filter_dif <-mrs_filtered_dif%>%
#       filter(the_metric >= quants[[q_low]] & the_metric <= quants[[q_up]])
#     # map
#     #read in shapefile
#     taz <- st_read("data/CTPS_TAZ_simplified.shp")%>%
#       st_transform(2249)
#     taz_mrs <- taz %>%
#       inner_join(mrs, by= c("TAZ"= "TAZ_ID"))
# 
#     taz_filtered <- taz_mrs%>%
#       right_join(filter, by= c("TAZ"= "TAZ_ID"))
#     
#     taz_na <- taz%>%
#       inner_join(mrs_na, by= c("TAZ"= "TAZ_ID"))
#     
#     taz_filtered_dif <- taz_mrs %>%
#       right_join(filter_dif, by = c("TAZ"= "TAZ_ID"))
# 
#     #plot map
#     ggplot()+
#       geom_sf(data=taz_mrs, size=.15, color= "dark gray", fill= "dark gray", alpha= .25)+
#       geom_sf(data=taz_na, size=.15, color= "#6b5069", fill= "#946f91", alpha= .5)+
#       geom_sf(data= taz_filtered, size= .15, color= "#b84f00", fill= "#d95f02", alpha= .5)+
#       geom_sf(data= taz_filtered_dif, size= .15, color= "#44947c", fill= "#5fcfad", alpha= .35)+
#       coord_sf()+
#       theme_map()
#       
#   })
#   
#   ranges <- reactiveValues(x=NULL, y=NULL)
#   output$TAZ_map2<-renderPlot({
#     metric_filter <- input$metric_map
#     met_int <- met_list %>%
#        filter(metric_name== metric_filter)
#     met <- met_int$metric
#     pct_met <- paste0("pct_", met)
#     dif_met <- paste0("dif_", met)
#     mrs_filtered <- mrs%>%
#       select(TAZ_ID, the_metric= pct_met)
#     mrs_na <- mrs_filtered %>%
#       filter(is.na(the_metric)==TRUE | is.infinite(the_metric)== TRUE)
#     mrs_filtered_dif <- mrs_diff%>%
#       select(TAZ_ID, the_metric = dif_met)
#     
#     filter <- brushedPoints(mrs_filtered, input$plot_brush)
#     pct_in_selection <- as.numeric(input$pct_taz)/100
#     quants <- quantile(mrs_filtered_dif$the_metric, probs= c(.005,.01,.015, .025, .05, .1, .15, .2, .25,.75,.8, .85,.9, .95, .975,.985, .99,.995))
#     
#     #filter_dif <- brushedPoints(mrs_filtered_dif, input$plot_brush_dif)
#     
#     #pct_in_selection <- as.numeric(input$pct_pop)/100
#     q_up<- paste0(((.5 + pct_in_selection/2) *100), "%")
#     q_low <- paste0(((.5 - pct_in_selection/2) *100), "%")
#     
#     filter_dif <-mrs_filtered_dif%>%
#       filter(the_metric >= quants[[q_low]] & the_metric <= quants[[q_up]])
#     # map
#     #read in shapefile
#     taz <- st_read("data/CTPS_TAZ_simplified.shp")%>%
#       st_transform(2249)
#     taz_mrs <- taz %>%
#       inner_join(mrs, by= c("TAZ"= "TAZ_ID"))
#     
#     taz_filtered <- taz_mrs%>%
#       right_join(filter, by= c("TAZ"= "TAZ_ID"))
#     
#     taz_na <- taz%>%
#       inner_join(mrs_na, by= c("TAZ"= "TAZ_ID"))
#     
#     taz_filtered_dif <- taz_mrs %>%
#       right_join(filter_dif, by = c("TAZ"= "TAZ_ID"))
#     
#     #plot map
#     ggplot()+
#       geom_sf(data=taz_mrs, size=.15, color= "dark gray", fill= "dark gray", alpha= .25)+
#       geom_sf(data=taz_na, size=.15, color= "#6b5069", fill= "#946f91", alpha= .5)+
#       geom_sf(data= taz_filtered, size= .15, color= "#b84f00", fill= "#d95f02", alpha= .5)+
#       geom_sf(data= taz_filtered_dif, size= .15, color= "#44947c", fill= "#5fcfad", alpha= .35)+
#       coord_sf(xlim=ranges$x, ylim=ranges$y, expand= TRUE)+
#       theme_map()
#   })
#   
#   observe({
#     brush<- input$plot2_brush
#     if(!is.null(brush)){
#       ranges$x <- c(brush$xmin, brush$xmax)
#       ranges$y <- c(brush$ymin, brush$ymax)
#     } else {
#       ranges$x <- NULL
#       ranges$y <- NULL
#     }
#   })
# Slider text  ####################################
  output$AccText <- renderText({
    dim1 <- as.numeric(input$Dim1Acc)
    dim2 <- input$Dim2Acc
    dim3 <- input$Dim3Acc
    
    text1<- paste("Impact threshold:", dim2, "%", "<br>Disproportionality threshold:", dim3, "%")
    print(text1)
  })
  output$EnvText <- renderText({
    dim1 <- as.numeric(input$Dim1Env)
    dim2 <- input$Dim2Env
    dim3 <- input$Dim3Env
    
    text2<- paste("Impact threshold:", dim2, "%", "<br>Disproportionality threshold:", dim3, "%")
    print(text2)
  })
  output$MobText <- renderText({
    dim1 <- as.numeric(input$Dim1Mob)
    dim2 <- input$Dim2Mob
    dim3 <- input$Dim3Mob
    
    text3<- paste("Impact threshold:", dim2, "%", "<br>Disproportionality threshold:", dim3, "%")
    print(text3)
  })

  # Metric Plot Acc #######################################################
  output$metric_plotAcc <- renderPlot({
    
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricAcc 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1Acc)*0.01
    dim2 <- input$Dim2Acc*0.01
    dim3 <- input$Dim3Acc*0.01
    
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
    dim1 <- as.numeric(input$Dim1Env)*0.01
    dim2 <- input$Dim2Env*0.01
    dim3 <- input$Dim3Env*0.01
    
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
    dim1 <- as.numeric(input$Dim1Mob)*0.01
    dim2 <- input$Dim2Mob*0.01
    dim3 <- input$Dim3Mob*0.01
    
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
# Dynamic text for metric plot #################################
  output$change <- renderText({
    
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metric 
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
    
    paste("For the metric ", tolower(metric_filter), "at the confidence level of ", input$Dim1, "%, ", "there is projected to be ", tolower(income_change), " in the income population group, and there is projected to be ", tolower(min_change), " in the minority population group." )
    
  })
# Impact plot #####################################################  
  # Impact plot Acc #########################################################
  output$impact_plotAcc <- renderPlot({
    #user inputs
    #metric selected from selectInput dropdown
    metric_filter <- input$metricAcc 
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1Acc)*0.01
    dim2 <- input$Dim2Acc*0.01
    dim3 <- input$Dim3Acc*0.01
    
    
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
    dim1 <- as.numeric(input$Dim1Env)*0.01
    dim2 <- input$Dim2Env*0.01
    dim3 <- input$Dim3Env*0.01
    
    
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
    dim1 <- as.numeric(input$Dim1Mob)*0.01
    dim2 <- input$Dim2Mob*0.01
    dim3 <- input$Dim3Mob*0.01
    
    
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
    dim1 <- as.numeric(input$Dim1Acc)*0.01
    dim2 <- input$Dim2Acc*0.01
    dim3 <- input$Dim3Acc*0.01
    
    
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
    dim1 <- as.numeric(input$Dim1Env)*0.01
    dim2 <- input$Dim2Env*0.01
    dim3 <- input$Dim3Env*0.01
    
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
    dim1 <- as.numeric(input$Dim1Mob)*0.01
    dim2 <- input$Dim2Mob*0.01
    dim3 <- input$Dim3Mob*0.01
    
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
# DIDB count table Acc###########################################  
  output$DIDBAcc <- output$DIDBAcc2 <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1Acc)*0.01
    dim2 <- input$Dim2Acc*0.01
    dim3 <- input$Dim3Acc*0.01
    
  
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
      mutate(change_test = cell_spec(change_test, "html", color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
             ) %>%
      rename("Population Group" = poptype)%>%
      rename("Uncertainty Test" = change_test)%>%
      rename("Practical Impact Test"= impact_test)%>%
      rename("Disproportionality Test"= DB)%>%
      rename("DI/DB" = instance) %>%
      rename("Reason"= DB_reason)
    
    
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
  # DIDB count table Env###########################################  
  output$DIDBEnv <- output$DIDBEnv2 <-  renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1Env)*0.01
    dim2 <- input$Dim2Env*0.01
    dim3 <- input$Dim3Env*0.01
    
    
    cat <- "Environmental"
    
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
      mutate(change_test = cell_spec(change_test, "html", color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype)%>%
      rename("Uncertainty Test" = change_test)%>%
      rename("Practical Impact Test"= impact_test)%>%
      rename("Disproportionality Test"= DB)%>%
      rename("DI/DB" = instance) %>%
      rename("Reason"= DB_reason)
    
    
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
  
  # DIDB count table Mob###########################################  
  output$DIDBMob <- output$DIDBMob2 <- renderText({
    #user inputs
    # percentage threshold set by slider
    dim1 <- as.numeric(input$Dim1Mob)*0.01
    dim2 <- input$Dim2Mob*0.01
    dim3 <- input$Dim3Mob*0.01
    
    
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
      mutate(change_test = cell_spec(change_test, "html", color= ifelse(change_test == "Yes", "red", "black"), 
                                     bold = ifelse(change_test == "Yes", TRUE, FALSE)),
             impact_test = cell_spec(impact_test, "html", color= ifelse(impact_test == "Yes", "red", "black"),
                                     bold = ifelse(impact_test == "Yes", TRUE, FALSE)),
             instance = cell_spec(instance, "html", color= ifelse(instance== "Yes", "red", "black"),
                                  bold = ifelse(instance == "Yes", TRUE, FALSE)),
             DB = cell_spec(DB, "html", color = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", "red", "black"),
                            bold = ifelse(DB == "Protected population burdened more" | DB == "Non-protected population benefits more", TRUE, FALSE))
      ) %>%
      rename("Population Group" = poptype)%>%
      rename("Uncertainty Test" = change_test)%>%
      rename("Practical Impact Test"= impact_test)%>%
      rename("Disproportionality Test"= DB)%>%
      rename("DI/DB" = instance) %>%
      rename("Reason"= DB_reason)
    
    
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