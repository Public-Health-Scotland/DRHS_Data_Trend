#Name: Drug Related Hospital Statistics (DRHS) Data trend page
#Author: Mike Smith
#Created: 24/01/2019
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.5.1 
#Output: Shiny application

#Descripion: The Data Trend page is a one-page shiny app that is intended to give a high 
#level overview of data at at the level of Scotland, health boards or Alcohol and
#Drug Partnerships (ADP's). It is intended to mirror some of the functionality 
#of the current transformed publications data trend page. 

#There will be three charts in total in the data trend page
# - 1) Rates of Activity Measure (Stays/Patients/New Patients)
# - 2) Rates of Stays broken down by substance categories
# - 3) Rates of Patients broken down by Demographic measure (Age/Sex/Deprivation). 


#There will be intitially three options to choose from 
# - 1) Hospital/Clinical Type
# - 2) Geography Type
# = 3) Geography

#A fourth option will be used to 'toggle' between Age, Sex and SIMD for the 
#demographic charts. 

library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(forcats)
library(DT)
library(RColorBrewer)
library(stringr)


##############################################.
############## Data Input ----
##############################################.


#The current data is stored on the stats server in the SubstanceMisuse1 directory 
#Current approach to reading in data is to take the SPSS output and then cut it down to 
#size and then save as csv files for use. 

filepath<- "\\\\nssstats01\\SubstanceMisuse1\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Temp\\"

#Data to be used for explorer and trend pages
all_data<- readRDS(paste0(filepath,"s06-temp09_num_rate_perc_R-SHINY_rounded.rds"))
#need to rename the final column as value
all_data<-all_data %>% 
  rename("value" = value_Round)


#round the data to nearest two 
all_data <- all_data %>% mutate(value = round(value, 2))



#We will manually change the names of factors in R until we have an agreed 
#terminology for the hospital-clinical type. 

all_data<-all_data %>% 
  mutate(hos_clin_type= fct_recode(hos_clin_type, 
                                   "General Acute and Psychiatric - Combined" = "Combined (SMR01/04) - Combined (Mental & Behavioural/Overdose)",  
                                   "General Acute and Psychiatric - Mental & Behavioural" = "Combined (SMR01/04) - Mental & Behavioural",                      
                                   "General Acute and Psychiatric - Overdose" = "Combined (SMR01/04) - Overdose" ,                                  
                                   "General acute - Combined" =  "General acute (SMR01) - Combined (Mental & Behavioural/Overdose)",
                                   "General acute - Mental & Behavioural" = "General acute (SMR01) - Mental & Behavioural",                    
                                   "General acute - Overdose" =  "General acute (SMR01) - Overdose" ,                               
                                   "Psychiatric - Combined" = "Psychiatric (SMR04) - Combined (Mental & Behavioural/Overdose)" ,  
                                   "Psychiatric - Mental & Behavioural" = "Psychiatric (SMR04) - Mental & Behavioural" ,                      
                                   "Psychiatric - Overdose" =  "Psychiatric (SMR04) - Overdose" ))


activity_summary<-all_data %>% 
  filter(drug_type == "All", 
         age_group == "All",
         sex == "All",
         simd == "All", 
         measure == "Rate")

activity_summary<-activity_summary %>% 
  mutate(activity_type= fct_relevel(activity_type,rev))

#filter by drug type
drug_types<- as.character(unique(all_data$drug_type)[2:7])

drug_summary<- all_data %>% 
  filter(activity_type == "Stays",
         drug_type %in% drug_types,
         age_group == "All",
         sex == "All",
         simd == "All", 
         measure == "Rate") %>% 
  droplevels()

#filter by demography
demographic_summary<- all_data  %>% 
  filter(drug_type == "All",
         activity_type =="Patients",
         ((age_group != "All" & sex == "All" & simd =="All")|
            (age_group == "All" & sex != "All" & simd =="All")|
            (age_group == "All" & sex == "All" & simd !="All")), 
         measure == "Rate") 



#we will also set user input options
clinical_types <- as.character(unique(activity_summary$hos_clin_type))
location_types <- as.character(unique(activity_summary$geography_type))

demographic_types<-c("Age","Sex", "Deprivation")


#For colour 
#Colour Scheme 1 - currently blue
Colour_Scheme<-c('#afeeee','#90cdf5','#1E90FF','#c5e8f7','#84a3b6','#48647a','#0c2a42')
#Alternative Colour Schemes
#Colour_Scheme<-c('#2195f2','#0000FF','#4cbeed','#c5e8f7','#84a3b6','#48647a','#0c2a42')
#Colour_Scheme<-c(brewer.pal(7, "Paired"))


#Beginning of script
{
  ##############################################.
  ############## User Interface ----
  ##############################################.
  ui <- fluidPage(
    
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h1(tags$b("Trend Data")),
    p(
      HTML(
        "The charts shown below provide an overview of drug-related general acute
         and psychiatric hospital stays in Scotland. 
        "), 
      HTML("The charts show the following analyses over time:"),
      tags$ul(
        tags$li("Activity Type (stay rates, patient rates and new patient rates)"),
        tags$li("Drug Type"),
        tags$li("Patient demographics (Age/Sex/Deprivation)")
      ),
      HTML("Using the drop down boxes, the charts can be modified to show: "),
      tags$ul(
        tags$li("data from general acute or psychiatric hospitals
                (or a combination of both);"),
        tags$li("stays associated with mental and behavioural diagnoses, 
                accidental poisonings/overdoses (or a combination of both); and,"),
        tags$li("data from a specific NHS Board or Alcohol and Drug Partnership 
                (ADP) using the drop down boxes ‘Location Type’ and ‘Location’.")
      ),
      HTML(
        "For a more detailed breakdown please consult the [insert link to 
        data explorer when available]"
      )
    ),
    
    HTML("To view your data selection in a table, use the 'Show/hide table' button
         below each chart. Individual trend lines can be hidden by clicking on the
         labels shown in the chart legend. At the top-right corner of the chart, 
         you will see a toolbar with four buttons:"),
    br(),
    br(),
    tags$ul(
      tags$li(
        icon("camera"),
        tags$b("Download plot as a png"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        icon("search"),
        tags$b("Zoom"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        icon("move", lib = "glyphicon"),
        tags$b("Pan"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        icon("home"),
        tags$b("Reset axes"),
        " - click this button to return the axes to their
        default range."
      )
    ),
    
    p(
      br(),
      tags$b(
        "Note: Statistical disclosure control has been applied to protect
        patient confidentiality. Therefore, the figures presented here
        may not be additive and may differ to previous
        sources of information."
      )
    ),
    downloadButton(outputId = "download_glossary", 
                   label = "Download glossary", 
                   class = "glossary"),
    tags$head(
      tags$style(".glossary { background-color: #0072B2; } 
                          .glossary { color: #FFFFFF; }")
    ),

    p(""),
    
    wellPanel(
      tags$style(
        ".well { background-color: #FFFFFF;
        border: 0px solid #336699; }"
      ),
      
      #Insert the reactive filters. As location  is dependent on 
      #location type this part has to be set up in the server as a 
      #reactive object and then placed into the UI. 
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Hospital_Clinic_Type",
          label = "Select hospital - clinical type",
          choices = clinical_types,
          selected = clinical_types[9]
        )
      ),
      
      column(
        4,
        uiOutput("location_types")
      ),
      column(
        4,
        uiOutput("locations")
      )
    ),
   
    
    
    #In the main panel of the summary tab, insert the first plot
    br(),
    br(),
    h3("Activity Type"), 
    br(),
    p("This chart shows drug-related hospital stay rates, patient rates, 
      and new patient rates over time."),
    
    mainPanel(
      width = 12,
      plotlyOutput("activity_summary_plot",
                   width = "1090px",
                   height = "600px"),
      HTML("<button data-toggle = 'collapse' href = '#activitysummary'
                   class = 'btn btn-primary' id = 'activitysummary_link'> 
                   <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'activitysummary' class = 'collapse'>"),
      br(),
      dataTableOutput("activity_summary_table"),
      HTML("</div>"),
      br(),
      br()
    ),
    
    
    tags$head(
      tags$style(HTML("hr {border: 1px solid #000000;}"))
    ),
    
    p(
      
      br(),
      tags$ul(
        tags$li("Insert summary point here [1]"),
        tags$li("Insert summary point here [2]"),
        tags$li("Insert summary point here [3]")
      ),
      hr()
      
    ), 
    h3("Drug type"),
  
    br(),
    p("This chart shows drug-related hospital stay rates, broken down by drug 
      type, over time. "),
    br(),
    
    #then insert the drugs plot
    mainPanel(
      width = 12,
      plotlyOutput("drugs_plot",
                   width = "1090px",
                   height = "600px"),
      HTML("<button data-toggle = 'collapse' href = '#drugs'
                   class = 'btn btn-primary' id = 'drugs_link'> 
                   <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'drugs' class = 'collapse'>"),
      br(),
      dataTableOutput("drugs_table"),
      HTML("</div>"),
      br(),
      br()
    ),
    p(
      
      br(),
      tags$ul(
        tags$li("Insert summary point here [1]"),
        tags$li("Insert summary point here [2]"),
        tags$li("Insert summary point here [3]")
      ),
      hr()
      
    ),
    
    p(
      h3("Demographics"), 
      
      br(),
      (
        "This chart shows drug-related hospital patient rates, broken down by age
        group, sex and deprivation quintile, over time. You can toggle between
        the age group, sex and deprivation analyses using the blue buttons 
        above the chart."  
      ),
      br()
    ), 
    
    
    #Insert demographic options 
    #This part to be converted into toggle button
    column(
      width = 5,
      shinyWidgets::radioGroupButtons(
        inputId = "summary_demographic",
        label = "Show: ",
        choices = demographic_types,
        status = "primary",justified = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                         no = icon("remove", lib = "glyphicon")),
        selected = "Age"
      )
    ),
    #then final demographic plot
    mainPanel(
      width = 12,
      plotlyOutput("demographic_plot",
                   width = "1090px",
                   height = "600px"),
      HTML("<button data-toggle = 'collapse' href = '#demographic'
                   class = 'btn btn-primary' id = 'demographic_link'> 
                   <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'demographic' class = 'collapse'>"),
      br(),
      dataTableOutput("demographic_table"),
      HTML("</div>"),
      br(),
      br(), 
      p(
        
        br(),
        tags$ul(
          tags$li("Insert summary point here [1]"),
          tags$li("Insert summary point here [2]"),
          tags$li("Insert summary point here [3]")
        )
      )
    )
    #End of UI part
    
  )  
  
  
  ##############################################.
  ############## Server ----
  ##############################################.
  
  
  #Beginning of server
  server  <-  function(input, output)
  {
    #We first need to set up the location type and location options 
    #so that they location options are dependent on location type
    
    output$location_types <- renderUI({
      shinyWidgets::pickerInput(inputId = "Location_type", 
                                label = "Select location type ",
                                choices = location_types, 
                                selected = "Scotland")
    })
    
    output$locations <- renderUI({
      shinyWidgets::pickerInput(inputId = "Location",
                                label = "Select location", 
                                options = list(size=5),
                                choices = 
                                  unique(
                                    as.character(
                                      activity_summary$geography
                                      [activity_summary$geography_type %in% input$Location_type]
                                      
                                    )
                                  )
      )
    }) 
    
    #need to work out how to get Scotland options in correct order
    #'Scotland', 'Outside Scotland', 'Unknown'
    #levels(factor(location)) %in% input$Location_type
    
    #we can then plot the graph based on the user input.
    #First we create a subset  of the data based on user input
    
    #For the activity summary
    activity_summary_new <- reactive({
      activity_summary %>%
        filter(
          hos_clin_type %in% input$Hospital_Clinic_Type
          & geography %in% input$Location
        )%>%
        select(year,hos_clin_type, activity_type,geography,value)
    })
    
    #for the substances summary
    drug_summary_new <- reactive({
      drug_summary %>%
        filter(
          hos_clin_type %in% input$Hospital_Clinic_Type
          & geography %in% input$Location
        )%>%
        select(year,hos_clin_type,drug_type,geography,value)
    })
    
    #for the demographic summary
    #as this is based on two files (and on two separate columns in one
    #file) then an if/else function is employed to select the correct data
    demographic_summary_new <- reactive({
      if (input$summary_demographic == "Age")
      {
        demographic_summary %>%
          filter(
            hos_clin_type %in% input$Hospital_Clinic_Type
            & geography %in% input$Location
            & age_group != "All"
          )%>%
          select(year,hos_clin_type,geography,age_group,value)
      }
      else if(input$summary_demographic == "Sex")
      {demographic_summary %>%
          filter(
            hos_clin_type %in% input$Hospital_Clinic_Type
            & geography %in% input$Location
            & sex != "All"
          ) %>%
          select(year,hos_clin_type,geography,sex,value)
        
      }
      else if (input$summary_demographic == "Deprivation")
      {
        demographic_summary %>%
          filter(hos_clin_type %in% input$Hospital_Clinic_Type
                 & geography %in% input$Location
                 & simd != "All"
          )%>%
          select(year,hos_clin_type,geography,simd,value)
      }
    })
    
    
    #Then we can plot the actual graph, with labels
    
    #Activity Summary plot
    
    #Tooltip for graphs. 
    output$activity_summary_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        "Activity type: ", 
        activity_summary_new()$activity_type,
        "<br>",
        "Financial year: ",
        activity_summary_new()$year,
        "<br>",
        "Rate: ",
        activity_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = activity_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  activity_type,
        colors = ~ Colour_Scheme,
        #tooltip
        text = tooltip_summary,
        hoverinfo = "text",
        #type
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        width = 1000,
        height = 600
      ) %>%
        
        #add in title to chart
        
        layout(title =
                 ( paste0(str_to_sentence(paste0("Activity type rates for ",
                                         input$Hospital_Clinic_Type,
                        " in ")), input$Location)),
               
               separators = ".",
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(activity_summary_new()$value, na.rm = TRUE) +
                             (max(activity_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR per 100,000",
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ),
                 collapse = ""),
                 showline = TRUE,
                 ticks = "outside"
                 
               ),
               
               #Set the tick angle to minus 45. It's the only way for the x...
               #axis tick labels (fin. years) to display without overlapping...
               #with each other.
               #Wrap the x axis title in blank spaces so that it doesn't...
               #overlap with the x axis tick labels.
               
               xaxis = list(range = c(-1,22),
                            tickangle = -45,
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE,
                            ticks = "outside"),
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 90, r = 60, b = 160, t = 90),
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #insert legend
               showlegend = TRUE,
               legend = list(bgcolor = 'rgba(255, 255, 255, 0)',
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                             'zoomOut2d', 'autoScale2d',
                                             'toggleSpikelines',
                                             'hoverCompareCartesian',
                                             'hoverClosestCartesian'),
               displaylogo = F, collaborate = F, editable = F)
      
    })
    
    #Insert table
    output$activity_summary_table <- renderDataTable({
      datatable(activity_summary_new(),
                colnames = c("Financial year",
                             "Hospital - clinical type",
                             "Activity type",
                             "Location",
                             "Rate"),
                rownames = FALSE,
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
                
      )
    })
    
    # Substances Plot
    #Again start with the tooltip summary
    output$drugs_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        "Drug type: ",
        drug_summary_new()$drug_type,
        "<br>",
        "Financial year: ",
        drug_summary_new()$year,
        "<br>",
        "Rate: ",
        drug_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = drug_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  drug_type,
        colors = ~ Colour_Scheme,
        #tooltip
        text = tooltip_summary,
        hoverinfo = "text",
        #type
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        width = 1000,
        height = 600
      )%>%
        
        #add in title to chart
        
        layout(title =
                 paste0(str_to_sentence(paste0("Stay rates for ",
                                               input$Hospital_Clinic_Type,
                        " in ")), input$Location, " by drug type"),
               
               separators = ".",
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(drug_summary_new()$value, na.rm = TRUE) +
                             (max(drug_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR per 100,000",
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ),
                 collapse = ""),
                 showline = TRUE,
                 ticks = "outside"
                 
               ),
               
               #Set the tick angle to minus 45. It's the only way for the x...
               #axis tick labels (fin. years) to display without overlapping...
               #with each other.
               #Wrap the x axis title in blank spaces so that it doesn't...
               #overlap with the x axis tick labels.
               
               xaxis = list(range = c(-1,22),
                            tickangle = -45,
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE,
                            ticks = "outside"),
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 90, r = 60, b = 160, t = 90),
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #insert legend
               showlegend = TRUE,
               legend = list(bgcolor = 'rgba(255, 255, 255, 0)',
                             bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                             'zoomOut2d', 'autoScale2d',
                                             'toggleSpikelines',
                                             'hoverCompareCartesian',
                                             'hoverClosestCartesian'),
               displaylogo = F, collaborate = F, editable = F)
      
    })
    
    output$drugs_table <- renderDataTable({
      datatable(drug_summary_new(),
                colnames = c("Financial year",
                             "Hospital - clinical type",
                             "Drug type",
                             "Location",
                             "Rate"),
                rownames = FALSE,
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
      )
    })
    
    
    #Demographic Plot
    
    output$demographic_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        input$summary_demographic, ": ",
        demographic_summary_new()[,4],
        "<br>",
        "Financial year: ",
        demographic_summary_new()$year,
        "<br>",
        
        
        "Rate: ",
        demographic_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = demographic_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  demographic_summary_new()[,4],
        colors = ~ Colour_Scheme,
        #tooltip
        text = tooltip_summary,
        hoverinfo = "text",
        #type
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        width = 1000,
        height = 600
      )%>%
        
        #add in title to chart
        
        
        layout(title = (
          if (input$summary_demographic == "Deprivation")
          {
            paste0(str_to_sentence(paste0("Patient rates for ",
                   input$Hospital_Clinic_Type,
                   " in ")), input$Location, 
                   " by deprivation quintile")
          }
          else if (input$summary_demographic == "Age")
          {
            paste0(str_to_sentence(paste0("Patient rates for ",
                   input$Hospital_Clinic_Type,
                   " in ")), input$Location, 
                   " by age group")
          }
          else {
            paste0(str_to_sentence(paste0("Patient rates for ",
                   input$Hospital_Clinic_Type,
                   " in ")), input$Location, 
                   " by sex")
          }
        ),
               
               separators = ".",
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(demographic_summary_new()$value, na.rm = TRUE) +
                             (max(demographic_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR per 100,000",
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)
                 ),
                 collapse = ""),
                 showline = TRUE,
                 ticks = "outside"
                 
               ),
               
               #Set the tick angle to minus 45. It's the only way for the x...
               #axis tick labels (fin. years) to display without overlapping...
               #with each other.
               #Wrap the x axis title in blank spaces so that it doesn't...
               #overlap with the x axis tick labels.
               
               xaxis = list(range = c(-1,22),
                            tickangle = -45,
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE,
                            ticks = "outside"),
               
               #        #Fix the margins so that the graph and axis titles have enough...
               #       #room to display nicely.
               #      #Set the font sizes.
               #
               margin = list(l = 90, r = 60, b = 160, t = 90),
               font = list(size = 13),
               titlefont = list(size = 15),
               
               #insert legend
               showlegend = TRUE,
               legend = list(
                 bgcolor = 'rgba(255, 255, 255, 0)',
                 bordercolor = 'rgba(255, 255, 255, 0)')) %>%
        
        #        #Remove unnecessary buttons from the modebar.
        
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                             'zoomOut2d', 'autoScale2d',
                                             'toggleSpikelines',
                                             'hoverCompareCartesian',
                                             'hoverClosestCartesian'),
               displaylogo = F, collaborate = F, editable = F)
      
    })
    
    
    #Insert table
    output$demographic_table <- renderDataTable({
      datatable(demographic_summary_new(),
                rownames = FALSE,
                colnames = c("Financial year",
                             "Hospital - clinical type",
                             "Location",
                             input$summary_demographic,
                             "Rate"),
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
      )
      
       
    })
      
    #glossary link
    
      output$download_glossary <- downloadHandler(
        filename = 'glossary.pdf',
        content = function(file) {
          file.copy(paste0(filepath, "www\\glossary.pdf"), file)
        }
      )
      
    #End of server
  }
  #End of script
}

shinyApp(ui = ui, server = server)
