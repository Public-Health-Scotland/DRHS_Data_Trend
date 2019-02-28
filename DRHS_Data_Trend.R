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
# - 3) Rates of Patients broken down by Demographic measure (Age/Sex/SIMD). 


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


##############################################.
############## Data Input ----
##############################################.


#The current data is stored on the stats server in the SubstanceMisuse1 directory 
#Current approach to reading in data is to take the SPSS output and then cut it down to 
#size and then save as csv files for use. 

path<-"\\\\nssstats01\\SubstanceMisuse1\\Topics\\DrugRelatedHospitalStats\\Publications\\DRHS\\20181218\\Temp\\Dashboard_Data_Temp\\"

activity_summary <- read.csv(paste0(path,"activity_summary.csv"))
drug_summary <- read.csv(paste0(path, "drug_summary.csv"))
demographic_summary<- read.csv(paste0(path,"demographic_summary.csv"))

#we will also set user input options
clinical_types <- as.character(unique(activity_summary$hos_clin_type))
location_types <- as.character(unique(activity_summary$geography_type))

demographic_types<-c("Age","Sex", "SIMD")


#Beginning of script
{
  ##############################################.
  ############## User Interface ----
  ##############################################.
  ui <- fluidPage(

    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h1(tags$b("Data Trends In Scotland")),
    p(
      HTML(
        "This is a one page tab that covers data at a high level overview
         of Scotland. The text wording can be agreed at a later date. "), 
        br(),
        br(),
        "It could include stuff like - ",
      tags$ul(
      tags$li("Information about what this page shows"),
      tags$li("Information about what this page ", tags$i("doesn't"), " show"),
      tags$li("A link to the data explorer")
      )),
    
    tags$ul(
      tags$li(
        tags$b("Download plot as a png"),
        icon("camera"),
        " - click this button to save the graph as an image
        (please note that Internet Explorer does not support this
        function)."
      ),
      tags$li(
        tags$b("Zoom"),
        icon("search"),
        " - zoom into the graph by clicking this button and then
        clicking and dragging your mouse over the area of the
        graph you are interested in."
      ),
      tags$li(
        tags$b("Pan"),
        icon("move", lib = "glyphicon"),
        " - adjust the axes of the graph by clicking this button
        and then clicking and moving your mouse in any direction
        you want."
      ),
      tags$li(
        tags$b("Reset axes"),
        icon("home"),
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
          label = "Select clinical type",
          choices = clinical_types,
          selected = "Combined (General acute/Psychiatric) - Combined (Mental and Behavioural/Overdose)"
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

    h3("Activity Measure"), 
br(),
p("Insert text here explaing what this shows. My preference would be to include only
  text that is either above or below the graph (not a bit above and a bit 
  underneath as in data trend page). I think that this makes everything a little 
  bit clearer"),

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
  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
),

    p(
      br(),

      br(),
      
      hr()
      
      
    ), 
h3("Substances"),

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
      h3("Demographics"), 
      br(),
      (
        "We can then put some more text underneath this plot. We now add a drop down menu
        for the demographic stuff. This would look better as a button type thing rather 
        than a drop down menu, but this will suffice for now"  
    ),
    br()
    ), 
    

    #Insert demographic options 
    #This part to be converted into toggle button
    column(
      width = 6,
      shinyWidgets::radioGroupButtons(
        inputId = "summary_demographic",
        label = "Select demographic",
        choices = demographic_types,
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
        br()
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
      else if (input$summary_demographic == "SIMD")
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
        "Financial year: ",
        activity_summary_new()$year,
        "<br>",
        "Location: ",
        activity_summary_new()$geography,
        "<br>",
        "Clinical Type: ",
        activity_summary_new()$hos_clin_type,
        "<br>",
        "EASR rates: ",
        activity_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = activity_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  activity_type,
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
                 paste0("Activity rates for ",input$Hospital_Clinic_Type,
                        " in ", input$Location),
               
               separators = ".",
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(activity_summary_new()$value, na.rm = TRUE) +
                             (max(activity_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR Rates",
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
               
               xaxis = list(tickangle = -45,
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
                colnames = c("Financial Year",
                             "Hospital - Clinical Type",
                             "Activity Measure",
                             "Location",
                             "EASR Rates"),
                rownames = FALSE,
                style = "Bootstrap"
                  )
    })
    
    # Substances Plot
    #Again start with the tooltip summary
    output$drugs_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        "Financial year: ",
        drug_summary_new()$year,
        "<br>",
        "Location: ",
        drug_summary_new()$geography,
        "<br>",
        "Clinical Type: ",
        drug_summary_new()$hos_clin_type,
        "<br>",
        "EASR rates: ",
        drug_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = drug_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  drug_type,
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
                 paste0("Stay rates for ",input$Hospital_Clinic_Type,
                        " in ", input$Location, " by drug category"),
               
               separators = ".",
               
               yaxis = list(
                 
                exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(drug_summary_new()$value, na.rm = TRUE) +
                             (max(drug_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR Rates",
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
               
               xaxis = list(tickangle = -45,
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
                colnames = c("Financial Year",
                             "Hospital - Clinical Type",
                             "Substance",
                             "Location",
                             "EASR Rates"),
                rownames = FALSE,
                style = "Bootstrap"
      )
    })
    
    
    #Demographic Plot
    
    output$demographic_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
      "Financial year: ",
        demographic_summary_new()$year,
        "<br>",
        "Location: ",
        demographic_summary_new()$geography,
        "<br>",
        "Clinical Type: ",
        demographic_summary_new()$hos_clin_type,
        "<br>",
        "EASR rates: ",
        demographic_summary_new()$value
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = demographic_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  demographic_summary_new()[,4],
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
                 paste0("Patient rates for ",
                        input$Hospital_Clinic_Type,
                        " in ", input$Location, 
                        " by ", input$summary_demographic),
               
               separators = ".",
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(demographic_summary_new()$value, na.rm = TRUE) +
                             (max(demographic_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 
                 title = paste0(c(
                   rep("&nbsp;", 20),
                   "EASR Rates",
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
               
               xaxis = list(tickangle = -45,
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
                colnames = c("Financial Year",
                             "Hospital - Clinical Type",
                             "Location",
                             input$summary_demographic,
                             "EASR Rates"),
                style = "Bootstrap"
      )
    })
    
    #End of server
  }
  #End of script
  }

shinyApp(ui = ui, server = server)
