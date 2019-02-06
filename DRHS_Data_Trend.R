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

#So first input will be a slightly simplified table and will only inlcude 
#enough data to test that user inputs are working correctly. 
# - There will be intitially three options to choose from 
# - 1) Hospital/Clinical Type
# - 2) Geography Type
# = 3) Geography

#A fourth option will be used to 'toggle' between Age, Sex and SIMD for the 
#demographic charts. It is presented here as a drop down menu at present
#to simply determine that the reactive elements are working correctly. 

library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(forcats)

##############################################.
############## Data Input ----
##############################################.


#The data that is being read in here is a simplified version of the real 
#data that would be used. In practice the data that is generated for the data trend 
#page would be utilised for this page. 

#The first input would be the data explorer "time trend" files that could 
#be used to generate the following tables
# 1) The file filtered to only include 'all drugs' in the substances category
activity_summary <- read.csv("Time_trend_skeleton.csv")
activity_summary<- activity_summary%>%
  filter(Substance=="All", 
         Measure== "Rates")

# 2) The file filtered to only inlcude 'stays' in the activity measure column
drug_summary <- read.csv("Time_trend_skeleton.csv")
#choose only the 6 main categories (not sub categories)
Substances<- c("Opiods", "Cannabinoids", "Sedatives/Hypnotics",
               "Cocaine","Other Stimulants","Multiple/Other")

drug_summary<-drug_summary%>%
  filter(Activity=="Stays", 
         Measure== "Rates",
         Substance %in% Substances)

#The following demographic tables would be read in from two files 
#one for the age/sex file and one for the SIMD file 
#Note- SIMD changed to factor. 

demographic_summary<-read.csv("Age_Sex_Data_Trend_Skeleton.csv")
demographic_summary<- demographic_summary%>%
  mutate(Age = fct_relevel(Age, "Under 15"))%>%
  mutate(Age = fct_relevel(Age, "Over 64", after = 6))

str(demographic_summary$Age)
SIMD_summary<-read.csv("SIMD_Data_Trend_Skeleton.csv")
SIMD_summary$SIMD<-as.factor(SIMD_summary$SIMD)


#we will also set user input options
clinical_types <- as.character(unique(activity_summary$Hospital.Clinical.Type))
location_types <- c("Scotland", "Health Board", "ADP")
location <- factor(activity_summary$Geography)%>%
  fct_relevel("Scotland")
str(location)
#location <- as.character(unique(activity_summary$Geography))
demographic_types<-c("Age","Sex", "SIMD")


#Beginning of script
{
  ##############################################.
  ############## User Interface ----
  ##############################################.
  ui <- fluidPage(
    style = "width: 100%; height: 100%; max-width: 1200px;",
    tags$head(
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),
      
      #The following chunk of code does three things:
      # 1. Paints the ribbon that contains the tab headers white.
      # 2. Highlights the header of the active tab in blue.
      # 3. Sets the font size for the sentence that appears above the...
      # cross-boundary flow diagram.
      
      tags$style(
        HTML(
          ".tabbable > .nav > li > a {
          color: #000000;
}

.tabbable > .nav > li[class = active] > a {
background-color: #0072B2;
color: #FFFFFF;
}

#flow_text {
font-size: 15px;
}")
)
        ),

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
          selected = "Combined- Combined "
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
                   height = "600px")
),

tags$head(
  tags$style(HTML("hr {border-top: 1px solid #000000;}"))
),

    p(
      br(),
      (
        "We can then put some more text underneath the plot. We sill need to format 
        it neatly (with headers and the likes)"
      ),
      br(),
      
      hr(),
      (
        "It would also be useful to maybe have lines between the graphs to make the 
        separation between them clear, but so far I can only figure out how to generate
        it when there is some text above!"
      )
      
    ), 
h3("Substances"),

    #then insert the drugs plot
    mainPanel(
      width = 12,
      plotlyOutput("drugs_plot",
                   width = "1090px",
                   height = "600px")
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
      ),
      #then final demographic plot
      mainPanel(
        width = 12,
        plotlyOutput("demographic_plot",
                     width = "1090px",
                     height = "600px")
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
                                choices = sort(
                                  unique(
                                    as.character(
                                      activity_summary$Geography
                                      [activity_summary$Geography.Type %in% input$Location_type]
                                    )
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
          Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
          & Geography %in% input$Location
        )
    })
    
    #for the substances summary
    drug_summary_new <- reactive({
      drug_summary %>%
        filter(
          Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
          & Geography %in% input$Location
        )
    })
    
    #for the demographic summary
    #as this is based on two files (and on two separate columns in one
    #file) then an if/else function is employed to select the correct data
    demographic_summary_new <- reactive({
      if (input$summary_demographic == "Age")
      {
        demographic_summary %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
            & Geography %in% input$Location
            & Sex == "All"
          )
      }
      else if(input$summary_demographic == "Sex")
      {demographic_summary %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
            & Geography %in% input$Location
            & Age == "All"
          ) 
          
      }
      else if (input$summary_demographic == "SIMD")
      {
        SIMD_summary %>%
          filter(Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
                 & Geography %in% input$Location
                 )
      }
    })
    

    #Then we can plot the actual graph, with labels
    
    #Activity Summary plot
    
    #Tooltip for graphs. 
    output$activity_summary_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        "Financial year: ",
        activity_summary_new()$Years,
        "<br>",
        "Location: ",
        activity_summary_new()$Geography,
        "<br>",
        "Clinical Type: ",
        activity_summary_new()$Hospital.Clinical.Type,
        "<br>",
        "EASR rates: ",
        activity_summary_new()$Values
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = activity_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  Years,
        y = ~  Values,
        color = ~  Activity,
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
                 
                 range = c(0, max(activity_summary_new()$Values, na.rm = TRUE) +
                             (max(activity_summary_new()$Values, na.rm = TRUE)
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
    
    # Substances Plot
    #Again start with the tooltip summary
    output$drugs_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        "Financial year: ",
        drug_summary_new()$Years,
        "<br>",
        "Location: ",
        drug_summary_new()$Geography,
        "<br>",
        "Clinical Type: ",
        drug_summary_new()$Hospital.Clinical.Type,
        "<br>",
        "EASR rates: ",
        drug_summary_new()$Values
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = drug_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  Years,
        y = ~  Values,
        color = ~  Substance,
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
                 
                 range = c(0, max(drug_summary_new()$Values, na.rm = TRUE) +
                             (max(drug_summary_new()$Values, na.rm = TRUE)
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
    
    #Demographic Plot
    
    output$demographic_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
      "Financial year: ",
        demographic_summary_new()$Years,
        "<br>",
        "Location: ",
        demographic_summary_new()$Geography,
        "<br>",
        "Clinical Type: ",
        demographic_summary_new()$Hospital.Clinical.Type,
        "<br>",
        "EASR rates: ",
        demographic_summary_new()$Values
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = demographic_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  Years,
        y = ~  Values,
        color = ~  get(input$summary_demographic),
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
                 
                 range = c(0, max(demographic_summary_new()$Values, na.rm = TRUE) +
                             (max(demographic_summary_new()$Values, na.rm = TRUE)
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
    
    #End of server
  }
  #End of script
  }

shinyApp(ui = ui, server = server)
