#Time trend tab geography

#So for this we need more filters for activity, geography type, substances and measure.
#We need to make it so that we can choose by multiple geographies. 
#We also need to make sure that we can filter so that Scotland is chosen regardless 
#of what the other options are

#three tabs for this part initially
# we will keep a basic landing tab with some of the explanotory data


#libraries
library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(stringr)

##############################################.
############## Reading In Data ----
##############################################.


time_trend <- read.csv("Time_trend_Skeleton.csv")

clinical_types <- as.character(unique(time_trend$Hospital.Clinical.Type))
activity_measure <- as.character(unique(time_trend$Activity))
location_types <- as.character(unique(time_trend$Geography.Type))
locations<- as.character(unique(time_trend$Geography))
drug_types<- as.character(unique(time_trend$Substance))
drug_types1<- list("Main Categories" = as.character(unique(time_trend$Substance)[c(1,5:10)]),
                  "Opioids Sub Categories" = as.character(unique(time_trend$Substance)[2:4]))
drug_types2<- as.character(unique(time_trend$Substance)[c(1,5:10)])
measures<- as.character(unique(time_trend$Measure))



##############################################.
############## User Interface ----
##############################################.
  {
    #Beginning of UI
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

#We are going to divide our UI into discrete sections, called tab panels.
#To do this, we need the layout "tabsetPanel()".

tabsetPanel(
  id = "Panels",
  
##############################################.
############## Home tab ----
##############################################.
  
  #We begin with an introduction tab, where we introduce the explorer and...
  #its purpose.
  #
  
  tabPanel(
    "Home",
    icon = icon("info-circle"),
    style = "float: top; height: 95%; width: 95%;
    background-color: #FFFFFF; border: 0px solid #FFFFFF;",
    column(2,
           h3("Data explorer")
           #End of column 2     
    ),
    column(
      8,
      p(
        br(),
        "The explorer allows you to visualise DRHS data
        in a variety of ways."
      ),
      tags$ul(
        tags$li(
          tags$b(actionLink(
            "link_to_geography", "Geography"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by location."
      ),
      
        tags$li(
          tags$b(actionLink(
            "link_to_substances", "Substances"
          )),
          icon("line-chart"),
          " - shows data on specific DRHS activty over time, by comparing by substances."
        ),
      tags$li(
        tags$b(actionLink(
          "link_to_age_sex", "Age/Sex"
        )),
        icon("child"),
        " - shows data on specific DRHS activty by age and sex."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_deprivation", "Deprivation"
        )),
        icon("bar-chart"),
        " - shows data on specific DRHS activty by deprivation."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_table", "Table"
        )),
        icon("table"),
        " - data tables that underly graphs."
      )
      ),
      
      
      p(
        "When using the data explorer, please take the following
        factors into consideration:"
      ),
      tags$ul(tags$li("Insert text here."),
              tags$li("More text here.")),
      
      p("Still need to amend this page"),
      
      p(
        br(),
        "If you have any trouble using the explorer or have further
        questions relating to the data, please contact us at:",
        tags$b(
          tags$a(href = "mailto:nss.isdtransformingpublishing@nhs.net",
          "nss.isdtransformingpublishing@nhs.net.")
          )
      )
      
      #End of column 8 part
      )
    #End of tab panel
  ),
  
  
##############################################.
############## Geography tab ----
##############################################.
  
  #Create a tab for geography data.
  #Insert the description a
  
  tabPanel(
    "Geography",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Geography"),
    p(
      HTML(
        "This section will contain text about the DRHS data followed
        by link to the 4 types of buttons and lots of graphs that 
        explains all sort of geography stuff"
      )
      ),
    
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
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital/Clinical type
      # 2 - Activity Measure
      # 3 - Geography Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
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
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure",
          label = "Select Activity Measure",
          choices = activity_measure,
          selected = "Stays"
        )
        ),
        
        column(
          4,
          
          shinyWidgets::pickerInput(
            inputId = "Substances",
            label = "Select substances",
            choices = drug_types,
            selected = "Opioids"
          )
          ),
      
      column(
        4 ,
        uiOutput("time_trend_location_types")
        
      ), 
      column(
        4 ,
        uiOutput("time_trend_locations")
      ), 
      
              column(
                4 ,
                shinyWidgets::pickerInput(
                  inputId = "Measure",
                  label = "Select measure",
                  choices = measures,
                  selected = "Rate"
                )
        
      )
    ),
    
    #In the main panel of the tab, insert the geography plot
    
    mainPanel(
      width = 12,
      plotlyOutput("geography_plot",
                   width = "1090px",
                   height = "600px")
    )
    
   
   
    
    #End of tab panel
    ),

##############################################.
############## Substances tab ----
##############################################.

  tabPanel(
    "Substances",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
    h3("Substances"),
    p(
      HTML(
        "This section will contain text about the DRHS data followed
        by link to the 4 types of buttons and lots of graphs that 
        explains all sort of substances related stuff"
      )
      ),
    
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
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital/Clinical type
      # 2 - Activity Measure
      # 3 - Geography Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
      column(
        6,
        
        uiOutput("time_trend_clinical_type2")
        
        
      ),
      
      column(
        6,
        shinyWidgets::pickerInput(
          inputId = "Activity_Measure2",
          label = "Select Activity Measure",
          choices = activity_measure,
          selected = "Stays"
        )
      ),
      
      column(
        6,
        uiOutput("time_trend_location_types2"),
        
        uiOutput("time_trend_substance2")
      ),
      
      column(
        6,
        uiOutput("time_trend_locations2"),
        
        shinyWidgets::pickerInput(
          inputId = "Measure2",
          label = "Select measure",
          choices = measures,
          selected = "Rate"
        )
        
      )
    ),
    
    #In the main panel of the tab, insert the substances plot
    
    mainPanel(
      width = 12,
      plotlyOutput("substances_plot",
                   width = "1090px",
                   height = "600px")
    )
    
    #End of tab panel
      ),


##############################################.
############## Age/Sex tab ----
##############################################.

tabPanel(
  "Age/Sex",
  icon = icon("child")
    ),


##############################################.
############## Deprivation tab ----
##############################################.
tabPanel(
  "Deprivation",
  icon = icon("bar-chart")
    ),

##############################################.
############## Table tab ----
##############################################.

tabPanel(
  "Table",
  icon = icon("table")
)

  #End of tabset panel
    )
#End of UI part
  )
    
    
##############################################.      
############## Server ----
##############################################.  
    
    server  <-  function(input, output, session)
    {
      #These observeEvent() commands will be combined with action buttons in...
      #the User Interface to allow the user to navigate to each tab by clicking...
      #on links in the Introduction page (in addition to the classic way of...
      #navigating, which is by clicking on the tab header itself).
      
##############################################.
############## Home Tab ----
##############################################.
      
      observeEvent(
        input$link_to_geography,
        {
          updateTabsetPanel(session, "Panels", selected = "Geography")
        })
      observeEvent(
        input$link_to_substances,
        {
          updateTabsetPanel(session, "Panels", selected = "Substances")
        })
      observeEvent(
        input$link_to_age_sex,
        {
          updateTabsetPanel(session, "Panels", selected = "Age/Sex")
        })
      observeEvent(
        input$link_to_deprivation,
        {
          updateTabsetPanel(session, "Panels", selected = "Deprivation")
        })
      observeEvent(
        input$link_to_table,
        {
          updateTabsetPanel(session, "Panels", selected = "Table")
        })
      

##############################################.
############## Geography tab ----
##############################################.  
      
      #We need to include the input for geography types in the server section 
      #rather than the UI section. 
      #This is because the 'location' input is dependent on the 'location
      #type' input. 
      
      
      output$time_trend_location_types <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography_type", 
                                  label = "Select location type (multiple selections allowed?)",
                                  choices = location_types, 
                                  selected = "Scotland", 
                                  multiple = TRUE)
      })
      #As Geography Type "Scotland" now has three options within it
      #we may have to switch to if/else function. 
      
      output$time_trend_locations <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography",
                                  label = "Select location (multiple selections allowed)",  
                                  choices = c("Scotland",
                                    sort(
                                  unique(
                                  as.character(
                time_trend$Geography
                [time_trend$Geography.Type == input$Geography_type]
              )
            )
          )
          ),
          multiple = TRUE,
          selected = "Scotland"
          )
      }) 
      
      
      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
      #first for the main 1st summary report (probably need to rename these
      #files so they make more sense)
      geography_new <- reactive({
        time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_Type
            & Activity %in% input$Activity_Measure
            & Geography %in% input$Geography
            & Substance %in% input$Substances
            & Measure %in% input$Measure 
          )
      })
      
      #then we can plot the actual graph, with labels
      output$geography_plot <- renderPlotly({
        #first the tooltip label
        tooltip_geography <- paste0(
          "Financial year: ",
          geography_new()$Years,
          "<br>",
          "Location: ",
          geography_new()$Geography,
          "<br>",
          "Clinical Type: ",
          geography_new()$Hospital.Clinical.Type,
          "<br>",
          geography_new()$Measure,": ",
          geography_new()$Values
        )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = geography_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  Years,
          y = ~  Values,
          color = ~  Geography,
            
          #tooltip
          text = tooltip_geography,
          hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 8),
          width = 1000,
          height = 600
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
          title =
                   paste0( input$Hospital_Clinic_Type,  ", "  ,
                            input$Activity_Measure , " ",input$Measure,
                           " in ", input$Geography_type),
                 separators = ".",
          
          #y=axis formatting       
           yaxis = list(
                                      
                   exponentformat = "none",
                   
                   separatethousands = TRUE,
                   
                   range = c(0, max(geography_new()$Values, na.rm = TRUE) +
                               (max(geography_new()$Values, na.rm = TRUE)
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
                 margin = list(l = 90, r = 60, b = 120, t = 90),
                 font = list(size = 13),
                 titlefont = list(size = 15),
                 
                 #insert legend
                 showlegend = TRUE,
                 legend = list(orientation = 'h',
                               x = 0,
                               y = -0.5,
                               bgcolor = 'rgba(255, 255, 255, 0)',
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
      
      
      
##############################################.
############## Substances tab ----
##############################################.
      
      #We need to include the input for substances types in the server section 
      #rather than the UI section. 
      #This is because the 'location' input is dependent on the 'location
      #type' input. 
      
      
      output$time_trend_location_types2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography_type2", 
                                  label = "Select type of location",
                                  choices = location_types, 
                                  selected = "Scotland")
      })
      
      output$time_trend_locations2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Geography2",
                                  label = "Select location",  
                                  choices = sort(
                                    unique(
                                      as.character(
                                        time_trend$Geography
                                        [time_trend$Geography.Type %in% c("Scotland",input$Geography_type2)]
                                      )
                                    )
                                  ),
                                  selected = "Scotland"
        )
      }) 
      
      
      output$time_trend_clinical_type2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Hospital_Clinic_type2", 
                                  label = "Select clinical",
                                  choices = clinical_types, 
                                  selected = "Combined- Combined ")
      })
      
      output$time_trend_substance2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances2",
                                  label = "Select substances (multiple selections allowed)",  
                                  choices = (if(str_detect(input$Hospital_Clinic_type2, "Overdoses"))
                                  drug_types1
                                  else
                                  drug_types2),
                                  multiple = TRUE, 
                                  selected = "Opiods"
        )
      }) 

      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
      #first for the main 1st summary report (probably need to rename these
      #files so they make more sense)
      substances_new <- reactive({
        time_trend %>%
          filter(
            Hospital.Clinical.Type %in% input$Hospital_Clinic_type2
            & Activity %in% input$Activity_Measure2
            & Geography %in% input$Geography2
            & Substance %in% input$Substances2
            & Measure %in% input$Measure2
          )
      })
      
      #then we can plot the actual graph, with labels
      output$substances_plot <- renderPlotly({
        #first the tooltip label
                tooltip_substances <- paste0(
                  "Financial year: ",
                  substances_new()$Year,
                  "<br>",
                  "Location: ",
                  substances_new()$Geography,
                  "<br>",
                  "Clinical Type: ",
                  substances_new()$Hospital.Clinic.Type,
                  "<br>",
                  substances_new()$Measure,": ",
                  substances_new()$Values
                )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = substances_new(),
          #plot- we wont bother at this point with tailored colour
          x = ~  Years,
          y = ~  Values,
          color = ~  Substance,
          #tooltip
                    text = tooltip_substances,
                    hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 8),
          width = 1000,
          height = 600
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
            title =
              paste0( input$Hospital_Clinic_Type2,  ", "  ,
                      input$Activity_Measure2 , " ",input$Measure2,
                      " in ", input$Geography_type2),
            separators = ".",
            
            #y=axis formatting       
            yaxis = list(
              
              exponentformat = "none",
              
              separatethousands = TRUE,
              
              range = c(0, max(substances_new()$Values, na.rm = TRUE) +
                          (max(substances_new()$Values, na.rm = TRUE)
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
            margin = list(l = 90, r = 60, b = 120, t = 90),
            font = list(size = 13),
            titlefont = list(size = 15),
            
            #insert legend
            showlegend = TRUE,
            legend = list(orientation = 'h',
                          x = 0,
                          y = -0.5,
                          bgcolor = 'rgba(255, 255, 255, 0)',
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
      
      
##############################################.
############## Age/Sex tab ----
##############################################.
      
      
      
##############################################.
############## Deprivation tab ----
##############################################.
      
      
      
##############################################.
############## Table tab ----
##############################################.
      
      
      #End of server
    }
  
  #End of script
}

shinyApp(ui = ui, server = server)
