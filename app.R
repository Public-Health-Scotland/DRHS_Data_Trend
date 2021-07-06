# Name: Drug Related Hospital Statistics (DRHS) Data trend page
# Author: Mike Smith
# Created: 24/01/2019
# Updated: 28/05/2021
# Type: Data visualisation
# Written on: RStudio
# Written for: R version 3.5.1 
# Output: Shiny application

# Descripion: The Data Trend page is a one-page shiny app that is intended to give a high 
# level overview of data at at the level of Scotland, health boards or Alcohol and
# Drug Partnerships (ADP's). It is intended to mirror some of the functionality 
# of the current transformed publications data trend page. 

# There will be three charts in total in the data trend page
# - 1) Rates of Activity Measure (Stays/Patients/New Patients)
# - 2) Rates of Stays broken down by substance categories
# - 3) Rates of Patients broken down by Demographic measure (Age/Sex/Deprivation). 


# There will be intitially three options to choose from 
# - 1) Hospital Type
# - 2) Diagnosis Type
# = 3) Geography

# A fourth option will be used to 'toggle' between Age, Sex and SIMD for the 
# demographic charts. 

library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(forcats)
library(DT)
library(stringr)
library(shinyBS)
library(bsplus)
library(shinymanager)

##############################################.
############## Data Input ----
##############################################.



# Data to be used for explorer and trend pages
activity_summary<- readRDS("activity_summary.rds")
drug_summary<-readRDS("drug_summary.rds")
demographic_summary<-readRDS("demographic_summary.rds")

# We will also set user input options
hospital_types <- as.character(unique(activity_summary$hospital_type))
hospital_types<-c(hospital_types[3],hospital_types[1],hospital_types[2])
diagnosis_types <- as.character(unique(activity_summary$diagnosis_type))
diagnosis_types<-c(diagnosis_types[3],diagnosis_types[1],diagnosis_types[2])
locations <- as.character(unique(activity_summary$geography))
location_types<-list("Scotland" = locations[1],
                     "NHS Board of residence" = locations[2:15],
                     "ADP of residence" = locations[16:46])
drug_types<- as.character(unique(drug_summary$drug_type))
demographic_types<-c("Age","Sex", "Deprivation")


# Beginning of script
{
  ##############################################.
  ############## User Interface ----
  ##############################################.
  ui <- #secure_app( #uncomment if needing password protection
    fluidPage(style = "width: 100%; height: 100%; max-width: 1200px;",
    
                  tags$head(
                    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                                type="text/javascript")
                  ),
                  
    
    titlePanel(title=div(h1("Drug-Related Hospital Statistics"),
                         h4("Substance Use"), 
                         style = "height:96px;"),
               windowTitle = "Drug-Related Hospital Statistics"),
    tabPanel(title = "",
    
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",
  
    p(
      h4("The Trend data page provides an overview of drug-related hospital stays 
         in Scotland over time, based on the following charts:"),
      tags$ul(
        tags$li(tags$a(href= '#activity_link',tags$b("Activity type")),
                " (stay rates, patient rates and new patient rates)"),
        tags$li(tags$a(href = '#drugs_link',  
                       tags$b("Drug type"))),
        tags$li(tags$a(href='#demographics_link', tags$b("Patient demographics")),
                " (Age/Sex/Deprivation - choose between these using the blue 
                buttons above the chart)")
      )),

    
      bs_accordion(id = "drhs_text") %>%
        bs_set_opts(panel_type = "primary") %>%
        bs_append(title = tags$u("Data selection"), 
                  content = p("The charts can be modified using the drop down boxes: ",
                  tags$ul(
                    tags$li("Hospital type: general acute or psychiatric hospital data 
                (or any hospital type);"),
                    tags$li("Diagnosis grouping: mental & behavioural stays, accidental 
                poisoning/overdose stays (or any diagnosis); and,"),
                    tags$li("Location: data from Scotland, specific NHS Boards or 
                Alcohol and Drug Partnerships.")
                  )))%>% 
      bs_append(title = tags$u("Chart functions"), 
                content = p("At the top-right corner of the chart, you will see a ",
                            icon("camera"), "icon: use this to save an image of the chart ",
                            HTML(paste0("(",tags$b("not"))), 
                            tags$b("available in Internet "),
                            HTML(paste0(tags$b("Explorer"),").")),
                            br(),br(),
                            "Categories can be shown/hidden by clicking on labels in the
                            legend to the right of the chart.")
      )%>%
      bs_append(title = tags$u("Table functions"),
                content = p("To view your data selection in a table, use the
                            'Show/hide table' button below each chart.",
                  tags$ul(
                    tags$li(
                      icon("sort", lib = "glyphicon"),
                      tags$b("Sort"),
                      " - click to sort a table in ascending or descending 
                      order based on the values in a column. "
                    ),
                    tags$li(
                      tags$b("Page controls"),
                      " - switch to specific page of data within a table. "
                    )
                    ), 
                  "Categories can be shown/hidden by clicking on labels 
                  in the legend to the right of each chart."))
      
      ,
      
     p(
      HTML(paste0('A more detailed breakdown of these data is available in the <b> <a href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/data-explorer/">Data explorer</a></b>.'))
      ) ,
    p(
      "Technical terms are explained in the ", 
          HTML(paste0('<b><a href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/glossary/">Glossary</a></b>.'))
    ),
    p(
      "If you experience any problems using this dashboard or have further
      questions relating to the data, please contact us at:",
      HTML(paste0('<b> <a href="mailto:phs.drugsteam@phs.scot">phs.drugsteam@phs.scot</a></b>.'))
    ),
    

    
    p(
      tags$b(
        "Note: Statistical disclosure control has been applied to protect
        patient confidentiality. Therefore, the figures presented here
        may not be additive and may differ from previous publications."
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
          inputId = "Hospital_Type",
          label = "Hospital type",
          choices = hospital_types
        )
      ),
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Diagnosis_Type",
          label = "Diagnosis grouping",
          choices = diagnosis_types
        )
      ),
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Location",
          label = "Location",
          choices = location_types,
          options = list(size=5, 
                         `live-search`=TRUE)
        )
      )
    ),
   
    
    
    #In the main panel of the summary tab, insert the first plot
    br(),
    br(),
    h3("Activity type",id = 'activity_link'), 
    br(),
    
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
      p("Main points (Scotland)",
      tags$ul(
        tags$li("Between 1996/97 and 2019/20, there was a greater than threefold increase in the
                rate of drug-related hospital stays in Scotland from 74 to 282 stays per 100,000 population."),
        tags$li("The rate of drug-related general acute hospital stays increased from 51 to 243 stays 
                per 100,000 population between 1996/97 and 2019/20, with a sharper increase observed in recent years."),
        tags$li("After a lengthy period of stability, the rate of drug-related psychiatric hospital stays in Scotland increased from 29 to 40 
                stays per 100,000 population between 2014/15 and 2016/17, and has remained at around this level to 2019/20."),
        tags$li("In 2019/20, there were 5,586 people who were treated in a Scottish hospital (general acute 
                or psychiatric) in relation to drug use for the first time. The drug-related new patient rate
                increased from 55 new patients per 100,000 population in 2006/07 to 103 new patients per 100,000 population in 2019/20.")
      )),
      tags$a(href = '#Top',  
             icon("circle-arrow-up", lib= "glyphicon"),"Back to top"),
      hr()
      
    ), 
    h3("Drug type", id= 'drugs_link'),
  
    br(),
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
      p("Main points (Scotland)",
      tags$ul(
        tags$li("In 2019/20, there were 140 stays per 100,000 population associated with opioids in
                Scottish hospitals (general acute or psychiatric). The next most common drug types recorded
                in hospital stays were multiple/other drugs (64 stays per 100,000 population), sedatives/hypnotics (50),
                cannabinoids (38) and cocaine (35). Hospital stay rates for each of these drug types have increased since 2013/14."),
        tags$li("In 2019/20, opioids (drugs similar to heroin) were associated with 52% of drug-related general acute
                stays in Scotland. The percentage of drug-related general acute stays associated with opioids has
                decreased (from 70% in 2011/12) as stays related to other drug types have become more common. Between
                2014/15 and 2019/20, the percentage of drug-related general acute stays associated with sedatives/hypnotics
                increased from 8% to 19% and the percentage of cocaine-related stays increased from 7% to 14%."),
        tags$li("In 2019/20, over half (53%) of drug-related psychiatric stays in Scotland were associated with
                ‘multiple/other’ drugs (including, volatile solvents, multiple drug use and use of other psychoactive substances)")
      )),
      tags$a(href = '#Top',  
             icon("circle-arrow-up", lib= "glyphicon"),"Back to top"),
      hr()
      
    ),
    
    p(
      h3("Demographics", id= 'demographics_link')
    ), 
    
    
    #Insert demographic options 
    #This part to be converted into toggle button
    column(
      width = 5,
      shinyWidgets::radioGroupButtons(
        inputId = "summary_demographic",
        label = "Show: ",
        choices = demographic_types,
        status = "primary",
        justified = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")),
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
      p(
        
        br(),
        p("Main points (Scotland)",
        tags$ul(
          tags$li("Since 2013/14, the most common age group among people with a drug-related hospital stay 
                  (general acute or psychiatric) has been 35-44 years. Patient rates for this group increased
                  ninefold from 56 to 514 patients per 100,000 population between 1996/97 and 2019/20."),
          tags$li("Following a lengthy period of annual decreases, patient rates for 15-24 year olds 
                  increased from 126 patients per 100,000 population in 2012/13 to 226 patients per 100,000 population
                  in 2019/20. The 2019/20 patient rate for this age group was the highest recorded since
                  2002/03 (258 patients per 100,000 population)."),
          tags$li("Between 1996/97 and 2019/20, drug-related patient rates for males were consistently 
                  more than double drug-related patient rates for females."),
          tags$li("In 2019/20, approximately half of the patients with a drug-related general acute 
                  or psychiatric stay lived in the 20% of most deprived areas in Scotland.")
        ), 
        tags$a(href = '#Top',  
               icon("circle-arrow-up", lib= "glyphicon"),"Back to top")
      ))
    )
    #End of UI part
    
  ), 
  HTML('<div data-iframe-height></div>')
  )
#)#secure app  
  ##############################################.
  ############## Server ----
  ##############################################.
  
  
  #Beginning of server
  credentials <- readRDS("admin/credentials.rds")
  server  <-  function(input, output,session)
  {

    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
      reactiveValuesToList(res_auth)
    })
    
    #Graph information text output
    output$text_output<-renderUI({ 
      p(HTML("Show/hide table - show data in a table below the chart."),
        
      p(HTML("At the top-right corner of the chart, 
             you will see a toolbar with four buttons:"),
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
        HTML("Categories can be shown/hidden by clicking on labels
             in the legend to the right of each chart.")
          ))
    })
    
    
    #we can then plot the graph based on the user input.
    #First we create a subset  of the data based on user input
    
    #For the activity summary
    activity_summary_new <- reactive({
      activity_summary %>%
        filter(
          hospital_type %in% input$Hospital_Type
          & diagnosis_type %in% input$Diagnosis_Type
          & geography %in% input$Location
        )%>%
        select(year,hospital_type, diagnosis_type, activity_type,geography,value)
    })
    
    #for the substances summary
    drug_summary_new <- reactive({
      drug_summary %>%
        filter(
          hospital_type %in% input$Hospital_Type
          & diagnosis_type %in% input$Diagnosis_Type
          & geography %in% input$Location
        )%>%
        select(year,hospital_type,diagnosis_type,drug_type,geography,value)
    })
    
    #for the demographic summary
    #as this is based on two files (and on two separate columns in one
    #file) then an if/else function is employed to select the correct data
    demographic_summary_new <- reactive({
      if (input$summary_demographic == "Age")
      {
        demographic_summary %>%
          filter(
            hospital_type %in% input$Hospital_Type
            & diagnosis_type %in% input$Diagnosis_Type
            & geography %in% input$Location
            & age_group != "All age groups"
          )%>%
          select(year,hospital_type,diagnosis_type,geography,age_group,value)%>% 
          droplevels()
      }
      else if(input$summary_demographic == "Sex")
      {demographic_summary %>%
          filter(
            hospital_type %in% input$Hospital_Type
            & diagnosis_type %in% input$Diagnosis_Type
            & geography %in% input$Location
            & sex != "Both sexes"
          ) %>%
          select(year,hospital_type,diagnosis_type,geography,sex,value)%>% 
          droplevels()
        
      }
      else if (input$summary_demographic == "Deprivation")
      {
        demographic_summary %>%
          filter(hospital_type %in% input$Hospital_Type
                 & diagnosis_type %in% input$Diagnosis_Type
                 & geography %in% input$Location
                 & simd != "All"
          )%>%
          select(year,hospital_type,diagnosis_type,geography,simd,value)%>% 
          droplevels()
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
        formatC(activity_summary_new()$value, big.mark = ",",digits = 2,format = 'f')
        
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = activity_summary_new(),
        #plot
        x = ~  year,
        y = ~  value,
        color = ~  activity_type,
        colors = c('#3F3685','#9B4393','#0078D4'),
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
       
      
      
        
        layout(title = list(text=
                              paste0(  
                            " Drug-related hospital rates by activity type 1996/97 to 2019/20",
                            "<br>","(",
                            input$Location,
                            "; ",
                            input$Hospital_Type, "; ",
                            word(input$Diagnosis_Type,start = 1,sep = " \\("),
                            ")"),
                 font = list(size = 15)),
               
               separators = ".",
               annotations = 
                 list(x = 1.0, y = -0.25, 
                      text = paste0("Source: Drug-Related","<br>",
                                    "Hospital Statistics","<br>",
                                    "(PHS, 2021)"), 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='left', yanchor='auto', xshift=0, yshift=0,
                      font=list(family = "arial", size=12, color="#7f7f7f")),
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(activity_summary_new()$value, na.rm = TRUE) +
                             (max(activity_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 fixedrange = TRUE,
                 
                 title = paste0(c(
                   str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
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
               
               xaxis = list(range = c(-1,24),
                            fixedrange = TRUE,
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
               displaylogo = F,  editable = F)
      
    })
    
    #Insert table
    output$activity_summary_table <- renderDataTable({
      datatable(activity_summary_new(),
                colnames = c("Financial year",
                             "Hospital type",
                             "Diagnosis type",
                             "Activity type",
                             "Location",
                             "Rate"),
                rownames = FALSE,
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
                
      )%>% 
        formatRound(columns = 6,digits = 2)
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
        formatC(drug_summary_new()$value, big.mark = ",",
                digits = 2,format = 'f')
        
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = drug_summary_new(),
        #plot
        x = ~  year,
        y = ~  value,
        color = ~  drug_type,
        colors = ~ c(
          '#9B4393',  #Cannabinoids
          '#83BB26',  #Cocaine
          '#C73918',  #Hallucinogens
          '#948DA3',  #Multiple/Other
          '#0078D4',  #Opioids
          '#1E7F84',  #Other stimulants
          '#6B5C85'   #Sedatives/Hypnotics
        ),
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
        
        layout(title = list(
          text=
            paste0(  
                     "Drug-related hospital stay rates by drug type 1996/97 to 2019/20",
                     "<br>", " (",
                     input$Location,
                     "; ",
                     input$Hospital_Type,"; ",
                     word(input$Diagnosis_Type,start = 1,sep = " \\("),
                     ")"
        ),font = list(size = 15)),
               
               separators = ".",
               
               annotations = 
                 list(x = 1.0, y = -0.25, 
                      text = paste0("Source: Drug-Related","<br>",
                                    "Hospital Statistics","<br>",
                                    "(PHS, 2021)"), 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='left', yanchor='auto', xshift=0, yshift=0,
                      font=list(family = "arial", size=12, color="#7f7f7f")),
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(drug_summary_new()$value, na.rm = TRUE) +
                             (max(drug_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 fixedrange = TRUE,
                 
                 title = paste0(c(
                   str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
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
               
               xaxis = list(range = c(-1,24),
                            fixedrange = TRUE,
                            tickangle = -45,
                            title = paste0(c(rep("&nbsp;", 20),
                                             "<br>",
                                             "Financial year",
                                             rep("&nbsp;", 20),
                                             rep("\n&nbsp;", 3)),
                                           collapse = ""),
                            showline = TRUE,
                            ticks = "outside"),
               font = list(size = 13),
               
               #Fix the margins so that the graph and axis titles have enough...
               #room to display nicely.
               #Set the font sizes.
               
               margin = list(l = 90, r = 60, b = 160, t = 90),
               
               
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
               displaylogo = F,  editable = F)
      
    })
    
    output$drugs_table <- renderDataTable({
      datatable(drug_summary_new(),
                colnames = c("Financial year",
                             "Hospital type",
                             "Diagnosis type",
                             "Drug type",
                             "Location",
                             "Rate"),
                rownames = FALSE,
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
      ) %>% 
        formatRound(columns = 6,digits = 2)
    })
    
    
    #Demographic Plot
    
    output$demographic_plot <- renderPlotly({
      #first the tooltip label
      tooltip_summary <- paste0(
        input$summary_demographic, ": ",
        demographic_summary_new()[,5],
        "<br>",
        "Financial year: ",
        demographic_summary_new()$year,
        "<br>",
        "Rate: ",
        formatC(demographic_summary_new()$value, big.mark = ",",
                digits = 2,format = 'f')
        
      )
      
      #Create the main body of the chart.
      
      plot_ly(
        data = demographic_summary_new(),
        #plot- we wont bother at this point with tailored colour
        x = ~  year,
        y = ~  value,
        color = ~  demographic_summary_new()[,5],
        colors = 
          if (input$summary_demographic == "Deprivation")
          {
            c("#3F3685",
              "#9B4393",
              "#0078D4",
              "#83BB26",
              "#948DA3"
              )
          }
        else if (input$summary_demographic == "Age")
        {
          c("#3F3685",
            "#9B4393",
            "#0078D4",
            "#83BB26",
            "#948DA3",
            "#1E7F84",
            "#6B5C85"
          )
        }
        else {
          c("#3F3685",
            "#9B4393")
        }
      
          
          ,
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
        
        layout(title = list (text= (
          if (input$summary_demographic == "Deprivation")
          {
            paste0(  
                     "Drug-related hospital patient rates by national deprivation quintile",
                     "<br>","(",
                     input$Location,
                     "; ",
                     input$Hospital_Type, "; ",
                     word(input$Diagnosis_Type,start = 1,sep = " \\("),
                     ")")
          }
          else if (input$summary_demographic == "Age")
          {
            paste0(  
                     "Drug-related hospital patient rates by age group",
                     "<br>","(",
                     input$Location,
                     "; ",
                     input$Hospital_Type,"; ",
                     word(input$Diagnosis_Type,start = 1,sep = " \\("),
                     ")")
          }
          else {
            paste0(  
                     "Drug-related hospital patient rates by sex",
                     "<br>","(",
                     input$Location,
                     "; ",
                     input$Hospital_Type,"; ",
                     word(input$Diagnosis_Type,start = 1,sep = " \\("),
                     ")")
          }
        ),font = list(size = 15)),
               
               separators = ".",
        annotations = 
          list(x = 0.96, y = -0.29, 
               text = paste0("Source: Drug-Related","<br>",
                             "Hospital Statistics","<br>",
                             "(PHS, 2021)"), 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(family = "arial", size=12, color="#7f7f7f")),
               
               yaxis = list(
                 
                 exponentformat = "none",
                 
                 separatethousands = TRUE,
                 
                 range = c(0, max(demographic_summary_new()$value, na.rm = TRUE) +
                             (max(demographic_summary_new()$value, na.rm = TRUE)
                              * 10 / 100)),
                 fixedrange = TRUE,
                 
                 title = paste0(c(
                   str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
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
               
               xaxis = list(range = c(-1,24),
                            fixedrange = TRUE,
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
               displaylogo = F,  editable = F)
      
    })
    
    
    #Insert table
    output$demographic_table <- renderDataTable({
      datatable(demographic_summary_new(),
                rownames = FALSE,
                colnames = c("Financial year",
                             "Hospital type",
                             "Diagnosis grouping",
                             "Location",
                             input$summary_demographic,
                             "Rate"),
                style = "Bootstrap", 
                options = list(searching= FALSE,
                               lengthChange= FALSE)
      )%>% 
        formatRound(columns = 6,digits = 2)
      
       
    })
      

      
    #End of server
  }
  #End of script
}

shinyApp(ui = ui, server = server)
