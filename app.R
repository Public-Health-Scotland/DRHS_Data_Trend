#Name: Drug Related Hospital Statistics (DRHS) Data explorer page
#Author: Mike Smith
#Created: 24/01/2019
#Type: Data visualisation
#Written on: RStudio
#Written for: R version 3.5.1 
#Output: Shiny application

#This is the full version of the data explorer for the new DRHS dashboard. 

#Currently there are tabs for 
# Tab 1) A Home Page
# Tab 2) Time Trend (Geography)
# Tab 3) Time Trend (Substances)
# Tab 4) Age/Sex 
# Tab 5) Deprivation
# Tab 6) Table

#libraries
library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(stringr)
library(DT)
library(forcats)
library(shinyBS)
library(bsplus)
library(shinymanager)


##############################################.
############## Reading In Data ----
##############################################.


#Data to be used for explorer and trend pages
#Following is rounded data

time_trend<- readRDS('time_trend.rds')
age_sex <- readRDS('age_sex.rds')
deprivation  <- readRDS('deprivation.rds')
deprivation_local <- readRDS('deprivation_local.rds')
monthly <- readRDS('r12-temp02_monthly_R_SHINY_rounded.rds')

#Data that is not visualized  
length_of_stay <- readRDS("r09-temp02_lsty_R_SHINY_rounded.rds")
emergency_admissions<- readRDS("r10-temp02_emerAdm_R_SHINY_rounded.rds")
drug_type_by_hospital<-readRDS ("r11-temp02_dist_hospit_R_SHINY_rounded.rds")


#We then create the options for users to choose from in the drop down menus. 
#Drug Types are created as list to allow different options dependent on the 
#Hospital admission types
hospital_types <- as.character(unique(time_trend$hospital_type))
hospital_types<-c(hospital_types[3],hospital_types[1],hospital_types[2])
diagnosis_types <- as.character(unique(time_trend$diagnosis_type))
diagnosis_types<-c(diagnosis_types[3],diagnosis_types[1],diagnosis_types[2])
activity_type <- as.character(unique(time_trend$activity_type))
location_types <- as.character(unique(time_trend$geography_type))
locations<- as.character(unique(time_trend$geography))

geography_list<-list("Scotland" = locations[1:3],
                     "NHS Board of residence" = locations[4:17],
                     "ADP of residence" = locations[18:48])

geography_deprivation_list<-list("Scotland" = locations[1],
                     "NHS Board of residence" = locations[4:17],
                     "ADP of residence" = locations[18:48])

drug_types<- as.character(unique(time_trend$drug_type))
drug_types1<- list("Main Categories" = as.character(unique(time_trend$drug_type)[1:8]),
                  "Opioids Sub Categories" = as.character(unique(time_trend$drug_type)[9:11]))
drug_types2<- as.character(unique(time_trend$drug_type)[1:8])
measures<- as.character(unique(time_trend$measure))


#Add in age, sex, SIMD and financial years options for demographic tabs
age <- as.character(unique(age_sex$age_group))
sex <- as.character(unique(age_sex$sex))
financial_years <- as.character(unique(time_trend$year))
SIMD<- as.character(unique(deprivation$simd))

# Read in chart for age/sex tornado chart
age_sex_tornado <- readRDS('age_sex_tornado.rds')

#we can now set up the data for that from the data trend page

activity_summary<- readRDS('activity_summary.rds')
drug_summary<- readRDS('drug_summary.rds')
demographic_summary<- readRDS ('demographic_summary.rds')




##############################################.
############## User Interface ----
##############################################.
  {
    #Beginning of UI
    ui <- #secure_app( #uncomment if needing password protection
      fluidPage(
      style = "width: 100%; height: 100%; max-width: 1200px;",
      tags$head(
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$head(
          tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                      type="text/javascript")
        ),
        
        
        #The following chunk of code does two things:
        # 1. Paints the ribbon that contains the tab headers white.
        # 2. Highlights the header of the active tab in blue.
        
        
        tags$style(
          HTML("
        .tabbable > .nav > li > a {background-color: #def0ff; color: #000000;}
        .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;}
            ")
)
          ),

#We will add in a title panel title. 
titlePanel(title=div(h1("Drug-Related Hospital Statistics"),
                     h4("Substance Use"), 
                     style = "height:96px;"),
           windowTitle = "Drug-Related Hospital Statistics"),


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
    
    "Introduction",
    icon = icon("info-circle"),
    style = "float: top; height: 95%; width: 95%;
    background-color: #FFFFFF; border: 0px solid #FFFFFF;",

    column(2),
    column(
      8,
      p(
        br(),
        "The Data explorer provides a detailed breakdown of Drug-Related 
        Hospital Statistics data in Scotland over time. You can visualise these 
        data using the following pages:"
      ),
      tags$ul(
        tags$li(
          tags$b(actionLink(
            "link_to_geography", "Location comparison"
          )),
          icon("line-chart"),
          " - compare data by location, over time."
      ),
      
        tags$li(
          tags$b(actionLink(
            "link_to_substances", "Drug type comparison"
          )),
          icon("line-chart"),
          " - compare data by drug type, over time."
        ),
      tags$li(
        tags$b(actionLink(
          "link_to_age_sex", "Age/sex"
        )),
        icon("child"),
        " - show data by age group and sex."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_deprivation", "Deprivation"
        )),
        icon("bar-chart"),
        " - show data by deprivation quintile."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_monthly", "Monthly trend"
        )),
        icon("line-chart"),
        " - show data by month."
      ),
      tags$li(
        tags$b(actionLink(
          "link_to_table", "Data"
        )),
        icon("table"),
        " - view and customise data tables."
      )
      ),
      
      p("A less detailed overview of drug-related hospital stays in Scotland 
        over time is available in the",
        tags$a(
          href = "https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/trend-data/", 
          "Trend data"
        ),
        " page"),
      
      bs_accordion(id = "drhs_introduction_text") %>% 
        bs_set_opts(panel_type = "primary") %>%
        bs_append(title = tags$u("Technical information"), 
                  content = 
                    tags$ul( 
                      tags$li(
                        "The Data explorer visualises information recorded in the SMR01 and SMR04
                        datasets. The SMR01 dataset records general acute hospital inpatient and day
                        case activity and SMR04 records psychiatric hospital inpatient and day case 
                        activity."
                      ),
                      tags$li(
                        "Information is generally available for financial years 1996/97 to 2020/21.
                        Where shown, Alcohol and Drug Partnership (ADP) information is available from 1997/98 and new patient trends 
                        are available from 2006/07."
                      ),
                      tags$li(
                        "Data completeness may vary slightly from year to year. As a result,
                        data are provisional and subject to change. For more information, visit 
                        the ", 
          tags$a(
            href = "https://www.opendata.nhs.scot/dataset/scottish-morbidity-record-completeness", 
            "SMR completeness"
          ),
          " webpage."
        ), 
        
        tags$li(
          "Diagnostic information is recorded using the International Statistical
          Classification of Diseases and Related Health Problems, 10th Edition
          (ICD-10). ICD-10 codes used to classify drug-related hospital stays 
          are listed in Appendix 1 (see Analytical definitions) in the ",
          HTML(paste0('<a href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/methods/">methods section</a>.')),
          "Note that patients may have more than one
          drug-related diagnosis per stay." 
          
        ),
        tags$li(
          "Statistical disclosure control has been applied to protect patient
          confidentiality. Therefore, the figures presented in this dashboard may 
          not be additive and may differ from previous publications.  
          For more information, please refer to the  ",
          HTML(paste0('<a href="https://beta.isdscotland.org/front-matter/data-protection-and-confidentiality/">PHS Statistical Disclosure Control Protocol</a>.'))
        ), 
        tags$li(
          "Further technical details can be seen on the ",
          tags$a(href = "https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/","Data overview"),
          " webpage. Technical terms are explained in the ", 
          HTML(paste0('<a href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/glossary/">Glossary</a>.'))
        )
        
      )
      ),
      
      p(
        br(),
        "If you experience any problems using this dashboard or have further
      questions relating to the data, please contact us at:",
        HTML(paste0('<b> <a href="mailto:phs.drugsteam@phs.scot">phs.drugsteam@phs.scot</a></b>.'))
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
    "Location comparison",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",

    h3("Location comparison"),
    
    p(
      h4(
        "Visualise drug-related hospital activity over time and make
        comparisons between locations. ")
      ),
    
    bs_accordion(id = "drhs_location_comparison_text") %>% 
      bs_set_opts(panel_type = "primary") %>%
      bs_append(title = tags$u("Data selection"), 
                content = p(
                  "The chart can be modified using the drop down boxes:", 
                  tags$ul(
                    tags$li("Hospital type: general acute or psychiatric 
                            hospital data (or any hospital type);"),
                    tags$li("Diagnosis grouping: mental & behavioural stays, 
                            accidental poisoning/overdose stays (or any diagnosis);"),
                    tags$li("Activity type: stays, patients or new patients;"),
                    tags$li("Location: data from Scotland, specific NHS
                            Boards or Alcohol and Drug Partnerships 
                            (choose up to 8 locations);"),
                    tags$li("Drug type: the type of drug associated with the 
                            stay (opioid sub categories are available if overdoses
                            are selected as diagnosis grouping); and,"),
                    tags$li("Measure: numbers, rates or percentages.")
                  ), 
                  "To download your data selection as a CSV file, use the
                  'Download data' button under the drop down boxes.", 
                  br(),br(),
                  "For technical information, please see the",
                  actionLink(
                    "link_to_home", "introduction"
                  ), " page."
                ))%>%
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
                content = p(HTML("To view 
        your data selection in a table, use the <a href = '#geography_link'> 
                            'Show/hide table' </a>  button at the
                            bottom of the page."),
                  tags$ul(
                    tags$li(tags$b("Show entries"), " - change the number of rows shown
                            in the table using the drop-down box."),
                    tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                            numerical value."),
                    tags$li(icon("sort", lib = "glyphicon"),
                            tags$b("Sort"), " - click to sort the table in ascending or 
                            descending order based on the values in a column."),
                    tags$li(tags$b("Page controls"), " - switch to specific page of data 
                            within the table.")
                  )
                )),
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
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital type
      # 2 - Diagnosis Type
      # 3 - Actvity Type
      # 4 - Geography (Multiple)
      # 5 - Substance
      # 6 - Measure
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Hospital_Type",
          label = "Hospital type",
          choices = hospital_types
        ), 
        shinyWidgets::pickerInput(
          inputId = "Location",
          label = "Location (multiple selection)",
          choices = geography_list,
          multiple = TRUE,
          selected = "Scotland",
          options = list(size=10, 
                         `live-search`=TRUE, 
                         `selected-text-format` = "count > 1", 
                         `count-selected-text` = "{0} locations chosen (8 Max)",
                         "max-options" = 8,
                         "max-options-text" = "Only 8 options can be chosen")
        )
      ),
      
      column(
        4,
        uiOutput("time_trend_diagnosis_type"), 
        uiOutput("time_trend_substance1")
        ),
        
        column(
          4,
          shinyWidgets::pickerInput(
            inputId = "Activity_Type",
            label = "Activity type",
            choices = activity_type
          ),
          shinyWidgets::pickerInput(
            inputId = "Measure",
            label = "Measure",
            choices = measures,
            selected = "Rate"
          )
          )
      
    ),
    
    downloadButton(outputId = "download_geography", 
                   label = "Download data", 
                   class = "geographybutton"),
    
    tags$head(
      tags$style(".geographybutton { background-color: 
                   #0072B2; } 
                   .geographybutton { color: #FFFFFF; }")
    ),
    
    #In the main panel of the tab, insert the geography plot
    
    mainPanel(
      width = 12,
      plotlyOutput("geography_plot",
                   width = "1090px",
                   height = "500px"),
      br(),
                  HTML("<button data-toggle = 'collapse' href = '#geography'
                   class = 'btn btn-primary' id = 'geography_link'> 
                          <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'geography' class = 'collapse'>"),
      br(),
      dataTableOutput("geography_table"),
      HTML("</div>"),
      br(),
      br()
    )
    
    #End of tab panel
    ),

##############################################.
############## Substances tab ----
##############################################.

  tabPanel(
    "Drug type comparison",
    icon = icon("line-chart"),
    style = "height: 95%; width: 95%; background-color: #FFFFFF;
    border: 0px solid #FFFFFF;",

    h3("Drug type comparison"),
    p(
      h4(
        "Visualise drug-related hospital activity over time and make 
        comparisons between different drug types. "
      )),

      bs_accordion(id = "drhs_drugs_comparison_text") %>% 
        bs_set_opts(panel_type = "primary") %>%
        bs_append(title = tags$u("Data selection"), 
                  content = p(
                    "The chart can be modified using the drop down boxes:", 
                    tags$ul(
                      tags$li("Hospital type: general acute or psychiatric 
                              hospital data (or any hospital type);"),
                      tags$li("Diagnosis grouping: mental & behavioural stays, 
                              accidental poisoning/overdose stays (or any diagnosis);"),
                      tags$li("Activity type: stays, patients or new patients; "),
                      tags$li("Location: data from Scotland, specific NHS
                              Boards or Alcohol and Drug Partnerships;"),
                      tags$li("Drug type: the type of drug associated with the 
                              stay (multiple selection) (opioid sub categories are available if overdoses
                              are selected as diagnosis grouping); and,"),
                      tags$li("Measure: numbers, rates or percentages.")
                      ), 
                    "To download your data selection as a CSV file, use the
                    'Download data' button under the drop down boxes.", 
                    br(),br(),
                    "For technical information, please see the",
                    actionLink(
                      "link_to_home2", "introduction"
                    ), " page."
                      ))%>%
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
                  content = p(HTML("To view 
                                   your data selection in a table, use the <a href = '#substances_link'> 
                                   'Show/hide table' </a>  button at the
                                   bottom of the page."),
                              tags$ul(
                                tags$li(tags$b("Show entries"), " - change the number of rows shown
                                        in the table using the drop-down box."),
                                tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                                        numerical value."),
                                tags$li(icon("sort", lib = "glyphicon"),
                                        tags$b("Sort"), " - click to sort the table in ascending or 
                                        descending order based on the values in a column."),
                                tags$li(tags$b("Page controls"), " - switch to specific page of data 
                                        within the table.")
                                )
                                )),
    
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
      
      #Insert the reactive filters.
      #We have SIX filters at this point 
      # 1 - Hospital type
      # 2 - Diagnosis Type
      # 3 - Activity Type
      # 4 - Geography 
      # 5 - Substance (Multiple)
      # 6 - Measure
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Hospital_Type2",
          label = "Hospital type",
          choices = hospital_types
        ), 
        shinyWidgets::pickerInput(
          inputId = "Location2",
          label = "Location",
          choices = geography_list,
          selected = "Scotland",
          options = list(size=10, 
                         `live-search`=TRUE)
        )
        
      ),
      
      column(
        4,
        uiOutput("time_trend_diagnosis_type2"), 
        uiOutput("time_trend_substance2")
      ),
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Activity_Type2",
          label = "Activity type",
          choices = activity_type
        ),
        shinyWidgets::pickerInput(
          inputId = "Measure2",
          label = "Measure",
          choices = measures,
          selected = "Rate"
        )
      )
    ),
    
    downloadButton(outputId = "download_substances", 
                   label = "Download data", 
                   class = "substancesbutton"),
    
    tags$head(
      tags$style(".substancesbutton { background-color: 
                     #0072B2; } 
                     .substancesbutton { color: #FFFFFF; }")
    ),
    
    #In the main panel of the tab, insert the substances plot
    
    mainPanel(
      width = 12,
      plotlyOutput("substances_plot",
                   width = "1090px",
                   height = "500px"), 
      br(),
      HTML("<button data-toggle = 'collapse' href = '#substances'
                   class = 'btn btn-primary' id = 'substances_link'> 
                   <strong> Show/hide table </strong></button>"),
      HTML("<div id = 'substances' class = 'collapse'>"),
      br(),
      dataTableOutput("substances_table"),
      HTML("</div>"),
      br(),
      br()
    )
    
    #End of tab panel
      ),


##############################################.
############## Age/Sex tab ----
##############################################.

tabPanel(
  "Age/sex",
  icon = icon("child"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",

  h3("Age/sex"),
  
  p(
    h4("Visualise drug-related hospital activity over time by patient 
         age group and sex (Scotland level only).")),

  bs_accordion(id = "drhs_age_sex_text") %>% 
    bs_set_opts(panel_type = "primary") %>%
    bs_append(title = tags$u("Data selection"), 
              content = p("The toggle buttons allow 
         the data to be visualised in two ways:",
                          
                          
                          tags$ul(
                            tags$li(
                              tags$b("Time trend"),
                              icon("line-chart"),
                              " - displays trends for specific age and sex groups."
                            ),
                            tags$li(
                              tags$b("Bar chart "),
                              icon("bar-chart"),
                              " - shows annual breakdowns by age group and sex."
                            )),
                          
                p("The charts can be modified using the drop down boxes:"), 
               p( tags$ul(style = "width:50%; float:left;",
                  tags$li("Hospital type: general acute or psychiatric 
                          hospital data (or any hospital type);"),
                  tags$li("Diagnosis grouping: mental & behavioural stays, 
                          accidental poisoning/overdose stays (or any diagnosis);"),
                  tags$li("Activity type: stays, patients or new patients;"),
                  tags$li("Drug type: the type of drug associated with the 
                          stay (multiple selection) (opioid sub categories are available if overdoses
                          are selected as diagnosis grouping);")
                  
                ),
                tags$ul(style = "width:50%; float:left;",
                  tags$li("Measure: numbers, rates or percentages;"),
                  tags$li("Age group (Time trend only): patient age (multiple selection);"),
                  tags$li("Sex (Time trend only): patient sex (multiple selection); and,"),
                  tags$li("Financial year (Bar chart only): use the slider to select 
                          year or the play button to visualise changes over time.")
                  )), 
                p(style = "width:100%; float:left;",
"To download your data selection as a CSV file, use the
                'Download data' button under the drop down boxes.", 
                br(),br(),
"For technical information, please see the",
actionLink(
  "link_to_home3", "introduction"
), " page.")
                  ))%>%
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
                  content = p(HTML("To view 
                                   your data selection in a table, use the <a href = '#age_and_sex_link'> 
                                   'Show/hide table' </a> button at the
                                   bottom of the page."),
                              tags$ul(
                                tags$li(tags$b("Show entries"), " - change the number of rows shown
                                        in the table using the drop-down box."),
                                tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                                        numerical value."),
                                tags$li(icon("sort", lib = "glyphicon"),
                                        tags$b("Sort"), " - click to sort the table in ascending or 
                                        descending order based on the values in a column."),
                                tags$li(tags$b("Page controls"), " - switch to specific page of data 
                                        within the table.")
                                )
                                )),

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
    
    #Insert the reactive filters.
    #We have FOUR filters at this point
    # 1 - Hospital type
    # 2 - Diagnosis type
    # 2 - Activity Type
    # 3 - Substance (dependent on Hospital/Diagnosis Type)
    # 4 - Measure
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Hospital_Type3",
             label = "Hospital type",
             choices = hospital_types
           ),
           uiOutput("age_sex_substance")),
    
    column(
      4,
      uiOutput("age_sex_diagnosis_type"),
      shinyWidgets::pickerInput(
        inputId = "Measure3",
        label = "Measure",
        choices = measures,
        selected = "Rate"
      )
    ), 
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Activity_Type3",
             label = "Activity type",
             choices = activity_type,
             selected = "Stays"
           ))
  ),

  
  #In the main panel of the tab, insert the time trend plot
  mainPanel(width = 12, 
            tabsetPanel(
    type = "pills",
    tabPanel(
      "Time trend",
      tags$style(
        HTML("
           .tabbable > .nav > li > a[data-value = 'Bar Chart'] {background-color: #D3D3D3; color: #000000;}
           .tabbable > .nav > li > a[data-value = 'Time Trend'] {background-color: #D3D3D3; color: #000000;}
           .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;} 
           ") 
      ),
      icon = icon("line-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Age",
          label = "Age group (multiple selection)",
          choices = age,
          multiple = TRUE,
          selected = "All age groups"
        ),

        tags$head(
          tags$style(
            ".myagesextrendbutton { background-color:
            #0072B2; }
            .myagesextrendbutton { color: #FFFFFF; }"
          )
        )
      ),
      
      column(
        4,
        shinyWidgets::pickerInput(
          inputId = "Sex",
          label = "Sex (multiple selection)",
          choices = sex,
          multiple = TRUE,
          selected = "Both sexes"
        )
      ),
      column (4,
              br(),
              
              downloadButton(
                outputId = "download_age_sex_trend",
                label = "Download data",
                class = "myagesextrendbutton"
              )
              ),
       
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      plotlyOutput("age_sex_time_plot",
                   width = "1090px",
                   height = "500px"),
      br(),
      HTML(
        "<button data-toggle = 'collapse' href = '#ageandsextrend'
        class = 'btn btn-primary' id = 'age_and_sex_link'>
        <strong>Show/hide table</strong></button>"
      ),
      HTML("<div id = 'ageandsextrend' class = 'collapse'>"),
      br(),
      dataTableOutput("age_sex_trend_table"),
      HTML("</div>"),
      br(),
      br()
      ),
    
    tabPanel(
      "Bar chart",
      icon = icon("bar-chart"),
      style = "height: 95%; width: 95%; background-color: #FFFFFF;
      border: 0px solid #FFFFFF;",
      br(),
      br(),
      column(
        8,
        chooseSliderSkin("Big"),
        shinyWidgets::sliderTextInput(
          inputId = "Financial_Year",
          label = "Financial year",
          choices = financial_years,
          selected = "2020/21",
          grid = T,
          animate = animationOptions(playButton = icon('play', 
                                                      "fa fa-play-circle fa-3x"),
                                     pauseButton = icon('pause', 
                                                        "fa fa-pause-circle fa-3x")),
          width = "1090px"
          
        )
        ),
      column(1),
      column(3,
             br(),
             br(),
        downloadButton(
          outputId = "download_age_sex_year",
          label = "Download data",
          class = "myagesexyearbutton"
        ),
        tags$head(
          tags$style(
            ".myagesexyearbutton { background-color:
                #0072B2; }
                .myagesexyearbutton { color: #FFFFFF; }"
          )
        )
      )
      ,
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
   
        plotlyOutput("age_sex_year_plot",
                     width = "1090px",
                     height = "500px"),
      br(),
        HTML(
          "<button data-toggle = 'collapse' href = '#ageandsexyear'
              class = 'btn btn-primary' id = 'age_and_sex_link'>
              <strong>Show/hide table</strong></button>"
        ),
        HTML("<div id = 'ageandsexyear' class = 'collapse'>"),
      br(),
        dataTableOutput("age_sex_year_table"),
        HTML("</div>"),
        br(),
        br()
    )
    
    )
    )
  
  #End of tab panel
    ),

##############################################.
############## Deprivation tab ----
##############################################.

tabPanel(
  "Deprivation",
  icon = icon("bar-chart"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",

  h3("Deprivation"),
  p(
    h4(
      "Visualise drug-related hospital activity by patient deprivation quintile."
    )),
    
    bs_accordion(id = "drhs_deprivation_text") %>% 
      bs_set_opts(panel_type = "primary") %>%
    bs_append(title = tags$u("Data selection"), 
              content = p("The toggle buttons allow 
         the data to be visualised in two ways:",
                          
                          
                          tags$ul(
                            tags$li(
                              tags$b("Location comparison"),
                              icon("globe-europe"),
                              " - displays data based on national deprivation quintiles for comparisons between Scotland, NHS Boards and ADPs."
                            ),
                            tags$li(
                              tags$b("Location profile"),
                              icon("city"),
                              " - displays the profile of an individual location (Scotland, NHS Board or ADP), based on local deprivation quintile data",
                              tags$b("(not to be used for comparison of"),
                              HTML(paste0(tags$b("locations)"),"."))
                            )),
                          p("For more information on deprivation analysis, and the difference between these two analyses please see the ",
                          HTML(paste0('<a href="https://beta.isdscotland.org/find-publications-and-data/lifestyle-and-behaviours/substance-use/drug-related-hospital-statistics/15-june-2021/methods/">methods section</a>.'))),
                          
                          p("The charts can be modified using the drop down boxes:"), 
                           tags$ul(
                                       tags$li("Hospital type: general acute or psychiatric 
                                               hospital data (or any hospital type);"),
                                       tags$li("Diagnosis grouping: mental & behavioural stays, 
                                               accidental poisoning/overdose stays (or any diagnosis);"),
                                       tags$li("Activity type: stays, patients or new patients;"),
                                       tags$li("Drug type: the type of drug associated with the 
                                               stay (multiple selection) (opioid sub categories are available if overdoses
                                               are selected as diagnosis grouping);"),
                                       tags$li("Financial year; and,"),
                                       tags$li("Measure: numbers, rates or percentages.")
                                       ), 
                            p(
                              "To download your data selection as a CSV file, use the
                              'Download data' button under the drop down boxes.", 
                              br(),br(),
                              "For technical information, please see the",
                              actionLink(
                                "link_to_home4", "introduction"
                              ), " page.")
                              ))%>%
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
                  content = p(HTML("To view 
                                   your data selection in a table, use the <a href = '#SIMD_link'> 
                                   'Show/hide table' </a> button at the
                                   bottom of the page."),
                              tags$ul(
                                tags$li(tags$b("Show entries"), " - change the number of rows shown
                                        in the table using the drop-down box."),
                                tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                                        numerical value."),
                                tags$li(icon("sort", lib = "glyphicon"),
                                        tags$b("Sort"), " - click to sort the table in ascending or 
                                        descending order based on the values in a column."),
                                tags$li(tags$b("Page controls"), " - switch to specific page of data 
                                        within the table.")
                                )
                                )),

  
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
    
    #Insert the reactive filters.
    #We have Five filters at this point
    # 1 - Hospital type
    # 2 - Diagnosis Type
    # 3 - Activity Type
    # 3 - Substance (dependent on Hospital/Diagnosis Type)
    # 4 - Financial Year
    # 5 - Measure
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Hospital_Type4",
             label = "Hospital type",
             choices = hospital_types
           ),
           uiOutput("SIMD_substance")
    ),
    
    column(
      4,
      
      uiOutput("SIMD_diagnosis_type"), 
      
      shinyWidgets::pickerInput(
        inputId = "Measure4",
        label = "Measure",
        choices = measures,
        selected = "Rate"
      )

    ),
    
    column(4,
           shinyWidgets::pickerInput(
             inputId = "Activity_Type4",
             label = "Activity type",
             choices = activity_type,
             selected = "Stays"
           )
    ),
    column(
      8,
      chooseSliderSkin("Big"),
      shinyWidgets::sliderTextInput(
        inputId = "Financial_Year2",
        label = "Financial year",
        choices = financial_years,
        selected = "2020/21",
        grid = T,
        animate = animationOptions(playButton = icon('play', 
                                                     "fa fa-play-circle fa-3x"),
                                   pauseButton = icon('pause', 
                                                      "fa fa-pause-circle fa-3x")),
        width = "1090px"
      )
    )
  ),
  
  
  #In the main panel of the tab, insert the SIMD plot
  mainPanel(width = 12, 
            tabsetPanel(
              type = "pills",
              
              #First tab part with comparison 
              tabPanel(
                "Location comparison",
                tags$style(
                  HTML("
                       .tabbable > .nav > li > a[data-value = 'Comparison'] {background-color: #D3D3D3; color: #000000;}
                       .tabbable > .nav > li > a[data-value = 'Local'] {background-color: #D3D3D3; color: #000000;}
                       .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;} 
                       ") 
                  ),
                icon = icon("globe-europe"),
                style = "height: 95%; width: 95%; background-color: #FFFFFF;
                border: 0px solid #FFFFFF;",
                br(),
                br(),
                column(
                  4,
                  shinyWidgets::pickerInput(
                    inputId = "Location3",
                    label = "Location (multiple selection)",
                    choices = geography_deprivation_list,
                    multiple = TRUE,
                    selected = "Scotland",
                    options = list(size=10, 
                                   `live-search`=TRUE, 
                                   `selected-text-format` = "count > 1", 
                                   `count-selected-text` = "{0} locations chosen (8 Max)",
                                   "max-options" = 8,
                                   "max-options-text" = "Only 8 options can be chosen")
                  )
                ),          
                column(4, br(),
                       downloadButton(
                  outputId = "download_SIMD",
                  label = "Download data",
                  class = "mySIMDtrendbutton"
                ),
                tags$head(
                  tags$style(
                    ".mySIMDtrendbutton { background-color:
               #0072B2; }
               .mySIMDtrendbutton { color: #FFFFFF; }"
                  )
                )),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                plotlyOutput("SIMD_plot",
                             width = "1090px",
                             height = "500px"),
                br(),
                HTML(
                  "<button data-toggle = 'collapse' href = '#SIMDchart'
      class = 'btn btn-primary' id = 'SIMD_link'>
      <strong>Show/hide table</strong></button>"
                ),
                HTML("<div id = 'SIMDchart' class = 'collapse'>"),
                br(),
                dataTableOutput("SIMD_table"),
                HTML("</div>"),
                br(),
                br()
              ),
            # Second tab part with the local within comparison
            tabPanel(
              "Location profile",
              icon = icon("city"),
              style = "height: 95%; width: 95%; background-color: #FFFFFF;
              border: 0px solid #FFFFFF;",
              br(),
              br(),
              
              column(
                4,
                shinyWidgets::pickerInput(
                  inputId = "Location4",
                  label = "Location",
                  choices = geography_deprivation_list,
                  selected = "Scotland",
                  options = list(size=10, 
                                 `live-search`=TRUE)
                )          
              ),
              column(4, br(),
                     downloadButton(
                       outputId = "download_SIMD_local",
                       label = "Download data",
                       class = "mySIMD_local_trendbutton"
                     ),
                     tags$head(
                       tags$style(
                         ".mySIMD_local_trendbutton { background-color:
                         #0072B2; }
                         .mySIMD_local_trendbutton { color: #FFFFFF; }"
                       )
                     )),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              plotlyOutput("SIMD_local_plot",
                           width = "1090px",
                           height = "500px"),
              br(),
              HTML(
                "<button data-toggle = 'collapse' href = '#SIMD_local_chart'
      class = 'btn btn-primary' id = 'SIMD_local_link'>
      <strong>Show/hide table</strong></button>"
              ),
              HTML("<div id = 'SIMD_local_chart' class = 'collapse'>"),
              br(),
              dataTableOutput("SIMD_local_table"),
              HTML("</div>"),
              br(),
              br()
            )
        )
  )
#End of tab panel
),
    
##############################################.
############## Monthly tab ----
##############################################.

#Create a tab for monthly data.
#Insert the description a

tabPanel(
  "Monthly trend",
  icon = icon("line-chart"),
  style = "height: 95%; width: 95%; background-color: #FFFFFF;
  border: 0px solid #FFFFFF;",
  
  h3("Monthly Trend"),
  
  p(
    h4(
      "Visualise drug-related hospital monthly activity over time and make
      comparisons between years ")
    ),
  
  bs_accordion(id = "drhs_monthly_trend_text") %>% 
    bs_set_opts(panel_type = "primary") %>%
    bs_append(title = tags$u("Data selection"), 
              content = p(
                "The toggle buttons allow the data to be visualised in three ways:", 
                tags$ul(
                  tags$li(
                    tags$b("Location comparison"),
                    icon("globe-europe"),
                    " - displays data based on location for Scotland, NHS Boards or ADPs."
                  ),
                  tags$li(
                    tags$b("Drug type comparison"),
                    icon("tablets"),
                    " - displays data based on drug type"
                  ),
                  tags$li(
                    tags$b("Age group comparison"),
                    icon("child"),
                    " - displays data based on age group"
                  )),
                p("The charts can be modified using the drop down boxes:"), 
                tags$ul(
                  tags$li("Hospital type: general acute or psychiatric 
                          hospital data (or any hospital type);"),
                  tags$li("Diagnosis grouping: mental & behavioural stays, 
                          accidental poisoning/overdose stays (or any diagnosis);"),
                  tags$li("Financial year: between 1996/97 to 2020/21")
                  ), 
                "To download your data selection as a CSV file, use the
                'Download data' button under the drop down boxes.", 
                br(),br(),
                "For technical information, please see the",
                actionLink(
                  "link_to_home5", "introduction"
                ), " page."
                  ))%>%
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
              content = p(HTML("To view 
                               your data selection in a table, use the <a href = '#geography_link'> 
                               'Show/hide table' </a>  button at the
                               bottom of the page."),
                          tags$ul(
                            tags$li(tags$b("Show entries"), " - change the number of rows shown
                                    in the table using the drop-down box."),
                            tags$li(tags$b("Search"), " - enter text to search data for a specific word or
                                    numerical value."),
                            tags$li(icon("sort", lib = "glyphicon"),
                                    tags$b("Sort"), " - click to sort the table in ascending or 
                                    descending order based on the values in a column."),
                            tags$li(tags$b("Page controls"), " - switch to specific page of data 
                                    within the table.")
                            )
                            )),
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
    
    #Insert the reactive filters.
    #We have SIX filters at this point 
    # 1 - Hospital type
    # 2 - Diagnosis Type
    # 3 - Location 
    # 4 - Year
    # 5 - Substance
    # 6 - Age
    
    column(
      4,
      shinyWidgets::pickerInput(
        inputId = "Hospital_Type5",
        label = "Hospital type",
        choices = hospital_types
      )
    ),
    
    column(
      4,
      uiOutput("monthly_diagnosis_type")#, 
      #uiOutput("monthly_substance")
    ),
    
    column(
      4,
      shinyWidgets::pickerInput(
        inputId = "Financial_Year3",
        label = "Financial year (multiple selection)",
        choices = rev(financial_years),
        multiple = TRUE,
        selected = "2020/21",
        options = list(size=10,  
                       `selected-text-format` = "count > 1", 
                       `count-selected-text` = "{0} years chosen (8 Max)",
                       "max-options" = 8,
                       "max-options-text" = "Only 8 options can be chosen")
      )
    )
    
  ),
  #In the main panel of the tab, insert the various plots
  mainPanel(width = 12, 
            tabsetPanel(
              type = "pills",
              tabPanel(
                "Location",
                tags$style(
                  HTML("
                       .tabbable > .nav > li > a[data-value = 'Location'] {background-color: #D3D3D3; color: #000000;}
                       .tabbable > .nav > li > a[data-value = 'Drug Type'] {background-color: #D3D3D3; color: #000000;}
                       .tabbable > .nav > li > a[data-value = 'Age Group'] {background-color: #D3D3D3; color: #000000;}
                       .tabbable > .nav > li[class = active] > a {background-color: #0072B2;color: #FFFFFF;} 
                       ") 
                  ),
                icon = icon("line-chart"),
                style = "height: 95%; width: 95%; background-color: #FFFFFF;
                border: 0px solid #FFFFFF;",
                br(),
                br(),
                column(4,
                  shinyWidgets::pickerInput(
                    inputId = "Location5",
                    label = "Location",
                    choices = geography_list,
                    selected = "Scotland",
                    options = list(size=10, 
                                   `live-search`=TRUE))
                ),
                column(4, br(),
                       downloadButton(
                         outputId = "download_Monthly_Location",
                         label = "Download data",
                         class = "myMonthlyLocationbutton"
                       ),
                       tags$head(
                         tags$style(
                           "myMonthlyLocationbutton { background-color:
               #0072B2; }
               myMonthlyLocationbutton { color: #FFFFFF; }"
                         )
                       )),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                plotlyOutput("monthly_location_plot",
                             width = "1090px",
                             height = "500px"),
                br(),
                HTML(
                  "<button data-toggle = 'collapse' href = '#Monthly_location_chart'
      class = 'btn btn-primary' id = 'Monthly_location_link'>
      <strong>Show/hide table</strong></button>"
                ),
                HTML("<div id = 'Monthly_location_chart' class = 'collapse'>"),
                br(),
                dataTableOutput("Monthly_location_table"),
                HTML("</div>"),
                br(),
                br()
                
                # End of location sub-Tab panel
                ),
              
              tabPanel(
                "Drug type",
                icon = icon("line-chart"),
                style = "height: 95%; width: 95%; background-color: #FFFFFF;
                border: 0px solid #FFFFFF;",
                br(),
                br(),
                column(4, uiOutput("monthly_substance")),
                column(4, br(),
                       downloadButton(
                         outputId = "download_Monthly_Drugs",
                         label = "Download data",
                         class = "myMonthlyDrugsbutton"
                       ),
                       tags$head(
                         tags$style(
                           "myMonthlyDrugsbutton { background-color:
               #0072B2; }
               myMonthlyDrugsbutton { color: #FFFFFF; }"
                         )
                       )),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                plotlyOutput("monthly_drugs_plot",
                             width = "1090px",
                             height = "500px"),
                br(),
                HTML(
                  "<button data-toggle = 'collapse' href = '#Monthly_drugs_chart'
      class = 'btn btn-primary' id = 'Monthly_drugs_link'>
      <strong>Show/hide table</strong></button>"
                ),
                HTML("<div id = 'Monthly_drugs_chart' class = 'collapse'>"),
                br(),
                dataTableOutput("Monthly_drugs_table"),
                HTML("</div>"),
                br(),
                br()
                
                # End of drug sub-Tab panel
              ),
              tabPanel(
                "Age group",
                icon = icon("line-chart"),
                style = "height: 95%; width: 95%; background-color: #FFFFFF;
                border: 0px solid #FFFFFF;",
                br(),
                br(),
                column(4, 
                       shinyWidgets::pickerInput(
                         inputId = "Ages2",
                         label = "Age group",
                         choices = age,
                         selected = "All age groups")
                ),
                column(4, br(),
                       downloadButton(
                         outputId = "download_Monthly_Age",
                         label = "Download data",
                         class = "myMonthlyAgebutton"
                       ),
                       tags$head(
                         tags$style(
                           "myMonthlyAgebutton { background-color:
               #0072B2; }
               myMonthlyAgebutton { color: #FFFFFF; }"
                         )
                       )),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                plotlyOutput("monthly_age_plot",
                             width = "1090px",
                             height = "500px"),
                br(),
                HTML(
                  "<button data-toggle = 'collapse' href = '#Monthly_age_chart'
      class = 'btn btn-primary' id = 'Monthly_age_link'>
      <strong>Show/hide table</strong></button>"
                ),
                HTML("<div id = 'Monthly_age_chart' class = 'collapse'>"),
                br(),
                dataTableOutput("Monthly_age_table"),
                HTML("</div>"),
                br(),
                br()
                # End of age sub-Tab panel
              )
              
                  )
              )
  

  
  #End of tab panel
    ),

##############################################.
############## Data tab ----
##############################################.

tabPanel(
  "Data", 
  icon = icon("table"), 
  style = "float: top; height: 95%; width: 95%; background-color: #FFFFFF; 
  border: 0px solid #FFFFFF;", 

  h3("Data"), 
  h4(
    "View and customise data tables used in the Trend data and Data explorer 
    dashboards and other data not visualised."
  ),
  
  p(
    HTML(
      "Use the 'Data file' drop down box to view a data table.
      " )),
  bs_accordion(id = "drhs_table_text") %>% 
    bs_set_opts(panel_type = "primary") %>%
    bs_append(title = tags$u("Table functions"), 
              content = p(tags$ul(
                            tags$li(icon("download-alt", lib = "glyphicon"),
                                    tags$b("Download data"), " - save the data within the table to your device."),
                            tags$li(icon("sort", lib = "glyphicon"),
                                    tags$b("Sort"), " - click to sort the table in ascending or 
                                    descending order based on the values in a column."),
                            tags$li(tags$b("Filter"), " - click the grey boxes below column titles 
                                           to select categories shown in the table (for columns 
                                           with numerical data, use the slider to select the 
                                           range of values to be shown)."),
                            tags$li(tags$b("Page controls"), " - switch to specific page of data 
                                    within the table.")
                            )
                            )),
  
  
  p(
    tags$b(
      "Note: Statistical disclosure control has been applied to protect
      patient confidentiality. Therefore, the figures presented here
      may not be additive and may differ from previous publications."
    )
    ),
  
  p(""),
  
  wellPanel(tags$style(".well { background-color: #FFFFFF; 
                       border: 0px solid #336699; }"),
            
            #We are only using one filter here, which contains the...
            #names of the files.
            
            column(6,
                   
                   shinyWidgets::pickerInput(
                     inputId = "table_filenames", 
                     label = "Data file",  
                     choices = c("Time trend (Data explorer)", 
                                 "Age/sex (Data explorer)", 
                                 "Deprivation comparison (Data explorer)", 
                                 "Deprivation profile (Data explorer)", 
                                 "Monthly trends (Data explorer)",
                                 "Activity summary (Trend data)",
                                 "Drug summary (Trend data)",
                                 "Demographic summary (Trend data)",
                                 "Length of stay",
                                 "Emergency admissions",
                                 "Drug type by hospital"), 
                     width = "95%"
                   )
                   
            ), 
            
            #We also insert the 'Download data' button.
            
            column(4,
                   
                   downloadButton(outputId = 'download_table', 
                                  label = 'Download data', 
                                  class = "mytablebutton", 
                                  style = "margin: 25px 10px 25px 10px")
                   
            )
  ),
  
  tags$head(
    tags$style(".mytablebutton { background-color: #0072B2; } 
                    .mytablebutton { color: #FFFFFF; }")
  ),
  
  
  #Finally, insert the actual table.
  
  mainPanel(width = 12, 
            dataTableOutput("table_tab")) 
  
    )

  #End of tabset panel
    ), 
HTML('<div data-iframe-height></div>')
      
#End of UI part
  )
#)#secure app  
    
##############################################.      
############## Server ----
##############################################.  
    credentials <- readRDS("admin/credentials.rds")
    server  <-  function(input, output, session)
    {
      
      res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
      )
      
      output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
      })
      
      #These observeEvent() commands will be combined with action buttons in...
      #the User Interface to allow the user to navigate to each tab by clicking...
      #on links in the Introduction page (in addition to the classic way of...
      #navigating, which is by clicking on the tab header itself).
      
##############################################.
############## Home Tab ----
##############################################.
      
      
      observeEvent(
        input$link_to_home,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Introduction")
        })
      observeEvent(
        input$link_to_home2,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Introduction")
        })
      observeEvent(
        input$link_to_home3,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Introduction")
        })
      observeEvent(
        input$link_to_home4,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Introduction")
        })
      
      observeEvent(
        input$link_to_home5,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Introduction")
        })
      
      observeEvent(
        input$link_to_geography,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Location comparison")
        })
      observeEvent(
        input$link_to_substances,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Drug type comparison")
        })
      observeEvent(
        input$link_to_age_sex,
        {
          updateTabsetPanel(session, "Panels",
                            selected = "Age/sex")
        })
      observeEvent(
        input$link_to_deprivation,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Deprivation")
        })
      observeEvent(
        input$link_to_monthly,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Monthly trend")
        })
      observeEvent(
        input$link_to_table,
        {
          updateTabsetPanel(session, "Panels", 
                            selected = "Data")
        })
      

##############################################.
############## Geography tab ----
##############################################.  
      
      #We need to include the input for diagnosis types and substances in the server section 
      #rather than the UI section. 
      #This is because the 'substances' input is dependent on the 'diagnosis
      #type' input. 
      

      output$time_trend_diagnosis_type <- renderUI({
        shinyWidgets::pickerInput(inputId = "Diagnosis_Type", 
                                  label = "Diagnosis grouping",
                                  choices = diagnosis_types)
      })

      output$time_trend_substance1 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances",
                                  label = "Drug type",  
                                  choices = (if(input$Diagnosis_Type == "Overdose")
                                    drug_types1
                                    else
                                      drug_types2), 
                                  selected = "Any drug type"
        )
      }) 
      
         
      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
      
      geography_new <- reactive({
        time_trend %>%
          filter(
            hospital_type %in% input$Hospital_Type
            & diagnosis_type %in% input$Diagnosis_Type
            & activity_type %in% input$Activity_Type
            & geography %in% input$Location
            & drug_type %in% input$Substances
            & measure %in% input$Measure 
          )%>%
          select(year, hospital_type, diagnosis_type, activity_type,
                 geography_type, geography, drug_type,value) %>% 
          droplevels(except= c(1,5))
      })
      
      #then we can plot the actual graph, with labels
      output$geography_plot <- renderPlotly({
        
        if ((input$Location == 
            "Outside Scotland"|
             input$Location == 
             "Other/Not Known")
          & input$Measure== 
             "Rate"
          )
          
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Rates are not available for locations outside Scotland or
            unknown", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        #Now let's create alt message.

        else if (input$Substances == 
                  "Any drug type"
                  & input$Measure== 
                  "Percentage")
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Percentages are not available for all drugs", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        
        else if (is.null(input$Location))
                 
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Please make a selection from the drop down menus", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        

        else {
        #first the tooltip label
        tooltip_geography <- paste0(
          "Financial year: ",
          geography_new()$year,
          "<br>",
          "Location: ",
          geography_new()$geography,
          "<br>",
          "Drug type: ",
          geography_new()$drug_type,
          "<br>",
          input$Measure,": ",
          formatC(geography_new()$value, big.mark = ",",
                  digits = ifelse(input$Measure =="Number",0,2),
                  format ="f")
        )
        
        #Create the main body of the chart.
        
        plot_ly(
          data = geography_new(),
          #plot
          x = ~  year,
          y = ~  value,
          color = ~  geography,
          colors = 
            #We use 8 colours that are considered to be 
            #blind friendly
            c('#3F3685','#9B4393','#0078D4',
              '#83BB26','#948DA3','#1E7F84',
              '#6B5C85','#C73918'
              )[1:length(input$Location)],
       
          symbol = ~ geography_type,
          symbols = c(17,15,16),
          
          name = ~  str_wrap(geography,10),
          #tooltip
          text = tooltip_geography,
          hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 7),
          width = 1000,
          height = 500
        ) %>%
   
          #add in title to chart
          
          
          layout(
            #Title
          title = list (text = (paste0("<b>",
                                       str_sub(input$Activity_Type,1,-2),
                                       " ",
                                       str_to_lower(input$Measure),
                                       "s for selected locations 1996/97 to 2020/21",
                                       "<br>", "(",input$Hospital_Type,"; ",
                                       word(input$Diagnosis_Type, start = 1, sep = " \\("), 
                                       "; ",
                                       input$Substances, 
                                       ")", "<b>")),
                                       font = list (size=15)),
            

          
          
                 separators = ".,",
          
          annotations = 
            list(x = 0.99, y = -0.27, 
                 text = paste0("Source: Drug-Related","<br>",
                               "Hospital Statistics","<br>",
                               "(PHS, 2021)"), 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='left', yanchor='auto', xshift=0, yshift=0,
                 font=list(family = "arial", size=12, color="#7f7f7f")),
          
          
          #y=axis formatting       
           yaxis = list(
                                      
                   exponentformat = "none",
                   
                   separatethousands = TRUE,
                   
                   range = c(0, max(geography_new()$value, na.rm = TRUE) +
                               (max(geography_new()$value, na.rm = TRUE)
                                * 10 / 100)),
                   fixedrange = TRUE,
                   
                   title = 
                     ifelse(input$Measure == "Rate",
                            paste0(c(
                              str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
                            ),
                            collapse = ""),
                            paste0(c(
                              rep("&nbsp;", 20),
                              input$Measure,
                              rep("&nbsp;", 20),
                              rep("\n&nbsp;", 3)
                            ),
                            collapse = "")
                     ),
                   showline = TRUE,
                   ticks = "outside"
                   
                 ),
               
                 #Set the tick angle to minus 45. It's the only way for the x...
                 #axis tick labels (fin. years) to display without overlapping...
                 #with each other.
                 #Wrap the x axis title in blank spaces so that it doesn't...
                 #overlap with the x axis tick labels.
                 
                 xaxis = list(range = c(-1,25),
                              fixedrange = TRUE,
                              tickangle = 0,
                              autotick = F,
                              dtick = 3,
                              title = paste0( "Financial year",
                                               "<br>",
                                               "<br>"),
                              showline = TRUE,
                              ticks = "outside"),
        
                      
                 #        #Fix the margins so that the graph and axis titles have enough...
                 #       #room to display nicely.
                 #      #Set the font sizes.
                 #
                 margin = list(l = 90, r = 60, b = 70, t = 90),
                 font = list(size = 13),
                 
                 #insert legend
                 showlegend = TRUE,
                 legend = list(
                               bgcolor = 'rgba(255, 255, 255, 0)',
                               bordercolor = 'rgba(255, 255, 255, 0)')
          ) %>%
          
          
          #Remove unnecessary buttons from the modebar.
          
          config(displayModeBar = TRUE,
                 modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                               'zoomOut2d', 'autoScale2d',
                                               'toggleSpikelines',
                                               'hoverCompareCartesian',
                                               'hoverClosestCartesian'),
                 displaylogo = F, editable = F)
        }
      })
      

      #Insert table
      output$geography_table <- renderDataTable({
        datatable(geography_new(),
                  colnames = c("Financial year",
                               "Hospital type",
                               "Diagnosis grouping",
                               "Activity type",
                               "Location type",
                               "Location",
                               "Drug type",
                               input$Measure),
                  rownames = FALSE,
                  style = "Bootstrap"
        ) %>% 
          formatRound(columns = 8, digits = ifelse(input$Measure =="Number",0,2))
    
      })
      
      
      
      
      output$download_geography <- downloadHandler(
        filename = 'time_trend_geography_data.csv',
        content = function(file) {
          write.table(geography_new(), 
                      file,
                      #Remove row numbers as the CSV file already has row numbers.
                      
                      row.names = FALSE,
                      col.names = c("Financial year", "Hospital type", "Diagnosis grouping",
                                    "Activity type" ,"Location type","Location", 
                                    "Drug type", 
                                    input$Measure), 
                      sep = ",")
        }
      )
      
##############################################.
############## Substances tab ----
##############################################.
      
      #We need to include the input for substances types in the server section 
      #rather than the UI section. 
      #This is because the 'location' input is dependent on the 'location
      #type' input. 
      
      output$time_trend_diagnosis_type2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Diagnosis_Type2", 
                                  label = "Diagnosis grouping",
                                  choices = diagnosis_types)
      })
      
      output$time_trend_substance2 <- renderUI({
        shinyWidgets::pickerInput(inputId = "Substances2",
                                  label = "Drug type (multiple selection)",  
                                  choices = (if(input$Diagnosis_Type2 == "Overdose")
                                  drug_types1
                                  else
                                  drug_types2),
                                  multiple = TRUE, 
                                  options = list (`selected-text-format` = "count > 1", 
                                                  `count-selected-text` = "{0} drug types chosen"),
                                  selected = "Any drug type"
        )
      }) 

      #we can then plot the graph based on the user input.
      #First we create a subset based on user input
    
      substances_new <- reactive({
        time_trend %>%
          filter(
            hospital_type %in% input$Hospital_Type2
            & diagnosis_type %in% input$Diagnosis_Type2
            & activity_type %in% input$Activity_Type2
            & geography %in% input$Location2
            & drug_type %in% input$Substances2
            & measure %in% input$Measure2
          )%>%
          select(year, hospital_type, diagnosis_type, activity_type,
                 geography_type, geography, drug_type,value)
      })
      
      #then we can plot the actual graph, with labels
      output$substances_plot <- renderPlotly({
        
        if ((input$Location2 == 
             "Outside Scotland"|
             input$Location2 == 
             "Other/Not Known")
            & input$Measure2 == 
            "Rate"
        )
          
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Rates are not available for locations outside Scotland or
            unknown", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        #Now let's create alt message.
        
        else if (input$Substances2 == 
                 "Any drug type"
                 & length(input$Substances2) ==1
                 & input$Measure2 == 
                 "Percentage")
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Percentages are not available for all drugs", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        
        else if (is.null(input$Substances2))
        
        { 
          
          #This is the message we are using.
          
          text_state_hosp <- list(
            x = 5, 
            y = 2,
            font = list(color = "#0072B2", size = 20),
            text = 
              "Please make a selection from the drop down menus", 
            xref = "x", 
            yref = "y",  
            showarrow = FALSE
          ) 
          
          #Visualise an empty graph with the above message in the middle.
          
          plot_ly() %>% 
            layout(annotations = text_state_hosp, 
                   yaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE), 
                   xaxis = list(showline = FALSE, 
                                showticklabels = FALSE, 
                                showgrid = FALSE)) %>%  
            config(displayModeBar = FALSE,
                   displaylogo = F, editable = F) 
          
        }
        
        
        
        else {
        
        
        
        #first the tooltip label
                tooltip_substances <- paste0(
                  "Financial year: ",
                  substances_new()$year,
                  "<br>",
                  "Location: ",
                  substances_new()$geography,
                  "<br>",
                  "Drug type: ",
                  substances_new()$drug_type,
                  "<br>",
                  input$Measure2,": ",
                  formatC(substances_new()$value, big.mark = ",", 
                          digits = ifelse(input$Measure2 =="Number",0,2),
                          format = 'f')
                )
                
                
                
        #Create the main body of the chart.
        
        plot_ly(
          data = substances_new(),
          #plot
          x = ~  year,
          y = ~  value,
          color = ~  drug_type,
          colors = 
            #Colors are assigned to each drug type
          c('#3F3685',  #Any drug type
            '#9B4393',  #Cannabinoids
            '#83BB26',  #Cocaine
            '#948DA3',  #Multiple/Other,
            '#1E7F84',  #Other stimulants
            '#6B5C85',  #Sedatives/Hypnotics
            '#C73918',  #Hallucinogens
            '#0078D4',  #Opioids
            '#9F9BC2',  #Heroin
            '#CDA1C9',  #Methadone
            '#80BCEA'   #Other Opioids
          ),
          name = ~ str_wrap(drug_type,10),
          #tooltip
                    text = tooltip_substances,
                    hoverinfo = "text",
          #type
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(size = 7),
          width = 1000,
          height = 500
        ) %>%
          
          #add in title to chart
          
          
          layout(
            #Title
            
            title = list (text = (paste0("<b>",str_sub(input$Activity_Type2,1,-2),
                                         " ",
                                         str_to_lower(input$Measure2),
                                         "s for selected drug types 1996/97 to 2020/21",
                                         "<br>", "(",
                                         input$Location2, 
                                         "; ",
                                         input$Hospital_Type2, "; ",
                                         word(input$Diagnosis_Type2, start = 1, sep = " \\("), 
                                         ")", "<b>")),
                          font = list (size=15)),
 
            separators = ".,",
            annotations = 
              list(x = 0.96, y = -0.27, 
                   text = paste0("Source: Drug-Related","<br>",
                                 "Hospital Statistics","<br>",
                                 "(PHS, 2021)"), 
                   showarrow = F, xref='paper', yref='paper', 
                   xanchor='left', yanchor='auto', xshift=0, yshift=0,
                   font=list(family = "arial", size=12, color="#7f7f7f")),
            
            #y=axis formatting       
            yaxis = list(
              
              exponentformat = "none",
              
              separatethousands = TRUE,
              
              range = c(0, max(substances_new()$value, na.rm = TRUE) +
                          (max(substances_new()$value, na.rm = TRUE)
                           * 10 / 100)),fixedrange = TRUE,
              
              title = ifelse(input$Measure2 == "Rate",
                             paste0(c(str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
                             ),
                             collapse = ""),
                             paste0(c(
                               rep("&nbsp;", 20),
                               input$Measure2,
                               rep("&nbsp;", 20),
                               rep("\n&nbsp;", 3)
                             ),
                             collapse = "")
              ),
              showline = TRUE,
              ticks = "outside"
              
            ),
            
            #Set the tick angle to minus 45. It's the only way for the x...
            #axis tick labels (fin. years) to display without overlapping...
            #with each other.
            #Wrap the x axis title in blank spaces so that it doesn't...
            #overlap with the x axis tick labels.
            
            xaxis = list(range = c(-1,25),
                         fixedrange = TRUE,
                         tickangle = 0,
                         autotick = F,
                         dtick = 3,
                         title = paste0("Financial year",
                                        "<br>",
                                        "<br>"),
                         showline = TRUE,
                         ticks = "outside"),
            
            #        #Fix the margins so that the graph and axis titles have enough...
            #       #room to display nicely.
            #      #Set the font sizes.
            #
            margin = list(l = 90, r = 60, b = 70, t = 90),
            font = list(size = 13),
            
            #insert legend
            showlegend = TRUE,
            legend = list(
                          bgcolor = 'rgba(255, 255, 255, 0)',
                          bordercolor = 'rgba(255, 255, 255, 0)')) %>%
          
          #Remove unnecessary buttons from the modebar.
          
          config(displayModeBar = TRUE,
                 modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                               'zoomOut2d', 'autoScale2d',
                                               'toggleSpikelines',
                                               'hoverCompareCartesian',
                                               'hoverClosestCartesian'),
                 displaylogo = F, editable = F)
        }  
      })
      

      
      #Insert table
      output$substances_table <- renderDataTable({
        datatable(substances_new(),
                  colnames = c("Financial year",
                               "Hospital type",
                               "Diagnosis grouping",
                               "Activity type",
                               "Location type",
                               "Location",
                               "Drug type",
                               input$Measure2),
                  rownames = FALSE,
                  style = "Bootstrap") %>% 
          formatRound(columns = 8, 
                      digits = ifelse(input$Measure2 =="Number",0,2))
      })
        
        output$download_substances <- downloadHandler(
          filename = 'time_trend_Substance_data.csv',
          content = function(file) {
            write.table(substances_new(), 
                        file,
                        #Remove row numbers as the CSV file already has row numbers.
                        
                        row.names = FALSE,
                        col.names = c("Financial year", 
                                      "Hospital type", 
                                      "Diagnosis grouping", 
                                      "Activity type" ,
                                      "Location type",
                                      "Location", 
                                      "Drug type", 
                                      input$Measure2), 
                        sep = ",")
          }
        )

##############################################.
############## Age/Sex tab ----
##############################################.

        
        output$age_sex_diagnosis_type <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Diagnosis_Type3",
            label = "Diagnosis grouping",
            choices = diagnosis_types
          )
        })
        
        output$age_sex_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances3",
            label = "Drug type",
            choices = (if (input$Diagnosis_Type3 == "Overdose")
              drug_types1
              else
                drug_types2),
            selected = "All"
          )
        })
        
        #So here we need to create two graphs
        #1) Time-Trend - a line chart showing the change over time by chosen age/sex factors.
        #2) Year only- a tornado chart that shows all age/sex by a year
        
        
        #1) Time trend
        
        #Filter it by options for time trend
        age_sex_time_new <- reactive({
          age_sex %>%
            filter(
              hospital_type %in% input$Hospital_Type3
              & diagnosis_type %in% input$Diagnosis_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
              #and the age/sex options
              & age_group %in% input$Age
              & sex %in% input$Sex
            ) %>%
            select(year, hospital_type, diagnosis_type, activity_type,
                   drug_type, age_group,sex,value)
        })

        
        #Create the main body of the chart.
        output$age_sex_time_plot <- renderPlotly({
          
          
          if (input$Substances3 == 
             "Any drug type"
             & input$Measure3 == 
             "Percentage")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Percentages are not available for all drugs", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          #Now let's create alt message.

          else if (is.null(input$Age)|
            is.null(input$Sex))
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Please make a selection from the drop down menus", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
          
          #Add in a tooltip
          tooltip_age_sex_time <- paste0(
            "Financial year: ",
            age_sex_time_new()$year,
            "<br>",
            "Age: ",
            age_sex_time_new()$age_group,
            "<br>",
            "Sex: ",
            age_sex_time_new()$sex,
            "<br>",
            input$Measure3,
            ": ",
            formatC(abs(age_sex_time_new()$value), big.mark = ",", 
                    digits = ifelse(input$Measure3 =="Number",0,2), 
                    format = 'f'
            )
          )
          
          plot_ly(
            data = age_sex_time_new(),
            #plot
            x = ~  year,
            y = ~  value,
            color = ~  age_group,
            colors = #Colors are assigned 
            c('#3F3685','#9B4393','#0078D4','#83BB26',
              '#948DA3','#1E7F84',
              '#6B5C85', '#C73918'
            ),
            #so we will use different linetypes to
            #distinguish between sex.
            linetype = ~ sex,
            #tooltip
            text = tooltip_age_sex_time,
            hoverinfo = "text",
            #type
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 7),
            width = 1000,
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout(
                   title = list (text = (paste0("<b>",
                                                str_sub(input$Activity_Type3,1,-2),
                                                " ",
                                                str_to_lower(input$Measure3),
                                                "s for selected age group/sex 1996/97 to 2020/21",
                                                "<br>", "(Scotland; ",input$Hospital_Type3,"; ",
                                                word(input$Diagnosis_Type3, start = 1, sep = " \\("), 
                                                "; ",
                                                input$Substances3,
                                                ")", "<b>")),
                                 font = list (size=15)),
                   separators = ".,",
                   annotations = 
                     list(x = 0.98, y = -0.27, 
                          text = paste0("Source: Drug-Related","<br>",
                                        "Hospital Statistics","<br>",
                                        "(PHS, 2021)"), 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='left', yanchor='auto', xshift=0, yshift=0,
                          font=list(family = "arial", size=12, color="#7f7f7f")),
                   
                   #We need to fix the range of the y axis, as R refuses to set...
                   #the lower end of this axis to zero.
                   #The following "range" command fixes the lower end to...
                   #zero, and calculates the upper end as the maximum...
                   #number visualised in the graph + 10% of this number.
                   #Also, wrap the y axis title in blank spaces so it doesn't...
                   #overlap with the y axis tick labels.
                   #Finally, make the y axis title reactive.
                   
                   yaxis = list(
                     
                     exponentformat = "none",
                     
                     separatethousands = TRUE,
                     
                     range = c(0, max(age_sex_time_new()$value, na.rm = TRUE) + 
                                 (max(age_sex_time_new()$value, na.rm = TRUE) 
                                  * 10 / 100)), fixedrange = TRUE,
                     
                     title = ifelse(input$Measure3 == "Rate",
                                    paste0(c(str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
                                    ),
                                    collapse = ""),
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      input$Measure3,
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = "")
                     ),
                     showline = TRUE, 
                     ticks = "outside"
                     
                   ),
                   
                   #Set the tick angle to minus 45. It's the only way for the x...
                   #axis tick labels (fin. years) to display without overlapping...
                   #with each other.
                   #Wrap the x axis title in blank spaces so that it doesn't...
                   #overlap with the x axis tick labels.
                   
                   xaxis = list(range = c(-1,25),
                                fixedrange = TRUE,
                                tickangle = 0,
                                autotick = F,
                                dtick = 3, 
                                title = paste0("Financial year",
                                               "<br>",
                                               "<br>"
                                                 ),
                                showline = TRUE, 
                                ticks = "outside"),
                   
                   #Fix the margins so that the graph and axis titles have enough...
                   #room to display nicely.
                   #Set the font sizes.
                   
                   margin = list(l = 90, r = 60, b = 70, t = 90),
                   font = list(size = 13),
                   
                   #Insert a legend so that the user knows which colour...
                   #corresponds to which location of treatment.
                   #Make the legend background and legend border white.              
                   
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
                   displaylogo = F, editable = F)
          }
        })
        

        
        #Table
        output$age_sex_trend_table <- renderDataTable({
          datatable(
            age_sex_time_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital  type",
              "Diagnosis grouping",
              "Activity type",
              "Drug type",
              "Age group",
              "Sex",
              input$Measure3
            )
          ) %>% 
            formatRound(columns = 8, 
                        digits = ifelse(input$Measure3 =="Number",0,2))
        })
        
        #Download button
        
        output$download_age_sex_trend <- downloadHandler(
          filename = 'age_sex_trend_data.csv',
          content = function(file) {
            write.table(
              age_sex_time_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Diagnosis grouping",
                "Activity type",
                "Drug type",
                "Age group",
                "Sex",
                input$Measure3
              ),
              sep = ","
            )
          }
        )
        
        
        ###
        ###Tornado Chart
        ###
        
        age_sex_year_new <- reactive({
          age_sex_tornado %>%
            filter(
              year %in% input$Financial_Year
              & hospital_type %in% input$Hospital_Type3
              & diagnosis_type %in% input$Diagnosis_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        age_sex_year_new_axis <- reactive({
          age_sex_tornado %>%
            filter(hospital_type %in% input$Hospital_Type3
                   & diagnosis_type %in% input$Diagnosis_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            droplevels()
        })
        
        
        #then plot it
        output$age_sex_year_plot <- renderPlotly({
          
          
          if (input$Substances3 == 
              "Any drug type"
              & input$Measure3 == 
              "Percentage")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Percentages are not available for all drugs", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          #Now let's create alt message.
          
          else if (input$Financial_Year %in% financial_years[1:10]
                   & input$Activity_Type3 == "New patients")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Data are not available for new patients before 
              2006/07", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {

          #add in tooltip
          tooltip_age_sex_year <- paste0(
            "Financial year: ",
            age_sex_year_new()$year,
            "<br>",
            "Age: ",
            age_sex_year_new()$age_group,
            "<br>",
            "Sex: ",
            age_sex_year_new()$sex,
            "<br>",
            input$Measure3,
            ": ",
            formatC(abs(age_sex_year_new()$value), big.mark = ",", 
                    digits = ifelse(input$Measure3 =="Number",0,2), 
                    format = 'f')
          )
          
          plot_ly(
            data = age_sex_year_new(),
            
            x = ~ value,
            y = ~ age_group,
            color = ~ sex,
            
            #Colour palette:
            #Dark blue for females and light blue for males.
            
            colors = c("#0072B2", "#ADD8E6"),
            
            text = tooltip_age_sex_year,
            hoverinfo = "text",
            
            #Select the type of chart you want, in this case a bar chart,...
            #and set the orientation to horizontal to achieve the...
            #"pyramid" look.
            
            type = 'bar',
            width = 1000,
            height = 500
          ) %>%
            
            layout(title = list (text = (paste0("<b>",
                                                str_sub(input$Activity_Type3,1,-2),
                                                " ",
                                                str_to_lower(input$Measure3),
                                                "s by age group/sex",
                                                "<br>", "(Scotland; ",
                                                input$Financial_Year,
                                                "; ",
                                                input$Hospital_Type3, "; ",
                                                word(input$Diagnosis_Type3, start = 1, sep = " \\("), 
                                                "; ",
                                                input$Substances3,
                                                ")", "<b>")),
                                 font = list (size=15)),
              
              
              bargap = 0.2,
              barmode = "overlay",
              
              yaxis = list(fixedrange = TRUE,
                title = paste0(c(
                  rep("&nbsp;", 20),
                  "Age group",
                  rep("&nbsp;", 20),
                  rep("\n&nbsp;", 3)
                ),
                collapse = ""),
                showline = TRUE,
                ticks = "outside"
              ),
              xaxis = list(
                exponentformat = "none",
                separatethousands = TRUE,
                tickmode = 'array',
                range = c(-round(max(
                  abs(age_sex_year_new_axis()$value)
                )
                * 110 / 100),
                round(max(
                  abs(age_sex_year_new_axis()$value)
                )
                * 110 / 100)
                ),fixedrange = TRUE,
                tickangle = 0,
                tickvals = c(
                  -round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1)), 
                  
                  -round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2
                  ,
                  
                  0,
                  round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                  
                  round(max(abs(
                    age_sex_year_new_axis()$value
                  )),
                  ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))
                ),
                ticktext = paste0(
                  formatC(
                  c(
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1)),
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                    
                    0,
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))/2,
                    
                    round(max(abs(
                      age_sex_year_new_axis()$value
                    )),
                    ((-nchar(as.integer(max(abs(age_sex_year_new_axis()$value)))))+1))
               
                    )
                  ,
                  big.mark=",")
                  )
                  
                ,
                
                #Make the x axis title reactive.
                
                title =  ifelse(input$Measure3 == "Rate",
                                str_wrap("European Age-sex Standardised Rate per 100,000 population",30),
                                  input$Measure3
                ),
                
                showline = TRUE,
                ticks = "outside"
                
              ),
              annotations = 
                list(x = 0.99, y = -0.21, 
                     text = paste0("Source: Drug-Related","<br>",
                                   "Hospital Statistics","<br>",
                                   "(PHS, 2021)"), 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='left', yanchor='auto', xshift=0, yshift=0,
                     font=list(family = "arial", size=12, color="#7f7f7f")),
      
              #Fix the margins so that the graph and axis titles have...
              #enough room to display nicely.
              #Set the font sizes.
              
              margin = list(
                l = 140,
                r = 10,
                b = 70,
                t = 90
              ),
              font = list(size = 13),
              
              #Insert a legend so that the user knows which colour...
              #corresponds to which sex.
              #Make the legend background and legend border white.
              
              showlegend = TRUE,
              legend = list(
                bgcolor = 'rgba(255, 255, 255, 0)',
                bordercolor = 'rgba(255, 255, 255, 0)'
              )
            ) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(
              displayModeBar = TRUE,
              modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                            'zoomOut2d', 'autoScale2d',
                                            'toggleSpikelines',
                                            'hoverCompareCartesian',
                                            'hoverClosestCartesian'),
              displaylogo = F,
              editable = F
            )
          }
        })
        
        #we can now add in the table for the bar chart- values
        #have to be reassigned to positive values
        age_sex_year_table <- reactive({
          age_sex_tornado %>%
            filter(
              year %in% input$Financial_Year
              & hospital_type %in% input$Hospital_Type3
              & diagnosis_type %in% input$Diagnosis_Type3
              & activity_type %in% input$Activity_Type3
              & drug_type %in% input$Substances3
              & measure %in% input$Measure3
            ) %>%
            select(-measure) %>%
            mutate(value = abs(value))
          
        })
        
        #Table
        output$age_sex_year_table <- renderDataTable({
          datatable(
            age_sex_year_table(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital type",
              "Diagnosis grouping",
              "Activity type",
              "Drug type",
              "Age",
              "Sex",
              input$Measure3
            )
          ) %>% 
            formatRound(columns = 8,
                        digits = ifelse(input$Measure3 =="Number",0,2))
        })
        
        #Download button
        
        output$download_age_sex_year <- downloadHandler(
          filename = 'age_sex_year_data.csv',
          content = function(file) {
            write.table(
              age_sex_year_table(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Diagnosis grouping",
                "Activity type",
                "Drug type",
                "Age",
                "Sex",
                input$Measure3
              ),
              sep = ","
            )
          }
        )
        
      
      
##############################################.
############## Deprivation tab ----
##############################################.
      
        
        output$SIMD_diagnosis_type <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Diagnosis_Type4",
            label = "Diagnosis grouping",
            choices = diagnosis_types
          )
        })
        
        output$SIMD_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances4",
            label = "Drug type",
            choices = (if (input$Diagnosis_Type4 == "Overdose")
              drug_types1
              else
                drug_types2),
            selected = "All"
          )
        })
        
        
        #So here we now need to set up two different charts 
        #1) For the comparison
        #2) For the local within comparison. 
        
        #1) SIMD Between Comparison  ####
        
        #Filter it by options for time trend
        SIMD_new <- reactive({
          deprivation %>%
            filter(
              hospital_type %in% input$Hospital_Type4
              & diagnosis_type %in% input$Diagnosis_Type4
              & geography %in% input$Location3
              & activity_type %in% input$Activity_Type4
              & drug_type %in% input$Substances4
              & measure %in% input$Measure4
              #and the year options
              & year %in% input$Financial_Year2
            )%>%
            select(year, hospital_type, diagnosis_type, activity_type,
                   geography_type,geography,drug_type,simd, value) %>% 
            droplevels()
        })
        
        
        
        #Create the main body of the chart.
        output$SIMD_plot <- renderPlotly({
          
          
          if (input$Financial_Year2 %in% financial_years[1:10]
              & input$Activity_Type4 == "New patients")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Data are not available for new patients before 
              2006/07", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
          
          #Add in a tooltip
          tooltip_SIMD <- paste0(
            "Financial year: ",
            SIMD_new()$year,
            "<br>",    
            "Location: ",
            SIMD_new()$geography,
            "<br>",
            "Deprivation index: ",
            SIMD_new()$simd,
            "<br>",
            "Substance: ",
            SIMD_new()$drug_type,
            "<br>",
            input$Measure4,
            ": ",
            formatC(SIMD_new()$value, big.mark = ",", 
                    digits = ifelse(input$Measure4 =="Number",0,2), 
                    format = 'f')
          )
          
          plot_ly(
            data = SIMD_new(),
            #plot- 
            x = ~  simd,
            y = ~  value,
            color = ~ geography,
            colors = c('#3F3685','#9B4393','#0078D4',
                '#83BB26','#948DA3','#1E7F84',
                '#6B5C85','#C73918')[1:length(input$Location3)],
            type = 'bar',
            #tooltip,
            name = ~ str_wrap(geography,10),
            text = tooltip_SIMD,
            hoverinfo = "text",
            #type
            
            width = 1000,
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout( title = list (text = (paste0("<b>",
                                                str_sub(input$Activity_Type4,1,-2),
                                                " ",
                                                str_to_lower(input$Measure4),
                                                "s by deprivation quintile and selected locations",
                                                "<br>","(",
                                                input$Financial_Year2,
                                                "; ",
                                                input$Hospital_Type4, "; ",
                                                word(input$Diagnosis_Type4, start = 1, sep = " \\("), 
                                                "; ",
                                                input$Substances4,
                                                ")", "<b>")),
                                 font = list (size=15)),
                   
                   
                   separators = ".,",
                   annotations = 
                     list(x = 0.97, y = -0.27, 
                          text = paste0("Source: Drug-Related","<br>",
                                        "Hospital Statistics","<br>",
                                        "(PHS, 2021)"), 
                          showarrow = F, xref='paper', yref='paper', 
                          xanchor='left', yanchor='auto', xshift=0, yshift=0,
                          font=list(family = "arial", size=12, color="#7f7f7f")),  
                   #overlap with the y axis tick labels.
                   #Finally, make the y axis title reactive.
                   yaxis = list(
                     
                     exponentformat = "none",
                     
                     separatethousands = TRUE,
                     
                     range = c(0, max(SIMD_new()$value, na.rm = TRUE) + 
                                 (max(SIMD_new()$value, na.rm = TRUE) 
                                  * 10 / 100)), fixedrange = TRUE,
                     
                     title = ifelse(input$Measure4 == "Rate",
                                    paste0(c(str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
                                    ),
                                    collapse = ""),
                                    paste0(c(
                                      rep("&nbsp;", 20),
                                      input$Measure4,
                                      rep("&nbsp;", 20),
                                      rep("\n&nbsp;", 3)
                                    ),
                                    collapse = "")
                     ),
                     showline = TRUE, 
                     ticks = "outside"
                     
                   ),
                   
                   #Set the tick angle to minus 45. It's the only way for the x...
                   #axis tick labels (fin. years) to display without overlapping...
                   #with each other.
                   #Wrap the x axis title in blank spaces so that it doesn't...
                   #overlap with the x axis tick labels.
                   
                   xaxis = list( fixedrange = TRUE,
                                title = paste0("Deprivation quintile",
                                               "<br>",
                                               "<br>"),
                                showline = TRUE, 
                                ticks = "outside"),
                   
                   #Fix the margins so that the graph and axis titles have enough...
                   #room to display nicely.
                   #Set the font sizes.
                   
                   margin = list(l = 90, r = 100, b = 70, t = 90),
                   font = list(size = 13),
                   
                   #Insert a legend so that the user knows which colour...
                   #corresponds to which location of treatment.
                   #Make the legend background and legend border white.              
                   
                   ##REMOVE LEGEND FOR NOW- until we have discussed whether 
                   #to have multiple options for any categories
                   showlegend = TRUE,
                   legend = list(
                                 bgcolor = 'rgba(0, 0, 0, 0)', 
                                 bordercolor = 'rgba(255, 255, 255, 0)')) %>%
            
            #Remove unnecessary buttons from the modebar.
            
            config(displayModeBar = TRUE,
                   modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                                 'zoomOut2d', 'autoScale2d',
                                                 'toggleSpikelines',
                                                 'hoverCompareCartesian',
                                                 'hoverClosestCartesian'), 
                   displaylogo = F, editable = F)
          }
        })
        
        
        #Table
        output$SIMD_table <- renderDataTable({
          datatable(
            SIMD_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital type",
              "Diagnosis grouping",
              "Activity type",
              "Location type",
              "Location",
              "Drug type",
              "Deprivation index",
              input$Measure4
            )
          ) %>% 
            formatRound(columns = 9, 
                        digits = ifelse(input$Measure4 =="Number",0,2))
        })
        
        #Download button
        
        output$download_SIMD <- downloadHandler(
          filename = 'deprivation.csv',
          content = function(file) {
            write.table(
              SIMD_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Diagnosis grouping",
                "Activity type",
                "Location type",
                "Location",
                "Drug type",
                "Deprivation index",
                input$Measure4
              ),
              sep = ","
            )
          }
        )
      

        #2) SIMD Within Comparison ####
        
        #Filter it by options for time trend
        SIMD_local_new <- reactive({
          deprivation_local %>%
            filter(
              hospital_type %in% input$Hospital_Type4
              & diagnosis_type %in% input$Diagnosis_Type4
              & activity_type %in% input$Activity_Type4
              & geography %in% input$Location4
              & drug_type %in% input$Substances4
              & measure %in% input$Measure4
              #and the year options
              & year %in% input$Financial_Year2
            )%>%
            select(year, hospital_type, diagnosis_type, activity_type,
                   geography_type,geography,drug_type,simd, value)
        })
        
        
        
        #Create the main body of the chart.
        output$SIMD_local_plot <- renderPlotly({
          
          
          if (input$Financial_Year2 %in% financial_years[1:10]
              & input$Activity_Type4 == "New patients")
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Data are not available for new patients before 
              2006/07", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
            
            #Add in a tooltip
            tooltip_SIMD_local <- paste0(
              "Financial year: ",
              SIMD_local_new()$year,
              "<br>",  
              "Location: ",
              SIMD_local_new()$geography,
              "<br>",
              "Deprivation index: ",
              SIMD_local_new()$simd,
              "<br>",
              "Substance: ",
              SIMD_local_new()$drug_type,
              "<br>",
              input$Measure4,
              ": ",
              formatC(SIMD_local_new()$value, big.mark = ",", 
                      digits = ifelse(input$Measure4 =="Number",0,2), 
                      format = 'f')
            )
            
            plot_ly(
              data = SIMD_local_new(),
              #plot- 
              x = ~  simd,
              y = ~  value,
              color = ~ geography,
              colors = '#3F3685',
              #tooltip
              text = tooltip_SIMD_local,
              hoverinfo = "text",
              #type
              type = 'bar',
              width = 1000,
              height = 500
            ) %>%
              
              #Make the graph title reactive.
              
              layout( title = list (text = (paste0("<b>",
                                                   str_sub(input$Activity_Type4,1,-2),
                                                   " ",
                                                   str_to_lower(input$Measure4),
                                                   "s by deprivation quintile",
                                                   "<br>","(",
                                                   input$Location4, 
                                                   "; ",
                                                   input$Financial_Year2,
                                                   "; ",
                                                   input$Hospital_Type4,"; ",
                                                   word(input$Diagnosis_Type4, start = 1, sep = " \\("), 
                                                   "; ",
                                                   input$Substances4,
                                                   ")", "<b>")),
                                    font = list (size=15)),
                      
                      
                      separators = ".,",
                      annotations = 
                        list(x = 0.97, y = -0.27, 
                             text = paste0("Source: Drug-Related","<br>",
                                           "Hospital Statistics","<br>",
                                           "(PHS, 2021)"), 
                             showarrow = F, xref='paper', yref='paper', 
                             xanchor='left', yanchor='auto', xshift=0, yshift=0,
                             font=list(family = "arial", size=12, color="#7f7f7f")),  
                      #overlap with the y axis tick labels.
                      #Finally, make the y axis title reactive.
                      
                      yaxis = list(
                        
                        exponentformat = "none",
                        
                        separatethousands = TRUE,
                        
                        range = c(0, max(SIMD_local_new()$value, na.rm = TRUE) + 
                                    (max(SIMD_local_new()$value, na.rm = TRUE) 
                                     * 10 / 100)), fixedrange = TRUE,
                        
                        title = ifelse(input$Measure4 == "Rate",
                                       paste0(c(str_wrap("European Age-sex Standardised Rate per 100,000 population",30)
                                       ),
                                       collapse = ""),
                                       paste0(c(
                                         rep("&nbsp;", 20),
                                         input$Measure4,
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 3)
                                       ),
                                       collapse = "")
                        ),
                        showline = TRUE, 
                        ticks = "outside"
                        
                      ),
                      
                      #Set the tick angle to minus 45. It's the only way for the x...
                      #axis tick labels (fin. years) to display without overlapping...
                      #with each other.
                      #Wrap the x axis title in blank spaces so that it doesn't...
                      #overlap with the x axis tick labels.
                      
                      xaxis = list( fixedrange = TRUE,
                        title = paste0("Deprivation quintile",
                                       "<br>",
                                       "<br>"),
                        showline = TRUE, 
                        ticks = "outside"),
                      
                      #Fix the margins so that the graph and axis titles have enough...
                      #room to display nicely.
                      #Set the font sizes.
                      
                      margin = list(l = 90, r = 100, b = 70, t = 90),
                      font = list(size = 13),
                      
                      #Insert a legend so that the user knows which colour...
                      #corresponds to which location of treatment.
                      #Make the legend background and legend border white.              
                      
                      ##REMOVE LEGEND FOR NOW- until we have discussed whether 
                      #to have multiple options for any categories
                      showlegend = FALSE,
                      legend = list(
                        bgcolor = 'rgba(0, 0, 0, 0)', 
                        bordercolor = 'rgba(255, 255, 255, 0)')) %>%
              
              #Remove unnecessary buttons from the modebar.
              
              config(displayModeBar = TRUE,
                     modeBarButtonsToRemove = list('select2d', 'lasso2d', 'zoomIn2d',
                                                   'zoomOut2d', 'autoScale2d',
                                                   'toggleSpikelines',
                                                   'hoverCompareCartesian',
                                                   'hoverClosestCartesian'), 
                     displaylogo = F, editable = F)
          }
        })
        
        
        #Table
        output$SIMD_local_table <- renderDataTable({
          datatable(
            SIMD_local_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Hospital type",
              "Diagnosis grouping",
              "Activity type",
              "Location type",
              "Location",
              "Drug type",
              "Deprivation index",
              input$Measure4
            )
          ) %>% 
            formatRound(columns = 9, 
                        digits = ifelse(input$Measure4 =="Number",0,2))
        })
        
        #Download button
        
        output$download_SIMD_local <- downloadHandler(
          filename = 'deprivation.csv',
          content = function(file) {
            write.table(
              SIMD_local_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Hospital type",
                "Diagnosis grouping",
                "Activity type",
                "Location type",
                "Location",
                "Drug type",
                "Deprivation index",
                input$Measure4
              ),
              sep = ","
            )
          }
        )
  
        
##############################################.
############## Monthly tab ----
##############################################.  

#We need to include the input for diagnosis types and substances in the server section 
#rather than the UI section. 
#This is because the 'substances' input is dependent on the 'diagnosis
#type' input. 
        
        
        output$monthly_diagnosis_type <- renderUI({
          shinyWidgets::pickerInput(inputId = "Diagnosis_Type5", 
                                    label = "Diagnosis grouping",
                                    choices = diagnosis_types)
        })
        
        
        output$monthly_substance <- renderUI({
          shinyWidgets::pickerInput(
            inputId = "Substances5",
            label = "Drug type",
            choices = (if (input$Diagnosis_Type5 == "Overdose")
              drug_types1
              else
                drug_types2),
            selected = "All"
          )
        })

        
        
        #So here we need to create three graphs
        #1) Location - a line chart showing the change over time by chosen Location.
        #2) Drugs - a line chart showing the change over time by chosen Drugs
        #3) Age Group - a line chart showing the change over time by chosen Age Group
        
        
        #1) Monthly Location ####
        
        #Filter it by options for time trend
        monthly_location_new <- reactive({
          monthly %>%
            filter(
              year %in% input$Financial_Year3 
              & hospital_type %in% input$Hospital_Type5
              & diagnosis_type %in% input$Diagnosis_Type5
              & drug_type == 'Any drug type'
              & age_group == 'All age groups'
              & geography == input$Location5
            ) %>%
            select(year, financial_month,
                   hospital_type, diagnosis_type, 
                   geography, 
                   number)%>% 
            droplevels(except= c(2,5))
        })
        
        
        # #Create the main body of the chart.
        output$monthly_location_plot <- renderPlotly({
          
          #Now let's create alt message.
          
          if (is.null(input$Financial_Year3))
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Please make a selection from the drop down menus", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
          

            #Add in a tooltip
            tooltip_monthly_location <- paste0(
              "Financial year: ",
              monthly_location_new()$year,
              "<br>",
              "Month: ",
              monthly_location_new()$financial_month,
              "<br>",
              "Number: ",
              monthly_location_new()$number
              )

            plot_ly(
              data = monthly_location_new(),
              #plot
              x = ~  financial_month,
              y = ~  number,
              color = ~  year,
              colors =             #We use 8 colours that are considered to be 
                #blind friendly
                c('#3F3685','#9B4393','#0078D4',
                  '#83BB26','#948DA3','#1E7F84',
                  '#6B5C85','#C73918'
                )[1:length(input$Financial_Year3)],
              #tooltip
              text = tooltip_monthly_location,
              hoverinfo = "text",
              #type
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(size = 7),
              width = 1000,
              height = 500
            ) %>%

              #Make the graph title reactive.

              layout(
                title = list (text = (paste0("<b>",
                                             "Number of hospital stays by month for selected financial years",
                                             "<br>", "(",
                                             input$Location5, 
                                             "; ",
                                             input$Hospital_Type5, "; ",
                                             word(input$Diagnosis_Type5, start = 1, sep = " \\("), 
                                             ")", "<b>")),
                              font = list (size=15)),
                separators = ".,",
                annotations =
                  list(x = 0.98, y = -0.27,
                       text = paste0("Source: Drug-Related","<br>",
                                     "Hospital Statistics","<br>",
                                     "(PHS, 2021)"),
                       showarrow = F, xref='paper', yref='paper',
                       xanchor='left', yanchor='auto', xshift=0, yshift=0,
                       font=list(family = "arial", size=12, color="#7f7f7f")),

                #We need to fix the range of the y axis, as R refuses to set...
                #the lower end of this axis to zero.
                #The following "range" command fixes the lower end to...
                #zero, and calculates the upper end as the maximum...
                #number visualised in the graph + 10% of this number.
                #Also, wrap the y axis title in blank spaces so it doesn't...
                #overlap with the y axis tick labels.
                #Finally, make the y axis title reactive.

                yaxis = list(

                  exponentformat = "none",

                  separatethousands = TRUE,

                  range = c(0, max(age_sex_time_new()$number, na.rm = TRUE) +
                              (max(age_sex_time_new()$number, na.rm = TRUE)
                               * 10 / 100)), fixedrange = TRUE,

                  title = "Number",
                  showline = TRUE,
                  ticks = "outside"

                ),

                #Set the tick angle to minus 45. It's the only way for the x...
                #axis tick labels (fin. years) to display without overlapping...
                #with each other.
                #Wrap the x axis title in blank spaces so that it doesn't...
                #overlap with the x axis tick labels.

                xaxis = list(range = c(-1,12),fixedrange = TRUE,
                             tickangle = 0,
                             autotick = F,
                             dtick = 2,
                             title = paste0("Month",
                                            "<br>",
                                            "<br>"
                             ),
                             showline = TRUE,
                             ticks = "outside"),

                #Fix the margins so that the graph and axis titles have enough...
                #room to display nicely.
                #Set the font sizes.

                margin = list(l = 90, r = 60, b = 70, t = 90),
                font = list(size = 13),

                #Insert a legend so that the user knows which colour...
                #corresponds to which location of treatment.
                #Make the legend background and legend border white.

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
                     displaylogo = F, editable = F)
          }
        })

        
        
        #Table
        output$Monthly_location_table <- renderDataTable({
          datatable(
            monthly_location_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Financial month",
              "Hospital  type",
              "Diagnosis grouping",
              "Location",
              "Number"
            )
          ) 
        })
        
        #Download button
        
        output$download_Monthly_Location <- downloadHandler(
          filename = 'location_monthly_data.csv',
          content = function(file) {
            write.table(
              monthly_location_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Financial Month",
                "Hospital  type",
                "Diagnosis grouping",
                "Location",
                "Number"
              ),
              sep = ","
            )
          }
        )     
              
        #2) Monthly Drugs ####
        
        #Filter it by options for time trend
        monthly_drugs_new <- reactive({
          monthly %>%
            filter(
              year %in% input$Financial_Year3 
              & hospital_type %in% input$Hospital_Type5
              & diagnosis_type %in% input$Diagnosis_Type5
              & drug_type == input$Substances5
              & age_group == 'All age groups'
              & geography == 'Scotland'
            ) %>%
            select(year, financial_month,
                   hospital_type, diagnosis_type, 
                   drug_type, 
                   number)%>% 
            droplevels(except= c(2,5))
        })
        
        
        # #Create the main body of the chart.
        output$monthly_drugs_plot <- renderPlotly({
          
          #Now let's create alt message.
          
          if (is.null(input$Financial_Year3))
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Please make a selection from the drop down menus", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
          
          #Add in a tooltip
          tooltip_monthly_drugs <- paste0(
            "Financial year: ",
            monthly_drugs_new()$year,
            "<br>",
            "Month: ",
            monthly_drugs_new()$financial_month,
            "<br>",
            "Number: ",
            monthly_drugs_new()$number
          )
          
          plot_ly(
            data = monthly_drugs_new(),
            #plot
            x = ~  financial_month,
            y = ~  number,
            color = ~  year,
            colors =             #We use 8 colours that are considered to be 
              #blind friendly
              c('#3F3685','#9B4393','#0078D4',
                '#83BB26','#948DA3','#1E7F84',
                '#6B5C85','#C73918'
              )[1:length(input$Financial_Year3)],
            #tooltip
            text = tooltip_monthly_drugs,
            hoverinfo = "text",
            #type
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 7),
            width = 1000,
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout(
              title = list (text = (paste0("<b>",
                                           "Number of hospital stays by month for selected financial years",
                                           "<br>", "(Scotland; ",
                                           input$Hospital_Type5, "; ",
                                           word(input$Diagnosis_Type5, start = 1, sep = " \\("), "; ",
                                           input$Substances5,
                                           ")", "<b>")),
                            font = list (size=15)),
              separators = ".,",
              annotations =
                list(x = 0.98, y = -0.27,
                     text = paste0("Source: Drug-Related","<br>",
                                   "Hospital Statistics","<br>",
                                   "(PHS, 2021)"),
                     showarrow = F, xref='paper', yref='paper',
                     xanchor='left', yanchor='auto', xshift=0, yshift=0,
                     font=list(family = "arial", size=12, color="#7f7f7f")),
              
              #We need to fix the range of the y axis, as R refuses to set...
              #the lower end of this axis to zero.
              #The following "range" command fixes the lower end to...
              #zero, and calculates the upper end as the maximum...
              #number visualised in the graph + 10% of this number.
              #Also, wrap the y axis title in blank spaces so it doesn't...
              #overlap with the y axis tick labels.
              #Finally, make the y axis title reactive.
              
              yaxis = list(
                
                exponentformat = "none",
                
                separatethousands = TRUE,
                
                range = c(0, max(age_sex_time_new()$number, na.rm = TRUE) +
                            (max(age_sex_time_new()$number, na.rm = TRUE)
                             * 10 / 100)), fixedrange = TRUE,
                
                title = "Number",
                showline = TRUE,
                ticks = "outside"
                
              ),
              
              #Set the tick angle to minus 45. It's the only way for the x...
              #axis tick labels (fin. years) to display without overlapping...
              #with each other.
              #Wrap the x axis title in blank spaces so that it doesn't...
              #overlap with the x axis tick labels.
              
              xaxis = list(range = c(-1,12),fixedrange = TRUE,
                           tickangle = 0,
                           autotick = F,
                           dtick = 2,
                           title = paste0("Month",
                                          "<br>",
                                          "<br>"
                           ),
                           showline = TRUE,
                           ticks = "outside"),
              
              #Fix the margins so that the graph and axis titles have enough...
              #room to display nicely.
              #Set the font sizes.
              
              margin = list(l = 90, r = 60, b = 70, t = 90),
              font = list(size = 13),
              
              #Insert a legend so that the user knows which colour...
              #corresponds to which location of treatment.
              #Make the legend background and legend border white.
              
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
                   displaylogo = F, editable = F)
          }
        })
        
        
        
        #Table
        output$Monthly_drugs_table <- renderDataTable({
          datatable(
            monthly_drugs_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Financial month",
              "Hospital  type",
              "Diagnosis grouping",
              "Drug type",
              "Number"
            )
          ) 
        })
        
        #Download button
        
        output$download_Monthly_Drugs <- downloadHandler(
          filename = 'drugs_monthly_data.csv',
          content = function(file) {
            write.table(
              monthly_drugs_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Financial Month",
                "Hospital  type",
                "Diagnosis grouping",
                "Drug type",
                "Number"
              ),
              sep = ","
            )
          }
        )     
        
        #3) Monthly Age ####
        
        #Filter it by options for time trend
        monthly_age_new <- reactive({
          monthly %>%
            filter(
              year %in% input$Financial_Year3 
              & hospital_type %in% input$Hospital_Type5
              & diagnosis_type %in% input$Diagnosis_Type5
              & drug_type == 'Any drug type'
              & age_group == input$Ages2
              & geography == 'Scotland'
            ) %>%
            select(year, financial_month,
                   hospital_type, diagnosis_type, 
                   age_group, 
                   number)%>% 
            droplevels(except= c(2,5))
        })
        
        
        # #Create the main body of the chart.
        output$monthly_age_plot <- renderPlotly({
          
          #Now let's create alt message.
          
          if (is.null(input$Financial_Year3))
            
          { 
            
            #This is the message we are using.
            
            text_state_hosp <- list(
              x = 5, 
              y = 2,
              font = list(color = "#0072B2", size = 20),
              text = 
                "Please make a selection from the drop down menus", 
              xref = "x", 
              yref = "y",  
              showarrow = FALSE
            ) 
            
            #Visualise an empty graph with the above message in the middle.
            
            plot_ly() %>% 
              layout(annotations = text_state_hosp, 
                     yaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE), 
                     xaxis = list(showline = FALSE, 
                                  showticklabels = FALSE, 
                                  showgrid = FALSE)) %>%  
              config(displayModeBar = FALSE,
                     displaylogo = F, editable = F) 
            
          }
          
          else {
          
          #Add in a tooltip
          tooltip_monthly_age <- paste0(
            "Financial year: ",
            monthly_age_new()$year,
            "<br>",
            "Month: ",
            monthly_age_new()$financial_month,
            "<br>",
            "Number: ",
            monthly_age_new()$number
          )
          
          plot_ly(
            data = monthly_age_new(),
            #plot
            x = ~  financial_month,
            y = ~  number,
            color = ~  year,
            colors =             #We use 8 colours that are considered to be 
              #blind friendly
              c('#3F3685','#9B4393','#0078D4',
                '#83BB26','#948DA3','#1E7F84',
                '#6B5C85','#C73918'
              )[1:length(input$Financial_Year3)],
            #tooltip
            text = tooltip_monthly_age,
            hoverinfo = "text",
            #type
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 7),
            width = 1000,
            height = 500
          ) %>%
            
            #Make the graph title reactive.
            
            layout(
              title = list (text = (paste0("<b>",
                                           "Number of hospital stays by month for selected financial years",
                                           "<br>", "(Scotland; ",
                                           input$Hospital_Type5, "; ",
                                           word(input$Diagnosis_Type5, start = 1, sep = " \\("), "; ",
                                           input$Ages2,
                                           ")", "<b>")),
                            font = list (size=15)),
              separators = ".,",
              annotations =
                list(x = 0.98, y = -0.27,
                     text = paste0("Source: Drug-Related","<br>",
                                   "Hospital Statistics","<br>",
                                   "(PHS, 2021)"),
                     showarrow = F, xref='paper', yref='paper',
                     xanchor='left', yanchor='auto', xshift=0, yshift=0,
                     font=list(family = "arial", size=12, color="#7f7f7f")),
              
              #We need to fix the range of the y axis, as R refuses to set...
              #the lower end of this axis to zero.
              #The following "range" command fixes the lower end to...
              #zero, and calculates the upper end as the maximum...
              #number visualised in the graph + 10% of this number.
              #Also, wrap the y axis title in blank spaces so it doesn't...
              #overlap with the y axis tick labels.
              #Finally, make the y axis title reactive.
              
              yaxis = list(
                
                exponentformat = "none",
                
                separatethousands = TRUE,
                
                range = c(0, max(age_sex_time_new()$number, na.rm = TRUE) +
                            (max(age_sex_time_new()$number, na.rm = TRUE)
                             * 10 / 100)), fixedrange = TRUE,
                
                title = "Number",
                showline = TRUE,
                ticks = "outside"
                
              ),
              
              #Set the tick angle to minus 45. It's the only way for the x...
              #axis tick labels (fin. years) to display without overlapping...
              #with each other.
              #Wrap the x axis title in blank spaces so that it doesn't...
              #overlap with the x axis tick labels.
              
              xaxis = list(range = c(-1,12),fixedrange = TRUE,
                           tickangle = 0,
                           autotick = F,
                           dtick = 2,
                           title = paste0("Month",
                                          "<br>",
                                          "<br>"
                           ),
                           showline = TRUE,
                           ticks = "outside"),
              
              #Fix the margins so that the graph and axis titles have enough...
              #room to display nicely.
              #Set the font sizes.
              
              margin = list(l = 90, r = 60, b = 70, t = 90),
              font = list(size = 13),
              
              #Insert a legend so that the user knows which colour...
              #corresponds to which location of treatment.
              #Make the legend background and legend border white.
              
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
                   displaylogo = F, editable = F)
          }
        })
        
        
        
        #Table
        output$Monthly_age_table <- renderDataTable({
          datatable(
            monthly_age_new(),
            style = 'bootstrap',
            rownames = FALSE,
            colnames = c(
              "Financial year",
              "Financial month",
              "Hospital  type",
              "Diagnosis grouping",
              "Age group",
              "Number"
            )
          ) 
        })
        
        #Download button
        
        output$download_Monthly_age <- downloadHandler(
          filename = 'age_monthly_data.csv',
          content = function(file) {
            write.table(
              monthly_age_new(),
              file,
              #Remove row numbers as the CSV file already has row numbers.
              
              row.names = FALSE,
              col.names = c(
                "Financial year",
                "Financial Month",
                "Hospital  type",
                "Diagnosis grouping",
                "Age Group",
                "Number"
              ),
              sep = ","
            )
          }
        )     
##############################################.
############## Table tab ----
##############################################.
      
        #On to the final tab, which is the Table tab.
        #The following piece of syntax tells R to switch between files...
        #according to the user's input in the filter SELECT DATA FILE.
        #The files below are the ones we read into R at the very beginning.
        #However, they require a few transformations before they can be displayed...
        #as a table.
        
        data_table <- reactive({
          switch(input$table_filenames,
                 "Time trend (Data explorer)" = time_trend %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Activity type" = activity_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Measure" = measure,
                          "Value" = value) ,
                 "Age/sex (Data explorer)" = age_sex %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Activity type" = activity_type,
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Measure" = measure,
                          "Value" = value),
                 "Deprivation comparison (Data explorer)" = deprivation %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Activity type" = activity_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Deprivation" = simd,
                          "Measure" = measure,
                          "Value" = value),
                 "Deprivation profile (Data explorer)" = deprivation_local %>%
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Activity type" = activity_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Deprivation" = simd,
                          "Measure" = measure,
                          "Value" = value),
                 "Monthly trends (Data explorer)" = monthly %>%
                   rename("Financial year" = year, 
                          "Month" = financial_month,
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Number" = number),
                 "Activity summary (Trend data)" = activity_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Diagnosis grouping" = diagnosis_type,
                        "Activity type" = activity_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Rate" = value),
                 "Drug summary (Trend data)" = drug_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Diagnosis grouping" = diagnosis_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Drug type" = drug_type,
                        "Rate" = value),
                 "Demographic summary (Trend data)" = demographic_summary %>% 
                 rename("Financial year" = year, 
                        "Hospital type" = hospital_type,
                        "Diagnosis grouping" = diagnosis_type,
                        "Location type" = geography_type, 
                        "Location" = geography, 
                        "Age group" = age_group,
                        "Sex" = sex,
                        "Deprivation index" = simd,
                        "Rate" = value),
                 "Length of stay" = length_of_stay %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Deprivation index" = simd,
                          "Number of stays" = total, 
                          "Median length of stay (days)" = med_los,
                          'Length of stay (days)' = length_of_stay,
                          'Percentage' = percentage 
                   ),
                 "Emergency admissions" = emergency_admissions %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Location type" = geography_type, 
                          "Location" = geography, 
                          "Drug type" = drug_type,
                          "Age group" = age_group,
                          "Sex" = sex,
                          "Deprivation index" = simd,
                          "Number of stays" = total,
                          "Emergency admissions (%)" = perc_adm_emer,
                          "Non-emergency admissions (%)" = perc_adm_other
                   ),
                 "Drug type by hospital" = drug_type_by_hospital %>% 
                   rename("Financial year" = year, 
                          "Hospital type" = hospital_type,
                          "Diagnosis grouping" = diagnosis_type,
                          "Activity type" = activity_type, 
                          "Drug type" = drug_type,
                          "SMR01 (%)" = perc_source01,
                          "SMR04 (%)" = perc_source04,
                          "SMR01 and SMR04 (%)" = perc_sourceBOTH, 
                          "Number" = total
                   )
          )
        })
        
        
        #Create the actual table for the Table tab.
        
        output$table_tab <- renderDataTable({
          datatable(data_table(), 
                    style = 'bootstrap', 
                    class = 'table-bordered table-condensed',
                    rownames = FALSE, 
                    options = list(
                      pageLength = 20, 
                      autoWidth = TRUE, 
                      dom = 'tip'),
                    
                    #Insert filters at the top of each column.
                    
                    filter = list(position = 'top'))
        }) 
        
        #We also create a download button for the table tab.
        
        output$download_table <- downloadHandler(
          filename = 'table_data.csv', 
          content = function(file) { 
            write.csv(
              
              #The command "input[["table_tab_rows_all"]]" tells R to create a CSV...
              #file that takes into account the user's input in the filters below...
              #the column names.
              
              data_table()[input[["table_tab_rows_all"]], ], 
              file, 
              row.names = FALSE
            )
          } 
        )
        
      
      #End of server
    }
  
  #End of script
}

shinyApp(ui = ui, server = server)