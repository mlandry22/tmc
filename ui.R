library(shiny)
library(RODBC)

# Get unique values for select list from the database, plus explicit, special purpose -All Conditions-
xf<-odbcConnect("analyticsODBC","SSAS_Member_Cube","SSAS_Member_Cube")
sqlList<-"SELECT DISTINCT CHRONIC_CONDITION, 1 as order1 FROM CHRONIC_DIAGNOSES
                UNION
                SELECT '-All Conditions-', 99 as order1
                ORDER BY order1, CHRONIC_CONDITION"
inputList<-sqlQuery(xf,sqlList)


# Define UI for dataset viewer application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Analytics - Surveillance Tool"),

  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("lob", "Line of business:",
                choices = c("Medicaid SSI","Medicare","TRS")
        ),
        selectInput("region", "* Region:",
                choices = c("All FirstCare","Abilene","Amarillo","Bryan/College Station","Harlingen","Lubbock","Midland/Odessa","New Mexico","San Angelo","South Texas","Wichita Falls")
        ),
    selectInput("condition", "CMS chronic conditions:",
                choices = inputList$CHRONIC_CONDITION
        ),
        selectInput("ahrq", "* AHRQ Hospital and Pediatric measures:",
                choices = c("Hospital 30-day all-cause readmissions","CKD - erythropoietin")
        ),
        selectInput("ambulatory", "* Ambulatory Care Sensitive Conditions:",
                choices = c("Nutritional Deficiencies","Iron Deficiency Anemia","Vaccine Preventable Conditions","Failure to Thrive","Congenital Syphilis")
        ),
        selectInput("fcmeasures", "* Internal FirstCare metrics:",
                choices = c("Medical paid PMPM","Med/Loss ratio","ER Visits per 1000","Admits per 1000")
        ),
        selectInput("hedis", "* HEDIS quality metrics:",
                choices = c("Diabetes - HBA1C control","Diabetes - LDL control")
        ),
        selectInput("ipro", "* Impact Pro measures:",
                choices = c("A/U Risk Score","Medical Risk Score","Probability of an Inpatient Admission, 12 months")
        )
),
  # Show a plot of chronic member rates for each month
  mainPanel(
    tabsetPanel(
      tabPanel("Plots",

        plotOutput("gg2Plot"),
        plotOutput("qccPlot")),
      tabPanel("Table",tableOutput("table")),
      tabPanel("Description",verbatimTextOutput("chronicDescription"))
    )

  )
))
