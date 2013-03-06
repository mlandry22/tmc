library(shiny)
library(ggplot2)
library(qcc)
library(plyr)

chronic<-read.csv("TRS_Chronic_Counts.csv",sep=",")
ssi_chronic<-read.csv("SSI_Chronic_Counts.csv",sep=",")
ma_chronic<-read.csv("MA_Chronic_Counts.csv",sep=",")
chronic$Report_Month<-as.POSIXct(strptime(chronic$Report_Month,format="%m/%d/%Y"))
allConditions<-ddply(chronic, c("Report_Month"),summarize,Members_With_Condition=sum(Members_With_Condition), Fully_Eligible_Members = sum(Fully_Eligible_Members))

ssi_chronic$Report_Month<-as.POSIXct(strptime(ssi_chronic$Report_Month,format="%m/%d/%Y"))
ssi_allConditions<-ddply(ssi_chronic, c("Report_Month"),summarize,Members_With_Condition=sum(Members_With_Condition), Fully_Eligible_Members = sum(Fully_Eligible_Members))

ma_chronic$Report_Month<-as.POSIXct(strptime(ma_chronic$Report_Month,format="%m/%d/%Y"))
ma_allConditions<-ddply(ma_chronic, c("Report_Month"),summarize,Members_With_Condition=sum(Members_With_Condition), Fully_Eligible_Members = sum(Fully_Eligible_Members))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  data <- reactive(function() {
        if(input$condition=="-All Conditions-" && input$lob=="TRS")
                {dist<-allConditions}
        else if(input$condition!="-All Conditions-" && input$lob=="TRS")
                {dist<-chronic[chronic$Chronic_Condition==input$condition,]}

        else if(input$condition!="-All Conditions-" && input$lob=="Medicare")
                {dist<-ma_chronic[ma_chronic$Chronic_Condition==input$condition,]}
        else if(input$condition=="-All Conditions-" && input$lob=="Medicare")
                {dist<-ma_allConditions}

        else if(input$condition=="-All Conditions-" && input$lob=="Medicaid SSI")
                {dist<-ssi_allConditions}
        else if(input$condition!="-All Conditions-" && input$lob=="Medicaid SSI")
                {dist<-ssi_chronic[ssi_chronic$Chronic_Condition==input$condition,]}

    dist <- data.frame(seq(1:length(dist$Members_With_Condition)),round(dist$Members_With_Condition*1.0/dist$Fully_Eligible_Members,6))
    colnames(dist)<-c("month","rate")
    dist
  })

  output$gg2Plot <- reactivePlot(function() {

    dist <- data()
    p<-ggplot(data=dist,aes(x=month,y=rate))+geom_point() + geom_smooth(aes(group=1))
    print(p)
  })

  output$qccPlot <- reactivePlot(function() {

    dist<-data()
    qcc(dist$rate, type="xbar.one")
  })

  output$table <- reactiveTable(function() {
    #dist<-data()
    #dist$rate = round(dist$rate,5)
    #dist
    data()
  },digits=5)  ##pass the digits=5 to xtable through the reactiveTable ...

  output$chronicDescription <- reactiveText(function()
  {
    paste(
      "The CMS Chronic specifications can be found at: \n",
      "  http://www.ccwdata.org/cs/groups/public/documents/document/ccw_conditioncategories2011.pdf \n",
      "Each condition is a combination of diagnosis codes (and positions) to include, diagnosis codes (and positions) to exclude, \n", #to do: remove need for hard breaks
      "minimum numbers for [inpatient, SNF, Home Health, and any type] of visits, and a number of years to look back. \n",
      "The lookback period for each diagnosis is:\n",
      "  Acquired Hypothyroidism: 1 \n",
      "  Acute Myocardial Infarction: 1 \n",
      "  Alzheimers Disease: 3 \n",
      "  Alzheimers Disease and Related Disorders or Senile Dementia: 3 \n",
      "  Anemia: 1 \n",
      "  Asthma: 1 \n",
      "  Atrial Fibrillation: 1 \n",
      "  Benign Prostatic Hyperplasia: 1 \n",
      "  Cataract: 1 \n",
      "  Chronic Kidney Disease: 2 \n",
      "  Chronic Obstructive Pulmonary Disease and Bronchiectasis: 1 \n",
      "  Colorectal Cancer: 1 \n",
      "  Depression: 1 \n",
      "  Diabetes: 2 \n",
      "  Endometrial Cancer: 1 \n",
      "  Female/Male Breast Cancer: 1 \n",
      "  Glaucoma: 1 \n",
      "  Heart Failure: 2 \n",
      "  Hip/Pelvic Fracture: 1 \n",
      "  Hyperlipidemia: 1 \n",
      "  Hypertension: 1 \n",
      "  Ischemic Heart Disease: 2 \n",
      "  Lung Cancer: 1 \n",
      "  Osteoporosis: 1 \n",
      "  Prostate Cancer: 1 \n",
      "  RA/OA (Rheumatoid Arthritis/Osteoarthritis): 2 \n",
      "  Stroke / Transient Ischemic Attack: 1 \n",
      "\n",
      "-All Conditions- includes counts of all conditions, divided by the count of all elgibile members for all conditions. \n",
      "Members with more than one condition will be counted more than once, as will all members eligible for more than one condition.",
      sep="")
  })
})

