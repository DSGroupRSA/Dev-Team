#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(fluidPage(
    shinyjs::useShinyjs(),
    h1("Customer Registration"),
    splitLayout(textInput("name","Name",""),
             textInput("sname","Surname", "")),
    br(),
    radioButtons("radio","Gender",
                 choices = list("Female" = "F",
                                "Male" = "M",
                                "Other" = "O")),
    br(),
    splitLayout(textInput("email", "Email", ""),
                textInput("contact_no", "Contact numbers", "")),
    br(),
    selectInput("comm", "Preferred communication mode",
                choices = list("Email" = "email",
                               "SMS" = "sms",
                               "WhatsApp" = "whatsapp")),
    br(),
    verticalLayout(textInput("addressl1", "Address line 1", ""),
                   textInput("addressl1", "Address line 2", ""),
                   textInput("city", "City/Town", ""),
                   textInput("pcode", "Postal code", "", width = "20%")
    ),
    br(),
    selectInput("province", "Province",
                choices = list("Eastern Cape" = "ec",
                               "Free State" = "fs",
                               "Gauteng" = "gp",
                               "Kwa-Zulu Natal" = "kzn",
                               "Limpopo" = "lp",
                               "Mpumalang" = "mp",
                               "North West" = "nw",
                               "Northen Cape" = "nc",
                               "Western Cape" = "wc")),
    hr(),
    actionButton("add","Add customer"),
    DT::dataTableOutput("customertable")
    
    ) #fludiPage
  ) #dashboardBody
) #dashboardPage

server <- function(input,output){
  isValidEmail <- function(x) {
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
          ignore.case=TRUE)}
 
  
  vals <- reactiveValues()
  vals$df <- data.frame(Column1 = "Name", Colum2 = "Surname",
                        Column3 = "Gender", Column4 = "Email",
                        Column5 = "Contact no.", Column6 = "Preferred comm",
                        Column7 = "Address l1", Column8 = "Address l2",
                        Column9 = "City", Column10 = "Postal code",
                        Colum11 = "Province")
  observeEvent(input$add, {
    validate(
      need(input$name, 'Enter name'),
      need(input$sname, 'Enter surname'),
      need(input$gender, 'Please select gender'),
      need(input$email, 'Enter email'),
      need(input$contact_no, 'Please enter contact numbers'),
      need(input$pcomm, 'Please choose one'),
      need(input$addressl1, 'Please enter address'),
      need(input$addressl2, 'Please enter address'),
      need(input$city, 'Please enter city'),
      need(input$pcode, 'Please enter postal code'),
      need(input$province, 'Please select province')
    )
    newRow <- c(input$name, input$sname,
                input$gender, input$email,
                input$contact_no, input$comm,
                input$addressl1, input$addressl2,
                input$city, input$pcode,
                input$province)
    vals$df <- rbind(vals$df,newRow)
    
  })
  output$tbl <- renderDataTable({vals$df})
}

shinyApp(ui,server)

