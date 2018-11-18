library(shiny)
library(shinyjs)



# Get table metadata. For now, just the fields
# Further development: also define field types
# and create inputs generically
GetTableMetadata <- function() {
  fields <- c(name = "Name", 
              sname = "Surname",
              gender = "Gender",  
              email = "Email",
              contact_no = "Contact_no", 
              comm = "Preferred_comm",
              addressl1 = "Addressl1", 
              addressl2 = "Addressl2",
              city = "City", 
              pcode = "Postal_code", 
              province = "Province", 
              date = "Date")
  
  result <- list(fields = fields)
  return (result)
}

# Find the next ID of a new record
# (in mysql, this could be done by an incremental index)
#GetNextId <- function() {
#  if (exists("custormers") && nrow(custormers) > 0) {
#    max(as.integer(row.names(custormers))) + 1
#  } else {
#    return (1)
#  }
#}

#C
CreateData <- function(data) {
  
  data <- CastData(data)
  #rownames(data) <- NULL
  if (exists("custormers")) {
    custormers <<- rbind(custormers, data)
  } else {
    custormers <<- data
  }
}

#R
ReadData <- function() {
  if (exists("custormers")) {
    custormers
  }
}



#U
UpdateData <- function(data) {
  data <- CastData(data)
  custormers[row.names(custormers) == rownames(data), ] <<- data
}

#D
#DeleteData <- function(data) {
#  custormers <<- custormers[row.names(custormers) != unname(rownames(data)), ]
#}




# Cast from Inputs to a one-row data.frame
CastData <- function(data) {
  datar <- data.frame(name = data["name"], 
                      sname = data["sname"],
                      gender = data["gender"],  
                      email = data["email"],
                      contact_no = data["contact_no"], 
                      comm = data["comm"],
                      addressl1 = data["addressl1"], 
                      addressl2 = data["addressl2"],
                      city = data["city"], 
                      pcode = data["pcode"], 
                      province = data["province"], 
                      date = as.character(data["date"]))
  
  #rownames(datar) <- NULL
  return (datar)
}




# Return an empty, new record
#CreateDefaultRecord <- function() {
#  mydefault <- CastData(list(id = "0", 
#                             name = "", 
#                             sname = "",
#                             gender = "F",  
#                             email = "",
#                             contact_no = "", 
#                             comm = "sms",
#                             addressl1 = "", 
#                             addressl2 = "",
#                             city = "", 
#                             pcode = "0", 
#                             province = "", 
#                             date = ""))
#  return (mydefault)
#}

# Fill the input fields with the values of the selected record in the table
#UpdateInputs <- function(data, session) {
#  updateTextInput(session, "id", value = unname(rownames(data)))
#  updateTextInput(session, "name", value = unname(data["name"]))
#  updateCheckboxInput(session, "used_shiny", value = as.logical(data["used_shiny"]))
#  updateSliderInput(session, "r_num_years", value = as.integer(data["r_num_years"]))
#}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)}

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}



ui <- fluidPage(
  #use shiny js to disable the ID field
  shinyjs::useShinyjs(),
  
  #data table
  DT::dataTableOutput("custormers", width = 300), 
  
  tags$h1("Customer Registration"),
  splitLayout(textInput("name",labelMandatory("Name"),""),
              textInput("sname",labelMandatory("Surname"), "")),
  tags$br(),
  radioButtons("gender",labelMandatory("Gender"),
               choices = list("Female" = "F",
                              "Male" = "M",
                              "Other" = "O")),
  tags$br(),
  splitLayout(textInput("email", labelMandatory("Email"), ""),
  textInput("contact_no", labelMandatory("Contact numbers"), "")),
  tags$br(),
  selectInput("comm", "Preferred communication mode",
              choices = list("Email" = "email",
                             "SMS" = "sms",
                             "WhatsApp" = "whatsapp")),
  tags$br(),
  verticalLayout(textInput("addressl1", labelMandatory("Address line 1"), ""),
                 textInput("addressl2", "Address line 2", ""),
                 textInput("city", labelMandatory("City/Town"), ""),
                 textInput("pcode", labelMandatory("Postal code"), "", width = "20%")),
  tags$br(),
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
  tags$br(),
  dateInput("date", labelMandatory("Date"), value = "2018-01-01"),
  tags$hr(),
  
  #action buttons
  actionButton("submit", "Submit"),
  #actionButton("new", "New"),
  actionButton("delete", "Delete")
)


server <- function(input, output, session) {
  
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
  })
  
  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    validate(
      need(input$name!='','Please enter name'),
      need(input$sname!='','Please enter surname'),
      need(isValidEmail(input$email),'Please enter valid email'),
      need(nchar(as.numeric(input$contact_no)) != 10, 'Please enter valid contact numbers'),
      need(input$addressl1!='', 'Please enter address'),
      need(input$city!='', 'Please enter city'),
      need(nchar(as.numeric(input$pcode)) != 4, 'Please enter valid postal code')
    )
    CreateData(formData())
    UpdateData(formData())
    #UpdateInputs(CreateDefaultRecord(), session)

  }, priority = 1)
  
  # Press "New" button -> display empty record
  #observeEvent(input$new, {
  #  UpdateInputs(CreateDefaultRecord(), session)
  #})
  
  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    if (length(input$custormers_rows_selected) > 0) {
      #data <- ReadData()[as.numeric(input$custormers_rows_selected)]
      #custormers = custormers[-as.numeric(input$custormers_rows_selected)]
      }
    
    #UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)
  
  # Select row in table -> show details in inputs
  #observeEvent(input$custormers_rows_selected, {
  #  if (length(input$custormers_rows_selected) > 0) {
  #    data <- ReadData()[input$custormers_rows_selected, ]
  #    #UpdateInputs(data, session)
  #  }
  #  
  #})
  
  # display table
  output$custormers <- DT::renderDataTable({
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    ReadData()
  }, server = FALSE, selection = "single"
  #colnames = unname(GetTableMetadata()$fields)[-1]
  )     
  
  
  
}


# Shiny app with 3 fields that the user can submit data for
shinyApp(ui = ui, server = server)