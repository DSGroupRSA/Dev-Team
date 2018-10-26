## app.R ##

library(shiny)
library(shinydashboard)
library(shinyjs)

labelMandatory <- function(label) {
    tagList(
        label,
        span('*', class = 'mandatory_star')
    )
}

isValidEmail <- function(x) {
    grepl('\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>', as.character(x),
    ignore.case=TRUE)
}

appCSS <-
    '.mandatory_star { color: red; }'


ui <- dashboardPage(
        skin = 'blue',
    dashboardHeader(
        title = 'Data Analytics'
    ),
    dashboardSidebar(
        sidebarSearchForm(
            textId = 'searchText',
            buttonId = 'searchButton',
            label = 'Search ...'
        ),
        sidebarMenu(
            menuItem('Registration', tabName = 'registration'),
            menuItem('Employee charter', tabName = 'employees'),
            menuItem('Clients/Customers', tabName = 'clients'),
            menuItem('Ads', tabName = 'ads'),
            menuItem('Sales', tabName = 'sales', badgeLabel = 'new', badgeColor = 'green'),
            menuItem('Suppliers', tabName = 'suppliers')
        )
    ),
    dashboardBody(

        tags$head(
            tags$style(
                HTML('
                    .shiny-output-error-validation {
                        color: red;
                    }'
                )
            )
        ),

        useShinyjs(),

        inlineCSS(appCSS),
        #<h2>Registration form </h2>
        tags$h2('Registration form'),
        tags$p('All fields marked (*) are mandatory.'),

        tags$div(
            id = 'form',
            textInput('firstname', labelMandatory('Name'), value = 'pp'),
            textInput('lastname', labelMandatory('Surname')),
            radioButtons('radio', label = 'Gender', choices = list('Female' = 'Female', 'Male' = 'Male', 'Other' = 'Other'), selected = 'Female'),
            textInput('email', labelMandatory('Email'), value = 'example@gmail.com'),
            textInput('contact_number', labelMandatory('Contact Number')),
            selectInput('', 'Preferred Communication:', c('Email', 'SMS','WhatsApp')),
            textInput('address', labelMandatory('Address '), value = '1234 Main St'),
            textInput(inputId = 'addresstwo', label = 'Address 2', value = 'Apartment, studio, or floor'),
            textInput('city', labelMandatory('City'), value = 'City'),
            selectInput('province', 'Province', c('', 'Free State','Limpopo', 'Gauteng', 'KwaZulu-Natal', 'Eastern Cape', 'Western Cape', 'Mpumalanga', 'North West', 'Northern Cape')),
            textInput(inputId = 'postal_code', labelMandatory('Postal Code'), value = '4-digit'),
            dateInput('', 'date', value = NULL, min = NULL, max = NULL, format = 'yyyy-mm-dd', startview = 'month', weekstart = 0, language = 'en'),
            br(),
            actionButton('addCustomer', 'AddCustomer', class = 'btn-primary'),

            tableOutput('table')
        )
    )
)

server <- function(input, output, session) {

    observeEvent( input$addCustomer,{
        data <- reactive({
            validate(
                need(input$firstname != '', 'Please enter your name'),
                need(input$lastname != '', 'Please enter your surname'),
                need(isValidEmail(input$email), 'Please enter valid email address'),
                need(nchar(as.numeric(input$contact_number)) == 10, 'Please enter valid contact number'),
                need(input$address != '', 'Please enter your address'),
                need(input$city != '', 'Please enter your city'),
                need(input$province != '', 'Please select your province'),
                need(nchar(as.numeric(input$postal_code)) == 4, 'Please enter valid 4-digit number')
                )
                get(input$firstname, input$lastname, input$email, input$contact_number, input$address, input$city, input$province, input$postal_code, 'package:datasets')
        })

        output$table <- renderTable({
    
            head(data())
        })
    })
}
shinyApp(ui, server)
