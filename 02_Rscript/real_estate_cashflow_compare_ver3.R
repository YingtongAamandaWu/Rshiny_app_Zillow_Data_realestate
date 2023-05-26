## ----load libraries-------------------------------------------------------------------------------------------------------
library("rsconnect")  # For publishing apps online
# library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(shinythemes)


## ----read path and load data------------------------------------------------------------------------
homevalue.df<-read.csv("Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")
rental.df<-read.csv("Zip_zori_sm_month.csv")
# make sure to put the data file to the same folder as this R script when deploying R shiny 
# You can always check getwd(), and see if that is the same folder where you put the CSV files

## ----UI-------------------------------------------------------------------------------------------------------------------
# UI interface
ui <- 
  fluidPage(
    titlePanel("Calculate real estate cashflow performance compared in the context of each metro area (Zillow data, March 2023)"),
    sidebarLayout(
      position = "left",
      sidebarPanel(h3("Inputs"), 
                   selectInput("Metro_area_name", "1. Select metro area", 
                               choices = unique(homevalue.df$Metro)[order(unique(homevalue.df$Metro))], 
                               selected = "San Francisco-Oakland-Berkeley, CA"),
                   numericInput("Home_price", "2. Enter your purchase price ($)", value = 500000),
                   sliderInput(inputId = "Percent_downpayment",   label = "3. your down payment percentage (%)", 
      min=0, max=100, value= c(0.5)),
                   numericInput("Closing_cost", "4. Enter your closing cost ($)", value = 15000),
                   numericInput("Rent", "5. Enter your monthly rent amount generated from your propoerty ($)", value = 3500),             
      numericInput("Loan_payment", "6. Enter your monthly loan payment ($)", value = 2500),              
      numericInput("Taxes", "7. Enter your annual property tax ($)", value = 5000),
      numericInput("Insurance", "8. Enter your annual insurance ($)", value = 1000),
      numericInput("HOA", "9. Enter your monthly HOA fee ($)", value = 0),
      numericInput("Vacancy_cost", "10. Enter your annual vacancy cost ($)", value = 0),
      numericInput("Management_fee", "11. Enter your annual management fee ($)", value = 0),
      numericInput("Capital_expenditure", "12. Enter your annual captial expenditure ($)", value = 0),     
      numericInput("Utilities", "13. Enter your monthly utilities ($)", value = 0),     
      actionButton("action", "Submmit", class = "btn-success")
                   ),
                   mainPanel(
                             plotOutput("house_value.metro.hist"),
                             plotOutput("monthly_rental.metro.hist"),
                             tableOutput("summarytable")
                            # textOutput("Rent_to_price.metro"),
                             # textOutput("Rent_to_price.yourproperty")
                             )
      )
    )



## ----server---------------------------------------------------------------------------------------------------------------
server <- function(input, output,session) {

  # reactive environment for the data frames 
  homevalue.df.ra <- reactive({
    filter(homevalue.df,
           Metro == input$Metro_area_name)
  })
  
  rental.df.ra <- reactive({
    filter(rental.df,
           Metro == input$Metro_area_name)
  })
  
  # house value histogram
output$house_value.metro.hist <- renderPlot(ggplot(homevalue.df.ra(), aes(x = X2023.03.31)) + 
	geom_histogram(bins = 100, fill="pink", colour = "black") +
	  xlab("Home values of selected metro area ($)")+ylab("Counts")+ 
	  geom_vline(xintercept = median(homevalue.df.ra()$X2023.03.31), linetype="dashed",  # market value
                color = "coral2", linewidth=1.5)+
	  geom_vline(xintercept = input$Home_price, linetype="solid",  # your value
                color = "black", linewidth=1.5)+
	  ggtitle("Home values of selected metro area ($)", subtitle = "Market's median value = Dashed line. Your value = solide line")+theme_bw()+ 
    theme(plot.title = element_text(size = 20, face = "bold"))+
	  scale_x_continuous(n.breaks = 20,labels = function(x) format(x, scientific = FALSE))
	)

# monthly rent histogram
output$monthly_rental.metro.hist <- renderPlot(ggplot(rental.df.ra(), aes(x = X2023.03.31)) + 
	geom_histogram(bins = 100,  fill="aquamarine", colour = "black")+ 
	  geom_vline(xintercept = median(rental.df.ra()$X2023.03.31), linetype="dashed",   # market value
                color = "darkgreen", linewidth=1.5)+
	  geom_vline(xintercept = input$Rent, linetype="solid",  # your value
                color = "black", linewidth=1.5)+
  xlab("Monthly rental of selected metro area ($)")+ylab("Counts")+
  ggtitle("Monthly rental of selected metro area ($)", subtitle = "Market's median value = Dashed line. Your value = solide line")+theme_bw()+ 
    theme(plot.title = element_text(size = 20, face = "bold"))+
	  scale_x_continuous(n.breaks = 20,labels = function(x) format(x, scientific = FALSE))
	)

# metro area median rent to price ratio
 # output$Rent_to_price.metro<-renderText({paste ("Rent to price ratio of selected metro area:", round(median(rental.df.ra()$X2023.03.31,na.rm = TRUE)/median(homevalue.df.ra()$X2023.03.31,na.rm = TRUE)*100,2), "%")  })

# Your property rent to price ratio
#  output$Rent_to_price.yourproperty<- renderText({paste ("Rent to price ratio of your property:", round(input$Rent*100/input$Home_price, 2), "%")    })

# Calculate: your total fixed cost 
  fixedcost<-reactive({input$Home_price*input$Percent_downpayment/100+input$Closing_cost
    }) 

# Calculate: your annual cash flow
  net.annual.cashflow<-reactive({input$Rent*12-input$Loan_payment*12-input$Taxes-input$Insurance-input$HOA*12-input$Vacancy_cost-input$Management_fee-input$Capital_expenditure-input$Utilities*12
    }) 

# Calculate: your monthly cash flow
  net.monthly.cashflow<-reactive({net.annual.cashflow()/12})

  # Calculate: your monthly cost
  monthly.expense<-reactive({(input$Loan_payment*12+input$Taxes+input$Insurance+input$HOA*12+input$Vacancy_cost+input$Management_fee+input$Capital_expenditure+input$Utilities*12)/12})
  
# Calculate: your cash to cash return 
  coc.return <-reactive ({net.annual.cashflow()/fixedcost()*100})
  
# calculate: Your property rent to price ratio
 Your.property.rent.to.price.ratio <- reactive({round(input$Rent/input$Home_price*100, 2)})

# calculate: Your property rent to price ratio
   
# calculate: Market rent to price ratio
   Rent.to.price.ratio.of.selected.metro.area <-reactive({
   round(median(rental.df.ra()$X2023.03.31,na.rm = TRUE)/median(homevalue.df.ra()$X2023.03.31,na.rm = TRUE)*100,2)
    }) 
  
vec<-reactive({
  c(Rent.to.price.ratio.of.selected.metro.area(),Your.property.rent.to.price.ratio(),coc.return(),fixedcost(), net.annual.cashflow(),  net.monthly.cashflow(),monthly.expense())
      })
   
   sumtab<-reactive({
     sumtable<-data.frame( Matrices = c("Selected market rent-to-price ratio (%)",
                  "Your property rent-to-price ratio (%)",
                  "Your property cash-on-cash return (%)",
                  "Your fixed cost (upfront cash payment $)",
                  "Your annual cashflow ($)",
                  "Your monthly cashflow ($)",
                  "Your monthly expense ($)"), 
                  Number = vec())
     
   })
     
  output$summarytable <- renderTable({sumtab()}, 
                                     spacing = c( "m"),
                                     triped = TRUE, hover = TRUE, bordered = TRUE,
                                     rownames = FALSE, autowidth = TRUE,        
                                     columnDefs = list(list(width = '70%', targets = 1)))
  
}



## ----run Shiny------------------------------------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
# https://ytamandawu.shinyapps.io/Real_estate_cashflow_metroZillow2023March/
