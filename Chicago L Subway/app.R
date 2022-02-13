#
# load libraries
library(lubridate)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(DT)


###########################################################################

# # Load data files 


#
# to load "UIC-Halsted"
uicHalsted<-read.table("UIC-Halsted.csv", sep = "\t", header = TRUE, quote = "\"")

# convert string to Date type
uicHalsted$date<- as.Date(uicHalsted$date, format="%Y-%m-%d")


#
# load "O'Hare" 
oHare<-read.table("oHare.csv", sep = "\t", header = TRUE, quote = "\"")

# convert string to Date type
oHare$date<- as.Date(oHare$date, format="%Y-%m-%d")


#########################################################################

# # Variables

years<-c(2001:2021)
stations<-c("UIC-Halsted", "O'Hare")
options<-c("Yearly", "Daily", "Monthly", "Day of Week")

#####################################################################


ui <- dashboardPage(
  dashboardHeader(title="Chicago L Subway", titleWidth = 200),
  dashboardSidebar(
    
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    
    selectInput("station1", "Select station", stations, selected = "UIC-Halsted"),
    
    selectInput("yearToPlot1", "Select the year", years, selected = 2021),
    
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    
    selectInput("station2", "Select station", stations, selected = "O'Hare"),
    
    selectInput("yearToPlot2", "Select the year", years, selected = 2021)
    
  ),
  
  dashboardBody(
    
    fluidRow(
      column(width = 3,
          
            selectInput("sta1_g1", "Plot", options, selected = "Yearly", width = 100),
             
            box(width = NULL,
               plotOutput("s1_p1", height = 200)
            ),
            
            box(dataTableOutput("s1_t1", height=200), width=100)
      ),
      column(width = 3,
             
            selectInput("sta1_g2", "Plot", options, selected = "Daily", width = 100),
            
            box(width = NULL,
                plotOutput("s1_p2", height = 200)
            ),
            
            box(dataTableOutput("s1_t2", height=200), width=100)
      ),
      column(width = 3,
             
            selectInput("sta1_g3", "Plot", options, selected = "Monthly", width = 100),
            
            box(width=NULL,
                plotOutput("s1_p3", height = 200)
            ),
            
            box(dataTableOutput("s1_t3", height=200), width=100)
      ),
      column(width = 3,

             selectInput("sta1_g4", "Plot", options, selected = "Monthly", width = 100),

             box(width=NULL,
                 plotOutput("s1_p4", height = 200)
             ),

             box(dataTableOutput("s1_t4", height=200), width=100)
      )
    ),
    
    fluidRow(
      column(width = 3,
             
             selectInput("sta2_g1", "Plot", options, selected = "Yearly", width = 100),
             
             box(width = NULL,
                 plotOutput("s2_p1", height = 200)
             ),
             
             box(dataTableOutput("s2_t1", height=200), width=100)
      ),
      column(width = 3,
             
             selectInput("sta2_g2", "Plot", options, selected = "Daily", width = 100),
             
             box(width = NULL,
                 plotOutput("s2_p2", height = 200)
             ),
             
             box(dataTableOutput("s2_t2", height=200), width=100)
      ),
      column(width = 3,
             
             selectInput("sta2_g3", "Plot", options, selected = "Monthly", width = 100),
             
             box(width=NULL,
                 plotOutput("s2_p3", height = 200)
             ),
             
             box(dataTableOutput("s2_t3", height=200), width=100)
      ),
      column(width = 3,
             
             selectInput("sta2_g4", "Plot", options, selected = "Monthly", width = 100),
             
             box(width=NULL,
                 plotOutput("s2_p4", height = 200)
             ),
             
             box(dataTableOutput("s2_t4", height=200), width=100)
      )
    )
  )
)



server <- function(input, output) { 


  # # aggregate ridership by year
  uicHalstedYearly<-aggregate(uicHalsted$rides, list(uicHalsted$year), FUN=sum)
  colnames(uicHalstedYearly)<- c("year", "totalRides")
  
  oHareYearly<-aggregate(oHare$rides, list(oHare$year), FUN=sum)
  colnames(oHareYearly)<- c("year", "totalRides")
  
  
  # bar plots of YEARLY ridership
  uicByYear<-ggplot(uicHalstedYearly, aes(x=year, y=totalRides))+
             geom_bar(stat="identity", width=0.8, fill="indianred2")+
             labs(x="Year", y="Riders", title="Yearly Ridership at UIC-Halsted station")
  
  oHareByYear<-ggplot(oHareYearly, aes(x=year, y=totalRides))+
               geom_bar(stat="identity", width=0.8, fill="skyblue2")+
               labs(x="Year", y="Riders", title="Yearly Ridership at O'Hare station")
  
  
##############################################################################
  
  
  output$s1_p1 <- renderPlot({
    
    if (input$station1 == "UIC-Halsted")
    {
      uicByYear
    }
    else if (input$station1 == "O'Hare")
    {
      oHareByYear
    }
  })
  
  
  output$s1_t1 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station1 == "UIC-Halsted")
      {
        t<-uicHalstedYearly
      }
      else if (input$station1 == "O'Hare")
      {
        t<-oHareYearly
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s1_p2 <- renderPlot({
    
    ### Plot daily ridership
    if (input$station1 == "UIC-Halsted") {

      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot1,]
      
      ggplot(uicHalstedYear, aes(x=date, y=rides))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_date(date_labels = "%b")+
        labs(x='Date', y='Riders', title='Daily Riders of UIC-Halsted Station')
    }
    else if (input$station1 == "O'Hare") {

      oHareYear<-oHare[oHare$year == input$yearToPlot1, ]
      
      ggplot(oHareYear, aes(x=date, y=rides))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_date(date_labels = "%b")+
        labs(x='Date', y='Riders', title="Daily Riders of O'Hare Station")
    }
  })
  
  
  output$s1_t2 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station1 == "UIC-Halsted")
      {
        t<-uicHalsted[uicHalsted$year == input$yearToPlot1, c("date", "rides")]
      }
      else if (input$station1 == "O'Hare")
      {
        t<-oHare[oHare$year == input$yearToPlot1, c("date", "rides")]
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s1_p3 <- renderPlot({
    
    ### plot monthly ridership
    if (input$station1 == "UIC-Halsted") {
      
      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot1,]
      uicHalstedMonth<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$month), FUN=sum)
      colnames(uicHalstedMonth)<- c("month", "totalRides")
      
      ggplot(uicHalstedMonth, aes(x=month, y=totalRides))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))+
        labs(x="Month", y="Riders", title="Monthly Ridership at UIC-Halsted station")
    }
    else if (input$station1 == "O'Hare") {
      
      oHareYear<-oHare[oHare$year == input$yearToPlot1, ]
      oHareMonth<-aggregate(oHareYear$rides, list(oHareYear$month), FUN=sum)
      colnames(oHareMonth) <- c("month", "totalRides")
      
      ggplot(oHareMonth, aes(x=month, y=totalRides))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))+
        labs(x="Month", y="Riders", title="Monthly Ridership at O'Hare station")
      
    }
  })
  
  
  output$s1_t3 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station1 == "UIC-Halsted")
      {
        uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot1,]
        t<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$month), FUN=sum)
        colnames(t)<- c("month", "totalRides")
      }
      else if (input$station1 == "O'Hare")
      {
        oHareYear<-oHare[oHare$year == input$yearToPlot1, ]
        t<-aggregate(oHareYear$rides, list(oHareYear$month), FUN=sum)
        colnames(t) <- c("month", "totalRides")
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s1_p4 <- renderPlot({
    
    ### plot by day of the week
    if (input$station1 == "UIC-Halsted") {
      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot1,]
      uicHalstedDay<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$day), FUN=sum)
      colnames(uicHalstedDay) <- c("day", "totalRiders")
      uicHalstedDay<-uicHalstedDay[order(uicHalstedDay$day),]
      
      ggplot(uicHalstedDay, aes(x=day, y=totalRiders))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))+
        labs(x="Day of the Week", y="Total Riders", title="Total ridership by day of the week")
    }
    else if (input$station1 == "O'Hare") {
      oHareYear<-oHare[oHare$year == input$yearToPlot1, ]
      oHareDay<-aggregate(oHareYear$rides, list(oHareYear$day), FUN=sum)
      colnames(oHareDay) <- c("day", "totalRiders")
      oHareDay<-oHareDay[order(oHareDay$day),]
      
      ggplot(oHareDay, aes(x=day, y=totalRiders))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))+
        labs(x="Day of the Week", y="Total Riders", title="Total ridership by day of the week")
    }
  })
  
  
  output$s1_t4 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station1 == "UIC-Halsted")
      {
        uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot1,]
        t<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$day), FUN=sum)
        colnames(t) <- c("day", "totalRiders")
        t<-t[order(t$day),]
      }
      else if (input$station1 == "O'Hare")
      {
        oHareYear<-oHare[oHare$year == input$yearToPlot1, ]
        t<-aggregate(oHareYear$rides, list(oHareYear$day), FUN=sum)
        colnames(t) <- c("day", "totalRiders")
        t<-t[order(t$day),]
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s2_p1 <- renderPlot({
    
    if (input$station2 == "UIC-Halsted")
    {
      uicByYear
    }
    else if (input$station2 == "O'Hare")
    {
      oHareByYear
    }
  })
  
  
  output$s2_t1 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station2 == "UIC-Halsted")
      {
        t<-uicHalstedYearly
      }
      else if (input$station2 == "O'Hare")
      {
        t<-oHareYearly
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s2_p2 <- renderPlot({
    
    ### Plot daily ridership
    if (input$station2 == "UIC-Halsted") {
      
      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot2,]
      
      ggplot(uicHalstedYear, aes(x=date, y=rides))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_date(date_labels = "%b")+
        labs(x='Date', y='Riders', title='Daily Riders of UIC-Halsted Station')
    }
    else if (input$station2 == "O'Hare") {
      
      oHareYear<-oHare[oHare$year == input$yearToPlot2, ]
      
      ggplot(oHareYear, aes(x=date, y=rides))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_date(date_labels = "%b")+
        labs(x='Date', y='Riders', title="Daily Riders of O'Hare Station")
    }
  })
  
  
  output$s2_t2 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station2 == "UIC-Halsted")
      {
        t<-uicHalsted[uicHalsted$year == input$yearToPlot2, c("date", "rides")]
      }
      else if (input$station2 == "O'Hare")
      {
        t<-oHare[oHare$year == input$yearToPlot2, c("date", "rides")]
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s2_p3 <- renderPlot({
    
    ### plot monthly ridership
    if (input$station2 == "UIC-Halsted") {

      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot2,]
      uicHalstedMonth<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$month), FUN=sum)
      colnames(uicHalstedMonth)<- c("month", "totalRides")

      ggplot(uicHalstedMonth, aes(x=month, y=totalRides))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))+
        labs(x="Month", y="Riders", title="Monthly Ridership at UIC-Halsted station")
    }
    else if (input$station2 == "O'Hare") {

      oHareYear<-oHare[oHare$year == input$yearToPlot2, ]
      oHareMonth<-aggregate(oHareYear$rides, list(oHareYear$month), FUN=sum)
      colnames(oHareMonth) <- c("month", "totalRides")

      ggplot(oHareMonth, aes(x=month, y=totalRides))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_discrete(limits=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec"))+
        labs(x="Month", y="Riders", title="Monthly Ridership at O'Hare station")
    }
  })
  
  
  output$s2_t3 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station2 == "UIC-Halsted")
      {
        uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot2,]
        t<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$month), FUN=sum)
        colnames(t)<- c("month", "totalRides")
      }
      else if (input$station2 == "O'Hare")
      {
        oHareYear<-oHare[oHare$year == input$yearToPlot2, ]
        t<-aggregate(oHareYear$rides, list(oHareYear$month), FUN=sum)
        colnames(t) <- c("month", "totalRides")
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
  
  output$s2_p4 <- renderPlot({

    ### plot by day of the week
    if (input$station2 == "UIC-Halsted") {
      uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot2,]
      uicHalstedDay<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$day), FUN=sum)
      colnames(uicHalstedDay) <- c("day", "totalRiders")
      uicHalstedDay<-uicHalstedDay[order(uicHalstedDay$day),]
  
      ggplot(uicHalstedDay, aes(x=day, y=totalRiders))+
        geom_bar(stat="identity", fill="indianred2")+
        scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))+
        labs(x="Day of the Week", y="Total Riders", title="Total ridership by day of the week")
    }
    else if (input$station2 == "O'Hare") {
      oHareYear<-oHare[oHare$year == input$yearToPlot2, ]
      oHareDay<-aggregate(oHareYear$rides, list(oHareYear$day), FUN=sum)
      colnames(oHareDay) <- c("day", "totalRiders")
      oHareDay<-oHareDay[order(oHareDay$day),]
      
      ggplot(oHareDay, aes(x=day, y=totalRiders))+
        geom_bar(stat="identity", fill="skyblue2")+
        scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))+
        labs(x="Day of the Week", y="Total Riders", title="Total ridership by day of the week")
    }
  })
  
  output$s2_t4 <- DT::renderDataTable(
    DT::datatable({ 
      
      if (input$station2 == "UIC-Halsted")
      {
        uicHalstedYear<-uicHalsted[uicHalsted$year == input$yearToPlot2,]
        t<-aggregate(uicHalstedYear$rides, list(uicHalstedYear$day), FUN=sum)
        colnames(t) <- c("day", "totalRiders")
        t<-t[order(t$day),]
      }
      else if (input$station2 == "O'Hare")
      {
        oHareYear<-oHare[oHare$year == input$yearToPlot2, ]
        t<-aggregate(oHareYear$rides, list(oHareYear$day), FUN=sum)
        colnames(t) <- c("day", "totalRiders")
        t<-t[order(t$day),]
        print(t)
      }
      
      t<-as.data.frame(t)
    }, 
    options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc'))
    ), rownames = FALSE 
    )
  )
  
}

shinyApp(ui, server)



