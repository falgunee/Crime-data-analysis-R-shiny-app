
library(shinydashboard)
library(shiny)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(tidyr)
library(scales)
library(ggrepel)
library(readxl)
library(sp)
library(ggplot2)
library(ggmap)
library(maps)
library(rgdal)
library(scales)
library(maptools)
library(rgeos)
library(rgdal)

#-------------------------------------------------------------------

# Importing Excel Files
a = read.csv("byplacenew.csv")
b = read.csv("state_data.csv")
c = read.csv("OVERVIEW2.csv", header=TRUE, sep=",")
d = read_xlsx("mypie.xlsx")
e = read.csv("Gender.csv")


# Creating DataStruture for the plots
# For Pie Chart
mypie = data.frame(values = c(d$Oral_Complaints,d$Written_Complaints,d$Over_phone_call_100,d$sue_motto_byPolice))

#For GenderWise Crime Data
gend<-select(e,Type.Of.Crime,Age,Arrested.Female,Arrested.Male,Total)

#For map
state_sh = readOGR(dsn = "IND_adm1.shp")
draw.india<- fortify(state_sh)
indiamap1<-merge(x=draw.india,y=c,by="id",all.x = TRUE)


#Header Function
header = dashboardHeader(
  title = "Crime Data Of India",
  dropdownMenu(
    type = "messages",
    messageItem(from = "User1",
                message = "Get Dataset from Kaggle",
                href ="https://www.kaggle.com/rajanand/crime-in-india")
  )
)


#SideBar Menu
sidebar = dashboardSidebar(
  sidebarUserPanel("Aegis Student",image = "userIcon.png"),
  sidebarMenu(
    menuItem( "All Over Crime",tabName = "indCrime"),
    menuItem("State-Wise Crime",tabName = "statCrime"),
    menuItem("Gender-Wise Crime" , tabName="genCrime"),
    menuItem("Place-Wise Crime",tabName = "placeCrime"),
    menuItem("Nature of Complaint",tabName = "natCrime")
  )
)


#Body

body = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet",type = "text/css",href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "indCrime",h1("OverAll Crime in India"),plotOutput("graph2")
            ,br(),h2("Conclusion"),h4("In above map we are comparing different state crime and we can see that Maharastra State has the highest crime rate ")),
    
    tabItem(tabName = "statCrime",h1("State Wise Crime"),br(),
            box(selectInput(inputId = "state",label = "Select State",choices = c("Andhra Pradesh","Delhi UT",	"Haryana","Karnataka",	"Madhya Pradesh","Maharashtra",	"Odisha",	"Rajasthan",	"Uttar Pradesh",	"West Bengal"),selected = "Rajasthan")),
            box(conditionalPanel(
                  condition = "input.state == 'Andhra Pradesh'",
                  plotOutput(outputId = "AP")),
                conditionalPanel(
                  condition = "input.state == 'Delhi UT'",
                  plotOutput(outputId = "du")),
                conditionalPanel(
                  condition = "input.state == 'Haryana'",
                  plotOutput(outputId = "Hr")),
                conditionalPanel(
                  condition = "input.state == 'Karnataka'",
                  plotOutput(outputId = "Kr")),
                conditionalPanel(
                  condition = "input.state == 'Madhya Pradesh'",
                  plotOutput(outputId = "Md")),
                conditionalPanel(
                  condition = "input.state == 'Maharashtra'",
                  plotOutput(outputId = "MH")),
                conditionalPanel(
                  condition = "input.state == 'Odisha'",
                  plotOutput(outputId = "Od")),
                conditionalPanel(
                  condition = "input.state == 'Rajasthan'",
                  plotOutput(outputId = "Rj")),
                conditionalPanel(
                  condition = "input.state == 'Uttar Pradesh'",
                  plotOutput(outputId = "Up")),
                conditionalPanel(
                  condition = "input.state == 'West Bengal'",
                  plotOutput(outputId = "Wb"))
                          ),h3("Conclusion"),h4("From above graph we can summarize different types of crimes in a state")),
    
    tabItem(tabName = "genCrime", h1("Gender Crime"),h3("Total Male Arrested for Various Crimes"),plotOutput("genM"),br(),h3("Total Female Arrested for Various Crimes"),plotOutput("genF"),br(),h2("Conclusion"),h4("We can summarize from the obove graph different types of crime committed by Males and Females of Different Age")),
    
    tabItem(tabName = "placeCrime",h1("Area Wise Crime"),plotOutput("bar_graph"),br(),plotOutput("bar_graph1"),br(),plotOutput("bar_graph2"),br(),plotOutput("bar_graph3")),
    
    tabItem(tabName = "natCrime",h1("Nature of Call by Victims"),plotOutput("pie_chart1")
            ,br(),h2("Conclusion"),h4("The above is simple pie chart showing how the complaints where made in Delhi in 2018.Where we can observe that phone calls on 100 was most preferred,hence we can say people use more of helpline number which is most handy thing one can do.!"))
  ))

#Creating header,sidebar,body
header = header
sidebar = sidebar
body = body


#Creating ui & calling the dashboard
ui = dashboardPage(header = header,sidebar = sidebar,body = body)
server = function(input,output){
  output$pie_chart1 <- renderPlot({
   pie(mypie$values,labels =c("Oral Complaints","Written Complaints","Over Phone call 100","sue_by_motto_police"),main = "Nature of Complaints from Delhi 2018")
  })
  
  output$graph2 <- renderPlot({
    ggplot() +geom_polygon(data =indiamap1,aes(x = long, y = lat, group = group, fill = Total_Crime ),color = "white", size = 0.5) +coord_map()+labs(title="Crime in India - Distribution by State")+scale_fill_distiller(name="State" , breaks = pretty_breaks(n = 6))
    
    })
  
  
  output$bar_graph <-renderPlot({
    ggplot(a,aes(crime,Residence,fill=crime))+geom_bar(stat="identity")+ggtitle("Crimes committed in Residencial Areas")})
    output$bar_graph1 <-renderPlot({
    ggplot(a,aes(crime,Highways,fill=crime))+geom_bar(stat="identity")+ggtitle("Crimes committed in Highways")})
    output$bar_graph2 <-renderPlot({
    ggplot(a,aes(crime,RiverOrSea,fill=crime))+geom_bar(stat="identity")+ggtitle("Crimes committed in RiverOrSea")})
    output$bar_graph3<-renderPlot({
    ggplot(a,aes(crime,Railways,fill=crime))+geom_bar(stat="identity")+ggtitle("Crimes committed in Railways")
  })
  
  output$genM = renderPlot({
   g = ggplot(e,aes(x = e$Type.Of.Crime,y = e$Arrested.Male,fill = e$Age)) 
   g + geom_bar(stat = "identity")  + xlab("Types of Crime") +ylab("Male Arrested for Crime")
   
   
 })
  output$genF = renderPlot({
    g = ggplot(e,aes(x = e$Type.Of.Crime,y = e$Arrested.Female,fill = e$Age, position = "fill")) 
    g + geom_bar(stat = "identity") + xlab("Types of Crime") +ylab("Female Arrested for Crime")
    })
  
  output$AP = renderPlot({
    req(input$state)
    g <- ggplot(b, aes(x=Crimes,y=Andhra.Pradesh,color = I("Green"),fill = I("Blue")))
    g + geom_bar(stat = "identity")})
    
  output$du = renderPlot({
      req(input$state)
      g <- ggplot(b, aes(x=Crimes,y=Delhi.UT,color = I("Green"),fill = I("Blue")))
      g + geom_bar(stat = "identity")})
  
      output$Hr = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Haryana,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Kr = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Karnataka,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
        
      output$Md = renderPlot({
          req(input$state)
          g <- ggplot(b, aes(x=Crimes,y=Madhya.Pradesh,color = I("Green"),fill = I("Blue")))
          g + geom_bar(stat = "identity")})
      output$MH = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Maharashtra,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Od= renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Odisha,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Md = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Madhya.Pradesh,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Rj = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Rajasthan,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Up = renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=Uttar.Pradesh,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
      
      output$Wb= renderPlot({
        req(input$state)
        g <- ggplot(b, aes(x=Crimes,y=West.Bengal,color = I("Green"),fill = I("Blue")))
        g + geom_bar(stat = "identity")})
          
          
      
  
    

  
  }
  

shiny::shinyApp(ui,server)






