library(plotly)
d<-read.csv("Gender.csv")
gend<-select(e,Type.Of.Crime,Age,Arrested.Female,Arrested.Male,Total)
p <-gend%>%plot_ly(
  x = gend$Type.Of.Crime,
  y = gend$Arrested.Female,
  title="Gender wise Crime",
  color = gend$Age, 
  frame = gend$Age,
  hoverinfo = "text",
  type = "bar",
  mode = "markers"
) %>%
  layout(
    title = "Gender wise crime",
    xaxis = list(title = "Crime"),
    yaxis = list(title = "Number of a crimes")
    
     ) 
    

p
p <- p %>% 
  animation_button(
    x = 1, xanchor = "left", y = 0, yanchor = "bottom"
 )



