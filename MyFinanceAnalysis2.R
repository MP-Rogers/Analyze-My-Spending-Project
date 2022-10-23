
CleanData<-function(dataset){
d<-dataset
d<-d[,c(1:4)]
d$Date<-as.Date(d$Date)
d$Category<-as.factor(d$Category)
cat.labels<-c("Investment", "Mandatories", "Flexible")
levels(d$Category)<-cat.labels
#View(d)
full<-complete.cases(d)
d<-d[full,]
return(d)
}

Analyze.and.Visualize<-function(dataset){
  d<-dataset
  
  # number of transactions
  d2<-d %>% group_by(Category) %>% summarise(Amount) %>% tally()
  graph1<-ggplot(d2, mapping=aes(x = Category, y = n,fill = Category,text= paste("# Transactions", n)))+
    geom_bar(stat="identity")+
    ggtitle("Number of Transactions of Each Category")+
    labs(x="Category", y="Number of Transactions")+
    theme(plot.title = element_text(hjust = 0.5))
  graph1.interact<-ggplotly(graph1, tooltip = c("x", "text"))
  print(graph1.interact)
  
  #Volume of transactions
  d2<-d %>% group_by(Category) %>% summarise(Amount = sum(Amount))
  graph2<-ggplot(d2, mapping=aes(x=Category, y=Amount, fill=Category))+
    geom_bar(stat="identity")+
    ggtitle("Total Spending by Category")+
    labs(y="Spending in JMD", x="Category")+
    theme(plot.title = element_text(hjust= 0.5))
  graph2.interactive<-ggplotly(graph2, tooltip = c("x", "y"))
  print(graph2.interactive)
  
  #Spending month by month
  d2<-d %>% group_by(month=(floor_date(Date, "month")), Category) %>% summarise(Amount = sum(Amount))
  glimpse(d2)
  #print("this works 1")
  
  graph3<-ggplot(d2, mapping = aes(x = month, y = Amount, fill = Category))+
    geom_bar(stat = "identity")+
    labs(title = "Spending Each Month", y = "spending in JMD", x = "month")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(axis.text.x = element_text(angle = 35))+
    geom_hline(yintercept = 28000, linetype = "dashed")
  graph3.interactive<-ggplotly(graph3)
  print(graph3.interactive)
}





#Load Packages
library(tidyverse)
library(ggpubr)
library(lubridate)
library(gifski)
library(gganimate)
library(readxl)
library(plotly)

#Import Data
SpendingData <- read_excel("Personal Finances.xlsx")
Dataset<-CleanData(SpendingData)
Analyze.and.Visualize(Dataset)
