data.prep<-function(Finance){
  PF<-Finance[,c(1:4)]
  PF$Date<-ymd(PF$Date)
  classes<-c("Investments", "Mandatories", "Flexible")
  PF$Category<-as.factor(PF$Category)
  levels(PF$Category)<-classes
  full.entries<-complete.cases(PF)
  PF<-PF[full.entries,]
  return(PF)  
}

data.analysis<-function(d){
  d2<-d
  d2<-d2 %>% select(Category, Amount) %>% group_by(Category) %>% tally()
  
  graph1<-ggplot(d2, mapping<-aes(x=Category,y=n, fill=Category))+
   geom_bar(stat="identity")+
   ggtitle("Transactions by Amount")+
   labs(x= "Category of purchase", y= "Amount Spent in JMD")
  print(graph1)
  
  d2<-d %>% group_by(Category) %>% summarise(Amount = sum(Amount))
  graph2<-ggplot(d2,mapping = aes(x=Category, y=Amount, fill=Category))+
    geom_bar(stat="identity")+
    ggtitle("Spending by Category")+
    labs(x="Category of purchase", y="Amount spent in JMD")
  print(graph2)
 
  d2<- d %>% group_by(month=floor_date(Date, "month"), Category) %>% summarise(t=sum(Amount))
  graph3<-ggplot(d2, mapping = aes(x=month,y=t, colour=Category))+
    geom_line(size=1.2, alpha=0.8)+
    geom_point(alpha= 0.8)+
    ggtitle("Spending by month")+
    labs(x="Date", y="Amount Spent in JMD")
  print(graph3)
  graph3.animation <- graph3 + transition_reveal(month)
  print(graph3.animation + scale_x_date(date_labels= "%m-%Y"))
}


library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(readxl)
Finances <- read_excel("Investing and Finance/Personal Finance/Personal Finances.xlsx")
dataset<-data.prep(Finances)
data.analysis(dataset)




