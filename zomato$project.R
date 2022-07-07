# exploring and renaming the data
summary(zomato)
head(zomato , n = 17)
typeof(zomato)
glimpse(zomato)
status(zomato)
p_na(zomato) 
colnames(zomato)[colnames(zomato )== "approx_cost(for two people)"] = "cost_2"
colnames(zomato)[colnames(zomato )== "rate"] = "rating"
library(dplyr)
zomato =  rename(zomato , "type" = 16)
zomato = rename(zomato , "city" = 17)
as.character(zomato$cost_2)
typeof(zomato$cost_2)
unique(zomato$rating)
zomato$rating = gsub("/5" , "" , zomato$rating)
## visualising
###most famous rest chain
table(zomato$name)
library(ggplot2)

vector(zomato$name)
top_20 <- sort(table(zomato$name), decreasing = T)[0:20]
top_20 = as.data.frame(top_20)
View(top_20)
is.data.frame(top_20)
top_20  = rename(top_20 , "name" = 1)
barplot(top_20$name)
ggplot(  top_20 , aes(x = name , y = Freq )) + geom_col()
dput(head(top_20))


ggplot(top_20, aes(y = name, x= Freq , fill = name , legend(name)  )) +
  geom_bar( stat = "summary" ,  ) + xlab("Name of the restaurant") + ylab("No. of outlets") + geom_text(aes(label = Freq))


## most popular rest types
table(zomato$rest_type)

most_types = sort(table(zomato$rest_type) , decreasing = T)[0:20]
most_types = as.data.frame(most_types)
View(most_types)
most_types = rename(most_types, "type"  = "Var1" )
ggplot(most_types , aes(  y = type , x = Freq , fill = type , bins = 0.1)) + geom_col(col = "red") + geom_text(aes(label = most_types$Freq)) +   xlab("No. of outlets") + ylab("Type Of Restaurants") + labs(title =  " Count Of Each Restaurant Types") 
## rest in location
top_location = sort(table(zomato$location), decreasing = T)[0:20]
top_location = as.data.frame(top_location)
View(top_location)     
ggplot(top_location , aes(  y = Var1 , x = Freq , fill = Var1 , bins = 0.1)) + geom_col(col = "red") + geom_text(aes(label = top_location$Freq)) +  xlab("No. of Outlets") + ylab("Location") 




top_L = sort(table(zomato$city) , decreasing = T)[0:20]
View(top_L)
ggplot(data = zomato , aes(x = online_order , fill = book_table ))  +geom_bar()   
library(plotrix) 
##pie chart
num_oo = as.data.frame( table(zomato$online_order))
num_oo$percent = num_oo$Freq/sum(num_oo$Freq)  * 100
num_oo$percent = round(num_oo$percent)
num_oo = rename(num_oo , "online_order" = "Var1")
table_book = as.data.frame(table(zomato$book_table) , decreasing = T)
table_book$percent  = table_book$Freq/ sum(table_book$Freq) *100
table_book$percent = round(table_book$percent)
table_book = rename(table_book ,  "book_table"  = "Var1" )


ggp1 = ggplot(data= table_book , bins = 0.55) + geom_col(mapping = aes(x = "" , y =  percent , fill = book_table  )) + coord_polar(theta = "y") +  labs(title = "TABLE BOOKING") + theme(plot.title = element_text(hjust = 0.5, size = 20),
                                                                                                                                                                                         axis.title = element_blank(),
                                                                                                                                                                                         axis.text = element_blank(),
                                                                                                                                                                                         axis.ticks = element_blank(),
                                                                                                                                                                                         panel.grid=element_blank(),
                                                                                                                                                                                         panel.border = element_blank())
ggp2 = ggplot(data= num_oo , ) + geom_col(mapping = aes(x = "" , y =  percent , fill = online_order ), width = 2) + coord_polar(theta = "y") + labs(title = "ONLINE ORDER") + theme(plot.title = element_text(hjust = 0.5, size = 20),
                                                                                                                                                                                    axis.title = element_blank(),
                                                                                                                                                                                    axis.text = element_blank(),
                                                                                                                                                                                    axis.ticks = element_blank(),
                                                                                                                                                                                    panel.grid=element_blank(),
                                                                                                                                                                                    panel.border = element_blank())


grid.arrange(ggp1 , ggp2 , ncol = 2)

## cost~ online order& bookings
ggplot(data =  zomato , aes(x = online_order , y = cost_2, fill= book_table)) + geom_bar(stat = "summary")

histogram(  as.numeric(zomato$rating) ,  xlab = "rating") 
curve(dnorm(x), add = T)
table(factor(zomato$cost_2))

agg = aggregate(cost_2 ~location , zomato , mean)
agg$cost_2 = round(agg$cost_2)
view(agg)
ggplot(data =  agg , aes(x = location , y = cost_2 )) + geom_col(fill = "blue") + theme(axis.text.x = element_text(angle = 90 , size =  10) ) + geom_text(agg , mapping = aes(label = cost_2 ) , stat = "summary" ,position_dodge(width = 1), size = 3  , vjust = -0.5    )
## boxplot
sort_cost = sort(agg$cost_2 , decreasing = T)
sort_cost = as.data.frame(sort_cost)
ggplot(data = sort_cost ) +geom_boxplot(aes(x= sort_cost ) , fill = "blue")
round(zomato)
ggplot(data =  zomato , aes(x = (rating) , y = votes) ) + geom_point(pch = 2, col = "aquamarine")
