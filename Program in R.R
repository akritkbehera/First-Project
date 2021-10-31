
train <- read.csv("C:/Users/KIIT/Desktop/Internships/Train_UWu5bXk (2) (1) (1) (1).csv")
test <- read.csv("C:/Users/KIIT/Desktop/Internships/Test_u94Q5KV (2) (1) (1) (1).csv")
dim(train)
dim(test)
str(train)
table(is.na(train))
colSums(is.na(train))
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") +
  xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple")
+theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + ggtitle("Outlets vs Total Sales")
+ theme_bw()
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x =
                                                                                           element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet
Sales")+ggtitle("Item Type vs Sales")
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x =
                                                                                       element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type
vs Item MRP")
test$Item_Outlet_Sales <- 1
combi <- rbind(train, test)
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility)
levels(combi$Outlet_Size)[1] <- "Other"
 library(plyr)
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                    c("LF" = "Low Fat", "reg" = "Regular"))
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
 table(combi$Item_Fat_Content)
 library(dplyr)
 a <- combi%>%
   group_by(Outlet_Identifier)%>%
   tally()
 head(a)
 names(a)[2] <- "Outlet_Count"
 combi <- full_join(a, combi, by = "Outlet_Identifier")