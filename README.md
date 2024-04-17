# Exploratory-Data-Analysis-Using Rmarkdown
Exploratory Data Analysis of a dataset
---
title: "Kasireddy_Project2"
author: "Harsha vardhan reddy Kasireddy"
date: "2023-10-01"
output: html_document
---
<P><BR>
<CENTER>
<FONT SIZE = 5, COLOR="#000066">
<B>ALY6000</B>  
<B>Project2 Exploratory Data Analysis (EDA) of Two Data Sets</B>  
<B>Harsha vardhan reddy Kasireddy</B></FONT>  
<FONT SIZE = 4, COLOR="#660000">
Instructor: Dee Chiluiza, PhD  
Northeastern University  
Date: `r format(Sys.time(), "%d %B, %Y")`  
</FONT>
</CENTER>  

```{r setup, message=FALSE, warning=FALSE, include=FALSE}

library(rmarkdown)
library(readxl)       # to read data sets
library(readr)        # to read data sets
library(tidyverse)
library(dplyr)        # part of tidyverse
library(knitr)
library(magrittr)     # for the pipes %>% 
library(RColorBrewer) # For colors
data <- read_xlsx("M2Project_Data_2023.xlsx")
```

# Libraries used in this document
#library(rmarkdown)
#library(readxl)       # to read data sets
#library(readr)        # to read data sets
#library(tidyverse)
#library(dplyr)        # part of tidyverse
#library(knitr)
#library(magrittr)     # for the pipes %>% 
#library(RColorBrewer) # For colors


<P>
<FONT SIZE = 3, COLOR = "#A11515">
<B>INTRODUCTION</B>
</FONT>

<!--- below enter the information--->
<P>
In the ever-evolving landscape of data-driven decision-making, Exploratory Data Analysis (EDA) plays a pivotal role. EDA is a critical phase in the data analysis process, serving as the initial step to uncover insights, patterns, and anomalies within datasets. In this project, we embark on an exciting journey through the world of EDA, focusing on not one but two distinct datasets.

The primary objective of this project is to delve deep into the art and science of EDA while comparing and contrasting two diverse datasets. By conducting a comprehensive analysis of these datasets, we aim to extract meaningful information, identify trends, and gain a profound understanding of the underlying data structures. Through this process, we intend to draw valuable insights that can inform decision-makers and drive informed actions.

<P><BR>
<FONT SIZE = 2, COLOR = "#A11515">
<B>Task 1: Present the first 5 and the last 5 records from the dataset
</B>
</FONT><BR>

```{r task1}


first_fivee <- head(data,n=5)
last_fivee <- tail(data, n=5)

binded_dataa <- rbind(first_fivee,last_fivee)

knitr::kable(binded_dataa)


```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 2: Present a table with all categories of variable Market and their frequencies
</B>
</FONT><BR>

```{r task2}
market_tablee <- table(data$Market)
market_dataframee <-  data.frame(Market=names(market_tablee),Frequency=as.vector(market_tablee))
knitr::kable(market_dataframee,align ="c",format="html", table.attr='border="1"') 

```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 3: Present a bar graph with all categories of Market and their frequencies</B>
</FONT><BR>

```{r task3}
market_tablee <- table(data$Market)
barplot(market_tablee, horiz = TRUE, col=brewer.pal(n=length(market_tablee),name="Set3"), main="Market Category and Frequencies", xlab="Frequency", xlim=c(0,max(market_tablee)+10))
text(market_tablee+2, 1:length(market_tablee),fill=brewer.pal(n=length(market_tablee),name="Set3"))
par(xpd=TRUE)

```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 4: Pie chart of product category frequencies in Africa
</B>
</FONT><BR>

```{r task4}
task4Africa = dplyr::filter(data, Market=="Africa")
product_Atable <- table(task4Africa$Product_Category)
pie(product_Atable,labels=paste(names(product_Atable),"(",product_Atable,")",sep=""),main="Product Category Frequency in Africa", col=rainbow(length(product_Atable)))
legend("topright",legend=names(product_Atable),fill=rainbow(length(product_Atable)))




```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 5: Reproduce codes given by instructor
</B>
</FONT><BR>
```{r task5}
task5_table=table(task4Africa$Product_SubCategory)
t5bar=barplot(task5_table)
text(y=task5_table,t5bar,task5_table,cex=0.8,pos=3)


```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 6: Improve bar plot from task 5
</B>
</FONT><BR>
```{r task6, warning=FALSE}

cust_colors <- brewer.pal(n=length(market_tablee),name="Set2")
t5bar=barplot(task5_table,  col=cust_colors, main="Market Sub Categories and Frequencies \n- harsha",  las = 2)
mtext(side=1,line=4,"Product_SubCategory")
mtext(side=2,line=2,"Frequency")
text(y = task5_table,x= t5bar, lables = task5_table,cex = 0.8, pos = 3)
```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 7: Mean sales per subcategory in the African Market </B>
</FONT><BR>
```{r task7, warning=FALSE}
Mean_salesinAfrica <- tapply(task4Africa$Sales,task4Africa$Product_SubCategory, mean)
dotchart(Mean_salesinAfrica,
         main = "Mean Sales per Subcategory in the African Market",
         xlab = "Mean Sales",
         pch = 19, 
         cex = 0.7,
         xlim = c(0, max(Mean_salesinAfrica) + 50), 
         labels = paste(names(Mean_salesinAfrica), " (", round(Mean_salesinAfrica, 2), ")", sep = ""),
         color = "blue")
```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 8: Total sales per Region in the African Market</B>
</FONT><BR>
```{r task8}
Totalsales_Africa <- tapply(task4Africa$Sales,task4Africa$Region,sum,na.rm = TRUE)
barplot(Totalsales_Africa,main="Total sales per region in the African market\n - Harsha",xlab="Total sales",ylab="Region")
cust_colors <- brewer.pal(n=length(market_tablee),name="Set2")
text(Totalsales_Africa+50,1:length(Totalsales_Africa),labels=Totalsales_Africa,pos=4,col=cust_colors)

par(mar=c(4,1,1,4))
par(mai=c(0.8,0.7,1,0.2))
```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 9: Descriptions of: integer, factor, double, numeric</B>
</FONT><BR>
<P><BR>
<FONT SIZE=2,COLOR="black">
<B>

Integer:
Description: In R, integers represent whole numbers without any fractional or decimal components. They are expressed as precise numeric values.
Use Cases: Integers are appropriate for variables that need to hold count data, indices, or any form of discrete numerical values.
Author: Wickham and Grolemund (2016) in "R for Data Science."

Factor:
Description: Factors in R are utilized to represent categorical variables. They store data as distinct levels or categories and are internally encoded as integers.
Use Cases: Factors are employed for nominal or ordinal data where the arrangement of levels is significant, such as representing various categories or levels of a variable.
Author: Wickham and Grolemund (2016) in "R for Data Science."

Double:
Description: Double is a data type in R designed for floating-point numbers. It can accommodate real numbers, including those with decimal fractions.
Use Cases: Double is suitable for most numeric calculations, including mathematical operations involving fractions or decimals.
Author: Matloff (2011) in "The Art of R Programming."

Numeric:
Description: Numeric is a general data type in R that encompasses both integers and double-precision numbers. It is a versatile data type capable of storing various forms of numeric data.
Use Cases: Numeric data types are employed for a wide range of numeric data, including continuous and discrete values. They offer flexibility in handling numeric data.
Author: Matloff (2011) in "The Art of R Programming."

These descriptions provide a basic understanding of these data types in R. It's important to note that factors, although internally represented as integers, are used to store categorical data and are distinct from integers. The choice of data type depends on the nature of the data and the specific requirements of the analysis.</B></FONT>
<BR>

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 11: Duplicate codes presented by instructor</B>
</FONT><BR>
```{r task11}
par(mfcol=c(2,1),
    mai=c(4,4,0.5,2),
    mar=c(4,4,0.5,2))
boxplot(data$Profits,horizontal = T)
hist(data$Profits,breaks=50,main="Profits")
    
```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 12: Profits in the Latin American market</B>
</FONT><BR>

```{r task12, warning=FALSE}
task13LATAM<-dplyr::filter(data, Market=="LATAM")
box_plot_Latam <- ggplot(data = task13LATAM, aes(x = Market, y = Profits)) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "Box Plot of Profits in Latin America Market", x = "Market", y = "Profits") +
  theme_minimal() +
  geom_point(aes(x = Market, y = mean(Profits)), color = "black", size = 3, shape = 8)

# Create a histogram for profits in the Latin American market
histogram_Latam <- ggplot(data = task13LATAM, aes(x = Profits)) +
  geom_histogram(fill = "yellow", color = "black", bins = 20) +
  labs(title = "Distribution of Profits in Latin America Market", x = "Profits", y = "Frequency") +
  theme_minimal() +
  geom_vline(aes(xintercept = mean(Profits)), color = "black", linetype = "dashed", size = 1)

gridExtra::grid.arrange(box_plot_Latam, histogram_Latam, ncol = 2)
```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 13:Table total sales per region in Latin America</B>
</FONT><BR>

```{r task13}
Totalsales_LATAM = tapply(task13LATAM$Sales,task13LATAM$Region, sum)
Totalsales_LATAMTable <- data.frame(Region=names(Totalsales_LATAM),Totalsales_LATAM)
knitr::kable(Totalsales_LATAMTable,caption="Total sales per region in Latin America")

```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Task 14:Box plot for the profits per region in the Latin American market
</B>
</FONT><BR>

```{r task14}
library(ggplot2)

# Create a box plot for profits per region in the Latin American market
ggplot(data = task13LATAM, aes(x = Region, y = Profits, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Profits per Region in the Latin American Market",
    x = "Region",
    y = "Profits"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightblue", "lightblue")) +
  theme(legend.position = "none") 

```

<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Conclusion</B></FONT><BR>
<P><BR>
<B>In the course of this exploratory data analysis (EDA) project, we delved into two distinct datasets, aiming to uncover valuable insights and patterns within the data. EDA serves as a crucial initial step in the data analysis process, allowing us to gain a deeper understanding of the datasets and inform data-driven decision-making.

Throughout our analysis, we performed a series of tasks to explore the datasets, analyze various variables, and visualize key trends. Here are some of the noteworthy findings and conclusions drawn from our EDA:
<BR>
1.Dataset Overview: We began by presenting the first 5 and last 5 records from the dataset, giving us a glimpse of the data's structure and content.
<BR>
2.Market Categories: We analyzed the frequency distribution of the "Market" variable, which revealed the distribution of market categories within the dataset.
<BR>
3.Market Visualization: A bar graph further illustrated the distribution of market categories, providing a visual representation of the frequency of each category.
<BR>
4.Product Categories in Africa: We created a pie chart to visualize the distribution of product categories specifically in the African market, shedding light on the product landscape in this region.Mean Sales by Subcategory: Calculating the mean sales per subcategory in the African market, we identified which product subcategories tend to perform well in terms of sales.
<BR>
5.Total Sales by Region: By analyzing total sales per region in the African market, we gained insights into the regional distribution of sales, which can inform regional strategies.
<BR>
6.Data Types: We provided descriptions of various data types, including integer, factor, double, and numeric, clarifying their roles and use cases in data analysis.
<BR>
7.Additional Analysis: We also duplicated some codes presented by the instructor to explore further aspects of the data, such as analyzing profits through box plots and histograms.
<BR>
8.Latin American Market: In addition to the African market, we conducted a detailed analysis of the Latin American market, examining profits, total sales by region, and creating visualizations to facilitate insights.
<BR>
In summary, this EDA project allowed us to gain a comprehensive understanding of the datasets, identify patterns, and extract meaningful insights. Such insights can be invaluable for decision-makers and stakeholders who rely on data to drive informed actions. Moreover, this project showcased the power of R and data visualization tools in conducting effective EDA.

As we conclude this project, it is essential to acknowledge that EDA is an iterative process, and the insights presented here are just the beginning. Further analyses and modeling can build upon this foundation to derive even more valuable information from the data, ultimately contributing to data-driven decision-making and business success.</B>
<BR>


<P><BR>
<FONT SIZE =2, COLOR="#A11515">
<B>Bibliography</B>
<B>References:</B></FONT><BR>
<P><BR>
1. Kable and KableExtra link:https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
<BR>
2. Prof. Diana Chiluiza, About R, RPUBS: https://rpubs.com/Dee_Chiluiza/home
<BR>
3.DPlyr transformation   link:https://northeastern.instructure.com/courses/160378/assignments/1933008
<BR>
4. Bar plot , Geeks for geeks, https://www.geeksforgeeks.org/bar-plot-in-matplotlib/
<BR>
5. Histogram RMarkdown, youtube:https://www.youtube.com/watch?v=_GN81FhJ7qM
</B>
<BR>
