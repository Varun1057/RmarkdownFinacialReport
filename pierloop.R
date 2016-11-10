setwd("C:/Users/VSharma/Desktop/R files")


# Creating a function to create pier charts for revenue genrated by each customer type through each enterprise ##



File<-read.csv("Piereport.csv",stringsAsFactors = FALSE)
# gsub() removes all strings elements that match the first value
# and replace with the second value. Here all commas are replaced
# with empty string values i.e. no character
File$totalrev <- gsub(",", "",File$totalrev)
# convert to numeric
File$totalrev <- as.numeric(File$totalrev)

createchart<-function(CustomerType,Report){
  Report<-File
  Chart <- Report[which(Report$Customer.Type == CustomerType), ]
  pie(Chart$totalrev, Chart$Enterprise, main = CustomerType)
  return(NULL)
}




tb = table(File$Customer.Type)## table for different customers as a unique value 
names(tb)
tb
print(File)
length(tb)
par(mfrow=c(3,3))


#********Running a loop for pie chart for what customer generates what kind and how much of each revenue type*
#much revenue
for (ii in 1:length(tb))
  createchart(names(tb)[ii])  


