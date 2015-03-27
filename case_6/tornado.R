library(tree)
library(randomForest)

tornado <- read.csv("Tornados2012.csv")
# used to find duplicates: 
# tornado[which(tornado$Number %in% tornado$Number[duplicated(tornado$Number)]), ]
# Remove duplicates based on F-scale, then fatalities, then loss, then injuries
# This process removes observations 113, 116, 120, 122, 148, 149, 158, 168, 179, 180, 187, 188, 200, 202, 209, 936
# 399 was removed because it appears to be a coding error
removals <- c(113,116,120,122,148,149,158,168,179,180,187,188,200,202,209,399,936)
tornado <- tornado[-removals,]

# Using our expert judgment we decided not to use StartLon, EndLat, StartLat, EndLon, Year, Month, Day, Date, Time, State
drops <- c("Number", "Year", "Month", "Day", "Date", "Time", "State", "StartLat", "StartLon", "EndLat", "EndLon")
tornado <- tornado[, !(colnames(tornado) %in% drops)]

# Convert Fscale
tornado$Fscale <- factor(tornado$Fscale, c("0","1","2","3","4"))
tree.tornado <- tree(Fscale~., tornado)
plot(tree.tornado)
text(tree.tornado, pretty=0)
