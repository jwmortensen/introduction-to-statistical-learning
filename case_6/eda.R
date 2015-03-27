setwd("/Users/jacobmortensen/School/Winter 2015/Stat536/cases/6/")
tornado <- read.csv("Tornados2012.csv")
plot(tornado[,c(8,9:18)])
tornado$Time <- strptime(tornado$Time, format="%H:%M:%S")

# injuries, fatalities, loss, length, width

for (i in 1:ncol(tornado)) {
  print(class(tornado[,i]))
  print(colnames(tornado)[i])
  if (class(tornado[,i]) != "factor")
  boxplot(tornado[,i]~tornado[,8], main=bquote(.(colnames(tornado)[i])))
}
boxplot(as.POSIXct(tornado$Time)~tornado[,8])

pdf("injuries.pdf")
boxplot(tornado$Injuries~tornado$Fscale, main="Number of Injuries by F-scale")
dev.off()
pdf("fatalities.pdf")
boxplot(tornado$Fatalities~tornado$Fscale, main="Number of Fatalities by F-scale")
dev.off()
pdf("width.pdf")
boxplot(tornado$Width~tornado$Fscale, main="Width by F-scale")
dev.off()
pdf("length.pdf")
boxplot(tornado$Length~tornado$Fscale, main="Length by F-scale")
dev.off()

