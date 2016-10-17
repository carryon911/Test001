setwd("C:/Users/Student/Desktop/回測平台v4(C)")
rm(list=ls())
library(quantmod)
load(paste0(getwd(),"/futxts.rdata"))


from="2015-1-1"
to="2015-12-31"
vfile=paste0(getwd(),"/playground.csv")


selected_x <- futxts[paste0(as.Date(from),"/",as.Date(to))]
#drop first row in Due date
delindex<-logical(length = nrow(selected_x))
for(i in 1:(nrow(selected_x)-1)){
  if(time(selected_x)[i]==time(selected_x)[i+1]){
    delindex[i] <- TRUE
  }
}
selected_x<-selected_x[!delindex,]
#寫出xts物件之前，先轉型
TradingDate <- as.character(time(selected_x))
df.coredata <- as.data.frame(coredata(selected_x))
output <- data.frame(TradingDate,df.coredata)
write.table(output, file = vfile,sep = ",",
            col.names = TRUE,append = FALSE,
            row.names = FALSE)

