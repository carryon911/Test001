#!!!!重要!!!!!!
#請先設定工作目錄(setwd)
##請先設定工作目錄與報表名稱
##
rm(list = ls())
load("futxts.rdata")
decision<-read.table("playground.csv",sep=",",
                     header = TRUE,stringsAsFactors = FALSE)
#結算日當天部位強制結清
#找出經歷的結算日
library(quantmod)
begin<-as.Date(decision$TradingDate[1])
end<-as.Date(decision$TradingDate[nrow(decision)])
selected_futxts<-futxts[paste0(begin,"/",end)]


cdtemp<-data.frame(time(selected_futxts),coredata(selected_futxts),stringsAsFactors = FALSE)
names(cdtemp)<-names(decision)
cdtemp$TradingDate<-as.Date(cdtemp$TradingDate)

selindex<-logical(nrow(cdtemp))
for (i in 1:(nrow(cdtemp)-1)){
  if(cdtemp[i,"TradingDate"] == cdtemp[i+1,"TradingDate"]){
    selindex[i]<-TRUE    
  }
}

cdtemp<-cdtemp[selindex,]
cdtemp$Position<-0
cdtemp$PositionQ<-0
decision$TradingDate<-as.Date(decision$TradingDate)
report<-rbind(cdtemp,decision)
report["TradingDate"]<-as.Date(unlist(report["TradingDate"]))
rm(cdtemp,decision,futxts,selected_futxts,begin,end,selindex,i)
#合併排序，結算日當天會有兩筆資料，結清資料列要在新近期期貨前

Orderbydate <- order(report$TradingDate)
report<-report[Orderbydate,]
rm(Orderbydate)


#增加並且計算衍生欄位TBasis,ABasis,Income,cumIncome,Lmax,Lmin,Smax,Smin
for(i in 3:ncol(report)){
  report[,i]<-as.numeric(unlist(report[,i]))
}
#TBasis,ABasis
TBasis<-report["Close"]*report["PositionQ"]
names(TBasis)<-"TBasis"
for(i in 2: nrow(report)){
  if(report[i,"Position"]==report[i-1,"Position"] && 
     report[i,"PositionQ"]==report[i-1,"PositionQ"]){
    TBasis[i,"TBasis"]<-TBasis[i-1,"TBasis"]  
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] < report[i-1,"PositionQ"]
           ){
    TBasis[i,"TBasis"]<-TBasis[i,"TBasis"]*report[i,"PositionQ"]/report[i-1,"PositionQ"]
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] > report[i-1,"PositionQ"]){
    TBasis[i,"TBasis"]<-TBasis[i-1,"TBasis"]+report[i,"Close"]*(report[i,"PositionQ"]-report[i-1,"PositionQ"])
  }else if(report[i,"Position"] != report[i-1,"Position"]){
    TBasis[i,"TBasis"]<-report[i,"Close"]*report[i,"PositionQ"]
  }
}
ABasis<-TBasis[,"TBasis"]/report[,"PositionQ"]
ABasis[is.nan(ABasis)]<-0
report<-cbind(report,TBasis,ABasis)
rm(TBasis,ABasis)

#Income
Income<-numeric(NROW(report))
names(Income)<-"Income"
for(i in 2:nrow(report)){
  if(report[i,"Position"]==report[i-1,"Position"] && 
     report[i,"PositionQ"]==report[i-1,"PositionQ"]){
    Income[i]<-0
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] < report[i-1,"PositionQ"]
  ){
    Income[i]<-(report[i,"Close"]-report[i-1,"ABasis"])*(report[i,"PositionQ"] - report[i-1,"PositionQ"])*report[i,"Position"]
  }else if(report[i,"Position"] == report[i-1,"Position"] && 
           report[i,"PositionQ"] > report[i-1,"PositionQ"]){
    Income[i]<-0
  }else if((report[i,"Position"]==1 && report[i-1,"Position"]==-1)|
           (report[i,"Position"]==-1 && report[i-1,"Position"]==1)){
    Income[i]<-(report[i,"Close"]-report[i-1,"ABasis"])*report[i-1,"PositionQ"]*report[i-1,"Position"]
  }else if((report[i,"Position"]==0 && report[i-1,"Position"]==1)|
           (report[i,"Position"]==0 && report[i-1,"Position"]==-1)){
    Income[i]<-(report[i,"Close"]-report[i-1,"ABasis"])*report[i-1,"PositionQ"]*report[i-1,"Position"]
}
}
acumIncome <- Income
for (i in 2:nrow(report)) {
  acumIncome[i]<-acumIncome[i-1]+Income[i]  
}

report<-cbind(report,Income,acumIncome)
rm(Income,acumIncome)

#Lmax,Lmin,Smax,Smin
Lmax<-numeric(NROW(report))
Lmin<-numeric(NROW(report))
Smax<-numeric(NROW(report))
Smin<-numeric(NROW(report))
Lmax[1]<-NA
Lmin[1]<-NA
Smax[1]<-NA
Smin[1]<-NA
for (i in 2:NROW(report)) {
  Lmax[i] <- (report[i,"High"]-report[i-1,"ABasis"])*(report[i-1,"PositionQ"])
  Lmin[i] <- (report[i,"Low"]-report[i-1,"ABasis"])*(report[i-1,"PositionQ"]) 
  Smax[i] <- (report[i-1,"ABasis"]-report[i,"Low"])*(report[i-1,"PositionQ"])
  Smin[i] <- (report[i-1,"ABasis"]-report[i,"High"])*(report[i-1,"PositionQ"]) 
}
report<-cbind(report,Lmax,Lmin,Smax,Smin)
rm(Lmax,Lmin,Smax,Smin)
#輸出報表report.csv
write.table(report,file = "report(C).csv",sep = ",",row.names = FALSE)