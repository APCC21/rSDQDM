SelectData30Yr<-function(End_Yr_Fut,iCur_Yr,Date_Period,Model.Data.All) {
  # Select 30 years with the center of current year for BC & downscaling
  # Date_Current: the current date (e.g. "1976-01-01")
  # Date_Period: All days with Date format during all period (1976 to 2099)
  # Model.Data.All: moving time wondow (e.g. +-15 days)

  Model.Data.All<-data.frame(Model.Data.All)
  bg.Yr<-as.character(iCur_Yr-15)
  ed.Yr<-as.character(iCur_Yr+14)
  if(ed.Yr>End_Yr_Fut) {
    bg.Yr<-End_Yr_Fut-29
    ed.Yr<-End_Yr_Fut
  }
  bg.Date<-as.Date(paste(bg.Yr,"-01-01",sep=""))
  ed.Date<-as.Date(paste(ed.Yr,"-12-31",sep=""))

  #pic<-Combine.All.Data$Date_Period>=bg.Date & Combine.All.Data$Date_Period<=ed.Date
  pic<-Model.Data.All[,1]>=bg.Date & Model.Data.All[,1]<=ed.Date

  Selected.30Yr.Data<-Model.Data.All[pic,]
  return(Selected.30Yr.Data)
}
