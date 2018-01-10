CollectDataWindow<- function (iMon,Raw_Data) {
  # Collect data within a moving time window
  # Date_Tar: a day of the center of the moving time window (e.g. "1976-01-01")
  # Raw_Data: Data to be collected
  Collected.Data.Window=NULL
  iMon_array<-array(data=NA,dim=3)
  #iMon_array_text<-array(data=NA,dim=3)
  Month_text<-array(data=NA,dim=3)
  iMon_array[1]<-iMon-1
  iMon_array[2]<-iMon
  iMon_array[3]<-iMon+1
  for (i in 1:3) {

    if (iMon_array[i]>12) {
      iMon_array[i]<-1
    } else if (iMon_array[i]<1) {
      iMon_array[i]<-12
    }

    if (iMon_array[i]<10) {
      Month_text[i] <- paste("0",iMon_array[i],sep="")
    } else {
      Month_text[i] <- as.character(iMon_array[i])
    }
  }

  pic.date<-format(Raw_Data[,1],"%m")==Month_text[1] | format(Raw_Data[,1],"%m")==Month_text[2] |
            format(Raw_Data[,1],"%m")==Month_text[3]
  Collected.Data.Window<-Raw_Data[pic.date,]

  return(Collected.Data.Window)
}
