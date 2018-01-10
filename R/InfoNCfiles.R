InfoNCfiles<- function (Folder,filelist,...){

  for (i in 1:length(filelist)) {
    split_GCMlist<-unlist(strsplit(filelist[i],"_"))[3]  #GCM name
    split_datelist<-unlist(strsplit(filelist[i],"_"))[6] #time period (e.g. 19760101-20051231.nc)
    yy_bg<-substr(split_datelist,1,4); mm_bg<-substr(split_datelist,5,6);dd_bg<-substr(split_datelist,7,8)
    yy_ed<-substr(split_datelist,10,13); mm_ed<-substr(split_datelist,14,15);dd_ed<-substr(split_datelist,16,17)
    bd<-as.Date(paste(yy_bg,"-",mm_bg,"-",dd_bg,sep=""))
    ed<-as.Date(paste(yy_ed,"-",mm_ed,"-",dd_ed,sep=""))
    if (substr(split_GCMlist,1,6)=="HadGEM") {
      if (mm_ed=="12" && dd_ed=="30") {
        ed<-as.Date(paste(yy_ed,"-",mm_ed,"-","31",sep=""))
      }
    }

    if(length(filelist)==1) {
      Date_start_NC<-bd
      Date_end_NC<-ed
    } else {
      if(i==1) {
        Date_start_NC<-bd
      } else if (i==length(filelist)) {
        Date_end_NC<-ed
      }
    }

    filename<-filelist[i]
    NC_path<-paste(Folder,"/",filename,sep="")
    nc_raw_data<-nc_open(NC_path)
    lon <- ncvar_get(nc_raw_data,"lon")
    lat <- ncvar_get(nc_raw_data,"lat")
    nc_close(nc_raw_data)
  }  # i
  Date_NC_Period<-as.Date(seq(Date_start_NC,Date_end_NC,by="1 day"))
  Ndays_NC_Period=as.numeric(length(Date_NC_Period))
  return(list(lon=lon,lat=lat,Ndays_NC_Period=Ndays_NC_Period))
}
