Read_HadGEM<- function (Folder,NCfileName,Num_days_Cal,Variable,split_date,...) {

  # NCfileName <- filelist[i]
  # Num_days_Cal <- Num_days_Cal
  # Variable <- Variable
  # split_date <- split_datelist

  filename<-NCfileName
  NC_path<-paste(Folder,"/",filename,sep="")
  nc_raw_data<-nc_open(NC_path)
  lon <- ncvar_get(nc_raw_data,"lon")
  lat <- ncvar_get(nc_raw_data,"lat")
  nlon<-as.numeric(length(lon))
  nlat<-as.numeric(length(lat))
  nc_Var <- ncvar_get(nc_raw_data,Variable)
  Num_days_NC<-as.numeric(length(nc_Var[1,1,]))
  nc_close(nc_raw_data)

  nc_Var_filling=array(0,dim=c(nlon,nlat,Num_days_Cal))

  yr_bg<-substr(split_date,1,4); mm_bg<-substr(split_date,5,6);dd_bg<-substr(split_date,7,8)
  yr_ed<-substr(split_date,10,13); mm_ed<-substr(split_date,14,15);dd_ed<-substr(split_date,16,17)

  norder_GCM=0
  norder_fill=0
  Nleap_Yr=0

  for (iYear in yr_bg:yr_ed) {

    if (yr_bg==yr_ed) {
      startmonth<-mm_bg
      endmonth<-mm_ed
    } else if(iYear==yr_ed) {
      startmonth<-1
      endmonth<-mm_ed
    } else if(iYear==yr_bg) {
      startmonth<-mm_bg
      endmonth<-12
    } else {
      startmonth<-1
      endmonth<-12
    }

    nday.in.month_NC=0
    for (imonth in startmonth:endmonth) {
      for (iday in 1:30) {
        norder_GCM=norder_GCM+1
        if(imonth==1 || imonth==3 || imonth==5 || imonth==7 || imonth==8 || imonth==10 || imonth==12) {
          norder_fill=norder_fill+1
          nc_Var_filling[,,norder_fill]<-nc_Var[,,norder_GCM]
          if (iday==30) {
            norder_fill=norder_fill+1
            nc_Var_filling[,,norder_fill]<-nc_Var[,,norder_GCM]
          }
        } else if (imonth==2) {

          if(leap.year(iYear)) {
            nday_end_Feb<-29
          } else {
            nday_end_Feb<-28
          }
          if (iday<=nday_end_Feb) {
            norder_fill=norder_fill+1
            nc_Var_filling[,,norder_fill]<-nc_Var[,,norder_GCM]
          }
        } else {
          norder_fill=norder_fill+1
          nc_Var_filling[,,norder_fill]<-nc_Var[,,norder_GCM]
        }
      }  #iday
    }    #imonth
  }      #iYear

  return(list(nc_Var_filling=nc_Var_filling,lon=lon,lat=lat))
}
