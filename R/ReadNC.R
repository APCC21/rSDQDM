ReadNC<- function (Folder,NCfileName,Num_days_Cal,Variable,yr_bg,yr_ed,End_Year_ref,...) {
  # Reading netCDF files
  # Each GCM has different lengths of historical runs and projections.
  # According to file names, this function combines all netCDF files into one and extract the reference and projection periods
  # Folder: Directory which netCDF files exist
  # NCfileName: the name of netCDF file
  # Num_days_Cal: Number of calendar days within the period of netCDF file
  # Variable: "pr", "tmin","tmax"
  # yr_bg,yr_ed,End_Year_ref: beginning, ending years, and ending year of reference period
  # Folder <- Folder
  # NCfileName <- filelist[i]
  # yr_bg <- yy_bg
  # yr_ed <- yy_ed
  # End_Year_ref <- End_Year
  ########################
  filename<-NCfileName
  splitfilenms <- unlist(strsplit(filename,"_"))[3]
  NC_path<-paste(Folder,"/",filename,sep="")
  nc_raw_data<-nc_open(NC_path)
  lon <- ncvar_get(nc_raw_data,"lon")
  lat <- ncvar_get(nc_raw_data,"lat")
  nlon<-as.numeric(length(lon))
  nlat<-as.numeric(length(lat))
  nc_Var <- ncvar_get(nc_raw_data,Variable)
  Num_days_NC<-as.numeric(length(nc_Var[1,1,]))
  nc_close(nc_raw_data)

  if(Num_days_NC==Num_days_Cal) {
    nc_Var_filling<-nc_Var
  } else {
    nc_Var_filling=array(0,dim=c(nlon,nlat,Num_days_Cal))

    norder_GCM=0
    norder_fill=0
    Nleap_Yr=0
    if(yr_ed>End_Year_ref){
      yr_ed_NC<-End_Year_ref
    } else {
      yr_ed_NC<- yr_ed
    }
    for (iYear in yr_bg:yr_ed_NC) {
      if(!leap.year(iYear)){
        if(splitfilenms=="bcc-csm1-1" & iYear==yr_ed_NC){
          norder_start<-norder_GCM+1
          norder_end<-norder_GCM+364
          norder_start_fill<-norder_fill+1
          norder_end_fill<-norder_fill+364

          nc_Var_filling[,,norder_start_fill:norder_end_fill]<-nc_Var[,,norder_start:norder_end]
          for (ilon in 1:nlon) {
            for (ilat in 1:nlat){
              x<-c(1,3)
              y<-c(nc_Var[ilon,ilat,norder_end],nc_Var[ilon,ilat,norder_end])
              nc_Var_filling[ilon,ilat,norder_end_fill+1]<-approx(x,y,2)$y
            }
          }
        } else {
          norder_start<-norder_GCM+1
          norder_end<-norder_GCM+365
          norder_start_fill<-norder_fill+1
          norder_end_fill<-norder_fill+365

          nc_Var_filling[,,norder_start_fill:norder_end_fill]<-nc_Var[,,norder_start:norder_end]
        }

      } else if (leap.year(iYear)){

        Nleap_Yr=Nleap_Yr+1
        norder_start<-norder_GCM+1
        norder_end<-norder_GCM+59
        norder_start_fill<-norder_fill+1
        norder_end_fill<-norder_fill+59
        nc_Var_filling[,,norder_start_fill:norder_end_fill]<-nc_Var[,,norder_start:norder_end]

        for (ilon in 1:nlon) {
          for (ilat in 1:nlat){
            x<-c(1,3)
            y<-c(nc_Var[ilon,ilat,norder_end],nc_Var[ilon,ilat,norder_end+1])
            nc_Var_filling[ilon,ilat,norder_end_fill+1]<-approx(x,y,2)$y
          }
        }

        if(splitfilenms=="bcc-csm1-1" & iYear==yr_ed_NC){
          norder_start<-norder_end+1
          norder_end<-norder_end+305
          norder_start_fill<-norder_end_fill+1+1
          norder_end_fill<-norder_end_fill+305+1
          nc_Var_filling[,,norder_start_fill:norder_end_fill]<-nc_Var[,,norder_start:norder_end]
          for (ilon in 1:nlon) {
            for (ilat in 1:nlat){
              x<-c(1,3)
              y<-c(nc_Var[ilon,ilat,norder_end],nc_Var[ilon,ilat,norder_end])
              nc_Var_filling[ilon,ilat,norder_end_fill+1]<-approx(x,y,2)$y
            }
          }
        } else {
          norder_start<-norder_end+1
          norder_end<-norder_end+306
          norder_start_fill<-norder_end_fill+1+1
          norder_end_fill<-norder_end_fill+306+1
          nc_Var_filling[,,norder_start_fill:norder_end_fill]<-nc_Var[,,norder_start:norder_end]
        }
      }
      norder_GCM<-norder_end
      norder_fill<-norder_end_fill
    }
  }
  return(list(nc_Var_filling=nc_Var_filling,lon=lon,lat=lat))
}
