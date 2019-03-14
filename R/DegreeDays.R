# coolingDegreeDaysPlot
#
# This is an function
# which generates a cooling Degree day plot.
#
#' @examples
#' DegreeDays::coolingDegreeDaysPlot(datastream,base_temp,surface,area_url,area_name,login_api,building_api,energy_consumption_unit)
#' base_temp <- 21
#' surface<- 3382.6
#' datastream <- "http://hesso-entropy.euprojects.net/api/v1/query/executequery/59e5ca1be7d4380671da0c7b"
#' area_name<-"iig_v2"
#' area_url <- "http://hesso-entropy.euprojects.net//api/v1/building/degreedays"
#' login_api<-"http://hesso-entropy.euprojects.net/api/v1/auth/login"
#' building_api <- "http://hesso-entropy.euprojects.net/api/v1/building/energy_profile"
#' energy_consumption_unit <- "Kwatt"
#' @param datastream entropy datastream
#' @param surface area surface
#' @param base_temp base temperature
#' @param area_url area url
#' @param area_name area name
#' @return plot1 if cooling degree days plot as html
#' @export
coolingDegreeDaysPlot <- function(datastream,base_temp,surface,area_url,area_name,login_api,building_api,energy_consumption_unit){

  unit_validator <- check_energy_consumption_unit(energy_consumption_unit)
  if (unit_validator==0){return;}

  base_temp <- as.numeric(base_temp)
  surface <- as.numeric(surface)
  df <- jsonlite::stream_in(url(datastream))
  dfjson <- jsonlite::fromJSON(jsonlite::toJSON(df))
  mydata <- dfjson$nameValuePairs
  columnnames <-colnames(mydata)


  #Get building energy profile
  r <- httr::POST(login_api, body = "{\"username\": \"entropy\",\"password\": \"!entropy!\"}")
  authorization_key = httr::headers(r)$authorization
  #req <- httr::GET(building_api, httr::add_headers(Authorization = authorization_key))
  req <- httr::POST(building_api, body = area_name,httr::add_headers(Authorization = authorization_key))
  content <- httr::content(req)
  htc <-  content$nameValuePairs$HeatTransferCoefficient
  print (htc)
  print(columnnames)

  result = tryCatch({
    if(identical(columnnames[grepl('temperature_outdoor',columnnames)], character(0)))
    {
      print("temperature_outdoor is missing.");
      return;
    }
  }, error = function(e) {
    print("temperature_outdoor is missing. Is your query well-formed?");
    return;
  })


  if(identical(columnnames[grepl('temperature_indoor',columnnames)], character(0)))
  {
    print("temperature_indoor is missing. Is your query well-formed?");
    return;
  }

  consumption_columns <-columnnames[grepl('active_power',columnnames)]

  if(identical(consumption_columns, character(0)))
  {
    print("active_power is missing. Is your query well-formed?");
    return;
  }

  x<-data.frame(mydata$inDateTime$nameValuePairs)
  tempout_index <- grep(columnnames[grepl('temperature_outdoor',columnnames)], colnames(mydata))
  tempin_index <- grep(columnnames[grepl('temperature_indoor',columnnames)], colnames(mydata))

  #if many consumptions aggregate them
  if (length(consumption_columns)>1){
    power_to_aggregate <- dplyr::select(mydata, consumption_columns)
    aggregated_power <- rowSums(power_to_aggregate, na.rm = FALSE)
    xy <- data.frame(mydata[tempin_index],mydata[tempout_index],aggregated_power,x)
  }else{
    power_index <- grep(columnnames[grepl('active_power',columnnames)], colnames(mydata))
    xy <- data.frame(mydata[tempin_index],mydata[tempout_index],mydata[power_index],x)
  }

  xy <- na.omit(xy)
  #xy <- outliers::rm.outlier(xy, fill = FALSE, median = FALSE, opposite = FALSE)

  colnames(xy) <- c("tempin","tempout","power", "time")
  xy <- subset(xy, xy$tempout > -30)
  xy <- subset(xy, xy$tempout < 100)
  xy$power <- xy$power * as.integer(unit_validator)
  summary(xy)

  # should we calculate in the followig loop the energy need/baseline????
  # Energy_need [kW] = SHTC [kW/[Km2] * cdh [Kh] * surface [m2]/ timestamp_length [h]
  # HTC [kW/K] = (energyperformance [kWh/m3] * floor_area [m2] * 3 [m]) / (24 [h/d] * HDD [Kd])
  # SHTC [kW/(Km2)]  = HTC [kW/K] / floor_area [m2]
  # energy_waste[kWh] = SHTC[kW/(Km2) * floor_area[m2] * temp_diff[K] * timestamp_length [h]
  # energy_waste[kW] = HTC[kW/K] * temp_diff[K]


  for(i in 1:nrow(xy))
  {
    row <- xy[i, ]
    #print(row$time)
    mydatetime <- as.POSIXct(row$time)
    myhour<-lubridate::hour(mydatetime)

    out_diff <- (row$tempout - base_temp)
    in_diff <- (row$tempin - base_temp)
    temp_diff <- base_temp - row$tempin
    if ((out_diff > 0) & (in_diff < 0))
    {
      if( myhour>20 || myhour<8 || lubridate::wday(mydatetime)==1 || lubridate::wday(mydatetime)==7)
       { row$cdd <- 0}
      else{row$cdd <- as.numeric((row$tempout - base_temp) / 24)}
      energy_waste_indicative <- temp_diff* htc #measured in kwatt
    }
    else if ((out_diff > 0) & (in_diff > 0))
    {
      if( myhour>20 || myhour<8 || lubridate::wday(mydatetime)==1 || lubridate::wday(mydatetime)==7)
        {row$cdd <-0}
      else{row$cdd <- as.numeric((row$tempout - base_temp) / 24)}
      energy_waste_indicative <- 0
    }
    else if (out_diff < 0)
    {
      row$cdd <- 0
      energy_waste_indicative <- 0
    }
    xy[i, "cdd"] <- row$cdd
    xy[i, "energy_waste"] <- energy_waste_indicative
  }

  print(summary(xy))

  xy$time <- as.POSIXct(xy$time)
  #  xy$cddconsumption <- xy$cdd / xy$power


  # energy need, served as baseline, measured in kW
  #  xy$cddhtc <- htc * xy$cdd
  xy$baseline <- htc * xy$cdd

  # plot1: plot the CDD in an hourly basis
  plot1 <- ggplot2::qplot(x=xy$time, y=xy$cdd,
                          data=xy, na.rm=TRUE,
                          size=I(0.6),
                          main="Cooling Degree Days (in an hourly basis)",
                          xlab="Date", ylab="Degree hours (kh)")+
    #ggplot2::geom_point(na.rm=TRUE, color="red", size=3, pch=4)+
    ggplot2::geom_line(ggplot2::aes(y = xy$cdd, col ="CDD (hourly)"), color="blue") +
    ggplot2::scale_x_datetime(date_breaks = "12 hour") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
    ggplot2::theme(legend.position="bottom", legend.box = "horizontal")

  plot(plot1)

  plot33 <- ggplot2::qplot(x=xy$time, y=xy$power,
                           data=xy, na.rm=TRUE,
                           size=I(0.6),
                           main="Real Power vs Forecasted (degree days)",
                           xlab="Date", ylab="Power (Watt)")+
    ggplot2::geom_line(ggplot2::aes(y = xy$power, colour = "Real Power")) +
    ggplot2::geom_line(ggplot2::aes(y = xy$baseline*1000, colour = "Baseline")) +
    ggplot2::geom_point(y = xy$baseline*1000, na.rm=TRUE, color="blue", size=1, pch=16) +
    #ggplot2::geom_line(ggplot2::aes(y = xy$energy_waste*1000, colour = "Energy Waste")) +
    #ggplot2::geom_point(y = xy$energy_waste*1000, na.rm=TRUE, color="gray", size=2, pch=16) +
    ggplot2::scale_x_datetime(date_breaks = "12 hour") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle =20, hjust = 1)) +
    ggplot2::theme(legend.position="bottom", legend.box = "horizontal") +
    ggplot2::scale_color_manual(name="",values = c("Real Power"="#00ba38", "Forecasted"="#87CEFA")) + ggplot2::expand_limits(y=0)

  # plot(plot33)

  plot34 <- ggplot2::qplot(x=xy$time, y=xy$energy_waste,
                           data=xy, na.rm=TRUE,
                           size=I(0.6),
                           main="Indication for Energy Waste (degree days)",
                           xlab="Date", ylab="Power (Watt)")+
    ggplot2::geom_line(ggplot2::aes(y = xy$energy_waste*1000)) +
    ggplot2::geom_point(y = xy$energy_waste*1000, na.rm=TRUE, color="cornflowerblue", size=1, pch=16) +
    ggplot2::scale_x_datetime(date_breaks = "12 hour") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
    ggplot2::expand_limits(y=0)


  # Use cowplot for preparing one aligned graph with both figures

  #coolingplot2 <- cowplot::plot_grid(plot33, plot34, nrow=2, align="v", rel_heights = c(1.3, 1))
  #plot(coolingplot2)

  #save plot locally
  png(filename="plot0.png")
  plot(plot1)
  dev.off()

  png(filename="plot1.png")
  plott <- gridExtra::grid.arrange(plot33, plot34, ncol=1, nrow = 2, heights=c(2,1))
  plot(plott)
  dev.off()

  #png(filename="plot3.png")
  #plott <- gridExtra::grid.arrange(plot1,plot33, plot34, ncol=1, nrow = 3, heights=c(1,2,1))
  #plot(plott)
  #dev.off()



     #assosiate degree day plot with area
  h <- curl::new_handle()
  curl::handle_setform(h,areaname = area_name, file = curl::form_file("plot0.png", "image/png"))
  req <- curl::curl_fetch_memory(area_url, handle = h)

     h <- curl::new_handle()
     curl::handle_setform(h,areaname = area_name, file = curl::form_file("plot1.png", "image/png"))
     #req <- curl::curl_fetch_memory("http://localhost:8080/api/v1/building/degreedays", handle = h)
     #curl::handle_setform(h,areaname = area_name, file = curl::form_file(plot1))

     req <- curl::curl_fetch_memory(area_url, handle = h)

   }



  # heatingDegreeDaysPlot
  #
  # This is an function
  # which generates a heating Degree day plot.
  #
  #' @examples
  #' DegreeDays::heatingDegreeDaysPlot(datastream,base_temp,surface,area_url,area_name,login_api,building_api,energy_consumption_unit)
  #' base_temp <- 24
  #' surface<- 3382.6
  #' datastream <- "http://hesso-entropy.euprojects.net/api/v1/query/executequery/59e5ca1be7d4380671da0c7b"
  #' area_name<-"iig_v2"
  #' area_url <- "http://hesso-entropy.euprojects.net//api/v1/building/degreedays"
  #' login_api<-"http://hesso-entropy.euprojects.net/api/v1/auth/login"
  #' building_api <- "http://hesso-entropy.euprojects.net/api/v1/building/energy_profile"
  #' @param datastream entropy datastream
  #' @param surface area surface
  #' @param base_temp base temperature
  #' @param area_url area url
  #' @param area_name area name
  #' @return plot3 plot3
  #' @return xy xy
  #' @export
  # curl http://212.101.173.76/ocpu/library/DegreeDays/R/heatingDegreeDaysPlot -d "base_temp=21&surface=3382.6&datastream='http://hesso-entropy.euprojects.net/api/v1/query/executequery/59e5ca1be7d4380671da0c7b'&area_name='iig_v2'&area_url='http://hesso-entropy.euprojects.net//api/v1/building/degreedays'&login_api='http://hesso-entropy.euprojects.net/api/v1/auth/login'&building_api='http://hesso-entropy.euprojects.net/api/v1/building/energy_profile'&energy_consumption_unit='Kwatthour'"
  heatingDegreeDaysPlot <-function (datastream, base_temp, surface,area_url,area_name,login_api,building_api,energy_consumption_unit) {

    unit_validator <- check_energy_consumption_unit(energy_consumption_unit)
    if (unit_validator==0){return;}

    base_temp <- as.numeric(base_temp)
    surface <- as.numeric(surface)
    df <- jsonlite::stream_in(url(datastream))
    dfjson <- jsonlite::fromJSON(jsonlite::toJSON(df))
    mydata <- dfjson$nameValuePairs

    #Get building energy profile
    r <- httr::POST(login_api, body = "{\"username\": \"entropy\",\"password\": \"!entropy!\"}")
    authorization_key = httr::headers(r)$authorization
    #req <- httr::GET(bulding_api, httr::add_headers(Authorization = authorization_key))
    req <- httr::POST(building_api, body = area_name,httr::add_headers(Authorization = authorization_key))
    content <- httr::content(req)
    htc <-  content$nameValuePairs$HeatTransferCoefficient
    print (htc)
    columnnames <- colnames(mydata)

    if(!("temperature_outdoor" %in% colnames(mydata)))
    {
      print("temperature_outdoor is missing. Is your query well-formed?");
      return;
    }
    if(!("temperature_indoor" %in% colnames(mydata)))
    {
      print("temperature_indoor is missing. Is your query well-formed?");
      return;
    }
    if(!("active_power" %in% colnames(mydata)))
    {
      print("active_power is missing. Is your query well-formed?");
      return;
    }

    consumption_columns <-columnnames[grepl('active_power',columnnames)]


    x <- data.frame(mydata$inDateTime$nameValuePairs)
    tempout_index <- grep(columnnames[grepl("temperature_outdoor",columnnames)], colnames(mydata))
    tempin_index <- grep(columnnames[grepl("temperature_indoor",columnnames)], colnames(mydata))

    #if many consumptions aggregate them
    if (length(consumption_columns)>1){
      power_to_aggregate <- dplyr::select(mydata, consumption_columns)
      aggregated_power <- rowSums(power_to_aggregate, na.rm = FALSE)
      xy <- data.frame(mydata[tempin_index],mydata[tempout_index],aggregated_power,x)
    }else{
      power_index <- grep(columnnames[grepl("active_power", columnnames)],colnames(mydata))
      xy <- data.frame(mydata[tempin_index], mydata[tempout_index],mydata[power_index], x)
    }


    xy <- na.omit(xy)
    colnames(xy) <- c("tempin", "tempout", "power", "time")
    xy <- subset(xy, xy$tempout > -30)
    xy <- subset(xy, xy$tempout < 100)
    xy$power <- xy$power * as.integer(unit_validator)
    summary(xy)

    for (i in 1:nrow(xy)) {
      row <- xy[i, ]
      out_diff <- (row$tempout - base_temp)
      in_diff <- (row$tempin - base_temp)
      temp_diff <- (row$tempin - base_temp)
      print("out_diff")
      print(out_diff)
      print(in_diff)
      print(temp_diff)
      mydatetime <- as.POSIXct(row$time)
      myhour<-lubridate::hour(mydatetime)

      if ((out_diff < 0) & (in_diff > 0)) {

        if( myhour>20 || myhour<8 || lubridate::wday(mydatetime)==1 || lubridate::wday(mydatetime)==7)
          { row$hdd <- 0}
        else{row$hdd <- as.numeric((base_temp - row$tempout)/24)}

        energy_waste_indicative <- temp_diff * htc
      }
      else if ((out_diff < 0) & (in_diff < 0)) {
        if(myhour>20 || myhour<8 || lubridate::wday(mydatetime) == 1 || lubridate::wday(mydatetime) == 7){ row$hdd <- 0}
        else{row$hdd <- as.numeric((base_temp - row$tempout)/24)}
        energy_waste_indicative <- 0
      }
      else if ((out_diff > 0)) {
        row$hdd <- 0
        energy_waste_indicative <- 0
      }else {
        row$hdd <- 0
        energy_waste_indicative <- 0
      }
      xy[i, "hdd"] <- row$hdd
      xy[i, "energy_waste"] <- energy_waste_indicative

    }
    print(summary(xy))

    xy$time <- as.POSIXct(xy$time)
    xy$hddconsumption <- xy$hdd / xy$power

    # energy need, served as baseline, measured in kW
    xy$hddhtc <- htc * xy$hdd
    xy$baseline <- htc * xy$hdd


    # plot1: plot the HDD in an hourly basis
    plot1 <- ggplot2::qplot(x=xy$time, y=xy$hdd,
                            data=xy, na.rm=TRUE,
                            size=I(0.6),
                            main="Heating Degree Days (in an hourly basis)",
                            xlab="Date", ylab="Degree hours (kh)")+
      #ggplot2::geom_point(na.rm=TRUE, color="red", size=3, pch=18)+
      ggplot2::geom_line(ggplot2::aes(y = xy$hdd, col ="HDD (hourly)"), color="blue") +
      ggplot2::scale_x_datetime(date_breaks = "12 hour") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
      ggplot2::theme(legend.position="bottom", legend.box = "horizontal")

    plot(plot1)

    plot33 <- ggplot2::qplot(x=xy$time, y=xy$power,
                             data=xy, na.rm=TRUE,
                             size=I(0.6),
                             main="Real Power vs Forecasted (degree days)",
                             xlab="Date", ylab="Power (Watt)")+
      ggplot2::geom_line(ggplot2::aes(y = xy$power, colour = "Real Power")) +
      ggplot2::geom_line(ggplot2::aes(y = xy$baseline*1000, colour = "Forecasted")) +
      ggplot2::geom_point(y = xy$baseline*1000, na.rm=TRUE, color="blue", size=1, pch=16) +
      #ggplot2::geom_line(ggplot2::aes(y = xy$energy_waste*1000, colour = "Energy Waste")) +
      #ggplot2::geom_point(y = xy$energy_waste*1000, na.rm=TRUE, color="gray", size=2, pch=16) +
      ggplot2::scale_x_datetime(date_breaks = "12 hour") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
      ggplot2::theme(legend.position="bottom", legend.box = "horizontal") +
      ggplot2::scale_color_manual(name="",
                                  values = c("Real Power"="#00ba38", "Forecasted"="#87CEFA")) + ggplot2::expand_limits(y=0)

    plot(plot33)

    plot34 <- ggplot2::qplot(x=xy$time, y=xy$energy_waste,
                             data=xy, na.rm=TRUE,
                             size=I(0.6),
                             main="Indication for Energy Waste (degree days)",
                             xlab="Date", ylab="Power (Watt)")+
      ggplot2::geom_line(ggplot2::aes(y = xy$energy_waste*1000)) +
      ggplot2::geom_point(y = xy$energy_waste*1000, na.rm=TRUE, color="gray", size=1, pch=16) +
      ggplot2::scale_x_datetime(date_breaks = "12 hour") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
      ggplot2::expand_limits(y=0)

    plot(plot34)

    # Use cowplot for preparing one aligned graph with both figures

    # heatingplot <- cowplot::plot_grid(plot1, plot33, plot34, nrow=3, align="v", rel_heights = c(1, 1.3, 1))
    # plot(heatingplot)

    #heatingplot2 <- cowplot::plot_grid(plot33, plot34, nrow=2, align="v", rel_heights = c(1.3, 1))
    #plot(heatingplot2)

    #png(filename="plot1.png")
    #gridExtra::grid.arrange(plot1, plot2, ncol=1, nrow = 2)
    #cowplot::plot_grid(plot1, plot33, plot34, nrow=3, align="v")
    #dev.off()


    # #save plot locally
    png(filename="plot0.png")
    plot(plot1)
    dev.off()

    png(filename="plot1.png")
    plott <- gridExtra::grid.arrange(plot33, plot34, ncol=1, nrow = 2, heights=c(2,1))
    plot(plott)
    dev.off()

    #assosiate degree day plot with area
    h <- curl::new_handle()
    curl::handle_setform(h,areaname = area_name, file = curl::form_file("plot0.png", "image/png"))

    req <- curl::curl_fetch_memory(area_url, handle = h)

    h <- curl::new_handle()
    curl::handle_setform(h,areaname = area_name, file = curl::form_file("plot1.png", "image/png"))
    req <- curl::curl_fetch_memory(area_url, handle = h)


    #return(plot3)
  }

  #' @return 1000, 1 or 0
  check_energy_consumption_unit <-function (energy_consumption_unit){
    v <- c('Kwatt','Kwatthour','watt','watthour')

    if (energy_consumption_unit %in% v){

      if (energy_consumption_unit=="Kwatt" || energy_consumption_unit=="watthour"){
        result <- "1000"
      }else  result <- "1"

    }else{
      print(paste("energy_consumption_unit is not valid",energy_consumption_unit, sep=" "))
      result <- "0";
    }

    return(result)
  }
