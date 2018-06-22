options(rgl.useNULL=TRUE)
options(shiny.maxRequestSize=30*1024^2)


library(shiny)
library(shinyRGL)
library(rgl)
#library(devtools)
library(compiler)
library(plyr)
library(doParallel)
#library(ggplot2)

source("shinyfunctions.R")

#cl <- makeCluster(2)
cl <- makeCluster(6)
registerDoParallel(cl)
#####Check for rgl package installed on machine running program
# required.packages <- c("rgl","shinyRGL","shiny","devtools","compiler","plyr","doParallel","ggplot2")
# new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)


show(getwd())

show("Current Working Directory")
show(getwd())
work.direct <- as.character(getwd())  

#####Clear console screen
cat("\014") 

rawdata14 <- read.csv(file = "MahanCotton2014mod.csv", stringsAsFactors = FALSE,
                      strip.white = TRUE, na.strings = c("NA",""),header = TRUE)
rawdata12 <- read.csv(file = "MahanCotton2012mod.csv", stringsAsFactors = FALSE,
                      strip.white = TRUE, na.strings = c("NA",""),header = TRUE)
rawdata11 <- read.csv(file = "MahanCotton2011mod.csv", stringsAsFactors = FALSE,
                      strip.white = TRUE, na.strings = c("NA",""),header = TRUE)

years <- as.numeric(c(14,12,11))

doy.treats11 <- as.numeric(c(1,rep(102,4),rep(119,4),rep(133,4),rep(145,4),rep(160,4),rep(173,4),rep(192,4)))#2011
doy.treats12 <- as.numeric(c(1,rep(95,4),rep(108,4),rep(122,4),rep(138,4),rep(150,4),rep(164,4),rep(178,4)))#2012
doy.treats14 <- as.numeric(c(1,rep(93,4),rep(105,4),rep(119,4),rep(133,4),rep(154,4),rep(167,4),rep(182,4)))#2014

rawdata14.hu <- read.csv(file = "MahanCotton2014HUMOD.csv", stringsAsFactors = FALSE,
                         strip.white = TRUE, na.strings = c("NA",""),header = TRUE)
rawdata12.hu <- read.csv(file = "MahanCotton2012HUMOD.csv", stringsAsFactors = FALSE,
                         strip.white = TRUE, na.strings = c("NA",""),header = TRUE)
rawdata11.hu <- read.csv(file = "MahanCotton2011HUMOD.csv", stringsAsFactors = FALSE,
                         strip.white = TRUE, na.strings = c("NA",""),header = TRUE)

years.hu <- as.numeric(c(14,12,11))

doy.treats11.hu <- as.numeric(c(1,rep(102,4),rep(119,4),rep(133,4),rep(145,4),rep(160,4),rep(173,4),rep(192,4)))#2011
doy.treats12.hu<- as.numeric(c(1,rep(95,4),rep(108,4),rep(122,4),rep(138,4),rep(150,4),rep(164,4),rep(178,4)))#2012
doy.treats14.hu <- as.numeric(c(1,rep(93,4),rep(105,4),rep(119,4),rep(133,4),rep(154,4),rep(167,4),rep(182,4)))#2014


shinyServer(function(input, output, session) {
  
  observe({
    
    p.height <- as.numeric(input$p.height)
    
    p.height.hu <- as.numeric(input$p.height.hu)
    
    p.height.cust <- as.numeric(input$p.height.cust)
    
    p.height.hu.ct <- as.numeric(input$p.height.cust)  
    
    
    
    output$sctPlot <- renderWebGL({
      
      
      raw.yr.choices <- sort(as.numeric(input$raw.choices))
      num.raw.choices <- length(raw.yr.choices)

      
      treat.choices <- sort(as.numeric(input$water.treatments) + 1) #Because first column is time column
      num.t.choices <- length(treat.choices)
      
      date.choices <- sort(as.numeric(input$plant.dates))
      num.p.dates <- length(as.numeric(input$plant.dates))
      
      num.raw.col <- (num.t.choices * num.p.dates * num.raw.choices)# / num.raw.choices
      
      midnight <- which(rawdata14[,1] == "0:00:00")
      
      ext.x <- as.numeric(input$ext.x)
      ext.y <- as.numeric(input$ext.y)
      ext.z <- as.numeric(input$ext.z)
      
      #####################################################################################
#       zlim <- (c(0,60))*ext.z
#       zlen <- zlim[2]
#       
#       color.start <- input$color.range.start
#       color.end <- input$color.range.end
#       colorlut <- rev(rainbow(zlen, start=color.start, end=color.end, alpha=1)) 
      direct.tempsynbol <- "/TempSymbology"
      new.work.direct <- paste(work.direct, direct.tempsynbol, sep="")
      setwd(new.work.direct)
      col.t <- read.csv(file = "TempSymbologyFull.csv", header = TRUE)
      zlim <- (c(0,67)) * ext.z
      colorlut2 <- seq(1,length(col.t[,1]))
      for (i in 1:length(colorlut2)){
        colorlut2[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
      }
      colorlut <- colorlut2      
#####################################################################################
      
      if(input$mask.b.num > as.numeric(0))
      {
        mask.b.temp <- (as.numeric(input$mask.b.num) + 1) * ext.z  
        colorlut[1:mask.b.temp]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
      }
      
      if(input$mask.a.num > as.numeric(0))
      {
        mask.a.temp <- (as.numeric(input$mask.a.num) + 1) * ext.z  
        colorlut[mask.a.temp:max(zlim)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
      }
      #####################################################################################
      show("Building.....")
      ##############################################################
      treat.choices.offset <- create.treatment.columns(num.p.dates,treat.choices,date.choices,num.t.choices)
      ##############################################################
      rawdata <- create.rawdata(rawdata14,rawdata12,rawdata11,num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
      yr.titles <- create.yr.titles(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
      doy.treats <- create.doy.treats(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates,doy.treats14,doy.treats12,
                                      doy.treats11)
      ##############################################################
      titles <- c('P1','P2','P3','P4','P5','P6','P7')#c(4.03,4.15,4.29,5.13,5.13,6.03,6.16)
      col.titles <- titles[date.choices]
      plant.titles <- rep(col.titles,(num.raw.choices * num.p.dates))
      #############################################################################    
      num.treatments <- as.numeric(ncol(rawdata)-1)
      treatment.days <- round((length(rawdata[,2]) - 1) / 96)
      doy.dap.view <- as.numeric(input$y.view.radio)
      if(doy.dap.view == 2){
        treatment.col1 <- as.numeric(input$n.start)#1)#readline("Enter the start of DAP -----> "))
        treatment.col2 <- as.numeric(input$n.end)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
      }
      if(doy.dap.view == 1){
        treatment.col1 <- as.numeric(input$n.start)#1)#readline("Enter the start of DAP -----> "))
        treatment.col2 <- as.numeric(input$n.end)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
      }
      treat.range <- treatment.col2 - treatment.col1
      #####################################################################################
      dap.seq <- create.dap.matrix(treatment.days,num.treatments,rawdata,doy.treats,midnight,session)
      #####Loop for Sensor dataset that will be graphed####################################
      if(input$y.view.radio == 1)temp.marker <- as.numeric(0)#DOY View
      if(input$y.view.radio == 2)temp.marker <- as.numeric(1)#DAP View
      treatment.array <- create.treatment.array(dap.seq,treatment.col1,num.raw.col,treat.range,
                                                num.treatments,rawdata,temp.marker,2)
      #####Check for canopy temperature outliners > 150####################################
      treatment.array[treatment.array > 100] <- c(0)
      treatment.array[treatment.array < 0] <- c(0)
      treatment.array[is.na(treatment.array)] <- c(0)
      #show("Changed all temperature values less than 0 & greater than 100 to-----> 0") 
      ##############################################################################
      open3d()
      bg3d("grey")
      #axes3d()
      phi.angle <- as.numeric(input$pov.angle) 
      ##############################################################################
      clear3d(type=("lights"))
      view3d( theta=-90, phi=phi.angle)
      ##############################################################################
      #Dap and Dop x axis view options
      x.start <- treatment.col1
      x.end <- treatment.col2 
      ##############################################################################
      #####Assign variables from treatment matrix to variables used in rgl.surface
      x <- ext.x * (1:96) #TOD
      y <- ext.y *(1:treat.range)
      z.vector <- array(dim = c(96, treat.range, num.treatments))
      x.vector <- z.vector
      offset.stack <- as.numeric(input$stack.offset)
      width.offset <- as.numeric(input$side.width.offset)
      side.heigth.offset <- as.numeric(input$side.heigth.offset)
      #####################################################################################
      progress <- shiny::Progress$new(session,"working")
      on.exit(progress$close())
      progress$set(message = 'Working...')
      if(as.numeric(input$view.radio) == 1){
        l_ply(1,transform,
              stack.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,num.t.choices,0,z.vector,treatment.array,
               colorlut,zlim,treat.range,0,offset.stack,session,width.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(1)
      }else if(as.numeric(input$view.radio) == 2){
        l_ply(1,transform,
              side.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,0,num.t.choices,0,x.vector,treatment.array,
               colorlut,zlim,treat.range,width.offset,session,side.heigth.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(0)
        
      }
      if(as.numeric(input$labels) == 1)plot.axis.labels(x.start, x.end,ext.x,ext.y,ext.z,treat.range,
                                                        num.t.choices,0,x,x.vector,
                                                        num.t.choices,0,treat.choices,num.raw.col,
                                                        plant.titles,yr.titles,offset.stack,marker,side.heigth.offset,width.offset)
      ####################################################################################
    },height = p.height)
    ###################################################################################################################################################################
    output$sctPlot.cust <- renderWebGL({
      
    inFile <- input$file1
      
    if(is.null(inFile))
        return(NULL)
      
      
      rawdata.cust <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
      updateCheckboxGroupInput(session, "plant.dates.cust",choices = names(rawdata.cust), selected = input$plant.dates.cust)
      rawdata.cust[, input$plant.date.cust, drop = FALSE]
      
      raw.yr.choices <- sort(as.numeric(1))#input$raw.choices))
      num.raw.choices <- length(raw.yr.choices)
      
      
      temp <- names(rawdata.cust)
      date.temp <- as.character(input$plant.dates.cust)

      date.temp.match <- match(temp,date.temp)
      date.choices <- which(date.temp.match != is.na(date.temp.match))#c(which(temp[i] == date.temp[i]))#input$plant.dates.cust)#sort(as.numeric(input$plant.dates.cust))
      num.p.dates <- length(date.choices)#as.numeric(input$plant.dates.cust))
      
      num.raw.col <- num.p.dates#(num.t.choices * num.p.dates * num.raw.choices)# / num.raw.choices

      midnight <- which(rawdata.cust[,1] == "0:00:00")
      
      ext.x <- as.numeric(input$ext.x.cust)
      ext.y <- as.numeric(input$ext.y.cust)
      ext.z <- as.numeric(input$ext.z.cust)
      
    depth.plots <- as.numeric(input$d.plots)
#####################################################################################
#       zlim <- (c(0,60))*ext.z
#       zlen <- zlim[2]
#       
#       color.start <- input$color.range.start.cust
#       color.end <- input$color.range.end.cust
#       colorlut <- rev(rainbow(zlen, start=color.start, end=color.end, alpha=1)) # .755height color lookup table
      direct.tempsynbol <- "/TempSymbology"
      new.work.direct <- paste(work.direct, direct.tempsynbol, sep="")
      setwd(new.work.direct)
      col.t <- read.csv(file = "TempSymbologyFull.csv", header = TRUE)
      zlim <- (c(0,67)) * ext.z
      colorlut2 <- seq(1,length(col.t[,1]))
      for (i in 1:length(colorlut2)){
        colorlut2[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
      }
      colorlut <- colorlut2      
      
######################################################################################
      if(input$mask.b.num.cust > as.numeric(0))
      {
        mask.b.temp <- (as.numeric(input$mask.b.num.cust) + 1) * ext.z  
        colorlut[1:mask.b.temp]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
      }
      
      if(input$mask.a.num.cust > as.numeric(0))
      {
        mask.a.temp <- (as.numeric(input$mask.a.num.cust) + 1) * ext.z  
        colorlut[mask.a.temp:max(zlim)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
      }
#####################################################################################
      show("Building.....")
##############################################################
treat.choices.offset <- date.choices
##############################################################
  rawdata <- rawdata.cust[,c(1,date.choices)]
      #rawdata <- create.rawdata(rawdata14,rawdata12,rawdata11,num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
      #yr.titles <- create.yr.titles(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
      #doy.treats <- create.doy.treats(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates,doy.treats14,doy.treats12,
      #                                doy.treats11)
##############################################################
      titles <- c('P1','P2','P3','P4','P5','P6','P7')#c(4.03,4.15,4.29,5.13,5.13,6.03,6.16)
      col.titles <- titles[date.choices]
      plant.titles <- rep(col.titles,(num.raw.choices * num.p.dates))
      #############################################################################    
      num.treatments <- as.numeric(ncol(rawdata)-1)
      treatment.days <- round((length(rawdata[,2]) - 1) / 96)
      doy.dap.view <- as.numeric(input$y.view.radio.cust)
      treatment.col1 <- as.numeric(input$n.start.cust)#1)#readline("Enter the start of DAP -----> "))
      treatment.col2 <- as.numeric(input$n.end.cust)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
      treat.range <- treatment.col2 - treatment.col1
      #####################################################################################
      #dap.seq <- create.dap.matrix(treatment.days,num.treatments,rawdata,doy.treats,midnight,session)
      #####Loop for Sensor dataset that will be graphed####################################
      #if(input$y.view.radio.cust == 1)temp.marker <- as.numeric(0)#DOY View
      #if(input$y.view.radio.cust == 2)temp.marker <- as.numeric(1)#DAP View
      treatment.array <- create.treatment.array.cust(treatment.col1,num.raw.col,treat.range,
                                                     num.treatments,rawdata,2)
      #####Check for canopy temperature outliners > 150####################################
      treatment.array[treatment.array > 100] <- c(0)
      treatment.array[treatment.array < 0] <- c(0)
      treatment.array[is.na(treatment.array)] <- c(0)
      #show("Changed all temperature values less than 0 & greater than 100 to-----> 0") 
      ##############################################################################
      open3d()
      bg3d("grey")
      #axes3d()
      phi.angle <- as.numeric(input$pov.angle.cust) 
      ##############################################################################
      clear3d(type=("lights"))
      view3d( theta=-90, phi=phi.angle)
      ##############################################################################
      #Dap and Dop x axis view options
      x.start <- treatment.col1
      x.end <- treatment.col2 
      ##############################################################################
      #####Assign variables from treatment matrix to variables used in rgl.surface
      x <- ext.x * (1:96) #TOD
      y <- ext.y *(1:treat.range)
      z.vector <- array(dim = c(96, treat.range, num.treatments))
      x.vector <- z.vector
      offset.stack <- as.numeric(input$stack.offset.cust)
      width.offset <- as.numeric(input$side.width.offset.cust)
      side.heigth.offset <- as.numeric(input$side.heigth.offset.cust)
      #####################################################################################
      progress <- shiny::Progress$new(session,"working")
      on.exit(progress$close())
      progress$set(message = 'Working...')
      if(as.numeric(input$view.radio.cust) == 1){
        l_ply(1,transform,
              stack.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,depth.plots,0,z.vector,treatment.array,
               colorlut,zlim,treat.range,0,offset.stack,session,width.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(1)
      }else if(as.numeric(input$view.radio.cust) == 2){
        l_ply(1,transform,
              side.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,0,depth.plots,0,x.vector,treatment.array,
               colorlut,zlim,treat.range,width.offset,session,side.heigth.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(0)
        
      }
#       if(as.numeric(input$labels.cust) == 1)plot.axis.labels(x.start, x.end,ext.x,ext.y,ext.z,treat.range,
#                                                              num.t.choices,0,x,x.vector,
#                                                              num.t.choices,0,treat.choices,num.raw.col,
#                                                              plant.titles,yr.titles,offset.stack,marker,side.heigth.offset,width.offset)
      
      ####################################################################################
    },height = p.height.cust)

output$sctPlot.hu.ct <- renderWebGL({
  

  #updateCheckboxGroupInput(session, "plant.dates.cust",choices = names(rawdata.cust), selected = input$plant.dates.cust)
  #rawdata.cust[, input$plant.date.cust, drop = FALSE]
  
  #raw.yr.choices <- sort(as.numeric(1))#input$raw.choices))
  #num.raw.choices <- length(raw.yr.choices)
  
  
  #temp <- names(rawdata.cust)
  #date.temp <- as.character(input$plant.dates.cust)
  
  #date.temp.match <- match(temp,date.temp)
  #date.choices <- which(date.temp.match != is.na(date.temp.match))#c(which(temp[i] == date.temp[i]))#input$plant.dates.cust)#sort(as.numeric(input$plant.dates.cust))
  #num.p.dates <- length(date.choices)#as.numeric(input$plant.dates.cust))
  
  #num.raw.col <- num.p.dates#(num.t.choices * num.p.dates * num.raw.choices)# / num.raw.choices
  raw.yr.choices <- sort(as.numeric(input$raw.choices))
  num.raw.choices <- length(raw.yr.choices)
  
  
  treat.choices <- sort(as.numeric(input$water.treatments) + 1) #Because first column is time column
  num.t.choices <- length(treat.choices)
  
  date.choices <- sort(as.numeric(input$plant.dates))
  num.p.dates <- length(as.numeric(input$plant.dates))
  
  num.raw.col <- (num.t.choices * num.p.dates * (2*num.raw.choices))
  
  midnight <- which(rawdata14[,1] == "0:00:00")
  
  ext.x <- as.numeric(input$ext.x.hu.ct)
  ext.y <- as.numeric(input$ext.y.hu.ct)
  ext.z <- as.numeric(input$ext.z.hu.ct.c)
  
  ext.z.hu.ct.h <- as.numeric(input$ext.z.hu.ct.h)
  
  depth.plots <- as.numeric(input$d.plots)
  #####################################################################################
  zlim <- (c(0,60))*ext.z
  zlen <- zlim[2]
  
 
  zlim.hu.ct.h <- (c(0,1))*ext.z.hu.ct.h
  zlen.hu.ct.h <- zlim.hu.ct.h[2]
  

  
  color.start <- input$color.range.start.hu.ct.c
  color.end <- input$color.range.end.hu.ct.c
  colorlut <- rev(rainbow(zlen, start=color.start, end=color.end, alpha=1)) # .755height color lookup table
    
  color.start.hu.ct.h <- input$color.range.start.hu.ct.h
  color.end.hu.ct.h <- input$color.range.end.hu.ct.h
  colorlut.hu.ct.h <- rev(rainbow(zlen.hu.ct.h, start=color.start.hu.ct.h, end=color.end.hu.ct.h, alpha=1)) # .755height color lookup table
  
#   show(colorlut)
#   show(colorlut.hu.ct.h)
  
  if(input$mask.b.num.hu.ct > as.numeric(0))
  {
    mask.b.temp.ct <- (as.numeric(input$mask.b.num.hu.ct) + 1) * ext.z  
    colorlut[1:mask.b.temp.ct]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
    mask.b.temp.hu.ct.h <- ((mask.b.temp.ct*100) / 600) #* ext.z.hu.ct.h
    colorlut.hu.ct.h[1:mask.b.temp.hu.ct.h]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
    show(match(colorlut,"#FFFFFFFF"))#mask.b.temp.ct)
    show(match(colorlut.hu.ct.h,"#FFFFFFFF"))#mask.b.temp.ct)
  }

  if(input$mask.a.num.hu.ct > as.numeric(0))
  {
    mask.a.temp.ct <- (as.numeric(input$mask.a.num.hu.ct) + 1) * ext.z  
    colorlut[mask.a.temp.ct:max(zlim)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
  }
  #####################################################################################

#   
#   if(input$mask.b.num.hu > as.numeric(0))
#   {
#     mask.b.temp.hu.ct.h <- (as.numeric(input$mask.b.num.hu.ct.h) + 1) * ext.z 
#     colorlut.hu[1:mask.b.temp.hu.ct.h]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
#   }
#   
#   if(input$mask.a.num.hu > as.numeric(0))
#   {
#     mask.a.temp.hu.ct.h <- (as.numeric(input$mask.a.num.hu.ct.h) + 1) * ext.z  
#     colorlut.hu[mask.a.temp.hu.ct.h:max(zlim.hu)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
#   }
  #####################################################################################
  show("Building.....")
  ##############################################################
  treat.choices.offset <- create.treatment.columns(num.p.dates,treat.choices,date.choices,num.t.choices)
  ##############################################################
  #rawdata <- rawdata.hu.ct[,c(1,date.choices)]
  rawdata <- create.rawdata.hu.ct(rawdata14,rawdata12,rawdata11,rawdata14.hu,rawdata12.hu,rawdata11.hu,num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
  #rawdata <- rawdata[!is.na(rawdata)]
  #rawdata[is.na(rawdata)] <- c('')
  #write.csv(rawdata,file="rawdatadebug.csv",row.names=FALSE)
  yr.titles <- create.yr.titles(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
  doy.treats <- create.doy.treats(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates,doy.treats14,doy.treats12,
                                  doy.treats11)
  ##############################################################
  titles <- c('P1','P2','P3','P4','P5','P6','P7')#c(4.03,4.15,4.29,5.13,5.13,6.03,6.16)
  col.titles <- titles[date.choices]
  plant.titles <- rep(col.titles,(num.raw.choices * num.p.dates))
  #############################################################################    
  num.treatments <- as.numeric(ncol(rawdata)-1)
  treatment.days <- round((length(rawdata[,2]) - 1) / 96)
  doy.dap.view <- as.numeric(input$y.view.radio.hu.ct)
  treatment.col1 <- as.numeric(input$n.start.hu.ct)#1)#readline("Enter the start of DAP -----> "))
  treatment.col2 <- as.numeric(input$n.end.hu.ct)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
  treat.range <- treatment.col2 - treatment.col1
  #####################################################################################
  dap.seq <- create.dap.matrix(treatment.days,num.treatments,rawdata,doy.treats,midnight,session)
  #####Loop for Sensor dataset that will be graphed####################################
  if(input$y.view.radio.hu.ct == 1)temp.marker <- as.numeric(0)#DOY View
  if(input$y.view.radio.hu.ct == 2)temp.marker <- as.numeric(1)#DAP View
  treatment.array <- create.treatment.array(dap.seq,treatment.col1,num.raw.col,treat.range,
                                            num.treatments,rawdata,temp.marker,2)
  #####Check for canopy temperature outliners > 150####################################
  treatment.array[treatment.array > 100] <- c(0)
  treatment.array[treatment.array < 0] <- c(0)
  treatment.array[is.na(treatment.array)] <- c(0)
  #show("Changed all temperature values less than 0 & greater than 100 to-----> 0") 
  ##############################################################################
  open3d()
  bg3d("grey")
  #axes3d()
  phi.angle <- as.numeric(input$pov.angle.hu.ct) 
  ##############################################################################
  clear3d(type=("lights"))
  view3d( theta=-90, phi=phi.angle)
  ##############################################################################
  #Dap and Dop x axis view options
  x.start <- treatment.col1
  x.end <- treatment.col2 
  ##############################################################################
  #####Assign variables from treatment matrix to variables used in rgl.surface
  x <- ext.x * (1:96) #TOD
  y <- ext.y *(1:treat.range)
  z.vector <- array(dim = c(96, treat.range, num.treatments))
  x.vector <- z.vector
  offset.stack <- as.numeric(input$stack.offset.hu.ct)
  width.offset <- as.numeric(input$side.width.offset.hu.ct)
  side.heigth.offset <- as.numeric(input$side.heigth.offset.hu.ct)
  #####################################################################################
  progress <- shiny::Progress$new(session,"working")
  on.exit(progress$close())
  progress$set(message = 'Working...')
  if(as.numeric(input$view.radio.hu.ct) == 1){
    l_ply(1,transform,
          stack.rgl.3d.plot
          (num.raw.col,x,y,z,ext.x,ext.y,ext.z,depth.plots,0,z.vector,treatment.array,
           colorlut,zlim,treat.range,0,offset.stack,session,width.offset),
          #.progress="working",
          .parallel=TRUE)
    marker <- as.numeric(1)
  }else if(as.numeric(input$view.radio.hu.ct) == 2){
    l_ply(1,transform,
          side.rgl.3d.plot.hu.ct
          (num.raw.col,x,y,z,ext.x,ext.y,ext.z,ext.z.hu.ct.h,0,depth.plots,0,x.vector,treatment.array,
           colorlut,colorlut.hu.ct.h,zlim,zlim.hu.ct.h,treat.range,width.offset,session,side.heigth.offset),
          #.progress="working",
          .parallel=TRUE)
    marker <- as.numeric(0)
    
  }
#         if(as.numeric(input$labels.hu.ct) == 1)plot.axis.labels(x.start, x.end,ext.x,ext.y,ext.z,treat.range,
#                                                                num.t.choices,0,x,x.vector,
#                                                                num.t.choices,0,treat.choices,num.raw.col,
#                                                                plant.titles,yr.titles,offset.stack,marker,side.heigth.offset,width.offset)
  
  ####################################################################################
},height = p.height.hu.ct)


################################################################################################################################################
    output$downloadData.14 <- downloadHandler(
      filename = function() { 
        paste('MahanCotton2014', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(rawdata14, file, row.names = FALSE)
      }
    )
    output$downloadData.hu.14 <- downloadHandler(
      filename = function() { 
        paste('MahanCotton2014HUmod', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(rawdata14.hu, file, row.names = FALSE)
      }
    )
    
    output$downloadData.hu.12 <- downloadHandler(
      filename = function() { 
        paste('MahanCotton2012HUmod', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(rawdata12.hu, file, row.names = FALSE)
      }
    )
output$downloadData.hu.12 <- downloadHandler(
  filename = function() { 
    paste('MahanCotton2012HUmod', '.csv', sep='') 
  },
  content = function(file) {
    write.csv(rawdata12.hu, file, row.names = FALSE)
  }
)
  
output$downloadData.hu.11 <- downloadHandler(
      filename = function() { 
        paste('MahanCotton2011HUmod', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(rawdata11.hu, file, row.names = FALSE)
      }
    )

output$downloadData.hu.11 <- downloadHandler(
  filename = function() { 
    paste('MahanCotton2011HUmod', '.csv', sep='') 
  },
  content = function(file) {
    write.csv(rawdata11.hu, file, row.names = FALSE)
  }
)

############################################################################################################################################################
output$mytable14 <- renderDataTable({
      rawdata14[, input$show_vars.14, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
output$mytable14.hu <- renderDataTable({
      rawdata14.hu[, input$show_vars.14.hu, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
    
output$mytable12 <- renderDataTable({
      rawdata12[, input$show_vars.12, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
output$mytable12.hu <- renderDataTable({
  rawdata12.hu[, input$show_vars.12.hu, drop = FALSE]}, options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    pageLength = 15
  )
)
    
output$mytable11 <- renderDataTable({
      rawdata11[, input$show_vars.11, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
output$mytable14.hu <- renderDataTable({
  rawdata11.hu[, input$show_vars.11.hu, drop = FALSE]}, options = list(
    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
    pageLength = 15
  )
)
################################################################################################################################################################
    
output$mytable.cust <- renderDataTable({
      inFile <- input$file1
      
      if(is.null(inFile))
        return(NULL)
      
      
      rawdata.cust <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
      updateCheckboxGroupInput(session, "show_vars.cust",choices = names(rawdata.cust), selected =input$show_vars.cust)
      rawdata.cust[, input$show_vars.cust, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
      
    )
    
############################################################################################################################################################
    output$sctPlot.hu <- renderWebGL({
      
      raw.yr.choices.hu <- sort(as.numeric(input$raw.choices.hu))
      num.raw.choices.hu <- length(raw.yr.choices.hu)
      
      treat.choices.hu <- (sort(as.numeric(input$water.treatments.hu)) + 1) #Because first column is time column
      num.t.choices.hu <- length(treat.choices.hu)
      
      date.choices.hu <- sort(as.numeric(input$plant.dates.hu))
      num.p.dates.hu <- length(as.numeric(input$plant.dates.hu))
      
      num.raw.col.hu <- (num.t.choices.hu * num.p.dates.hu * num.raw.choices.hu)# / num.raw.choices
      
      midnight.hu <- which(rawdata14.hu[,1] == "0:00:00")
      
      ext.x.hu <- as.numeric(input$ext.x.hu)
      ext.y.hu <- as.numeric(input$ext.y.hu)
      ext.z.hu <- as.numeric(input$ext.z.hu)
      
      #####################################################################################
      zlim.hu <- (c(0,1))*ext.z.hu
      zlen.hu <- zlim.hu[2]
      
      color.start.hu <- input$color.range.start.hu
      color.end.hu <- input$color.range.end.hu
      colorlut.hu <- rev(rainbow(zlen.hu, start=color.start.hu, end=color.end.hu, alpha=1)) # .755height color lookup table
      
      if(input$mask.b.num.hu > as.numeric(0))
      {
        mask.b.temp.hu <- (as.numeric(input$mask.b.num.hu) + 1) * ext.z.hu 
        colorlut.hu[1:mask.b.temp.hu]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
      }
      
      if(input$mask.a.num.hu > as.numeric(0))
      {
        mask.a.temp.hu <- (as.numeric(input$mask.a.num.hu) + 1) * ext.z.hu  
        colorlut.hu[mask.a.temp.hu:max(zlim.hu)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
      }
      #####################################################################################
      show("Building.....")
      ##############################################################
      treat.choices.offset.hu <- create.treatment.columns(num.p.dates.hu,treat.choices.hu,date.choices.hu,num.t.choices.hu)
      ##############################################################
      rawdata.hu <- create.rawdata(rawdata14.hu,rawdata12.hu,rawdata11.hu,num.raw.choices.hu,raw.yr.choices.hu,treat.choices.offset.hu,num.p.dates.hu)
      yr.titles.hu <- create.yr.titles(num.raw.choices.hu,raw.yr.choices.hu,treat.choices.offset.hu,num.p.dates.hu)
      doy.treats.hu <- create.doy.treats(num.raw.choices.hu,raw.yr.choices.hu,treat.choices.offset.hu,num.p.dates.hu,doy.treats14.hu,doy.treats12.hu,
                                         doy.treats11.hu)
      ##############################################################
      titles.hu <- c('P1','P2','P3','P4','P5','P6','P7')#c(4.03,4.15,4.29,5.13,5.13,6.03,6.16)
      col.titles.hu <- titles.hu[date.choices.hu]
      plant.titles.hu <- rep(col.titles.hu,(num.raw.choices.hu * num.p.dates.hu))
      #############################################################################    
      num.treatments.hu <- as.numeric(ncol(rawdata.hu)-1)
      treatment.days.hu <- round((length(rawdata.hu[,2]) - 1) / 96)
      doy.dap.view.hu <- as.numeric(input$y.view.radio.hu)
      
      treatment.col1.hu <- as.numeric(input$n.start.hu)#1)#readline("Enter the start of DAP -----> "))
      treatment.col2.hu <- as.numeric(input$n.end.hu)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
      
      treat.range.hu <- treatment.col2.hu - treatment.col1.hu
      #####################################################################################
      dap.seq.hu <- create.dap.matrix(treatment.days.hu,num.treatments.hu,rawdata.hu,doy.treats.hu,midnight.hu,session)
      #####Loop for Sensor dataset that will be graphed####################################
      if(input$y.view.radio.hu == 1)temp.marker.hu <- as.numeric(0)#DOY View
      if(input$y.view.radio.hu == 2)temp.marker.hu <- as.numeric(1)#DAP View
      treatment.array.hu <- create.treatment.array(dap.seq.hu,treatment.col1.hu,num.raw.col.hu,treat.range.hu,
                                                   num.treatments.hu,rawdata.hu,temp.marker.hu,2)
      #####Check for canopy temperature outliners > 150####################################
      treatment.array.hu[treatment.array.hu > 100] <- c(0)
      treatment.array.hu[treatment.array.hu < 0] <- c(0)
      treatment.array.hu[is.na(treatment.array.hu)] <- c(0)
      #show("Changed all temperature values less than 0 & greater than 100 to-----> 0") 
      ##############################################################################
      open3d()
      bg3d("grey")
      #axes3d()
      phi.angle.hu <- as.numeric(input$pov.angle.hu) 
      ##############################################################################
      clear3d(type=("lights"))
      view3d( theta=-90, phi=phi.angle.hu)
      ##############################################################################
      #Dap and Dop x axis view options
      x.start.hu <- treatment.col1.hu
      x.end.hu <- treatment.col2.hu
      ##############################################################################
      #####Assign variables from treatment matrix to variables used in rgl.surface
      x.hu <- ext.x.hu * (1:96) #TOD
      y.hu <- ext.y.hu *(1:treat.range.hu)
      z.vector.hu <- array(dim = c(96, treat.range.hu, num.treatments.hu))
      x.vector.hu <- z.vector.hu
      offset.stack.hu <- as.numeric(input$stack.offset.hu)
      width.offset.hu <- as.numeric(input$side.width.offset.hu)
      side.heigth.offset.hu <- as.numeric(input$side.heigth.offset.hu)
      #####################################################################################
      progress <- shiny::Progress$new(session,"working")
      on.exit(progress$close())
      progress$set(message = 'Working...')
      if(as.numeric(input$view.radio.hu) == 1){
        l_ply(1,transform,
              stack.rgl.3d.plot
              (num.raw.col.hu,x.hu,y.hu,z.hu,ext.x.hu,ext.y.hu,ext.z.hu,num.t.choices.hu,0,z.vector.hu,treatment.array.hu,
               colorlut.hu,zlim.hu,treat.range.hu,0,offset.stack.hu,session,width.offset.hu),
              #.progress="working",
              .parallel=TRUE)
        marker.hu <- as.numeric(1)
      }else if(as.numeric(input$view.radio.hu) == 2){
        l_ply(1,transform,
              side.rgl.3d.plot
              (num.raw.col.hu,x.hu,y.hu,z.hu,ext.x.hu,ext.y.hu,ext.z.hu,0,num.t.choices.hu,0,x.vector.hu,treatment.array.hu,
               colorlut.hu,zlim.hu,treat.range.hu,width.offset.hu,session,side.heigth.offset.hu),
              #.progress="working",
              .parallel=TRUE)
        marker.hu <- as.numeric(0)
        
      }
      if(as.numeric(input$labels.hu) == 1)plot.axis.labels(x.start.hu, x.end.hu,ext.x.hu,ext.y.hu,ext.z.hu,treat.range.hu,
                                                           num.t.choices.hu,0,x.hu,x.vector.hu,
                                                           num.t.choices.hu,0,treat.choices.hu,num.raw.col.hu,
                                                           plant.titles.hu,yr.titles.hu,offset.stack.hu,marker.hu,side.heigth.offset.hu,width.offset.hu)
      
    },height = p.height.hu)
################################################################################################################################################################
  })#End of Observe
})
