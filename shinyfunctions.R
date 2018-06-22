library(compiler)

create.treatment.columns.cust <- function(num.p.dates, treat.choices, date.choices,num.t.choices) {
  
  #col.inc <- as.numeric(c(0,4,8,12,16,20,24,28))
  
  col.inc <- as.numeric(0:200)
  treat.choices.offset <- treat.choices + col.inc[date.choices[1]]
  if(num.p.dates >= 2){
    col.inc <- col.inc[date.choices]
    #show(col.inc)
    for(k in 2:num.p.dates){
      
      loop.offset <- treat.choices + col.inc[k]
      #show(loop.offset)
      treat.choices.offset <- c(treat.choices.offset,loop.offset)
    }
  }
  
  return(treat.choices.offset)
  
}
##############################################################################
create.treatment.columns <- function(num.p.dates, treat.choices, date.choices,num.t.choices) {
  
  col.inc <- as.numeric(c(0,4,8,12,16,20,24,28))
  
  
  treat.choices.offset <- treat.choices + col.inc[date.choices[1]]
  if(num.p.dates >= 2){
    col.inc <- col.inc[date.choices]
    #show(col.inc)
    for(k in 2:num.p.dates){
      
      loop.offset <- treat.choices + col.inc[k]
      #show(loop.offset)
      treat.choices.offset <- c(treat.choices.offset,loop.offset)
    }
  }
  
  return(treat.choices.offset)
  
}
##############################################################################
plot.axis.labels <- function(x.start, x.end,ext.x,ext.y,ext.z,treat.range,num.t.choices,x.offset,x,x.vector,
                             y.offset,y.offset.2,treat.choices,num.raw.col,plant.titles,
                             yr.titles,offset.stack,marker,side.heigth.offset,width.offset){
  side.row.col.offset <- (ext.x * c(50,150,250,350))
  stack.row.col.offset <- (ext.z * offset.stack * c(0,1,2,3))
  
  
  r.titles <- c(0,0,1.5,3,6)
  
  text3d(color = "black", x = -500, y = 0, z = 0, text = x.start)#Doy/Dap Start
  text3d(color = "black", x = -500, y = 0, z = (ext.y * treat.range), text = x.end)#Doy/Dap End
  
  if(marker == 0)for(i in 1:num.t.choices)text3d(color = "black", x = side.row.col.offset[i], y = 0, z = -1500, 
                                                 text = r.titles[treat.choices[i]])
  
  if(marker == 1)for(i in 1:num.t.choices)text3d(color = "black", x = 750, y = stack.row.col.offset[i], z = -1500, 
                                                 text = r.titles[treat.choices[i]])
  
  
  title.temp <- as.numeric(1) #Temporary variable to increment title of Treatment
  title.temp.bot <- as.numeric(1) #Temporary variable to increment title of Treatment
  title.offset <- as.numeric(0)#500 * ext.y)
  middle.z <- as.numeric((ext.y * treat.range)/2)
  
  for(d in 1:num.raw.col)
  {
    x.vector[,,d] <- as.numeric(x + x.offset)
    y.offset.2 <- y.offset.2 + 1
    #x.offset <- x.offset + (ext.x * 100)
    x.offset <- x.offset + (ext.x * side.heigth.offset)
    if(y.offset == y.offset.2) 
    {
      x.offset <- as.numeric(0)
      y.offset.2 <- as.numeric(0)
      text3d(color = "black", x = (1500+max(x.vector[,,d])), y = 0, 
             z = (middle.z + title.offset), text = plant.titles[title.temp])#date.choices[title.temp]])
      text3d(color = "black", x = -1000, y = 0, 
             z = (middle.z + title.offset), text = yr.titles[title.temp.bot])
      
      title.temp.bot <- title.temp.bot + 1
      title.offset <- title.offset + (treat.range * ext.y) + width.offset
      title.temp <- title.temp + 1
    }        
    
  }
  
}

##############################################################################
#Side by Side View
side.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,x.offset,y.offset,y.offset.2,x.vector,treatment.array,
                        colorlut,zlim,treat.range,width.offset,session,side.heigth.offset) 
{
  # withProgress(session, 1,num.raw.col, {
  #       setprogress(message = "Loading 3D plots now...")
  progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
  on.exit(progress$close())
  progress$set(message = 'Loading 3D plots now...')
  
  
  
  for(d in 1:num.raw.col)
  {
    show(d)
    progress$set(value = d)
    #Sys.sleep(1)
    z <- ext.z * treatment.array[,,d]
    x.vector[,,d] <- as.numeric(x + x.offset)
    col <- colorlut[ z - zlim[1] + 1]
    #####Apply offset to each surface#######   
    #if(d==1)
      rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
    #else if(d==2)rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
    #save.image(t)
    y.offset.2 <- y.offset.2 + 1
    x.offset <- x.offset + (ext.x * side.heigth.offset)
    if(y.offset == y.offset.2) 
    {
      y <- y + (ext.y * treat.range) + width.offset
      z <- ext.z * treatment.array[,,d]
      x.offset <- as.numeric(0)
      y.offset.2 <- as.numeric(0)
    }        
    
    #Sys.sleep(1)
  }#End of for loop of rgl scene
  
}
#Side by Side View
side.rgl.3d.hu.ct <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,ext.z.hu.ct.h,x.offset,y.offset,y.offset.2,x.vector,treatment.array,
                        colorlut,colorlut.hu.ct.h,zlim,zlim.hu.ct.h,treat.range,width.offset,session,side.heigth.offset) 
{
  # withProgress(session, 1,num.raw.col, {
  #       setprogress(message = "Loading 3D plots now...")
  progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
  on.exit(progress$close())
  progress$set(message = 'Loading 3D plots now...')
  
  
  
  for(d in 1:num.raw.col)
  {
    show(d)
    progress$set(value = d)
    #Sys.sleep(1)
    if(d==1)z <- ext.z * treatment.array[,,d]
    if(d==2)z <- ext.z.hu.ct.h * treatment.array[,,d]
    x.vector[,,d] <- as.numeric(x + x.offset)
    col <- colorlut[ z - zlim[1] + 1]
    col2 <- colorlut.hu.ct.h[ z - zlim.hu.ct.h[1] + 1]
    #####Apply offset to each surface#######   
    #if(d==1)
      rgl.surface(x.vector[,,d], y, z , color=col, alpha=1, front="fill", back="fill")
    #if(d==2)rgl.surface(x.vector[,,d], y, z , color=col2, alpha=1, front="fill", back="fill")
    #save.image(t)
    y.offset.2 <- y.offset.2 + 1
    x.offset <- x.offset + (ext.x * side.heigth.offset)
    if(y.offset == y.offset.2) 
    {
      y <- y + (ext.y * treat.range) + width.offset
      z <- ext.z * treatment.array[,,d]
      x.offset <- as.numeric(0)
      y.offset.2 <- as.numeric(0)
    }        
    
    #Sys.sleep(1)
  }#End of for loop of rgl scene
  
}

##############################################################################
#Stacked View
stack.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,y.offset,y.offset.2,z.vector,treatment.array,
                         colorlut,zlim,treat.range,z.offset,offset.stack,session,width.offset) 
{
  #withProgress(session, min=1,max=num.raw.col, {
  # setProgress(message = "Loading 3D plots now...")
  #      #Sys.sleep(1)
  progress <- shiny::Progress$new(session, min=1,max=num.raw.col)
  on.exit(progress$close())
  progress$set(message = 'Loading 3D plots now...')
  for(d in 1:num.raw.col)
  {
    show(d)
    progress$set(value = d)
    z <- ext.z * treatment.array[,,d]
    z.vector[,,d] <- as.numeric(z + z.offset)
    col <- colorlut[ z - zlim[1] + 1]
    #####Apply offset to each surface#######   
    rgl.surface(x, y, z.vector[,,d], color=col, alpha=1, front="fill", back="fill")
    y.offset.2 <- y.offset.2 + 1
    z.offset <- z.offset + (ext.z * offset.stack)
    if(y.offset == y.offset.2) 
    {
      y <- y + (ext.y * treat.range) + width.offset#100
      z <- ext.z * treatment.array[,,d]
      z.offset <- as.numeric(0)
      y.offset.2 <- as.numeric(0)
    }        
    #Sys.sleep(.5)
  }#End of for loop of rgl scene 
  #})
}
##############################################################################
side.rgl.3d.plot <- cmpfun(side.rgl.3d)
side.rgl.3d.plot.hu.ct <- cmpfun(side.rgl.3d.hu.ct)
stack.rgl.3d.plot <- cmpfun(stack.rgl.3d)
##############################################################################
create.dap <- function(treatment.days,num.treatments,rawdata,doy.treats,midnight,session){
  
  progress <- shiny::Progress$new(session, min=1,max=num.treatments)
  on.exit(progress$close())
  progress$set(message = 'Creating DAP scale now...')
  dap<-rep(rep(seq(91,(treatment.days+91)),each=96),2)
  
  #show(dap[1:10])
  
  dap.start <- matrix(ncol=num.treatments)
  dap.cols <- as.numeric(1)
  
  for(i in 2:(num.treatments+1))
  {
    dap.temp <- which(is.na(rawdata[,i]) == FALSE)
    dap.start[dap.cols] <- dap.temp[1]
    dap.cols <- dap.cols + 1
  }
  show(dap.start)
  start.dap <- matrix(ncol=num.treatments)
  dap.seq <- matrix(nrow = length(rawdata[,2]),ncol=num.treatments)
  for(i in 1:num.treatments)
  {
    progress$set(value = i)
    start.dap[i] <- dap[dap.start[i]] - doy.treats[i]
    day.increment <- start.dap[i]
    dap.start.midnight <- as.numeric(max(which(midnight < dap.start[i])))
    dap.end.midnight <- dap.start.midnight + 1
    dap.seq[1:(midnight[dap.start.midnight]-1),i] <- 0
    #if(start.dap[i] > 1)dap.seq[(96*(dap.start.midnight-start.dap[i])+1):(midnight[dap.start.midnight]-1),i] <- rep(seq(1,as.numeric((start.dap[i]-1))),each=96)
    dap.seq[midnight[dap.start.midnight]:(midnight[dap.end.midnight]-1),i] <- start.dap[i]
    for(j in dap.start.midnight:(length(midnight)-1))
    {
      dap.seq[midnight[j]:(midnight[j+1]-1),i] <- day.increment
      day.increment <- day.increment + 1
    }
    dap.seq[midnight[length(midnight)]:length(rawdata[,2]),i] <- day.increment
    #Sys.sleep(1)
  }
  return(dap.seq)
}
create.dap.matrix <- cmpfun(create.dap)
create.treatment.array <- function(dap.seq,treatment.col1,num.raw.col,treat.range,
                                   num.treatments,rawdata,temp.marker,y.temp)
{
  doy.seq <- dap.seq + 93 
  treatment.array <- array(dim = c(96, treat.range, num.treatments))
  
  for(t in 1:num.raw.col)
  {
    if(temp.marker == 0)#DOY View
    {
      
      if(treat.range > 272 && treatment.col1 < 93){
        temp.start <- 1
        temp.end <- temp.start + 95
        
        for(j in 1:treat.range)
        {      
          if(j >= 1 && j <= (93-treatment.col1)){
            
            treatment.array[,j,t] <- c(0)
            
          }else if(j > (93-treatment.col1) && j <= treat.range) {
            
            treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
            temp.start <- temp.end + 1
            temp.end <- temp.start + 95
          }
        }
        
      }else {
        
        #if(t > 1)doy.seq <- cbind(doy.seq,doy.seq)
        temp.start <- 1#(min(which(doy.seq[,t]==treatment.col1))+1)
        temp.end <- temp.start + 95
        for(j in 1:treat.range)
        {
          if(j >= 1 && j <= (93-treatment.col1)){
            
            treatment.array[,j,t] <- c(0)
            
          }else if(j > (93-treatment.col1) && j <= treat.range) {
            treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
            temp.start <- temp.end + 1
            temp.end <- temp.start + 95
          }
        }
      }
      if(treat.range < 272 && treatment.col1 > 93)doy.seq <- cbind(doy.seq,doy.seq)
    }
    
    if(temp.marker == 1)#DAP View
    {
      temp.start <- (min(which(dap.seq[,t]==treatment.col1))+1)
      temp.end <- temp.start + 95
      for(j in 1:treat.range)
      {      
        
        treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
        temp.start <- temp.end + 1
        temp.end <- temp.start + 95
      } 
    }
    y.temp <- y.temp + 1
    
  }
  
  return(treatment.array)
}
create.treatment.array.cust <- function(treatment.col1,num.raw.col,treat.range,
                                        num.treatments,rawdata,y.temp)
{
  #doy.seq <- dap.seq + 93 
  treatment.array <- array(dim = c(96, treat.range, num.treatments))
  
  for(t in 1:num.raw.col)
  {
    
    temp.start <- 1#(min(which(doy.seq[,t]==treatment.col1))+1)
    temp.end <- temp.start + 95
    for(j in 1:treat.range)
    {
      
      treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
      temp.start <- temp.end + 1
      temp.end <- temp.start + 95
    }    
    
    
    y.temp <- y.temp + 1
  }
  
  return(treatment.array)
}

cr.rawdata <- function(rawdata14,rawdata12,rawdata11,num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
{
  if(num.raw.choices == 1){
    if(max(raw.yr.choices) == 1){
      rawdata <- rawdata14[,c(1,treat.choices.offset)]
    }else if(max(raw.yr.choices) == 2){
      rawdata <- rawdata12[,c(1,treat.choices.offset)]
    }else if(max(raw.yr.choices) == 3){
      rawdata <- rawdata11[,c(1,treat.choices.offset)]
    }
  } else{
    if(num.raw.choices == 2 && min(raw.yr.choices) == 1){
      if(max(raw.yr.choices) == 2){
        rawdata <- rawdata14
        rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata12[,c(treat.choices.offset)])
      }else {
        rawdata <- rawdata14
        rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
      }
    }
    if(num.raw.choices == 2 && min(raw.yr.choices) == 2){
      rawdata <- rawdata12
      rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
    }
    if(num.raw.choices == 3){
      rawdata <- rawdata14
      rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata12[,c(treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
    }
  }
  
  return(rawdata)
}
cr.rawdata.hu.ct <- function(rawdata14,rawdata12,rawdata11,rawdata14.hu,rawdata12.hu,rawdata11.hu,num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
{
  if(num.raw.choices == 1){
    if(max(raw.yr.choices) == 1){
      rawdata <- cbind(rawdata14[,c(1,treat.choices.offset)],rawdata14.hu[,c(treat.choices.offset)])
    }else if(max(raw.yr.choices) == 2){
      rawdata <- cbind(rawdata12[,c(1,treat.choices.offset)],rawdata12.hu[,c(treat.choices.offset)])
    }else if(max(raw.yr.choices) == 3){
      rawdata <- cbind(rawdata11[,c(1,treat.choices.offset)],rawdata11.hu[,c(treat.choices.offset)])
    }
  } else{
    if(num.raw.choices == 2 && min(raw.yr.choices) == 1){
      if(max(raw.yr.choices) == 2){
        rawdata <- rawdata14
        rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata12[,c(treat.choices.offset)])
      }else {
        rawdata <- rawdata14
        rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
      }
    }
    if(num.raw.choices == 2 && min(raw.yr.choices) == 2){
      rawdata <- rawdata12
      rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
    }
    if(num.raw.choices == 3){
      rawdata <- rawdata14
      rawdata <- cbind(rawdata[,c(1,treat.choices.offset)],rawdata12[,c(treat.choices.offset)],rawdata11[,c(treat.choices.offset)])
    }
  }
  
  return(rawdata)
}

create.yr.titles <- function(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates)
{
  if(num.raw.choices == 1){
    if(max(raw.yr.choices) == 1){
      yr.titles <- rep(14,each=num.p.dates)
    }else if(max(raw.yr.choices) == 2){
      yr.titles <- rep(12,each=num.p.dates)
    }else if(max(raw.yr.choices) == 3){
      yr.titles <- rep(11,each=num.p.dates)
      #rawdata <- rawdata11[,c(1,treat.choices.offset)]
    }
  } else{
    if(num.raw.choices == 2 && min(raw.yr.choices) == 1){
      if(max(raw.yr.choices) == 2){
        yr.titles <- rep(c(14,12),each=num.p.dates)
      }else {
        yr.titles <- rep(c(14,11),each=num.p.dates)
      }
    }
    if(num.raw.choices == 2 && min(raw.yr.choices) == 2){
      yr.titles <- rep(c(12,11),each=num.p.dates)
    }
    if(num.raw.choices == 3){
      yr.titles <- rep(c(14,12,11),each=num.p.dates)
    }
  }
  
  return(yr.titles)
}

create.doy.treats <- function(num.raw.choices,raw.yr.choices,treat.choices.offset,num.p.dates,doy.treats14,doy.treats12,
                              doy.treats11)
{
  if(num.raw.choices == 1){
    if(max(raw.yr.choices) == 1){
      doy.treats <- doy.treats14[treat.choices.offset]
    }else if(max(raw.yr.choices) == 2){
      doy.treats <- doy.treats12[treat.choices.offset]
    }else if(max(raw.yr.choices) == 3){
      doy.treats <- doy.treats11[treat.choices.offset]
    }
  } else{
    if(num.raw.choices == 2 && min(raw.yr.choices) == 1){
      if(max(raw.yr.choices) == 2){
        doy.treats <- cbind(doy.treats14[treat.choices.offset],doy.treats12[treat.choices.offset])
      }else {
        doy.treats <- cbind(doy.treats14[treat.choices.offset],doy.treats11[treat.choices.offset])
      }
    }
    if(num.raw.choices == 2 && min(raw.yr.choices) == 2){
      doy.treats <- cbind(doy.treats12[treat.choices.offset],doy.treats11[treat.choices.offset])
    }
    if(num.raw.choices == 3){
      doy.treats <- cbind(doy.treats14[treat.choices.offset],doy.treats12[treat.choices.offset],doy.treats11[treat.choices.offset])
    }
  }
  
  return(doy.treats)
}
create.rawdata <- cmpfun(cr.rawdata)
create.rawdata.hu.ct <- cmpfun(cr.rawdata.hu.ct)
#create.yr.titles <- cmpfun(cr.yr.titles)
#create.doy.treats <- cmpfun(cr.doy.treats)
