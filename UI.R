# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)
library(shiny)
library(shinyRGL)


rawdata14 <- read.csv(file = "MahanCotton2014mod.csv", header = TRUE)
rawdata12 <- read.csv(file = "MahanCotton2012mod.csv", header = TRUE)
rawdata11 <- read.csv(file = "MahanCotton2011mod.csv", header = TRUE)
rawdata.cust <- rawdata14#eead.csv(file = "MC2.csv", header = TRUE)

rawdata14.hu <- read.csv(file = "MahanCotton2014HUMOD.csv", header = TRUE)
rawdata12.hu <- read.csv(file = "MahanCotton2012HUMOD.csv", header = TRUE)
rawdata11.hu <- read.csv(file = "MahanCotton2011HUMOD.csv", header = TRUE)

today <- as.character(Sys.Date())
#version <- format(today, format = "%m%d%Y")
version <- paste("Clayton Dorrity's 3D PLOTING v",today)
shinyUI(navbarPage(version,
                   tabPanel("Year(11,12,14) Plot",             
                            splitLayout(cellWidths = c("20%", "80%"),    
                                        column(7,
                                               #sidebarPanel(
                                               wellPanel(h5("Axis Labels"),  
                                                         radioButtons("labels", label=NULL, 
                                                                      choices = list('ON' = 1, 'OFF' = 2),
                                                                      selected = 2)
                                               ),
                                               wellPanel(
                                                 checkboxGroupInput("raw.choices", label = h5("Cotton Season Year"), 
                                                                    choices = list("2014" = 1, "2012" = 2, "2011" = 3),
                                                                    selected = 1),#list(1,2)),
                                                 checkboxGroupInput("water.treatments", label = h5("Irrigation Treatment"), 
                                                                    choices = list('6.0mm' = 4,'3.0mm'=3,'1.5mm'=2,'0.0mm'=1),
                                                                    selected = 1),#list(1,4)),
                                                 checkboxGroupInput("plant.dates", label = h5("Planting Dates"), 
                                                                    choices = list('P1' = 1,'P2'= 2,'P3'= 3,'P4'=4,'P5'=5,
                                                                                   'P6'= 6,'P7'= 7),#list('4.03' = 1,'4.15'= 2,'4.29'= 3,'S-5.13'=4,'N-5.13'=5,'6.03'= 6,'6.16'= 7),#'7.01'=8),
                                                                    selected = 1)#list(1,2,3))
                                               ),
                                               hr(),
                                               sliderInput("p.height", label = h5("Plot Height"),min = 700,max = 4000,
                                                           value = 700, step = 100),
                                               hr(),
                                               
                                               submitButton(h5("APPLY PLOT CHOICES")),
                                               #actionButton("submitButton", "APPLY PLOT CHOICES"),
                                               hr()
                                        ),
                                        webGLOutput("sctPlot", height = "100%")
                            ),
                            splitLayout(
                              column(7,
                                     wellPanel(h5("Mask Temperatures(C)"),
                                               numericInput("mask.b.num", label = h5("Below:"), value = 0),
                                               numericInput("mask.a.num", label = h5("Above:"), value = 0)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("View"),
                                               radioButtons("y.view.radio",label = NULL,choices = list("Day of Year (DOY)" = 1, "Days after Planting (DAP)" = 2), 
                                                            selected = 1),
                                               numericInput("n.start",label = h5("DOY/DAP Start"), value = 93),
                                               numericInput("n.end",label = h5("DOY/DAP End"), value = 365),
                                               sliderInput("pov.angle", label = h5("Point of View Angle"),min = 0,max = 360,
                                                           value = 90, step = 15)
                                     )
                                     
                              ),
                              column(7, 
                                     wellPanel(h5("Extrapolate"),
                                               numericInput("ext.x", label = h5("TOD(x) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.y", label = h5("DOY/DAP(y) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.z", label = h5("Surface(z) axis"),value = 1)#,min = 1,max = 100,
                                               #value = 15, step = 1)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("Plot Arrangement"),
                                               radioButtons("view.radio",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
                                                            selected = 2),
                                               numericInput("stack.offset", label = h5("Stacked View Plot Offset"),value = 100),#,min = 0,max = 1000,
                                               #value = 100, step = 25),
                                               numericInput("side.width.offset", label = h5("Side by Side Width Offset"),value = 100),#,min = 0,max = 10000,
                                               #value = 1000, step = 1000),
                                               numericInput("side.heigth.offset", label = h5("Side by Side Height Offset"),value = 100)#,min = 0,max = 500,
                                               #value = 100, step = 50)
                                     )
                              ),
                              column(7,
                                     wellPanel(
                                       sliderInput("color.range.start", label = h5("Canopy Color Start"),min = 0,max = 1,
                                                   value = .85, step = .01),
                                       sliderInput("color.range.end", label = h5("Canopy Color End"),min = 0,max = 1,
                                                   value = .75, step = .01)                               
                                     )
                              )
                              
                            )
                   ),
                   tabPanel("HU & CT Plot",             
                            splitLayout(cellWidths = c("20%", "80%"),    
                                        column(7,
                                               #sidebarPanel(
                                               #                                                wellPanel(
                                               #                                                  h5("Axis Labels"),  
                                               #                                                  radioButtons("labels.hu.ct", label=NULL, 
                                               #                                                               choices = list('ON' = 1, 'OFF' = 2),
                                               #                                                               selected = 2)
                                               #                                                  
                                               #                                                ),
                                               wellPanel(
                                                 checkboxGroupInput("raw.choices.hu.ct", label = h5("Cotton Season Year"), 
                                                                    choices = list("2014" = 1, "2012" = 2, "2011" = 3),
                                                                    selected = 1),#list(1,2)),
                                                 checkboxGroupInput("water.treatments.hu.ct", label = h5("Irrigation Treatment"), 
                                                                    choices = list('6.0mm' = 4,'3.0mm'=3,'1.5mm'=2,'0.0mm'=1),
                                                                    selected = 1),#list(1,2,3,4)),
                                                 checkboxGroupInput("plant.dates.hu.ct", label = h5("Planting Dates"), 
                                                                    choices = list('P1' = 1,'P2'= 2,'P3'= 3,'P4'=4,'P5'=5,
                                                                                   'P6'= 6,'P7'= 7),#list('4.03' = 1,'4.15'= 2,'4.29'= 3,'S-5.13'=4,'N-5.13'=5,'6.03'= 6,'6.16'= 7),#'7.01'=8),
                                                                    selected = 1)#list(1,2,3,4,5,6,7))
                                               ),
                                               hr(),
                                               sliderInput("p.height.hu.ct", label = h5("Plot Height"),min = 700,max = 4000,
                                                           value = 700, step = 100),
                                               hr(),
                                               submitButton(h5("APPLY PLOT CHOICES")),
                                               #actionButton("submitButton", "APPLY PLOT CHOICES"),
                                               hr()
                                        ),
                                        
                                        webGLOutput("sctPlot.hu.ct", height = "100%")
                            ),
                            splitLayout(
                              column(7,
                                     wellPanel(h5("Mask Temperatures(C)"),
                                               numericInput("mask.b.num.hu.ct", label = h5("Below:"), value = 25),
                                               numericInput("mask.a.num.hu.ct", label = h5("Above:"), value = 0)
                                     ),
                                     wellPanel(h5("Mask HEAT UNITS()"),
                                               numericInput("mask.b.num.hu.ct.h", label = h5("Below:"), value = 0),
                                               numericInput("mask.a.num.hu.ct.h", label = h5("Above:"), value = 0)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("View"),
                                               radioButtons("y.view.radio.hu.ct",label = NULL,choices = list("Day of Year (DOY)" = 1, "Days after Planting (DAP)" = 2), 
                                                            selected = 1),
                                               numericInput("n.start.hu.ct",label = h5("DOY/DAP Start"), value = 93),
                                               numericInput("n.end.hu.ct",label = h5("DOY/DAP End"), value = 365),
                                               sliderInput("pov.angle.hu.ct", label = h5("Point of View Angle"),min = 0,max = 360,
                                                           value = 90, step = 15)
                                     )
                                     
                              ),
                              column(7, 
                                     wellPanel(h5("Extrapolate"),
                                               numericInput("ext.x.hu.ct", label = h5("TOD(x) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.y.hu.ct", label = h5("DOY/DAP(y) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.z.hu.ct.c", label = h5("Surface(z) axis"),value = 10),#,min = 1,max = 100,
                                               #value = 15, step = 1),
                                               numericInput("ext.z.hu.ct.h", label = h5("Surface(z) axis"),value = 1000)#,min = 1,max = 100,
                                               #value = 15, step = 1)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("Plot Arrangement"),
                                               radioButtons("view.radio.hu.ct",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
                                                            selected = 2),
                                               numericInput("stack.offset.hu.ct", label = h5("Stacked View Plot Offset"),value = 100),#,min = 0,max = 1000,
                                               #value = 100, step = 25),
                                               numericInput("side.width.offset.hu.ct", label = h5("Side by Side Width Offset"),value = 100),#,min = 0,max = 10000,
                                               #value = 1000, step = 1000),
                                               numericInput("side.heigth.offset.hu.ct", label = h5("Side by Side Height Offset"),value = 100)#,min = 0,max = 500,
                                               #value = 100, step = 50)
                                     )
                              ),
                              column(7,
                                     wellPanel(
                                       sliderInput("color.range.start.hu.ct.c", label = h5("Canopy Range Start"),min = 0,max = 1,
                                                   value = .85, step = .01),
                                       sliderInput("color.range.end.hu.ct.c", label = h5("Canopy Range End"),min = 0,max = 1,
                                                   value = .75, step = .01),                               
                                       #),
                                       
                                       sliderInput("color.range.start.hu.ct.h", label = h5("HU Color Start"),min = 0,max = 1,
                                                   value = .85, step = .01),
                                       sliderInput("color.range.end.hu.ct.h", label = h5("HU Color End"),min = 0,max = 1,
                                                   value = .16, step = .01)                               
                                     )
                              )
                              
                            )
                   ),
                   
                   
                   tabPanel("HEAT UNITS Plot",             
                            splitLayout(cellWidths = c("20%", "80%"),    
                                        column(7,
                                               #sidebarPanel(
                                               wellPanel(
                                                 h5("Axis Labels"),  
                                                 radioButtons("labels.hu", label=NULL, 
                                                              choices = list('ON' = 1, 'OFF' = 2),
                                                              selected = 2)
                                                 
                                               ),
                                               wellPanel(
                                                 checkboxGroupInput("raw.choices.hu", label = h5("Cotton Season Year"), 
                                                                    choices = list("2014" = 1, "2012" = 2, "2011" = 3),
                                                                    selected = 1),#list(1,2)),
                                                 checkboxGroupInput("water.treatments.hu", label = h5("Irrigation Treatment"), 
                                                                    choices = list('6.0mm' = 4,'3.0mm'=3,'1.5mm'=2,'0.0mm'=1),
                                                                    selected = 1),#list(1,2,3,4)),
                                                 checkboxGroupInput("plant.dates.hu", label = h5("Planting Dates"), 
                                                                    choices = list('P1' = 1,'P2'= 2,'P3'= 3,'P4'=4,'P5'=5,
                                                                                   'P6'= 6,'P7'= 7),#list('4.03' = 1,'4.15'= 2,'4.29'= 3,'S-5.13'=4,'N-5.13'=5,'6.03'= 6,'6.16'= 7),#'7.01'=8),
                                                                    selected = 1)#list(1,2,3,4,5,6,7))
                                               ),
                                               hr(),
                                               sliderInput("p.height.hu", label = h5("Plot Height"),min = 700,max = 4000,
                                                           value = 700, step = 100),
                                               hr(),
                                               submitButton(h5("APPLY PLOT CHOICES")),
                                               #actionButton("submitButton", "APPLY PLOT CHOICES"),
                                               hr()
                                        ),
                                        
                                        webGLOutput("sctPlot.hu", height = "100%")
                            ),
                            splitLayout(
                              column(7,
                                     wellPanel(h5("Mask Temperatures(C)"),
                                               numericInput("mask.b.num.hu", label = h5("Below:"), value = 0),
                                               numericInput("mask.a.num.hu", label = h5("Above:"), value = 0)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("View"),
                                               radioButtons("y.view.radio.hu",label = NULL,choices = list("Day of Year (DOY)" = 1, "Days after Planting (DAP)" = 2), 
                                                            selected = 1),
                                               numericInput("n.start.hu",label = h5("DOY/DAP Start"), value = 93),
                                               numericInput("n.end.hu",label = h5("DOY/DAP End"), value = 365),
                                               sliderInput("pov.angle.hu", label = h5("Point of View Angle"),min = 0,max = 360,
                                                           value = 90, step = 15)
                                     )
                                     
                              ),
                              column(7, 
                                     wellPanel(h5("Extrapolate"),
                                               numericInput("ext.x.hu", label = h5("TOD(x) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.y.hu", label = h5("DOY/DAP(y) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.z.hu", label = h5("Surface(z) axis"),value = 1000)#,min = 1,max = 100,
                                               #value = 15, step = 1)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("Plot Arrangement"),
                                               radioButtons("view.radio.hu",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
                                                            selected = 2),
                                               numericInput("stack.offset.hu", label = h5("Stacked View Plot Offset"),value = 100),#,min = 0,max = 1000,
                                               #value = 100, step = 25),
                                               numericInput("side.width.offset.hu", label = h5("Side by Side Width Offset"),value = 100),#,min = 0,max = 10000,
                                               #value = 1000, step = 1000),
                                               numericInput("side.heigth.offset.hu", label = h5("Side by Side Height Offset"),value = 100)#,min = 0,max = 500,
                                               #value = 100, step = 50)
                                     )
                              ),
                              column(7,
                                     wellPanel(
                                       sliderInput("color.range.start.hu", label = h5("Color Range Start"),min = 0,max = 1,
                                                   value = .85, step = .01),
                                       sliderInput("color.range.end.hu", label = h5("Color Range End"),min = 0,max = 1,
                                                   value = .16, step = .01)                               
                                     )
                              )
                              
                            )
                   ),

                   tabPanel("Custom Plot",             
                            splitLayout(cellWidths = c("20%", "80%"),    
                                        column(7,                
                                               wellPanel(
                                                 fileInput('file1', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
                                                 tags$hr(),
                                                 checkboxInput('header', 'Header', TRUE),
                                                 radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')
                                                 #radioButtons('quote', 'Quote',  c(None='','Double Quote'='"',Single Quote'="'"), '"')
                                                 #                                                  h5("Axis Labels"),  
                                                 #                                                  radioButtons("labels.cust", label=NULL, 
                                                 #                                                               choices = list('ON' = 1, 'OFF' = 2),
                                                 #                                                               selected = 2)                   
                                               ),
                                               wellPanel(
                                                 numericInput("d.plots",label = h5("Enter # for depth of plots"), value = 4),
                                                 checkboxGroupInput("plant.dates.cust", label = h5("Columns"), 
                                                                    choices = list('P1' = 1,'P2'= 2,'P3'= 3,'P4'=4,'P5'=5,
                                                                                   'P6'= 6,'P7'= 7),#list('4.03' = 1,'4.15'= 2,'4.29'= 3,'S-5.13'=4,'N-5.13'=5,'6.03'= 6,'6.16'= 7),#'7.01'=8),
                                                                    selected = 1)
                                                 #list(1,2,3,4,5,6,7))
                                               ),
                                               hr(),
                                               sliderInput("p.height.cust", label = h5("Plot Height"),min = 700,max = 4000,
                                                           value = 700, step = 100),
                                               hr(),
                                               submitButton(h5("APPLY PLOT CHOICES")),
                                               #actionButton("submitButton", "APPLY PLOT CHOICES"),
                                               hr()
                                        ),
                                        
                                        webGLOutput("sctPlot.cust", height = "100%")
                            ),
                            splitLayout(
                              column(7,
                                     wellPanel(h5("Mask Temperatures(C)"),
                                               numericInput("mask.b.num.cust", label = h5("Below:"), value = 0),
                                               numericInput("mask.a.num.cust", label = h5("Above:"), value = 0)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("View"),
                                               radioButtons("y.view.radio.cust",label = NULL,choices = list("Day of Year (DOY)" = 1, "Days after Planting (DAP)" = 2), 
                                                            selected = 1),
                                               numericInput("n.start.cust",label = h5("DOY/DAP Start"), value = 1),
                                               numericInput("n.end.cust",label = h5("DOY/DAP End"), value = 100),
                                               sliderInput("pov.angle.cust", label = h5("Point of View Angle"),min = 0,max = 360,
                                                           value = 90, step = 15)
                                     )
                                     
                              ),
                              column(7, 
                                     wellPanel(h5("Extrapolate"),
                                               numericInput("ext.x.cust", label = h5("TOD(x) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.y.cust", label = h5("DOY/DAP(y) axis"),value = 20),#,min = 1,max = 100,
                                               #value = 20, step = 1),
                                               numericInput("ext.z.cust", label = h5("Surface(z) axis"),value = 20)#,min = 1,max = 100,
                                               #value = 15, step = 1)
                                     )
                              ),
                              column(7,
                                     wellPanel(h5("Plot Arrangement"),
                                               radioButtons("view.radio.cust",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
                                                            selected = 2),
                                               numericInput("stack.offset.cust", label = h5("Stacked View Plot Offset"),value = 100),#,min = 0,max = 1000,
                                               #value = 100, step = 25),
                                               numericInput("side.width.offset.cust", label = h5("Side by Side Width Offset"),value = 100),#,min = 0,max = 10000,
                                               #value = 1000, step = 1000),
                                               numericInput("side.heigth.offset.cust", label = h5("Side by Side Height Offset"),value = 100)#,min = 0,max = 500,
                                               #value = 100, step = 50)
                                     )
                              ),
                              column(7,
                                     wellPanel(
                                       sliderInput("color.range.start.cust", label = h5("Color Range Start"),min = 0,max = 1,
                                                   value = .85, step = .01),
                                       sliderInput("color.range.end.cust", label = h5("Color Range End"),min = 0,max = 1,
                                                   value = .76, step = .01)                               
                                     )
                              )
                              
                            )
                   ),
                   tabPanel(
                     downloadButton('downloadData.14', 'Download Mahan Cotton 2014'),
                     downloadButton('downloadData.hu.14', 'Download Mahan Cotton Heat Units 2014'),
                     title = '2014',
                     sidebarLayout(                
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset14 === "rawdata14"',
                           checkboxGroupInput('show_vars.14', 'Select Columns to View:',
                                              names(rawdata14), selected = names(rawdata14))#NULL)
                         )                      
      
                       ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset14',
                           tabPanel('rawdata14', label="Data Set",dataTableOutput('mytable14')),
                           tabPanel('rawdata14.hu', label="Data Set",dataTableOutput('mytable14.hu'))#,
                         )
                       )
                     )
                   ),
                   tabPanel(
                     downloadButton('downloadData.12', 'Download Mahan Cotton 2012'),
                     downloadButton('downloadData.hu.12', 'Download Mahan Cotton Heat Units 2012'),
                     title = '2012',
                     sidebarLayout(                
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset12 === "rawdata12"',
                           checkboxGroupInput('show_vars.12', 'Select Columns to View:',
                                              names(rawdata12), selected = names(rawdata12))#NULL)
                         )
                       ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset12',
                           tabPanel('rawdata12', label="Data Set",dataTableOutput('mytable12'))#,
                           #tabPanel('Sparklines', dataTableOutput('mytable12'))
                         )
                       )
                     )
                   ),
                   tabPanel(
                     downloadButton('downloadData.11', 'Download Mahan Cotton 2011'),
                     downloadButton('downloadData.hu.11', 'Download Mahan Cotton Heat Units 2011'),
                     title = '2011',
                     sidebarLayout(                
                       sidebarPanel(
                         conditionalPanel(
                           'input.dataset11 === "rawdata11"',
                           checkboxGroupInput('show_vars.11', 'Select Columns to View:',
                                              names(rawdata11), selected = names(rawdata11))#NULL)
                         )
                       ),
                       mainPanel(
                         tabsetPanel(
                           id = 'dataset11',
                           tabPanel('rawdata11', label="Data Set",dataTableOutput('mytable11'))#,
                           #tabPanel('Sparklines', dataTableOutput('mytable11'))
                         )
                       )
                     )
                   ),

tabPanel("Source Code",
         sidebarLayout(
           sidebarPanel(
             #                                 fileInput('file1', 'Choose CSV File',accept=c('text/csv', 
             #                                                                               'text/comma-separated-values,text/plain', 
             #                                                                               '.csv')),
             
             #                                 tags$hr(),
             #                                 checkboxInput('header', 'Header', TRUE),
             #                                 radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')
             #                                 #radioButtons('quote', 'Quote',  c(None='','Double Quote'='"',Single Quote'="'"), '"')
             #                                 conditionalPanel('input.dataset.cust === "rawdata.cust"',
             #                                                  checkboxGroupInput('show_vars.cust', 'Select Columns to View:',choices = c(1,2,3),
             #                                                                     selected = names(rawdata.cust))#NULL)
           ),
           
           mainPanel(
             tabsetPanel(id = 'dataset.cust',
                         #tabPanel('rawdata.cust', label="Data Set",dataTableOutput('mytable.cust')),
                         tabPanel('Server Code', label="Source",pre(includeText("server.R"))),
                         tabPanel('Shinyfunctions Code', label="Source",pre(includeText("shinyfunctions.R"))),
                         tabPanel('UI Code', label="Source",pre(includeText("UI.R")))
                         
             )
           )
           
           #includeHTML("index.html")
         )
)
                   
))
