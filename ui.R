library(shiny)
source("COHb.R")
ui <- fluidPage(
  textOutput("abstract"),
  plotOutput("timePlot"),
  tabsetPanel(
    tabPanel("Sample",
             fluidRow(
               column(4,textInput("ID", label = "Sample Number:", value = "[Sample Number]")),
               column(4,selectInput("COHb_method", label = "COHb known from:", c("SpCO","blood","breath")))
               ),
             fluidRow(
               column(4,numericInput("XCOHb", label = "COHb in blood (%):", value = 18.4)),
               column(4,
                      conditionalPanel(condition="input.COHb_method!='breath'","Standard Deviation:",verbatimTextOutput("XCOHb.sd")),
                      conditionalPanel(condition="input.COHb_method=='breath'",numericInput("XCOHb.sd", label = "Standard Deviation (%):", value = 0))
                      ),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("XCOHb.rsd")))
             ),
             fluidRow(column(4),column(4,selectInput("Hb_method", label = "Hb known from:", c("gender","blood")))),
             fluidRow(
               column(4,
                      conditionalPanel(condition="input.Hb_method!='blood'","Hemoglobin in blood:",verbatimTextOutput("Hb")),
                      conditionalPanel(condition="input.Hb_method=='blood'",numericInput("Hb", label = "Hemoglobin in blood (grams/100mL):", value = 15.8))
                      ),
               column(4,
                      conditionalPanel(condition="input.Hb_method!='blood'","Standard Deviation:",verbatimTextOutput("Hb.sd")),
                      conditionalPanel(condition="input.Hb_method=='blood'",numericInput("Hb.sd", label = "Standard Deviation (grams/100mL):", value = 0))
                      ),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("Hb.rsd")))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(
                                column(4,"Fraction of COHb in blood sample:",verbatimTextOutput("COHb.D")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.D.sd")))
                                )
             )
    ),
    tabPanel("Employee",
             fluidRow(column(4,textInput("name", label = "Employee Name:", value = "[Employee Name]")),
                      column(4,selectInput("gender", label = "Gender", c("male","female")))
             ),
             fluidRow(
               column(4,numericInput("h", label = "Height (inches):", value = 67)),
               column(4,numericInput("h.sd", label = "Standard Deviation (inches):", value = 1)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("h.rsd")))
             ),
             fluidRow(
               column(4,numericInput("w", label = "Weight (pounds):", value = 166)),
               column(4,numericInput("w.sd", label = "Standard Deviation (pounds):", value = 2)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("w.rsd")))
             ),
             fluidRow(
               column(4,checkboxInput("smoker", "Smoker?", FALSE)),
               conditionalPanel("input.smoker",column(4,selectInput("SS_method", label = "Initial COHb known from:", c("cigarettes","status","percent"))))
               ),
             conditionalPanel("input.smoker",
                              conditionalPanel(condition="input.SS_method=='cigarettes'",fluidRow(
                                column(4,numericInput("cigarettes", label = "Cigarettes smoked per day:", value = 0, min = 0)),
                                column(4,numericInput("cigarettes.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("cigarettes.rsd")))
                              )),
                              conditionalPanel(condition="input.SS_method=='status'",fluidRow(
                                column(4,numericInput("SS", label = "Smoker Status:", value = 0, min = 0, max = 4)),
                                column(4,"Standard Deviation",verbatimTextOutput("SS.sd")),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("SS.rsd")))
                              )),
                              conditionalPanel(condition="input.SS_method=='percent'",fluidRow(
                                column(4,numericInput("XCOHb.0", label = "Initial COHb in blood (%):", value = 0.75)),
                                column(4,numericInput("XCOHb.0.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("XCOHb.0.rsd")))
                              ))
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(
                                column(4,"Estimated blood volume of employee:",verbatimTextOutput("Vb")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("Vb.sd")))
                              ),
                              fluidRow(
                                column(4,"Fraction of COHb in blood prior to exposure:",verbatimTextOutput("XCOHb.A")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.A.sd")))
                              ),
                              fluidRow(
                                column(4,"COHb in blood prior to exposure:",verbatimTextOutput("COHb.A")),
                                conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.A.sd")))
                              )
             )
    ),
    tabPanel("Environment",
             fluidRow(column(4),column(4,selectInput("PB_method", label = "Pressure known from:", c("elevation","pressure")))),
             conditionalPanel(condition="input.PB_method=='elevation'",
                              fluidRow(
                                column(4,numericInput("z", label = "Elevation (ft):", value = 0)),
                                column(4,numericInput("z.sd", label = "Standard Deviation (ft):", value = 100)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("z.rsd")))
                                ),
                              fluidRow(
                                column(4,"Atmospheric Pressure:",verbatimTextOutput("PB")),
                                column(4,"Standard Deviation:",verbatimTextOutput("PB.sd"))
                                )
             ),
             conditionalPanel(condition="input.PB_method=='pressure'",
                              fluidRow(
                                column(4,numericInput("PB", label = "Atmospheric Pressure (mmHg):", value = 740)),
                                column(4,numericInput("PB.sd", label = "Standard Deviation (mmHg):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("PB.rsd")))
                              )
             )
    ),
    tabPanel("Exposure",
             fluidRow(
               column(4,numericInput("t_e", label = "Duration (minutes):", value = 350)),
               column(4,numericInput("t_e.sd", label = "Standard Deviation (minutes):", value = 15)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_e.rsd")))
             ),
             fluidRow(
               column(4,numericInput("AL_e", label = "Activity Level:", value = 1, min = 0, max = 4)),
               column(4,numericInput("AL_e.sd", label = "Standard Deviation:", value = 0.5)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_e.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.O2_e", label = "Oxygen level (% oxygen):", value = 21)),
                                column(4,numericInput("x.O2_e.sd", label = "Standard Deviation (% oxygen):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_e.rsd")))
                              )
             ),
             conditionalPanel("input.smoker",
                              fluidRow(column(4),column(4,selectInput("fhs_e_method", label = "First hand smoke known from:", c("cigarettes","percent","ppm")))),
                              conditionalPanel(condition="input.fhs_e_method=='cigarettes'",fluidRow(
                                column(4,numericInput("fhs_e.cigarettes", label = "Cigarettes smoked:", value = 0)),
                                column(4,numericInput("fhs_e.cigarettes.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_e.cigarettes.rsd")))
                              )),
                              conditionalPanel(condition="input.fhs_e_method=='percent'",fluidRow(
                                column(4,numericInput("fhs_e.percent", label = "Fraction of employee's smoke rate (%):", value = 0)),
                                column(4,numericInput("fhs_e.percent.sd", label = "Standard Deviation (%):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_e.percent.rsd")))
                              )),
                              conditionalPanel(condition="input.fhs_e_method=='ppm'",fluidRow(
                                column(4,numericInput("fhs_e.ppm", label = "CO exposure from smoking (ppm):", value = 0)),
                                column(4,numericInput("fhs_e.ppm.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_e.ppm.rsd")))
                              ))
             ),
             fluidRow(
               column(4,checkboxInput("shs_e", "Second hand smoke?", FALSE)),
               conditionalPanel("input.shs_e",column(4,selectInput("shs_e_method", label = "Second hand smoke known from:", c("time","percent","ppm"))))
             ),
             conditionalPanel("input.shs_e",fluidRow(
               conditionalPanel(condition="input.shs_e_method=='time'",
                                column(4,numericInput("shs_e.time", label = "Exposure to second hand smoke (minutes):", value = 0)),
                                column(4,numericInput("shs_e.time.sd", label = "Standard Deviation (minutes):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_e.time.rsd")))
               ),
               conditionalPanel(condition="input.shs_e_method=='percent'",
                                column(4,numericInput("shs_e.percent", label = "Exposure to second hand smoke (%):", value = 0)),
                                column(4,numericInput("shs_e.percent.sd", label = "Standard Deviation (%):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_e.percent.rsd")))
               ),
               conditionalPanel(condition="input.shs_e_method=='ppm'",
                                column(4,numericInput("shs_e.ppm", label = "Exposure to second hand smoke (ppm):", value = 0)),
                                column(4,numericInput("shs_e.ppm.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_e.ppm.rsd")))
               )
             )),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_e.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_e.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_e.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_e.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_e.sd")))),
                              fluidRow(column(4,"x.CO_e:",verbatimTextOutput("x.CO_e")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.fhsd")))),
                              fluidRow(column(4,"Exposure to first hand smoke:",verbatimTextOutput("x.CO_e.fhs")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.fhs.sd")))),
                              fluidRow(column(4,"Exposure to second hand smoke::",verbatimTextOutput("x.CO_e.shs")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.shs.sd")))),
                              fluidRow(column(4,"x.CO_e.o:",verbatimTextOutput("x.CO_e.o")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_e.o.sd")))),
                              fluidRow(column(4,"8-hour total weight average (TWA) exposure:",verbatimTextOutput("TWA8Hours")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("TWA8Hours.sd"))))
             )
    ),
    tabPanel("Clearance",
             fluidRow(
               column(4,numericInput("t_c", label = "Duration (minutes):", value = 160)),
               column(4,numericInput("t_c.sd", label = "Standard Deviation (minutes):", value = 15)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_c.rsd")))
             ),
             fluidRow(
               column(4,numericInput("AL_c", label = "Activity Level:", value = 0)),
               column(4,numericInput("AL_c.sd", label = "Standard Deviation:", value = 0.5)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_c.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.O2_c", label = "Oxygen level (% oxygen):", value = 21)),
                                column(4,numericInput("x.O2_c.sd", label = "Standard Deviation (% oxygen):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_c.rsd")))
                              ),
                              fluidRow(
                                column(4,numericInput("x.CO_c", label = "Carbon Monoxide Level (ppm):", value = 2)),
                                column(4,numericInput("x.CO_c.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.CO_c.rsd")))
                              )
             ),
             conditionalPanel("input.smoker",
                              fluidRow(column(4),column(4,selectInput("fhs_c_method", label = "First hand smoke known from:", c("cigarettes","percent","ppm")))),
                              conditionalPanel(condition="input.fhs_c_method=='cigarettes'",fluidRow(
                                column(4,numericInput("fhs_c.cigarettes", label = "Cigarettes smoked:", value = 0)),
                                column(4,numericInput("fhs_c.cigarettes.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_c.cigarettes.rsd")))
                              )),
                              conditionalPanel(condition="input.fhs_c_method=='percent'",fluidRow(
                                column(4,numericInput("fhs_c.percent", label = "Fraction of employee's smoke rate (%):", value = 0)),
                                column(4,numericInput("fhs_c.percent.sd", label = "Standard Deviation (%):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_c.percent.rsd")))
                              )),
                              conditionalPanel(condition="input.fhs_c_method=='ppm'",fluidRow(
                                column(4,numericInput("fhs_c.ppm", label = "CO exposure from smoking (ppm):", value = 0)),
                                column(4,numericInput("fhs_c.ppm.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("fhs_c.ppm.rsd")))
                              ))
             ),
             fluidRow(
               column(4,checkboxInput("shs_c", "Second hand smoke?", FALSE)),
               conditionalPanel("input.shs_c",column(4,selectInput("shs_c_method", label = "Second hand smoke known from:", c("time","percent","ppm"))))
             ),
             conditionalPanel("input.shs_c",fluidRow(
               conditionalPanel(condition="input.shs_c_method=='time'",
                                column(4,numericInput("shs_c.time", label = "Exposure to second hand smoke (minutes):", value = 0)),
                                column(4,numericInput("shs_c.time.sd", label = "Standard Deviation (minutes):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_c.time.rsd")))
               ),
               conditionalPanel(condition="input.shs_c_method=='percent'",
                                column(4,numericInput("shs_c.percent", label = "Exposure to second hand smoke (%):", value = 0)),
                                column(4,numericInput("shs_c.percent.sd", label = "Standard Deviation (%):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_c.percent.rsd")))
               ),
               conditionalPanel(condition="input.shs_c_method=='ppm'",
                                column(4,numericInput("shs_c.ppm", label = "Exposure to second hand smoke (ppm):", value = 0)),
                                column(4,numericInput("shs_c.ppm.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("shs_c.ppm.rsd")))
               )
             )),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_c.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_c.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_c.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_c.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_c")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_c.sd")))),
                              fluidRow(column(4,"Exposure to first hand smoke:",verbatimTextOutput("x.CO_c.fhs")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_c.fhs.sd")))),
                              fluidRow(column(4,"Exposure to second hand smoke::",verbatimTextOutput("x.CO_c.shs")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("x.CO_c.shs.sd")))),
                              fluidRow(column(4,"Concentration of COHb prior to clearance:",verbatimTextOutput("COHb.B")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.B.sd")))),
                              fluidRow(column(4,"Fraction of COHb prior to clearance:",verbatimTextOutput("XCOHb.B")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.B.sd"))))
             )
    ),
    tabPanel("Oxygen Therapy",
             fluidRow(
               column(4,numericInput("t_t", label = "Duration (minutes):", value = 0)),
               column(4,numericInput("t_t.sd", label = "Standard Deviation (minutes):", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("t_t.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("AL_t", label = "Activity Level:", value = 0)),
                                column(4,numericInput("AL_t.sd", label = "Standard Deviation:", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("AL_t.rsd")))
                              )
             ),
             fluidRow(
               column(4,selectInput("OT_method", label = "Oxygen known from:", c("Nasal Cannula (NC)","Simple Face Mask (SFM)","Non-Rebreather (NRB)","Bag-valve-mask (BVM)","Oxygen level"))),
               conditionalPanel(condition="input.OT_method=='Nasal Cannula (NC)'",column(4,numericInput("NC.lpm", label = "Flow Rate (LPM):", value = 1))),
               conditionalPanel(condition="input.OT_method=='Simple Face Mask (SFM)'",column(4,numericInput("SFM.lpm", label = "Flow Rate (LPM):", value = 8))),
               conditionalPanel(condition="input.OT_method=='Non-Rebreather (NRB)'",column(4,numericInput("NRB.lpm", label = "Flow Rate (LPM):", value = 10))),
               conditionalPanel(condition="input.OT_method=='Bag-valve-mask (BVM)'",column(4,numericInput("BVM.lpm", label = "Flow Rate (LPM):", value = 10)))
             ),
             fluidRow(
               conditionalPanel(condition="input.OT_method!='Oxygen level'",column(4,"Oxygen level:",verbatimTextOutput("x.O2_t"))),
               conditionalPanel(condition="input.OT_method=='Oxygen level'",column(4,numericInput("x.O2_t", label = "Oxygen level (% oxygen):", value = 100))),
               column(4,numericInput("x.O2_t.sd", label = "Standard Deviation (% oxygen):", value = 0)),
               conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.O2_t.rsd")))
             ),
             conditionalPanel("input.advanced",
                              fluidRow(
                                column(4,numericInput("x.CO_t", label = "Carbon Monoxide Level (ppm):", value = 0)),
                                column(4,numericInput("x.CO_t.sd", label = "Standard Deviation (ppm):", value = 0)),
                                conditionalPanel("input.showRSD", column(4,"Relative standard deviation",verbatimTextOutput("x.CO_t.rsd")))
                              )
             ),
             conditionalPanel("input.intermediate",
                              fluidRow(column(4,"VA:",verbatimTextOutput("VA_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("VA_t.sd")))),
                              fluidRow(column(4,"DL:",verbatimTextOutput("DL_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("DL_t.sd")))),
                              fluidRow(column(4,"beta:",verbatimTextOutput("beta_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("beta_t.sd")))),
                              fluidRow(column(4,"PICO:",verbatimTextOutput("PICO_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PICO_t.sd")))),
                              fluidRow(column(4,"PCO2:",verbatimTextOutput("PCO2_t")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("PCO2_t.sd")))),
                              fluidRow(column(4,"Concentration of COHb prior to therapy:",verbatimTextOutput("COHb.C")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("COHb.C.sd")))),
                              fluidRow(column(4,"Fraction of COHb prior to therapy:",verbatimTextOutput("XCOHb.C")),
                                       conditionalPanel("input.doMonteCarlo",column(4,"Standard Deviation:",verbatimTextOutput("XCOHb.C.sd"))))
             )
    ),
    tabPanel("Standard",
             fluidRow(column(4,selectInput("ExposureLimit", label = "Occupational Exposure Limit:", c("OSHA PEL","NIOSH REL","custom PEL"))),
               column(4,
                      conditionalPanel(condition="input.ExposureLimit!='custom PEL'","TWA:",verbatimTextOutput("OEL")),
                      conditionalPanel(condition="input.ExposureLimit=='custom PEL'",numericInput("OEL", label = "TWA (ppm):", value = 50))
               )
             )
    ),
    tabPanel("Parameters",
             numericInput("deltaT", label = "Runge–Kutta time step (seconds):", value = 60),
             selectInput("shape", label = "Shape of a probability distribution:", c("uniform","normal")),
             checkboxInput("advanced", "Show advanced features", FALSE),
             checkboxInput("intermediate", "Show intermediate values", FALSE),
             checkboxInput("doMonteCarlo", "Perform Monte Carlo simulations", FALSE),
             conditionalPanel("input.doMonteCarlo",fluidRow(column(4,numericInput("n", label = "Number of Monte Carlo simulations:", value = 100)))),
             checkboxInput("showRSD", "Show relative standard deviation values", FALSE)
    ),
    tabPanel("Summary",
             fluidRow(column(4, textInput("employer", label = "Employeer:", value = "[Employer Name]"))),
             fluidRow(column(4, textInput("inspectionNumber", label = "Inspection Number:", value = "[Inspection Number]"))),
             fluidRow(column(4, textInput("complianceOfficer", label = "Compliance Officer:", value = "[Compliance Officer]"))),
             fluidRow(column(4, textInput("areaOffice", label = "Area:", value = "[Area]"))),
             fluidRow(column(4, numericInput("region", label = "Region:", value = "0", min = 1, max = 10, step = 1))),
             #fluidRow(column(4, dateInput("requestDate", label = "Request Date:", value = Sys.Date(), format = "DD the dd of MM yyyy"))),
             fluidRow(column(4, textInput("requestDate", label = "Request Date:", value = "[Request Date]"))),
             fluidRow(column(4, textInput("exposureDate", label = "Exposure Date:", value = "[Exposure Date]"))),
             downloadButton('report','Generate Report')
    )
  ),
  hr(),
  fluidRow(
    column(2,downloadButton("downloadData", "Save File")),
    column(4,fileInput("CSVfile", "Upload CSV",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))),
    column(4,fileInput("PRNfile", "Upload PRN",multiple = FALSE,accept = c(".prn")))
  ),
  textOutput("CSVcontents"),
  textOutput("PRNcontents")
)
