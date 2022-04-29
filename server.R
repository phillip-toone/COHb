source("COHb.R")
library(knitr)
server <- function(input, output, session) {
  #================================================== Sample ==================================================#
  # COHb in blood sample (%)
  XCOHb = reactive(input$XCOHb*percent)
  XCOHb.sd = reactive(
    if (input$COHb_method=='blood') 0.46726*(input$XCOHb)^-0.7553*(input$XCOHb*percent)
    else if (input$COHb_method=='SpCO') 2.8*percent
    else if (input$COHb_method=='breath') input$XCOHb.sd*percent
  )
  XCOHb.MC = reactive(
    if (input$shape=='uniform') runif(n(),XCOHb()-XCOHb.sd(),XCOHb()+XCOHb.sd())
    else if (input$shape=='normal') rnorm(n(),XCOHb(),XCOHb.sd()/sqrt(3))
  )
  output$XCOHb.sd = renderPrint(cat("XCOHb.sd =",XCOHb.sd()/percent,"%"))
  output$XCOHb.rsd = renderText(XCOHb.sd()/XCOHb())
  # Hemoglobin in blood sample (grams/100mL)
  Hb = reactive(
    if (input$Hb_method=='blood') input$Hb*gram/(100*mL)
    else if (input$Hb_method=='gender') {if (input$gender=='male') 15.8*gram/(100*mL) else 14.2*gram/(100*mL)}
  )
  Hb.sd = reactive(
    if (input$Hb_method=='blood') input$Hb.sd*gram/(100*mL)
    else if (input$Hb_method=='gender') {if (input$gender=='male') 1.1735*gram/(100*mL) else 1.0204*gram/(100*mL)}
  )
  Hb.MC = reactive(
    if (input$shape=='uniform') runif(n(),Hb()-Hb.sd(),Hb()+Hb.sd())
    else if (input$shape=='normal') rnorm(n(),Hb(),Hb.sd()/sqrt(3))
  )
  output$Hb = renderPrint(cat("Hb =",Hb()/(gram/(100*mL)),"grams/100mL"))
  output$Hb.sd = renderPrint(cat("Hb.sd =",Hb.sd()/(gram/(100*mL)),"grams/100mL"))
  output$Hb.rsd = renderText(Hb.sd()/Hb())
  # Fraction of COHb in blood sample (%)
  COHb.D = reactive(COHb(XCOHb=XCOHb(),Hb=Hb()))
  output$COHb.D = renderPrint(cat("COHb.D =",COHb.D(),"mL/mL"))
  COHb.D.MC = reactive(COHb(XCOHb=XCOHb.MC(),Hb=Hb.MC()))
  output$COHb.D.sd = renderPrint(cat(sd(COHb.D.MC()),"mL/mL"))
  #================================================== Employee ==================================================#
  # Weight (pounds)
  w = reactive(input$w*pound)
  w.sd = reactive(input$w.sd*pound)
  w.MC = reactive(
    if (input$shape=='uniform') runif(n(),w()-w.sd(),w()+w.sd())
    else if (input$shape=='normal') rnorm(n(),w(),w.sd()/sqrt(3))
    )
  output$w.rsd = renderText(w.sd()/w())
  # Height (inches)
  h = reactive(input$h*inch)
  h.sd = reactive(input$h.sd*inch)
  h.MC = reactive(
    if (input$shape=='uniform') runif(n(),h()-h.sd(),h()+h.sd())
    else if (input$shape=='normal') rnorm(n(),h(),h.sd()/sqrt(3))
    )
  output$h.rsd = renderText(h.sd()/h())
  # Estimated blood volume of employee (liters)
  Vb = reactive(
    if (input$gender=='male') Vb.m(W=w(),H=h())
    else if (input$gender=='female') Vb.f(W=w(),H=h())
  )
  output$Vb = renderPrint(cat("Vb =",Vb()/liter,"liter"))
  Vb.MC = reactive(Vb.m(W=w.MC(),H=h.MC()))
  output$Vb.sd = renderPrint(cat(sd(Vb.MC())/liter,"liter"))
  # Smoker Status
  SS = reactive({
      if (input$SS>4) updateNumericInput(session, "SS", value = 4)
      if (input$SS<0) updateNumericInput(session, "SS", value = 0)
      input$SS
  })
  SS.sd = reactive(if (SS()==0) 0 else 0.32*SS())
  SS.MC = reactive(
    if (input$shape=='uniform') runif(n(),SS()-SS.sd(),SS()+SS.sd())
    else if (input$shape=='normal') rnorm(n(),SS(),SS.sd()/sqrt(3))
    )
  output$SS = renderText(SS())
  output$SS.sd = renderText(SS.sd())
  output$SS.rsd = renderText(SS.sd()/SS())
  # Cigarettes smoked per day
  cigarettes=reactive({
    if (input$cigarettes>67) updateNumericInput(session, "cigarettes", value = 67)
    if (input$cigarettes<0) updateNumericInput(session, "cigarettes", value = 0)
    input$cigarettes
    })
  cigarettes.sd=reactive(input$cigarettes.sd)
  cigarettes.MC = reactive(
    if (input$shape=='uniform') runif(n(),cigarettes()-cigarettes.sd(),cigarettes()+cigarettes.sd())
    else if (input$shape=='normal') rnorm(n(),cigarettes(),cigarettes.sd()/sqrt(3))
        
    )
  output$cigarettes.rsd = renderText(cigarettes.sd()/cigarettes())
  # Initial COHb in blood
  XCOHb.0=reactive(input$XCOHb.0*percent)
  XCOHb.0.sd=reactive(input$XCOHb.0.sd*percent)
  XCOHb.0.MC = reactive(
    if (input$shape=='uniform') runif(n(),XCOHb.0()-XCOHb.0.sd(),XCOHb.0()+XCOHb.0.sd())
    else if (input$shape=='normal') rnorm(n(),XCOHb.0(),XCOHb.0.sd()/sqrt(3))
  )
  output$XCOHb.0.rsd = renderText(XCOHb.0.sd()/XCOHb.0())
  # Fraction of COHb in blood prior to exposure (%)
  XCOHb.A = reactive(
    if (input$smoker){
      if (input$SS_method=='cigarettes') XCOHb.0_c(cigs=cigarettes())
      else if (input$SS_method=='status') XCOHb.0_s(SS=SS())
      else if (input$SS_method=='percent') XCOHb.0()
    }
    else XCOHb.dat[1]
  )
  output$XCOHb.A = renderPrint(cat("XCOHb.A =",XCOHb.A()/percent,"%"))
  XCOHb.A.MC = reactive(
    if (input$smoker){
      if (input$SS_method=='cigarettes') XCOHb.0_c(cigs=cigarettes.MC())
      else if (input$SS_method=='status') XCOHb.0_s(SS=SS.MC())
      else if (input$SS_method=='percent') XCOHb.0.MC()
    }
    else XCOHb.dat[1]
  )
  output$XCOHb.A.sd = renderPrint(cat(sd(XCOHb.A.MC())/percent,"%"))
  # COHb in blood prior to exposure
  COHb.A = reactive(Hf*Hb()*XCOHb.A())
  output$COHb.A = renderPrint(cat("COHb.A =",COHb.A(),"mL/mL"))
  COHb.A.MC = reactive(Hf*Hb.MC()*XCOHb.A.MC())
  output$COHb.A.sd = renderPrint(cat(sd(COHb.A.MC()),"mL/mL"))
  #================================================== Enviroment ==================================================#
  # Elevation
  z = reactive(input$z*ft)
  z.sd = reactive(input$z.sd*ft)
  z.MC = reactive(
    if (input$shape=='uniform') runif(n(),z()-z.sd(),z()+z.sd())
    else if (input$shape=='normal') rnorm(n(),z(),z.sd()/sqrt(3))
    )
  output$z.rsd = renderText(z.sd()/z())
  # Atmospheric Pressure (mmHg)
  PB = reactive(
    if (input$PB_method=='elevation') P(z())
    else if (input$PB_method=='pressure') input$PB*mmHg
  )
  PB.sd = reactive(input$PB.sd*mmHg)
  PB.MC = reactive(
    if (input$PB_method=='elevation') P(z.MC())
    else if (input$PB_method=='pressure') (
      if (input$shape=='uniform') runif(n(),PB()-PB.sd(),PB()+PB.sd())
      else if (input$shape=='normal') rnorm(n(),PB(),PB.sd()/sqrt(3))
    )
  )
  output$PB = renderPrint(cat("PB =",PB()/mmHg,"mmHg"))
  output$PB.sd = renderPrint(cat(sd(PB.MC())/mmHg,"mmHg"))
  output$PB.rsd = renderText(PB.sd()/PB())
  #================================================== Exposure ==================================================#
  # Exposure duration (minutes)
  t_e = reactive(input$t_e*minute)
  t_e.sd = reactive(input$t_e.sd*minute)
  t_e.MC = reactive(
    if (input$shape=='uniform') runif(n(),t_e()-t_e.sd(),t_e()+t_e.sd())
    else if (input$shape=='normal') rnorm(n(),t_e(),t_e.sd()/sqrt(3))
  )
  output$t_e.rsd = renderText(t_e.sd()/t_e())
  # Exposure activity Level
  AL_e = reactive(input$AL_e)
  AL_e.sd = reactive(input$AL_e.sd)
  AL_e.MC = reactive(
    if (input$shape=='uniform') runif(n(),AL_e()-AL_e.sd(),AL_e()+AL_e.sd())
    else if (input$shape=='normal') rnorm(n(),AL_e(),AL_e.sd()/sqrt(3))
  )
  output$AL_e.rsd = renderText(AL_e.sd()/AL_e())
  # Oxygen level (% oxygen)
  x.O2_e = reactive(input$x.O2_e*percent)
  x.O2_e.sd = reactive(input$x.O2_e.sd*percent)
  x.O2_e.MC = reactive(
    if (input$shape=='uniform') runif(n(),x.O2_e()-x.O2_e.sd(),x.O2_e()+x.O2_e.sd())
    else if (input$shape=='normal') rnorm(n(),x.O2_e(),x.O2_e.sd()/sqrt(3))
  )
  output$x.O2_e.rsd = renderText(x.O2_e.sd()/x.O2_e())
  # Cigarettes smoked during exposure
  fhs_e.cigarettes = reactive(input$fhs_e.cigarettes)
  fhs_e.cigarettes.sd = reactive(input$fhs_e.cigarettes.sd)
  fhs_e.cigarettes.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_e.cigarettes()-fhs_e.cigarettes.sd(),fhs_e.cigarettes()+fhs_e.cigarettes.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_e.cigarettes(),fhs_e.cigarettes.sd()/sqrt(3))
  )
  output$fhs_e.cigarettes.rsd = renderText(fhs_e.cigarettes.sd()/fhs_e.cigarettes())
  # Fraction of exposure smoked (%)
  fhs_e.percent = reactive(input$fhs_e.percent*percent)
  fhs_e.percent.sd = reactive(input$fhs_e.percent.sd*percent)
  fhs_e.percent.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_e.percent()-fhs_e.percent.sd(),fhs_e.percent()+fhs_e.percent.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_e.percent(),fhs_e.percent.sd()/sqrt(3))
  )
  output$fhs_e.percent.rsd = renderText(fhs_e.percent.sd()/fhs_e.percent())
  # CO exposure from smoking (ppm)
  fhs_e.ppm = reactive(input$fhs_e.ppm*ppm)
  fhs_e.ppm.sd = reactive(input$fhs_e.ppm.sd*ppm)
  fhs_e.ppm.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_e.ppm()-fhs_e.ppm.sd(),fhs_e.ppm()+fhs_e.ppm.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_e.ppm(),fhs_e.ppm.sd()/sqrt(3))
  )
  output$fhs_e.ppm.rsd = renderText(fhs_e.ppm.sd()/fhs_e.ppm())
  # CO exposure from first hand smoke (ppm)
  x.CO_e.fhs = reactive({
    if (input$smoker){
      if (input$fhs_e_method=='cigarettes') steadyState_c(cigs=fhs_e.cigarettes()*960*minute/t_e())
      else if (input$fhs_e_method=='percent') {
        if (input$SS_method=='cigarettes') steadyState_c(cigs=cigarettes()*fhs_e.percent())
        else if (input$SS_method=='status') steadyState_s(SS=SS()*fhs_e.percent())
        # ERROR message intentional here if attempting to find x.CO_e.fhs from a known ppm for Initial COHb in blood.
      }
      else if (input$fhs_e_method=='ppm') fhs_e.ppm()
    }
    else 0 #steadyState_c(cigs=0)
  })
  output$x.CO_e.fhs = renderPrint(cat("x.CO_e.fhs =",x.CO_e.fhs()/ppm,"ppm"))
  x.CO_e.fhs.sd = reactive(fhs_e.ppm.sd())
  x.CO_e.fhs.MC = reactive({
    if (input$smoker){
      if (input$fhs_e_method=='cigarettes') steadyState_c(cigs=fhs_e.cigarettes.MC()*960*minute/t_e.MC())
      else if (input$fhs_e_method=='percent') {
        if (input$SS_method=='cigarettes') steadyState_c(cigs=cigarettes.MC()*fhs_e.percent.MC())
        else if (input$SS_method=='status') steadyState_s(SS=SS.MC()*fhs_e.percent.MC())
      }
      else if (input$fhs_e_method=='ppm') (
        if (input$shape=='uniform') runif(n(),x.CO_e.fhs()-x.CO_e.fhs.sd(),x.CO_e.fhs()+x.CO_e.fhs.sd())
        else if (input$shape=='normal') rnorm(n(),x.CO_e.fhs(),x.CO_e.fhs.sd()/sqrt(3))
      )
    }
    else 0
  })
  output$x.CO_e.fhs.sd = renderPrint(cat(sd(x.CO_e.fhs.MC())/ppm,"ppm"))
  output$x.CO_e.fhs.rsd = renderText(x.CO_e.fhs.sd()/x.CO_e.fhs())
  # Exposure to second hand smoke (minutes):
  shs_e.time = reactive(input$shs_e.time*minute)
  shs_e.time.sd = reactive(input$shs_e.time.sd*minute)
  shs_e.time.MC = reactive(
    if (input$shape=='uniform') runif(n(),shs_e.time()-shs_e.time.sd(),shs_e.time()+shs_e.time.sd())
    else if (input$shape=='normal') rnorm(n(),shs_e.time(),shs_e.time.sd()/sqrt(3))
  )
  output$shs_e.time.rsd = renderText(shs_e.time.sd()/shs_e.time())
  # Exposure to second hand smoke (%):
  shs_e.percent = reactive(input$shs_e.percent*percent)
  shs_e.percent.sd = reactive(input$shs_e.percent.sd*percent)
  shs_e.percent.MC = reactive(
    if (input$shape=='uniform') runif(n(),shs_e.percent()-shs_e.percent.sd(),shs_e.percent()+shs_e.percent.sd())
    else if (input$shape=='normal') rnorm(n(),shs_e.percent(),shs_e.percent.sd()/sqrt(3))
  )
  output$shs_e.percent.rsd = renderText(shs_e.percent.sd()/shs_e.percent())
  # Exposure to second hand smoke (ppm):
  shs_e.ppm = reactive(input$shs_e.ppm*ppm)
  shs_e.ppm.sd = reactive(input$shs_e.ppm.sd*ppm)
  output$shs_e.ppm.rsd = renderText(shs_e.ppm.sd()/shs_e.ppm())
  # CO exposure from second hand smoke (ppm)
  x.CO_e.shs = reactive({
    if (input$shs_e){
      if (input$shs_e_method=='time') steadyState_s(SS=1)*shs_e.time()/t_e()
      else if (input$shs_e_method=='percent') steadyState_s(SS=1)*shs_e.percent()
      else if (input$shs_e_method=='ppm') input$shs_e.ppm*ppm  # I think this needs to be changed to shs_e.ppm()
    }
    else 0
  })
  output$x.CO_e.shs = renderPrint(cat("x.CO_e.shs =",x.CO_e.shs()/ppm,"ppm"))
  x.CO_e.shs.MC = reactive({
    if (input$shs_e){
      if (input$shs_e_method=='time') steadyState_s(SS=1)*shs_e.time.MC()/t_e.MC()
      else if (input$shs_e_method=='percent') steadyState_s(SS=1)*shs_e.percent.MC()
      else if (input$shs_e_method=='ppm') (
        if (input$shape=='uniform') runif(n(),x.CO_e.shs()-shs_e.ppm.sd(),x.CO_e.shs()+shs_e.ppm.sd())
        else if (input$shape=='normal') rnorm(n(),x.CO_e.shs(),shs_e.ppm.sd()/sqrt(3))
      )
    }
    else 0*runif(n(),x.CO_e.shs()-shs_e.ppm.sd(),x.CO_e.shs()+shs_e.ppm.sd())
  })
  output$x.CO_e.shs.sd = renderPrint(cat(sd(x.CO_e.shs.MC())/ppm,"ppm"))
  #
  VA_e = reactive(VA(AL=AL_e(),PB=PB()))
  output$VA_e = renderPrint(cat("VA_e =",VA_e()/(liter/minute),"liter/minute"))
  VA_e.MC = reactive(VA(AL=AL_e.MC(),PB=PB.MC()))
  output$VA_e.sd = renderPrint(cat(sd(VA_e.MC())/(liter/minute),"liter/minute"))
  #
  DL_e = reactive(DL(AL=AL_e(),PB=PB(),x.O2=x.O2_e()))
  output$DL_e = renderPrint(cat("DL_e =",DL_e()/(mL/minute/mmHg),"mL/minute/mmHg"))
  DL_e.MC = reactive(DL(AL=AL_e.MC(),PB=PB.MC(),x.O2=x.O2_e.MC()))
  output$DL_e.sd = renderPrint(cat(sd(DL_e.MC())/(mL/minute/mmHg),"mL/minute/mmHg"))
  #
  beta_e = reactive(beta(PB=PB(),DL=DL_e(),VA=VA_e()))
  output$beta_e = renderPrint(cat("beta_e =",beta_e()/(mmHg/mL),"mmHg*second/mL"))
  beta_e.MC = reactive(beta(PB=PB.MC(),DL=DL_e.MC(),VA=VA_e()))
  output$beta_e.sd = renderPrint(cat(sd(beta_e.MC())/(mmHg/mL),"mmHg*second/mL"))
  #
  PICO_e = reactive(PICO(PB=PB(),x.CO=x.CO_e()))
  output$PICO_e = renderPrint(cat("PICO_e =",PICO_e()/mmHg,"mmHg"))
  PICO_e.MC = reactive(PICO(PB=PB.MC(),x.CO=x.CO_e.MC()))
  output$PICO_e.sd = renderPrint(cat(sd(PICO_e.MC())/mmHg,"mmHg"))
  #
  PCO2_e = reactive(PCO2(PB=PB(),x.CO=x.CO_e(),x.O2=x.O2_e()))
  output$PCO2_e = renderPrint(cat("PCO2_e =",PCO2_e()/mmHg,"mmHg"))
  PCO2_e.MC = reactive(PCO2(PB=PB.MC(),x.CO=x.CO_e.MC(),x.O2=x.O2_e.MC()))
  output$PCO2_e.sd = renderPrint(cat(sd(PCO2_e.MC())/mmHg,"mmHg"))
  #
  x.CO_e = reactive(findMeanCO(t=t_e(),COHb.i=COHb.A(),COHb.f=COHb.B(),Vb=Vb(),beta=beta_e(),Hb=Hb(),PB=PB(),x.O2=x.O2_e()))
  output$x.CO_e = renderPrint(cat("x.CO_e =",x.CO_e()/ppm,"ppm"))
  x.CO_e.MC = reactive(findMeanCO(t=t_e.MC(),COHb.i=COHb.A.MC(),COHb.f=COHb.B.MC(),Vb=Vb.MC(),beta=beta_e.MC(),Hb=Hb.MC(),PB=PB.MC(),x.O2=x.O2_e.MC()))
  output$x.CO_e.fhsd = renderPrint(cat(sd(x.CO_e.MC())/ppm,"ppm"))
  #
  x.CO_e.o = reactive(x.CO_e()-x.CO_e.fhs()-x.CO_e.shs())
  output$x.CO_e.o = renderPrint(cat("x.CO_e.o =",x.CO_e.o()/ppm,"ppm"))
  x.CO_e.o.MC = reactive(x.CO_e.MC()-x.CO_e.fhs.MC()-x.CO_e.shs.MC())
  output$x.CO_e.o.sd = renderPrint(cat(sd(x.CO_e.o.MC())/ppm,"ppm"))
  #================================================== Clearance ==================================================#
  # Clearance duration (minutes)
  t_c = reactive(input$t_c*minute)
  t_c.sd = reactive(input$t_c.sd*minute)
  t_c.MC = reactive(
    if (input$shape=='uniform') runif(n(),t_c()-t_c.sd(),t_c()+t_c.sd())
    else if (input$shape=='normal') rnorm(n(),t_c(),t_c.sd()/sqrt(3))
  )
  output$t_c.rsd = renderText(t_c.sd()/t_c())
  # Clearance activity Level:
  AL_c = reactive(input$AL_c)
  AL_c.sd = reactive(input$AL_c.sd)
  AL_c.MC = reactive(
    if (input$shape=='uniform') runif(n(),AL_c()-AL_c.sd(),AL_c()+AL_c.sd())
    else if (input$shape=='normal') rnorm(n(),AL_c(),AL_c.sd()/sqrt(3))
  )
  output$AL_c.rsd = renderText(AL_c.sd()/AL_c())
  # Clearance oxygen level (% oxygen)
  x.O2_c = reactive(input$x.O2_c*percent)
  x.O2_c.sd = reactive(input$x.O2_c.sd*percent)
  x.O2_c.MC = reactive(
    if (input$shape=='uniform') runif(n(),x.O2_c()-x.O2_c.sd(),x.O2_c()+x.O2_c.sd())
    else if (input$shape=='normal') rnorm(n(),x.O2_c(),x.O2_c.sd()/sqrt(3))
    )
  output$x.O2_c.rsd = renderText(x.O2_c.sd()/x.O2_c())
  # Cigarettes smoked during clearance
  fhs_c.cigarettes = reactive(input$fhs_c.cigarettes)
  fhs_c.cigarettes.sd = reactive(input$fhs_c.cigarettes.sd)
  fhs_c.cigarettes.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_c.cigarettes()-fhs_c.cigarettes.sd(),fhs_c.cigarettes()+fhs_c.cigarettes.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_c.cigarettes(),fhs_c.cigarettes.sd()/sqrt(3))
    )
  output$fhs_c.cigarettes.rsd = renderText(fhs_c.cigarettes.sd()/fhs_c.cigarettes())
  # Fraction of clearance smoked (%)
  fhs_c.percent = reactive(input$fhs_c.percent*percent)
  fhs_c.percent.sd = reactive(input$fhs_c.percent.sd*percent)
  fhs_c.percent.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_c.percent()-fhs_c.percent.sd(),fhs_c.percent()+fhs_c.percent.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_c.percent(),fhs_c.percent.sd()/sqrt(3))
  )
  output$fhs_c.percent.rsd = renderText(fhs_c.percent.sd()/fhs_c.percent())
  # CO clearance from smoking (ppm)
  fhs_c.ppm = reactive(input$fhs_c.ppm*ppm)
  fhs_c.ppm.sd = reactive(input$fhs_c.ppm.sd*ppm)
  fhs_c.ppm.MC = reactive(
    if (input$shape=='uniform') runif(n(),fhs_c.ppm()-fhs_c.ppm.sd(),fhs_c.ppm()+fhs_c.ppm.sd())
    else if (input$shape=='normal') rnorm(n(),fhs_c.ppm(),fhs_c.ppm.sd()/sqrt(3))
  )
  output$fhs_c.ppm.rsd = renderText(fhs_c.ppm.sd()/fhs_c.ppm())
  # CO clearance from first hand smoke (ppm)
  x.CO_c.fhs = reactive({
    if (input$smoker){
      if (input$fhs_c_method=='cigarettes') steadyState_c(cigs=fhs_c.cigarettes()*960*minute/t_c())
      else if (input$fhs_c_method=='percent') {
        if (input$SS_method=='cigarettes') steadyState_c(cigs=cigarettes()*fhs_c.percent())
        else if (input$SS_method=='status') steadyState_s(SS=SS()*fhs_c.percent())
        # ERROR message intentional here if attempting to find x.CO_c.fhs from a known ppm for Initial COHb in blood.
      }
      else if (input$fhs_c_method=='ppm') fhs_c.ppm()
    }
    else 0 # Should this be: steadyState_c(cigs=0)
  })
  output$x.CO_c.fhs = renderPrint(cat("x.CO_c.fhs =",x.CO_c.fhs()/ppm,"ppm"))
  x.CO_c.fhs.sd = reactive(fhs_c.ppm.sd())
  x.CO_c.fhs.MC = reactive({
    if (input$fhs_c_method=='cigarettes') steadyState_c(cigs=fhs_c.cigarettes.MC()*960*minute/t_c.MC())
    else if (input$fhs_c_method=='percent') {
      if (input$SS_method=='cigarettes') steadyState_c(cigs=cigarettes.MC()*fhs_c.percent.MC())
      else if (input$SS_method=='status') steadyState_s(SS=SS.MC()*fhs_c.percent.MC())
    }
    else if (input$fhs_c_method=='ppm') (
      if (input$shape=='uniform') runif(n(),x.CO_c.fhs()-x.CO_c.fhs.sd(),x.CO_c.fhs()+x.CO_c.fhs.sd())
      else if (input$shape=='normal') rnorm(n(),x.CO_c.fhs(),x.CO_c.fhs.sd()/sqrt(3))
    )
  })
  output$x.CO_c.fhs.sd = renderPrint(cat(sd(x.CO_c.fhs.MC())/ppm,"ppm"))
  output$x.CO_c.fhs.rsd = renderText(x.CO_c.fhs.sd()/x.CO_c.fhs())
  # Clearance to second hand smoke (minutes):
  shs_c.time = reactive(input$shs_c.time*minute)
  shs_c.time.sd = reactive(input$shs_c.time.sd*minute)
  shs_c.time.MC = reactive(
    if (input$shape=='uniform') runif(n(),shs_c.time()-shs_c.time.sd(),shs_c.time()+shs_c.time.sd())
    else if (input$shape=='normal') rnorm(n(),shs_c.time(),shs_c.time.sd()/sqrt(3))
  )
  output$shs_c.time.rsd = renderText(shs_c.time.sd()/shs_c.time())
  # Clearance to second hand smoke (%):
  shs_c.percent = reactive(input$shs_c.percent*percent)
  shs_c.percent.sd = reactive(input$shs_c.percent.sd*percent)
  shs_c.percent.MC = reactive(
    if (input$shape=='uniform') runif(n(),shs_c.percent()-shs_c.percent.sd(),shs_c.percent()+shs_c.percent.sd())
    else if (input$shape=='normal') rnorm(n(),shs_c.percent(),shs_c.percent.sd()/sqrt(3))
  )
  output$shs_c.percent.rsd = renderText(shs_c.percent.sd()/shs_c.percent())
  # Clearance to second hand smoke (ppm):
  shs_c.ppm = reactive(input$shs_c.ppm*ppm)
  shs_c.ppm.sd = reactive(input$shs_c.ppm.sd*ppm)
  output$shs_c.ppm.rsd = renderText(shs_c.ppm.sd()/shs_c.ppm())
  # CO clearance from second hand smoke (ppm)
  x.CO_c.shs = reactive({
    if (input$shs_c){
      if (input$shs_c_method=='time') steadyState_s(SS=1)*shs_c.time()/t_c()
      else if (input$shs_c_method=='percent') steadyState_s(SS=1)*shs_c.percent()
      else if (input$shs_c_method=='ppm') input$shs_c.ppm*ppm  # I think this needs to be changed to shs_c.ppm()
    }
    else 0
  })
  output$x.CO_c.shs = renderPrint(cat("x.CO_c.shs =",x.CO_c.shs()/ppm,"ppm"))
  x.CO_c.shs.MC = reactive({
    if (input$shs_c){
      if (input$shs_c_method=='time') steadyState_s(SS=1)*shs_c.time.MC()/t_c.MC()
      else if (input$shs_c_method=='percent') steadyState_s(SS=1)*shs_c.percent.MC()
      else if (input$shs_c_method=='ppm') (
        if (input$shape=='uniform') runif(n(),x.CO_c.shs()-shs_c.ppm.sd(),x.CO_c.shs()+shs_c.ppm.sd())
        else if (input$shape=='normal') rnorm(n(),x.CO_c.shs(),shs_c.ppm.sd()/sqrt(3))
      )
    }
    else 0*runif(n(),x.CO_c.shs()-shs_c.ppm.sd(),x.CO_c.shs()+shs_c.ppm.sd())
  })
  output$x.CO_c.shs.sd = renderPrint(cat(sd(x.CO_c.shs.MC())/ppm,"ppm"))
  # Carbon Monoxide Level (ppm)
  x.CO_c = reactive(x.CO_c.fhs()+x.CO_c.shs())
  x.CO_c.MC = reactive(x.CO_c.fhs.MC()+x.CO_c.shs.MC())
  x.CO_c.sd = reactive(sd(x.CO_c.MC()))
  output$x.CO_c = renderPrint(cat(x.CO_c()/ppm,"ppm"))
  output$x.CO_c.sd = renderPrint(cat(x.CO_c.sd()/ppm,"ppm"))
  output$x.CO_c.rsd = renderText(x.CO_c.sd()/x.CO_c())
  VA_c = reactive(VA(AL=AL_c(),PB=PB()))
  output$VA_c = renderPrint(cat("VA_c =",VA_c()/(liter/minute),"liter/minute"))
  VA_c.MC = reactive(VA(AL=AL_c.MC(),PB=PB.MC()))
  output$VA_c.sd = renderPrint(cat(sd(VA_c.MC())/(liter/minute),"liter/minute"))
  #
  DL_c = reactive(DL(AL=AL_c(),PB=PB(),x.O2=x.O2_c()))
  output$DL_c = renderPrint(cat("DL_c =",DL_c()/(mL/minute/mmHg),"mL/minute/mmHg"))
  DL_c.MC = reactive(DL(AL=AL_c.MC(),PB=PB.MC(),x.O2=x.O2_c.MC()))
  output$DL_c.sd = renderPrint(cat(sd(DL_c.MC())/(mL/minute/mmHg),"mL/minute/mmHg"))
  #
  beta_c = reactive(beta(PB=PB(),DL=DL_c(),VA=VA_c()))
  output$beta_c = renderPrint(cat("beta_c =",beta_c()/(mmHg/mL),"mmHg*second/mL"))
  beta_c.MC = reactive(beta(PB=PB.MC(),DL=DL_c.MC(),VA=VA_c.MC()))
  output$beta_c.sd = renderPrint(cat(sd(beta_c.MC())/(mmHg/mL),"mmHg*second/mL"))
  #
  PICO_c = reactive(PICO(PB=PB(),x.CO=x.CO_c()))
  output$PICO_c = renderPrint(cat("PICO_c =",PICO_c()/mmHg,"mmHg"))
  PICO_c.MC = reactive(PICO(PB=PB.MC(),x.CO=x.CO_c.MC()))
  output$PICO_c.sd = renderPrint(cat(sd(PICO_c.MC())/mmHg,"mmHg"))
  #
  PCO2_c = reactive(PCO2(PB=PB(),x.CO=x.CO_c(),x.O2=x.O2_c()))
  output$PCO2_c = renderPrint(cat("PCO2_c =",PCO2_c()/mmHg,"mmHg"))
  PCO2_c.MC = reactive(PCO2(PB=PB.MC(),x.CO=x.CO_c.MC(),x.O2=x.O2_c.MC()))
  output$PCO2_c.sd = renderPrint(cat(sd(PCO2_c.MC())/mmHg,"mmHg"))
  #
  COHb.B = reactive(findInitCOHb(t=t_c(),COHb.f=COHb.C(),Vb=Vb(),beta=beta_c(),PICO=PICO_c(),PCO2=PCO2_c(),Hb=Hb()))
  output$COHb.B = renderPrint(cat("COHb.B =",COHb.B(),"mL/mL"))
  COHb.B.MC = reactive(findInitCOHb(t=t_c.MC(),COHb.f=COHb.C.MC(),Vb=Vb.MC(),beta=beta_c.MC(),PICO=PICO_c.MC(),PCO2=PCO2_c.MC(),Hb=Hb.MC()))
  output$COHb.B.sd = renderPrint(cat(sd(COHb.B.MC()),"mL/mL"))
  #
  XCOHb.B = reactive(COHb.B()/Hf/Hb())
  output$XCOHb.B = renderPrint(cat("XCOHb.B =",XCOHb.B()/percent,"%"))
  XCOHb.B.MC = reactive(COHb.B.MC()/Hf/Hb.MC())
  output$XCOHb.B.sd = renderPrint(cat(sd(XCOHb.B.MC())/percent,"%"))
  #================================================== Oxygen Therapy ==================================================#
  # Oxygen Therapy duration (minutes)
  t_t = reactive(input$t_t*minute)
  t_t.sd = reactive(input$t_t.sd*minute)
  t_t.MC = reactive(
    if (input$shape=='uniform') runif(n(),t_t()-t_t.sd(),t_t()+t_t.sd())
    else if (input$shape=='normal') rnorm(n(),t_t(),t_t.sd()/sqrt(3))
  )
  output$t_t.rsd = renderText(t_t.sd()/t_t())
  # Oxygen Therapy activity Level
  AL_t = reactive(input$AL_t)
  AL_t.sd = reactive(input$AL_t.sd)
  AL_t.MC = reactive(
    if (input$shape=='uniform') runif(n(),AL_t()-AL_t.sd(),AL_t()+AL_t.sd())
    else if (input$shape=='normal') rnorm(n(),AL_t(),AL_t.sd()/sqrt(3))
  )
  output$AL_t.rsd = renderText(AL_t.sd()/AL_t())
  # Oxygen Therapy oxygen level (% oxygen)
  x.O2_t = reactive(
    if (input$OT_method=='Oxygen level') input$x.O2_t*percent
    else if (input$OT_method=='Nasal Cannula (NC)') approx(c(1,6),c(24,44),input$NC.lpm)$y*percent
    else if (input$OT_method=='Simple Face Mask (SFM)') approx(c(8,12),c(40,60),input$SFM.lpm)$y*percent
    else if (input$OT_method=='Non-Rebreather (NRB)') approx(c(10,15),c(80,100),input$NRB.lpm)$y*percent
    else if (input$OT_method=='Bag-valve-mask (BVM)') approx(c(10,15),c(60,100),input$BVM.lpm)$y*percent
  )
  output$x.O2_t = output$XCOHb.B.sd = renderPrint(cat(x.O2_t()/percent,"%"))
  #x.O2_t = reactive(input$x.O2_t*percent)
  x.O2_t.sd = reactive(input$x.O2_t.sd*percent)
  x.O2_t.MC = reactive(
    if (input$shape=='uniform') runif(n(),x.O2_t()-x.O2_t.sd(),x.O2_t()+x.O2_t.sd())
    else if (input$shape=='normal') rnorm(n(),x.O2_t(),x.O2_t.sd()/sqrt(3))
  )
  output$x.O2_t.rsd = renderText(x.O2_t.sd()/x.O2_t())
  # Oxygen Therapy carbon monoxide level (ppm)
  x.CO_t = reactive(input$x.CO_t*ppm)
  x.CO_t.sd = reactive(input$x.CO_t.sd*ppm)
  x.CO_t.MC = reactive(
    if (input$shape=='uniform') runif(n(),x.CO_t()-x.CO_t.sd(),x.CO_t()+x.CO_t.sd())
    else if (input$shape=='normal') rnorm(n(),x.CO_t(),x.CO_t.sd()/sqrt(3))
  )
  output$x.CO_t.rsd = renderText(x.CO_t.sd()/x.CO_t())
  #
  VA_t = reactive(VA(AL=AL_t(),PB=PB()))
  output$VA_t = renderPrint(cat("VA_t =",VA_t()/(liter/minute),"liter/minute"))
  VA_t.MC = reactive(VA(AL=AL_t.MC(),PB=PB.MC()))
  output$VA_t.sd = renderPrint(cat(sd(VA_t.MC())/(liter/minute),"liter/minute"))
  #
  DL_t = reactive(DL(AL=AL_t(),PB=PB(),x.O2=x.O2_t()))
  output$DL_t = renderPrint(cat("DL_t =",DL_t()/(mL/minute/mmHg),"mL/minute/mmHg"))
  DL_t.MC = reactive(DL(AL=AL_t.MC(),PB=PB.MC(),x.O2=x.O2_t.MC()))
  output$DL_t.sd = renderPrint(cat(sd(DL_t.MC())/(mL/minute/mmHg),"mL/minute/mmHg"))
  #
  beta_t = reactive(beta(PB=PB(),DL=DL_t(),VA=VA_t()))
  output$beta_t = renderPrint(cat("beta_t =",beta_t()/(mmHg/mL),"mmHg*second/mL"))
  beta_t.MC = reactive(beta(PB=PB.MC(),DL=DL_t.MC(),VA=VA_t.MC()))
  output$beta_t.sd = renderPrint(cat(sd(beta_t.MC())/(mmHg/mL),"mmHg*second/mL"))
  #
  PICO_t = reactive(PICO(PB=PB(),x.CO=x.CO_t()))
  output$PICO_t = renderPrint(cat("PICO_t =",PICO_t()/mmHg,"mmHg"))
  PICO_t.MC = reactive(PICO(PB=PB.MC(),x.CO=x.CO_t.MC()))
  output$PICO_t.sd = renderPrint(cat(sd(PICO_t.MC())/mmHg,"mmHg"))
  #
  PCO2_t = reactive(PCO2(PB=PB(),x.CO=x.CO_t(),x.O2=x.O2_t()))
  output$PCO2_t = renderPrint(cat("PCO2_t =",PCO2_t()/mmHg,"mmHg"))
  PCO2_t.MC = reactive(PCO2(PB=PB.MC(),x.CO=x.CO_t.MC(),x.O2=x.O2_t.MC()))
  output$PCO2_t.sd = renderPrint(cat(sd(PCO2_t.MC())/mmHg,"mmHg"))
  #
  COHb.C = reactive(findInitCOHb(t=t_t(),COHb.f=COHb.D(),Vb=Vb(),beta=beta_t(),PICO=PICO_t(),PCO2=PCO2_t(),Hb=Hb()))
  output$COHb.C = renderPrint(cat("COHb.C =",COHb.C(),"mL/mL"))
  COHb.C.MC = reactive(findInitCOHb(t=t_t.MC(),COHb.f=COHb.D.MC(),Vb=Vb.MC(),beta=beta_t.MC(),PICO=PICO_t.MC(),PCO2=PCO2_t.MC(),Hb=Hb.MC()))
  output$COHb.C.sd = renderPrint(cat(sd(COHb.C.MC()),"mL/mL"))
  #
  XCOHb.C = reactive(COHb.C()/Hf/Hb())
  output$XCOHb.C = renderPrint(cat("XCOHb.C =",XCOHb.C()/percent,"%"))
  XCOHb.C.MC = reactive(COHb.C.MC()/Hf/Hb.MC())
  output$XCOHb.C.sd = renderPrint(cat(sd(XCOHb.C.MC())/percent,"%"))
  #================================================== Standard ==================================================#
  # Occupational Exposure Limit (ppm)
  OEL = reactive(
    if (input$ExposureLimit=='custom PEL') input$OEL*ppm
    else if (input$ExposureLimit=='OSHA PEL') 50*ppm
    else if (input$ExposureLimit=='NIOSH REL') 35*ppm
  )
  OelLabel = reactive(input$ExposureLimit)
  output$OEL = renderPrint(cat("TWA =",OEL()/(ppm),"ppm"))
  #================================================== Parameters ==================================================#
  # Number of Monte Carlo simulations
  n = reactive(input$n)
  #================================================== Summary ==================================================#
  # Averaging period
  AveragingPeriod8h = reactive(ifelse(t_e() > 480*minute, t_e(), 480*minute))
  AveragingPeriod8h.MC = reactive(ifelse(t_e() > 480*minute, t_e.MC(), 480*minute))
  # 8-hour total weight average (TWA) exposure
  TWA8Hours = reactive(x.CO_e.o()*t_e()/AveragingPeriod8h())
  output$TWA8Hours = renderPrint(cat("TWA8Hours =",TWA8Hours()/ppm,"ppm"))
  TWA8Hours.MC = reactive(x.CO_e.o.MC()*t_e.MC()/AveragingPeriod8h.MC())
  output$TWA8Hours.sd = renderPrint(cat(sd(TWA8Hours.MC())/ppm,"ppm"))
  # SAE for exposure
  SAE.exposure = reactive({
    if (input$doMonteCarlo) round(1.64485*sd(x.CO_e.o.MC())/mean(x.CO_e.o.MC())/percent,1)
    else "?"
  })
  #SAE for 8-hour total weight average (TWA) exposure
  SAE.8hTWA = reactive({
    if (input$doMonteCarlo) round(1.64485*sd(TWA8Hours.MC())/mean(TWA8Hours.MC())/percent,1)
    else "?"
  })
  #SAE.ppmCOminutes
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('reportTemplate.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    contentType = 'application/pdf'
  )
  #================================================== Abstract ==================================================#
  output$abstract = renderPrint(
    cat(
      "Employee",input$name,"(sample ",input$ID,
      ") was subjected to a calculated mean carbon monoxide occupational exposure of",round(x.CO_e.o()/ppm,1),
      "ppm (SAE =",SAE.exposure(),
      "%) for a duration of",t_e()/minute,
      "minutes. The carboxyhemoglobin in the employee's blood reached a calculated peak level of",round(XCOHb.B()/percent,1),
      "% at the end of the exposure. The calculated 8-hour total weight average (TWA) exposure is",round(TWA8Hours()/ppm,1),
      "ppm CO (SAE =",SAE.8hTWA(),
      "%) which is",round(TWA8Hours()/OEL(),2),
      "times the",OelLabel(),
      "of",OEL()/ppm,
      "ppm."
    )
  )
  #================================================== Plot ==================================================#
  output$timePlot <- renderPlot({
    t.e = 0:t_e()
    t.e = seq(0,t_e(),input$deltaT)
    e = rk4(y=COHb.A(),t=t.e,parms=NULL,func=CFK,Vb=Vb(),beta=beta_e(),PICO=PICO_e(),PCO2=PCO2_e(),Hb=Hb())
    t.c = seq(t_e(),(t_c()+t_e()),input$deltaT)
    c = rk4(y=COHb.B(),times=t.c,parms=NULL,func=CFK,Vb=Vb(),beta=beta_c(),PICO=PICO_c(),PCO2=PCO2_c(),Hb=Hb())
    t.o = seq((t_c()+t_e()),(t_c()+t_e()+t_t()),input$deltaT)
    o = rk4(y=COHb.C(),times=t.o,parms=NULL,func=CFK,Vb=Vb(),beta=beta_t(),PICO=PICO_t(),PCO2=PCO2_t(),Hb=Hb())
    par(mar=c(4,4,1.5,1.5),mex=0.8,cex=0.8,mgp=c(2,0.5,0),tcl=0.3)
    plot(c(0,(t_e()+t_c()+t_t())/60),c(0,XCOHb.B()/percent),type='n',xlab="Time (minutes)",ylab="COHb (%)")
    polygon(c(0,e[,1]/60,t_e()/60), c(0,e[,2],0)/Hf/Hb()/percent, col='red', density=10, angle=60) 
    polygon(c(t_e()/60,c[,1]/60,(t_e()+t_c())/60), c(0,c[,2],0)/Hf/Hb()/percent, col='blue', density=15, angle=-30)
    polygon(c((t_e()+t_c())/60,o[,1]/60,(t_e()+t_c()+t_t())/60), c(0,o[,2],0)/Hf/Hb()/percent, col='green', density=10, angle=45)
    abline(h=XCOHb.B()/percent,lty=3)
    text(t_e()/60,1.02*XCOHb.B()/percent,sprintf("Maximum COHb = %.1f%%", XCOHb.B()/percent))
    text(0,0.96*XCOHb.B()/percent,sprintf("Occupational Exposure = %.1f ppm", x.CO_e.o()/ppm),adj = c(0,0))
  })
  #================================================== Download CSV ==================================================#
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$ID, ".csv", sep = "")
    },
    content = function(file) {
      df <- data.frame(value=double(), uncertainty=double(), units=character())
      # Sample
      df <- rbind(df, "ID" = list(value=NA, uncertainty=NA, units=input$ID), stringsAsFactors=FALSE)
      df <- rbind(df, "COHb evaluation method" = list(value=NA, uncertainty=NA, units=input$COHb_method), stringsAsFactors=FALSE)
      df <- rbind(df, "COHb in blood" = list(value=input$XCOHb, uncertainty=input$XCOHb.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "Hb evaluation method" = list(value=NA, uncertainty=NA, units=input$Hb_method), stringsAsFactors=FALSE)
      df <- rbind(df, "hemoglobin in blood" = list(value=input$Hb, uncertainty=input$Hb.sd, units="grams/100mL"), stringsAsFactors=FALSE)
      # Employee
      df <- rbind(df, "name" = list(value=NA, uncertainty=NA, units=input$name), stringsAsFactors=FALSE)
      df <- rbind(df, "gender" = list(value=NA, uncertainty=NA, units=input$gender), stringsAsFactors=FALSE)
      df <- rbind(df, "height" = list(value=input$h, uncertainty=input$h.sd, units="inch"), stringsAsFactors=FALSE)
      df <- rbind(df, "weight" = list(value=input$w, uncertainty=input$w.sd, units="pound"), stringsAsFactors=FALSE)
      df <- rbind(df, "smoker" = list(value=input$smoker, uncertainty=NA, units=NA), stringsAsFactors=FALSE)
      df <- rbind(df, "smoker evaluation method" = list(value=NA, uncertainty=NA, units=input$SS_method), stringsAsFactors=FALSE)
      df <- rbind(df, "cigarettes" = list(value=input$cigarettes, uncertainty=input$cigarettes.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "smoker status" = list(value=input$SS, uncertainty=SS.sd(), units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "initial COHb in blood" = list(value=input$XCOHb.0, uncertainty=input$XCOHb.0.sd, units="%"), stringsAsFactors=FALSE)
      # Environment
      df <- rbind(df, "pressure evaluation method" = list(value=NA, uncertainty=NA, units=input$PB_method), stringsAsFactors=FALSE)
      df <- rbind(df, "atmospheric pressure" = list(value=input$PB, uncertainty=input$PB.sd, units="mmHg"), stringsAsFactors=FALSE)
      df <- rbind(df, "elevation" = list(value=input$z, uncertainty=input$z.sd, units="ft"), stringsAsFactors=FALSE)
      # Exposure
      df <- rbind(df, "exposure duration" = list(value=input$t_e, uncertainty=input$t_e.sd, units="minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure activity level" = list(value=input$AL_e, uncertainty=input$AL_e.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure oxygen level" = list(value=input$x.O2_e, uncertainty=input$x.O2_e.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure first hand smoke evaluation method" = list(value=NA, uncertainty=NA, units=input$fhs_e_method), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure cigarettes smoked" = list(value=input$fhs_e.cigarettes, uncertainty=input$fhs_e.cigarettes.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure fraction smoked" = list(value=input$fhs_e.percent, uncertainty=input$fhs_e.percent.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure CO from smoking" = list(value=input$fhs_e.ppm, uncertainty=input$fhs_e.ppm.sd, units="ppm"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure second hand smoker" = list(value=input$shs_e, uncertainty=NA, units=NA), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure second hand smoke evaluation method" = list(value=NA, uncertainty=NA, units=input$shs_e_method), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure second hand smoke time" = list(value=input$shs_e.time, uncertainty=input$shs_e.time.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure second hand smoke percent" = list(value=input$shs_e.percent, uncertainty=input$shs_e.percent.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "exposure second hand smoke ppm" = list(value=input$shs_e.ppm, uncertainty=input$shs_e.ppm.sd, units="%"), stringsAsFactors=FALSE)
      # Clearance
      df <- rbind(df, "clearance duration" = list(value=input$t_c, uncertainty=input$t_c.sd, units="minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance activity level" = list(value=input$AL_c, uncertainty=input$AL_c.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance oxygen level" = list(value=input$x.O2_c, uncertainty=input$x.O2_c.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance first hand smoke evaluation method" = list(value=NA, uncertainty=NA, units=input$fhs_c_method), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance cigarettes smoked" = list(value=input$fhs_c.cigarettes, uncertainty=input$fhs_c.cigarettes.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance fraction smoked" = list(value=input$fhs_c.percent, uncertainty=input$fhs_c.percent.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance CO from smoking" = list(value=input$fhs_c.ppm, uncertainty=input$fhs_c.ppm.sd, units="ppm"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance second hand smoker" = list(value=input$shs_c, uncertainty=NA, units=NA), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance second hand smoke evaluation method" = list(value=NA, uncertainty=NA, units=input$shs_c_method), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance second hand smoke time" = list(value=input$shs_c.time, uncertainty=input$shs_c.time.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance second hand smoke percent" = list(value=input$shs_c.percent, uncertainty=input$shs_c.percent.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "clearance second hand smoke ppm" = list(value=input$shs_c.ppm, uncertainty=input$shs_c.ppm.sd, units="%"), stringsAsFactors=FALSE)
      # Oxygen Therapy
      df <- rbind(df, "oxygen therapy duration" = list(value=input$t_t, uncertainty=input$t_t.sd, units="minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "oxygen therapy activity level" = list(value=input$AL_t, uncertainty=input$AL_t.sd, units=""), stringsAsFactors=FALSE)
      df <- rbind(df, "oxygen therapy method" = list(value=NA, uncertainty=NA, units=input$OT_method), stringsAsFactors=FALSE)
      df <- rbind(df, "Nasal Cannula (NC)" = list(value=input$NC.lpm, uncertainty=0, units="liter/minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "Simple Face Mask (SFM)" = list(value=input$SFM.lpm, uncertainty=0, units="liter/minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "Non-Rebreather (NRB)" = list(value=input$NRB.lpm, uncertainty=0, units="liter/minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "Bag-valve-mask (BVM)" = list(value=input$BVM.lpm, uncertainty=0, units="liter/minute"), stringsAsFactors=FALSE)
      df <- rbind(df, "oxygen therapy oxygen level" = list(value=input$x.O2_t, uncertainty=input$x.O2_t.sd, units="%"), stringsAsFactors=FALSE)
      df <- rbind(df, "oxygen therapy carbon monoxide level" = list(value=input$x.CO_t, uncertainty=input$x.CO_t.sd, units="ppm"), stringsAsFactors=FALSE)
      # Summary
      df <- rbind(df, "employer" = list(value=NA, uncertainty=NA, units=input$employer), stringsAsFactors=FALSE)
      df <- rbind(df, "inspectionNumber" = list(value=NA, uncertainty=NA, units=input$inspectionNumber), stringsAsFactors=FALSE)
      df <- rbind(df, "complianceOfficer" = list(value=NA, uncertainty=NA, units=input$complianceOfficer), stringsAsFactors=FALSE)
      df <- rbind(df, "areaOffice" = list(value=NA, uncertainty=NA, units=input$areaOffice), stringsAsFactors=FALSE)
      df <- rbind(df, "region" = list(value=NA, uncertainty=NA, units=input$region), stringsAsFactors=FALSE)
      df <- rbind(df, "requestDate" = list(value=NA, uncertainty=NA, units=input$requestDate), stringsAsFactors=FALSE)
      df <- rbind(df, "exposureDate" = list(value=NA, uncertainty=NA, units=input$exposureDate), stringsAsFactors=FALSE)
      write.csv(df, file)
    }
  )
  #================================================== Upload CSV ==================================================#
  output$CSVcontents <- renderPrint({
    req(input$CSVfile)
    tryCatch(
      {
        inFile <- input$CSVfile
        if (is.null(inFile))return(NULL)
        tmp = read.csv(inFile$datapath, header = TRUE, row.names = 1)
        # Sample
        if ('ID' %in% row.names(tmp)) updateTextInput(session, inputId = "ID", value = tmp["ID","units"])
        if ('COHb evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "COHb_method", value = tmp["COHb evaluation method","units"])
        if ('COHb in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "XCOHb", value = tmp["COHb in blood","value"])
        if ('COHb in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "XCOHb.sd", value = tmp["COHb in blood","uncertainty"])
        if ('Hb evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "Hb_method", value = tmp["Hb evaluation method","units"])
        if ('hemoglobin in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "Hb", value = tmp["hemoglobin in blood","value"])
        if ('hemoglobin in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "Hb.sd", value = tmp["hemoglobin in blood","uncertainty"])
        # Employee
        if ('name' %in% row.names(tmp)) updateTextInput(session, inputId = "name", value = tmp["name","units"])
        if ('gender' %in% row.names(tmp)) updateTextInput(session, inputId = "gender", value = tmp["gender","units"])
        if ('height' %in% row.names(tmp)) updateTextInput(session, inputId = "h", value = tmp["height","value"])
        if ('height' %in% row.names(tmp)) updateTextInput(session, inputId = "h.sd", value = tmp["height","uncertainty"])
        if ('weight' %in% row.names(tmp)) updateTextInput(session, inputId = "w", value = tmp["weight","value"])
        if ('weight' %in% row.names(tmp)) updateTextInput(session, inputId = "w.sd", value = tmp["weight","uncertainty"])
        if ('smoker' %in% row.names(tmp)) updateCheckboxInput(session, inputId = "smoker", value = tmp["smoker","value"])
        if ('smoker evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "SS_method", value = tmp["smoker evaluation method","units"])
        if ('cigarettes' %in% row.names(tmp)) updateTextInput(session, inputId = "cigarettes", value = tmp["cigarettes","value"])
        if ('cigarettes' %in% row.names(tmp)) updateTextInput(session, inputId = "cigarettes.sd", value = tmp["cigarettes","uncertainty"])
        if ('smoker status' %in% row.names(tmp)) updateTextInput(session, inputId = "SS", value = tmp["smoker status","value"])
        if ('smoker status' %in% row.names(tmp)) updateTextInput(session, inputId = "SS.sd", value = tmp["smoker status","uncertainty"])
        if ('initial COHb in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "XCOHb.0", value = tmp["initial COHb in blood","value"])
        if ('initial COHb in blood' %in% row.names(tmp)) updateTextInput(session, inputId = "XCOHb.0.sd", value = tmp["initial COHb in blood","uncertainty"])
        # Environment
        if ('pressure evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "PB_method", value = tmp["pressure evaluation method","units"])
        if ('atmospheric pressure' %in% row.names(tmp)) updateTextInput(session, inputId = "PB", value = tmp["atmospheric pressure","value"])
        if ('atmospheric pressure' %in% row.names(tmp)) updateTextInput(session, inputId = "PB.sd", value = tmp["atmospheric pressure","uncertainty"])
        if ('elevation' %in% row.names(tmp)) updateTextInput(session, inputId = "z", value = tmp["elevation","value"])
        if ('elevation' %in% row.names(tmp)) updateTextInput(session, inputId = "z.sd", value = tmp["elevation","uncertainty"])
        # Exposure
        if ('exposure duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_e", value = tmp["exposure duration","value"])
        if ('exposure duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_e.sd", value = tmp["exposure duration","uncertainty"])
        if ('exposure activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_e", value = tmp["exposure activity level","value"])
        if ('exposure activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_e.sd", value = tmp["exposure activity level","uncertainty"])
        if ('exposure oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_e", value = tmp["exposure oxygen level","value"])
        if ('exposure oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_e.sd", value = tmp["exposure oxygen level","uncertainty"])
        if ('exposure first hand smoke evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e_method", value = tmp["exposure first hand smoke evaluation method","units"])
        if ('exposure cigarettes smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.cigarettes", value = tmp["exposure cigarettes smoked","value"])
        if ('exposure cigarettes smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.cigarettes.sd", value = tmp["exposure cigarettes smoked","uncertainty"])
        if ('exposure fraction smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.percent", value = tmp["exposure fraction smoked","value"])
        if ('exposure fraction smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.percent.sd", value = tmp["exposure fraction smoked","uncertainty"])
        if ('exposure CO from smoking' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.ppm", value = tmp["exposure CO from smoking","value"])
        if ('exposure CO from smoking' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_e.ppm.sd", value = tmp["exposure CO from smoking","uncertainty"])
        if ('exposure second hand smoker' %in% row.names(tmp)) updateCheckboxInput(session, inputId = "shs_e", value = tmp["exposure second hand smoker","value"])
        if ('exposure second hand smoke evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e_method", value = tmp["exposure second hand smoke evaluation method","units"])
        if ('exposure second hand smoke time' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.time", value = tmp["exposure second hand smoke time","value"])
        if ('exposure second hand smoke time' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.time.sd", value = tmp["exposure second hand smoke time","uncertainty"])
        if ('exposure second hand smoke percent' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.percent", value = tmp["exposure second hand smoke percent","value"])
        if ('exposure second hand smoke percent' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.percent.sd", value = tmp["exposure second hand smoke percent","uncertainty"])
        if ('exposure second hand smoke ppm' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.ppm", value = tmp["exposure second hand smoke ppm","value"])
        if ('exposure second hand smoke ppm' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_e.ppm.sd", value = tmp["exposure second hand smoke ppm","uncertainty"])
        # Clearance
        if ('clearance duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_c", value = tmp["clearance duration","value"])
        if ('clearance duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_c.sd", value = tmp["clearance duration","uncertainty"])
        if ('clearance activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_c", value = tmp["clearance activity level","value"])
        if ('clearance activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_c.sd", value = tmp["clearance activity level","uncertainty"])
        if ('clearance oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_c", value = tmp["clearance oxygen level","value"])
        if ('clearance oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_c.sd", value = tmp["clearance oxygen level","uncertainty"])
        if ('clearance first hand smoke evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c_method", value = tmp["clearance first hand smoke evaluation method","units"])
        if ('clearance cigarettes smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.cigarettes", value = tmp["clearance cigarettes smoked","value"])
        if ('clearance cigarettes smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.cigarettes.sd", value = tmp["clearance cigarettes smoked","uncertainty"])
        if ('clearance fraction smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.percent", value = tmp["clearance fraction smoked","value"])
        if ('clearance fraction smoked' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.percent.sd", value = tmp["clearance fraction smoked","uncertainty"])
        if ('clearance CO from smoking' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.ppm", value = tmp["clearance CO from smoking","value"])
        if ('clearance CO from smoking' %in% row.names(tmp)) updateTextInput(session, inputId = "fhs_c.ppm.sd", value = tmp["clearance CO from smoking","uncertainty"])
        if ('clearance second hand smoker' %in% row.names(tmp)) updateCheckboxInput(session, inputId = "shs_c", value = tmp["clearance second hand smoker","value"])
        if ('clearance second hand smoke evaluation method' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c_method", value = tmp["clearance second hand smoke evaluation method","units"])
        if ('clearance second hand smoke time' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.time", value = tmp["clearance second hand smoke time","value"])
        if ('clearance second hand smoke time' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.time.sd", value = tmp["clearance second hand smoke time","uncertainty"])
        if ('clearance second hand smoke percent' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.percent", value = tmp["clearance second hand smoke percent","value"])
        if ('clearance second hand smoke percent' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.percent.sd", value = tmp["clearance second hand smoke percent","uncertainty"])
        if ('clearance second hand smoke ppm' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.ppm", value = tmp["clearance second hand smoke ppm","value"])
        if ('clearance second hand smoke ppm' %in% row.names(tmp)) updateTextInput(session, inputId = "shs_c.ppm.sd", value = tmp["clearance second hand smoke ppm","uncertainty"])
        # Oxygen Therapy
        if ('oxygen therapy duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_t", value = tmp["oxygen therapy duration","value"])
        if ('oxygen therapy duration' %in% row.names(tmp)) updateTextInput(session, inputId = "t_t.sd", value = tmp["oxygen therapy duration","uncertainty"])
        if ('oxygen therapy activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_t", value = tmp["oxygen therapy activity level","value"])
        if ('oxygen therapy activity level' %in% row.names(tmp)) updateTextInput(session, inputId = "AL_t.sd", value = tmp["oxygen therapy activity level","uncertainty"])
        if ('oxygen therapy method' %in% row.names(tmp)) updateTextInput(session, inputId = "OT_method", value = tmp["oxygen therapy method","units"])
        if ('Nasal Cannula (NC)' %in% row.names(tmp)) updateTextInput(session, inputId = "NC.lpm", value = tmp["Nasal Cannula (NC)","value"])
        if ('Simple Face Mask (SFM)' %in% row.names(tmp)) updateTextInput(session, inputId = "SFM.lpm", value = tmp["Simple Face Mask (SFM)","value"])
        if ('Non-Rebreather (NRB)' %in% row.names(tmp)) updateTextInput(session, inputId = "NRB.lpm", value = tmp["Non-Rebreather (NRB)","value"])
        if ('Bag-valve-mask (BVM)' %in% row.names(tmp)) updateTextInput(session, inputId = "BVM.lpm", value = tmp["Bag-valve-mask (BVM)","value"])
        if ('oxygen therapy oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_t", value = tmp["oxygen therapy oxygen level","value"])
        if ('oxygen therapy oxygen level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.O2_t.sd", value = tmp["oxygen therapy oxygen level","uncertainty"])
        if ('oxygen therapy carbon monoxide level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.CO_t", value = tmp["oxygen therapy carbon monoxide level","value"])
        if ('oxygen therapy carbon monoxide level' %in% row.names(tmp)) updateTextInput(session, inputId = "x.CO_t.sd", value = tmp["oxygen therapy carbon monoxide level","uncertainty"])
        # Employee
        if ('employer' %in% row.names(tmp)) updateTextInput(session, inputId = "employer", value = tmp["employer","units"])
        if ('inspectionNumber' %in% row.names(tmp)) updateTextInput(session, inputId = "inspectionNumber", value = tmp["inspectionNumber","units"])
        if ('complianceOfficer' %in% row.names(tmp)) updateTextInput(session, inputId = "complianceOfficer", value = tmp["complianceOfficer","units"])
        if ('areaOffice' %in% row.names(tmp)) updateTextInput(session, inputId = "areaOffice", value = tmp["areaOffice","units"])
        if ('region' %in% row.names(tmp)) updateTextInput(session, inputId = "region", value = tmp["region","units"])
        if ('requestDate' %in% row.names(tmp)) updateTextInput(session, inputId = "requestDate", value = tmp["requestDate","units"])
        if ('exposureDate' %in% row.names(tmp)) updateTextInput(session, inputId = "exposureDate", value = tmp["exposureDate","units"])
      },
      error = function(e) {stop(safeError(e))}
    )
    return()
  })
  #================================================== Upload PRN ==================================================#
  output$PRNcontents <- renderPrint({
    req(input$PRNfile)
    tryCatch(
      {
        con = file(description=input$PRNfile$datapath, open="r")
        ID = scan(file=con, nlines=1, what="raw", sep=",", quiet=TRUE)[1]                                    #Sample Number: ID
          updateTextInput(session, inputId = "ID", value = paste(ID))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Exposure Duration (minutes): t_e
          t_e = as.numeric(temp[1])*minute
          t_e.rsd = as.numeric(temp[2])
          t_e.sd = t_e*t_e.rsd*sqrt(3)
          updateTextInput(session, inputId = "t_e", value = paste(t_e/minute))
          updateTextInput(session, inputId = "t_e.sd", value = paste(t_e.sd/minute))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Total Time for Clearance and Oxygen Therapy (minutes): t_c + t_t
          t_ct = as.numeric(temp[1])*minute
          t_ct.rsd = as.numeric(temp[2])
          t_ct.sd = t_ct*t_ct.rsd*sqrt(3)
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Exposure Duration (minutes): t_e
          XCOHb = as.numeric(temp[1])*percent
          XCOHb.rsd = as.numeric(temp[2])
          XCOHb.sd = XCOHb*XCOHb.rsd*sqrt(3)
          updateTextInput(session, inputId = "XCOHb", value = paste(XCOHb/percent))
          updateTextInput(session, inputId = "XCOHb.sd", value = paste(XCOHb.sd/percent))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Weight (pounds): w
          w = as.numeric(temp[1])*pound
          w.rsd = as.numeric(temp[2])
          w.sd = w*w.rsd*sqrt(3)
          updateTextInput(session, inputId = "w", value = paste(w/pound))
          updateTextInput(session, inputId = "w.sd", value = paste(w.sd/pound))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Height (inches): h
          h = as.numeric(temp[1])*inch
          h.rsd = as.numeric(temp[2])
          h.sd = h*h.rsd*sqrt(3)
          updateTextInput(session, inputId = "h", value = paste(h/inch))
          updateTextInput(session, inputId = "h.sd", value = paste(h.sd/inch))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Smoker Status: SS
          SS = as.numeric(temp[1])
          SS.rsd = as.numeric(temp[2])
          SS.sd = SS*SS.rsd*sqrt(3)
          updateTextInput(session, inputId = "SS", value = paste(SS))
          updateTextInput(session, inputId = "SS.sd", value = paste(SS.sd))
          updateCheckboxInput(session, inputId = "smoker", value = 1)
          updateSelectInput(session, inputId = "SS_method", selected = "status")
          updateTextInput(session, inputId = "cigarettes", value = 0)
          updateTextInput(session, inputId = "cigarettes.sd", value = 0)
          updateTextInput(session, inputId = "XCOHb.0", value = XCOHb.dat[1]/percent)
          updateTextInput(session, inputId = "XCOHb.0.sd", value = 0)
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Activity Level during exposure: AL_e
          AL_e = as.numeric(temp[1])
          AL_e.rsd = as.numeric(temp[2])
          AL_e.sd = AL_e*AL_e.rsd*sqrt(3)
          updateTextInput(session, inputId = "AL_e", value = paste(AL_e))
          updateTextInput(session, inputId = "AL_e.sd", value = paste(AL_e.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Activity Level during clearance: AL_c
          AL_c = as.numeric(temp[1])
          AL_c.rsd = as.numeric(temp[2])
          AL_c.sd = AL_c*AL_c.rsd*sqrt(3)
          updateTextInput(session, inputId = "AL_c", value = paste(AL_c))
          updateTextInput(session, inputId = "AL_c.sd", value = paste(AL_c.sd))
        AL_t = 0
        AL_t.sd = 0
          updateTextInput(session, inputId = "AL_t", value = paste(AL_t))
          updateTextInput(session, inputId = "AL_t.sd", value = paste(AL_t.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Hemoglobin in blood (grams/100mL): Hb
          Hb = as.numeric(temp[1])
          Hb.rsd = as.numeric(temp[2])
          Hb.sd = Hb*Hb.rsd*sqrt(3)
          updateTextInput(session, inputId = "Hb", value = paste(Hb))
          updateTextInput(session, inputId = "Hb.sd", value = paste(Hb.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Atmospheric Pressure (mmHg): PB
          PB = as.numeric(temp[1])
          PB.rsd = as.numeric(temp[2])
          PB.sd = PB*PB.rsd*sqrt(3)
          updateTextInput(session, inputId = "PB", value = paste(PB))
          updateTextInput(session, inputId = "PB.sd", value = paste(PB.sd))
        temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Carbon Monoxide Level during Exposure (ppm): x.CO_e
          x.CO_e = as.numeric(temp[1])
          x.CO_e.rsd = as.numeric(temp[2])
          x.CO_e.sd = x.CO_e*x.CO_e.rsd*sqrt(3)
          updateSelectInput(session, inputId = "fhs_e_method", selected = "ppm")
          updateTextInput(session, inputId = "fhs_e.ppm", value = paste(x.CO_e))
          updateTextInput(session, inputId = "fhs_e.ppm.sd", value = paste(x.CO_e.sd))
          updateTextInput(session, inputId = "fhs_e.cigarettes", value = 0)
          updateTextInput(session, inputId = "fhs_e.cigarettes.sd", value = 0)
          updateTextInput(session, inputId = "fhs_e.percent", value = 0)
          updateTextInput(session, inputId = "fhs_e.percent.sd", value = 0)
          updateCheckboxInput(session, inputId = "shs_e", value = 0)
          updateSelectInput(session, inputId = "shs_e_method", selected = "ppm")
          updateTextInput(session, inputId = "shs_e.ppm", value = 0)
          updateTextInput(session, inputId = "shs_e.ppm.sd", value = 0)
          updateTextInput(session, inputId = "shs_e.time", value = 0)
          updateTextInput(session, inputId = "shs_e.time.sd", value = 0)
          updateTextInput(session, inputId = "shs_e.percent", value = 0)
          updateTextInput(session, inputId = "shs_e.percent.sd", value = 0)
          temp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)                                 #Carbon Monoxide Level during Clearance (ppm): x.CO_c
          x.CO_c = as.numeric(temp[1])
          x.CO_c.rsd = as.numeric(temp[2])
          x.CO_c.sd = x.CO_c*x.CO_c.rsd*sqrt(3)
          updateSelectInput(session, inputId = "fhs_c_method", selected = "ppm")
          updateTextInput(session, inputId = "fhs_c.ppm", value = paste(x.CO_c))
          updateTextInput(session, inputId = "fhs_c.ppm.sd", value = paste(x.CO_c.sd))
          updateTextInput(session, inputId = "fhs_c.cigarettes", value = 0)
          updateTextInput(session, inputId = "fhs_c.cigarettes.sd", value = 0)
          updateTextInput(session, inputId = "fhs_c.percent", value = 0)
          updateTextInput(session, inputId = "fhs_c.percent.sd", value = 0)
          updateCheckboxInput(session, inputId = "shs_c", value = 0)
          updateSelectInput(session, inputId = "shs_c_method", selected = "ppm")
          updateTextInput(session, inputId = "shs_c.ppm", value = 0)
          updateTextInput(session, inputId = "shs_c.ppm.sd", value = 0)
          updateTextInput(session, inputId = "shs_c.time", value = 0)
          updateTextInput(session, inputId = "shs_c.time.sd", value = 0)
          updateTextInput(session, inputId = "shs_c.percent", value = 0)
          updateTextInput(session, inputId = "shs_c.percent.sd", value = 0)
          tmp = scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)
        t_t = as.numeric(tmp[1])*minute                                                                      #Oxygen therapy duration (minutes): t_t
          updateTextInput(session, inputId = "t_t", value = paste(t_t/minute))
          updateTextInput(session, inputId = "t_t.sd", value = 0)                                            #This is assumed to be zero and all uncertainty goes to clearance
        x.O2_e = as.numeric(tmp[2])                                                                          #Oxygen level (% oxygen) during exposure: x.O2_e
          updateTextInput(session, inputId = "x.O2_e", value = paste(x.O2_e/percent))
          updateTextInput(session, inputId = "x.O2_e.sd", value = 0)
        x.O2_c = x.O2_e                                                                                      #Oxygen level (% oxygen) during clearance: x.O2_c (assumed equal to exposure)
          updateTextInput(session, inputId = "x.O2_c", value = paste(x.O2_c/percent))
          updateTextInput(session, inputId = "x.O2_c.sd", value = 0)
          updateTextInput(session, inputId = "x.CO_t", value = 0)
          updateTextInput(session, inputId = "x.CO_t.sd", value = 0)
        t_c = t_ct - t_t                                                                                     #Clearance time (minutes): t_c (calculated)
          t_c.sd = t_ct.sd                                                                                   # We will assume that all of the uncertainty in t_ct can be attributed to t_c
          updateTextInput(session, inputId = "t_c", value = paste(t_c/minute))
          updateTextInput(session, inputId = "t_c.sd", value = paste(t_c.sd/minute))
        x.O2_t = as.numeric(scan(file=con, nlines=1, what="numeric", sep=",", quiet=TRUE)[1])                #Oxygen level (% oxygen) during therapy: x.O2_t
          updateTextInput(session, inputId = "x.O2_t", value = paste(x.O2_t/percent))
          updateTextInput(session, inputId = "x.O2_t.sd", value = 0)
          updateTextInput(session, inputId = "name", value = paste(""))
          updateTextInput(session, inputId = "gender", value = paste("male"))
          updateSelectInput(session, inputId = "PB_method", selected = "pressure")
          updateSelectInput(session, inputId = "COHb_method", selected = "breath")
          updateSelectInput(session, inputId = "Hb_method", selected = "blood")
          updateSelectInput(session, inputId = "OT_method", selected = "Oxygen level")
          close(con)
      },
      error = function(e) {stop(safeError(e))}
    )
    return()
  })
}
