source("COHb.R")
#========================================== Sample ==========================================#
XCOHb = 12*percent # COHb in blood sample (%)
Hb = 16*gram/(100*mL) # Hemoglobin in blood sample (grams/100mL)
COHb.D = COHb(XCOHb=XCOHb,Hb=Hb) # Fraction of COHb in blood sample (%)
#========================================= Employee =========================================#
w = 150*pound # Weight (pounds)
h = 72*inch # Height (inches)
Vb = Vb.m(W=w,H=h) # Estimated blood volume of employee (liters)
SS = 2 # Smoker Status
XCOHb.A = XCOHb.0_s(SS=SS) # Fraction of COHb in blood prior to exposure (%)
COHb.A = Hf*Hb*XCOHb.A # COHb in blood prior to exposure
#======================================== Enviroment ========================================#
z = 4000*ft # Elevation
PB = P(z=z) # Atmospheric Pressure (mmHg)
#====================================== Oxygen Therapy ======================================#
t_t = 60*minute # Oxygen Therapy duration (minutes)
AL_t = 0 # Oxygen Therapy activity Level
x.O2_t = 80*percent # Oxygen Therapy oxygen level (% oxygen)
x.CO_t = 0*ppm # Oxygen Therapy carbon monoxide level (ppm)
VA_t = VA(AL=AL_t,PB=PB)
DL_t = DL(AL=AL_t,PB=PB,x.O2=x.O2_t)
beta_t = beta(PB=PB,DL=DL_t,VA=VA_t)
PICO_t = PICO(PB=PB,x.CO=x.CO_t)
PCO2_t = PCO2(PB=PB,x.CO=x.CO_t,x.O2=x.O2_t)
COHb.C = findInitCOHb(t=t_t,COHb.f=COHb.D,Vb=Vb,beta=beta_t,PICO=PICO_t,PCO2=PCO2_t,Hb=Hb)
XCOHb.C = COHb.C/Hf/Hb
#========================================= Clearance =========================================#
t_c = 120*minute # Clearance duration (minutes)
AL_c = 1 # Clearance activity Level:
x.O2_c = 21*percent # Clearance oxygen level (% oxygen)
fhs_c.cigarettes = 1 # Cigarettes smoked during clearance
x.CO_c.fhs = steadyState_c(cigs=fhs_c.cigarettes*960*minute/t_c) # CO clearance from first hand smoke (ppm)
x.CO_c.shs = 0*ppm # CO clearance from second hand smoke (ppm)
x.CO_c = x.CO_c.fhs+x.CO_c.shs # Carbon Monoxide Level (ppm)
VA_c = VA(AL=AL_c,PB=PB)
DL_c = DL(AL=AL_c,PB=PB,x.O2=x.O2_c)
beta_c = beta(PB=PB,DL=DL_c,VA=VA_c)
PICO_c = PICO(PB=PB,x.CO=x.CO_c)
PCO2_c = PCO2(PB=PB,x.CO=x.CO_c,x.O2=x.O2_c)
COHb.B = findInitCOHb(t=t_c,COHb.f=COHb.C,Vb=Vb,beta=beta_c,PICO=PICO_c,PCO2=PCO2_c,Hb=Hb)
XCOHb.B = COHb.B/Hf/Hb
#========================================= Exposure =========================================#
t_e = 240*minute # Exposure duration (minutes)
AL_e = 3 # Exposure activity Level
x.O2_e = 21*percent # Oxygen level (% oxygen)
VA_e = VA(AL=AL_e,PB=PB)
DL_e = DL(AL=AL_e,PB=PB,x.O2=x.O2_e)
beta_e = beta(PB=PB,DL=DL_e,VA=VA_e)
x.CO_e = findMeanCO(t=t_e,COHb.i=COHb.A,COHb.f=COHb.B,Vb=Vb,beta=beta_e,Hb=Hb,PB=PB,x.O2=x.O2_e)
fhs_e.cigarettes = 2 # Cigarettes smoked during exposure
x.CO_e.fhs = steadyState_c(cigs=fhs_e.cigarettes*960*minute/t_e) # CO exposure from first hand smoke (ppm)
x.CO_e.shs = 0*ppm  # CO exposure from second hand smoke (ppm)
x.CO_e.o = x.CO_e-x.CO_e.fhs-x.CO_e.shs
PICO_e = PICO(PB=PB,x.CO=x.CO_e)
PCO2_e = PCO2(PB=PB,x.CO=x.CO_e,x.O2=x.O2_e)
#=========================================== Plot ===========================================#
t.e = 0:t_e
t.e = seq(0,t_e,1)
e = rk4(y=COHb.A,t=t.e,parms=NULL,func=CFK,Vb=Vb,beta=beta_e,PICO=PICO_e,PCO2=PCO2_e,Hb=Hb)
t.c = seq(t_e,(t_c+t_e),1)
c = rk4(y=COHb.B,times=t.c,parms=NULL,func=CFK,Vb=Vb,beta=beta_c,PICO=PICO_c,PCO2=PCO2_c,Hb=Hb)
t.o = seq((t_c+t_e),(t_c+t_e+t_t),1)
o = rk4(y=COHb.C,times=t.o,parms=NULL,func=CFK,Vb=Vb,beta=beta_t,PICO=PICO_t,PCO2=PCO2_t,Hb=Hb)
par(mar=c(4,4,1.5,1.5),mex=0.8,cex=0.8,mgp=c(2,0.5,0),tcl=0.3)
pdf("R_ExamplePlot.pdf", width=10, height=5)
plot(c(0,(t_e+t_c+t_t)/60),c(0,XCOHb.B/percent)*1.05,type='n',xlab="Time (minutes)",ylab="COHb (%)")
polygon(c(0,e[,1]/60,t_e/60), c(0,e[,2],0)/Hf/Hb/percent, col='red', density=10, angle=60) 
polygon(c(t_e/60,c[,1]/60,(t_e+t_c)/60), c(0,c[,2],0)/Hf/Hb/percent, col='blue', density=15, angle=-30)
polygon(c((t_e+t_c)/60,o[,1]/60,(t_e+t_c+t_t)/60), c(0,o[,2],0)/Hf/Hb/percent, col='green', density=10, angle=45)
abline(h=XCOHb.B/percent,lty=3)
text(0,1.03*XCOHb.B/percent,sprintf("Maximum COHb = %.1f%%", XCOHb.B/percent),adj = c(0,0))
text(0,0.93*XCOHb.B/percent,sprintf("Occupational Exposure = %.1f ppm", x.CO_e.o/ppm),adj = c(0,0))
text(-3,1.2*XCOHb.A/percent,sprintf("A", x.CO_e.o/ppm),adj = c(0,0))
text(t_e/60,1.05*XCOHb.B/percent,sprintf("B", XCOHb.B/percent))
text((t_e+t_c)/60,1.1*XCOHb.C/percent,sprintf("C", XCOHb.B/percent))
text((t_e+t_c+t_t)/60,1.15*XCOHb/percent,sprintf("D", XCOHb.B/percent))
dev.off()