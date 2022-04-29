library(nleqslv)
library(deSolve)

# units
inch = 0.0254#meter
pound  = 0.45359237#kg
mmHg = 133.322#Pa
mL  = 1e-6#m^3
minute = 60#second
ppm = 1e-6
percent = 1/100
gram = 1e-3#kg
liter = 1e-3#m^3
hour = 60*minute#second
ft = 0.3048#m

# constants
M = 218
Hf = 1.38*mL/gram
PH2O = 47*mmHg
PA = 49*mmHg
VCO = 0.007*mL/minute
g = 9.80665#m/s^2
Rg = 8.3144598#J/mol/K
Mb = 28.9644*gram#/mole
Pb = 760*mmHg
Tb = 288.15#K
Lb = -0.0065#K/m
hb = 0#m

# lookup tables
AL.des <- c("resting","light work","moderate work","heavy work")
AL <- function(x) AL.des[round(x+1)]
AL.dat = c(0, 1, 2, 3, 4)
DL.dat = c(20, 25, 30, 40, 50)*mL/minute/mmHg
DL.tab <- function(AL) approx(AL.dat,DL.dat,AL)$y
DL.tab <- function(AL) {
  AL = ifelse(AL<0, 0, AL)
  AL = ifelse(AL>4, 4, AL)
  approx(AL.dat,DL.dat,AL)$y
}

DL.1 <- function(AL,PB,x.O2) DL.tab(AL) / (1 + 0.0031/mmHg * (x.O2 * (PB - PH2O) - 150*mmHg))
DL.2 <- function(AL,PB,x.O2) DL.tab(AL) / (1 + (0.0031/mmHg + (1.27624E-05/mmHg^2) * (x.O2 * (PB - PH2O) - 150*mmHg)) * (x.O2 * (PB - PH2O) - 150*mmHg))
DL <- function(AL,PB,x.O2) {
  if (x.O2 > 0.21) DL.2(AL,PB,x.O2)
  else DL.1(AL,PB,x.O2)}
DL <- function(AL,PB,x.O2) ifelse(x.O2 > 0.21, DL.2(AL,PB,x.O2), DL.1(AL,PB,x.O2))
VA.dat = c(6, 12, 18, 24, 30)*liter/minute
VA <- function(AL,PB) approx(AL.dat,VA.dat,AL)$y*(PB-PH2O)/(760*mmHg)*(273.15/310.15)
VA <- function(AL,PB) {
  AL = ifelse(AL<0, 0, AL)
  AL = ifelse(AL>4, 4, AL)
  approx(AL.dat,VA.dat,AL)$y*(PB-PH2O)/(760*mmHg)*(273.15/310.15)
}
SS.dat = c(0, 1, 2, 3, 4)
XCOHb.dat = c(0.0075, 0.02, 0.05, 0.1, 0.2)
cigs.dat = c(0, 6, 20, 40, 67)
steadyState.dat = c(3.9, 12.0, 32.5, 69.8, 158.1)*ppm
XCOHb.0_s <- function(SS) {
  SS = ifelse(SS<0, 0, SS)
  SS = ifelse(SS>4, 4, SS)
  approx(SS.dat,XCOHb.dat,SS)$y
}
steadyState_s <- function(SS) {
  SS = ifelse(SS<0, 0, SS)
  SS = ifelse(SS>4, 4, SS)
  approx(SS.dat,steadyState.dat,SS)$y
}
XCOHb.0_c <- function(cigs) {
  cigs = ifelse(cigs<0, 0, cigs)
  cigs = ifelse(cigs>67, 67, cigs)
  approx(cigs.dat,XCOHb.dat,cigs)$y
}
steadyState_c <- function(cigs) {
  cigs = ifelse(cigs<0, 0, cigs)
  cigs = ifelse(cigs>67, 67, cigs)
  approx(cigs.dat,steadyState.dat,cigs)$y
}

# constitutive relations
Vb.m <- function(H,W) (366.9*H^3 + 32.19*W + 604.1)*mL
Vb.f <- function(H,W) (356.1*H^3 + 33.08*W + 183.3)*mL
PICO <- function(PB,x.CO) x.CO*(PB-PH2O)
PCO2 <- function(PB,x.CO,x.O2) x.O2*(PB-PH2O-PICO(PB=PB,x.CO=x.CO))-PA
beta <- function(PB,DL,VA) 1/DL + (PB-PH2O)/VA
COHb <- function(XCOHb,Hb) XCOHb*Hf*Hb
x.CO.g <- function(beta,Hb,PB,x.O2,COHb.f) (VCO*beta*Hf*Hb*M+(PA+(PH2O-PB)*x.O2-beta*M*VCO)*COHb.f)/((PH2O-PB)*(Hf*Hb*M-(M-x.O2)*COHb.f))
P <- function(z) Pb*(Tb/(Tb+Lb*(z-hb)))^(g*Mb/(Rg*Lb))

#functions
CFK <- function (t,COHb,parms,Vb,beta,PICO,PCO2,Hb) {list(VCO/Vb + PICO/(Vb*beta) - COHb*PCO2/(M*Vb*beta)/(Hf*Hb-COHb))}

solnCFK <- function(t,COHb.i,COHb.f,Vb,beta,PICO,PCO2,Hb) {
  Q = beta*M*VCO + M*PICO + PCO2
  R = Hf*Hb*PCO2
  S = Hf*Hb*M*(VCO*beta + PICO)
  Q/R*(t*Q/Vb/beta/M+COHb.i-COHb.f) - log((COHb.i*Q-S)/(COHb.f*Q-S))
}

findInitCOHb <- function(t,COHb.f,Vb,beta,PICO,PCO2,Hb) {
  eqn1 <- function(COHb.i) solnCFK(t=t,COHb.i=COHb.i,COHb.f=COHb.f,Vb=Vb,beta=beta,PICO=PICO,PCO2=PCO2,Hb=Hb)
  nleqslv(f = eqn1, x = COHb.f)$x
}

getInitCOHb <- function(t,COHb.f,Vb,PB,AL,x.O2,x.CO,Hb) {
  DL = DL(AL=AL,PB=PB,x.O2=x.O2)
  VA = VA(AL=AL)
  beta = beta(PB=PB,DL=DL,VA=VA)
  PICO = PICO(PB=PB,x.CO=x.CO)
  PCO2 = PCO2(PB=PB,x.CO=x.CO,x.O2=x.O2)
  findInitCOHb(t=t,COHb.f=COHb.f,Vb=Vb,beta=beta,PICO=PICO,PCO2=PCO2,Hb=Hb)
}

findMeanCO <- function(t,COHb.i,COHb.f,Vb,beta,Hb,PB,x.O2) {
  eqn2 <- function(x.CO) solnCFK(t=t,COHb.i=COHb.i,COHb.f=COHb.f,Vb=Vb,beta=beta,PICO=PICO(x.CO=x.CO,PB=PB),PCO2=PCO2(x.CO=x.CO,PB=PB,x.O2=x.O2),Hb=Hb)
  nleqslv(f = eqn2, x = 1.001*x.CO.g(beta,Hb,PB,x.O2,COHb.f))$x
}

getMeanCO <- function(t,COHb.f,Vb,AL,Hb,PB,x.O2,SS) {
  COHb.i = f.COHb.0(SS=SS,Hb=Hb)
  DL = DL(AL=AL,PB=PB,x.O2=x.O2)
  VA = VA(AL=AL)
  beta = beta(PB=PB,DL=DL,VA=VA)
  findMeanCO(t,COHb.i,COHb.f,Vb,beta,Hb,PB,x.O2)
}

SAE <- function(x) 1.64485*sd(x)/mean(x)