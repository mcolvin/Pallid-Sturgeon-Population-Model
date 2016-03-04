library(triangle)
library(reshape2)
library(plyr)
library(lattice)


# POPULATION CHARACTERISTICS
maxAge  			<- 41
fecundity			<- 4000
ageAtMaturity		<- 8 # split difference for males and females (S1002)
fecundity			<- 19064 # S1001
spawningInterval	<- 2.5 # split difference for males and females (S1002)
sex_ratio			<- 0.33


# POPULATION DEMOGRAPHIC RATES
baseline<- 0.051 # age 1 from Stephenson

# SET UP STAGES
# Gametes, Gametes and Developing Embryo, Free Embryo, Exogenously feeding larvae & age-0
S<- rep(0, 9)
S[5]<- 0.686 # Juvenile (age 1-age -2)  
S[6]<- 0.992 # Juvenile (age 2 to age at maturity)
S[7]<- 0.992 # Adult > age at maturity (8 to age 41)
S[8]<- 1 # Spawning adult
S[9]<- 0.992 # Recrudescent Adult
# END SURVIVALS

nyears=50


# DPH 0-365
## GAMETES AND DEVELOPING EMBRYOS
gade<- matrix(0,nrow=nyears,ncol=2)
## FREE EMBRYOS
fe<- matrix(0,nrow=nyears,ncol=2)
## EXOGENOUSLY FEED LARVAE AND AGE-0
efl<- matrix(0,nrow=nyears,ncol=2)


# AGE 1-SEXUAL MATURITY
## JUVENILES (1-9 years old)
juv<-array(0,dim=c(nyears,maxAge,2))#[year,age,1=natural & 2=hatchery]

# SEXUAL MATURITY TO MAX AGE
## ADULT
recadult1<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
recadult2<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
recadult3<-array(0,dim=c(nyears,maxAge,2))# [year,age,1=natural & 2=hatchery]
spawners<- matrix(0, nrow=maxAge, ncol=1)
# END SETUP

# INITIALIZE POPULATION

## INITIALIZE JUVENILES
juv_ini_n<- 5000
juv_ini_h<- 32000
p<-c(rep(1/length(1:8),length(1:8)),rep(0,33))
juv[1,,1]<- rmultinom(1,juv_ini_n,p)
juv[1,,2]<- rmultinom(1,juv_ini_h,p)

## INITIALIZE ADULTS
adults_ini_n<- 5000
adults_ini_h<- 32000

adults_ini_n<- rmultinom(1,adults_ini_n,prob=c(rep(1/3,3)))
recadult1[1,,1]<- rmultinom(1,adults_ini_n[1],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult2[1,,1]<- rmultinom(1,adults_ini_n[2],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult3[1,,1]<-rmultinom(1,adults_ini_n[3],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))

adults_ini_h<- rmultinom(1,adults_ini_h,prob=c(rep(1/3,3)))
recadult1[1,,2]<- rmultinom(1,adults_ini_h[1],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult2[1,,2]<- rmultinom(1,adults_ini_h[2],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))
recadult3[1,,2]<-rmultinom(1,adults_ini_h[3],prob=c(rep(0,8),rep(1/length(9:maxAge),length(9:maxAge))))

year0<- function(S0=0.051,stoch=FALSE)
{# THIS FUNCTION RETURNS A VECTOR SURVIVALS FOR THE 3 STAGES THAT
  # THAT OCCUR IN YEAR 0 
  # DEFAULT 0.051 IS FROM STEPHENSON ET AL...
  if(stoch==TRUE)
  {
    repeat{
      x<- runif(2, S0,0.999)
      if (prod(x)>S0){break}
      x<- c(x,exp(log(S0)-(log(x[1])+log(x[2]))))
    }
  }
  if(stoch==FALSE){x<- rep(S0^(1/3),3)}
  return(x)
}


initialize<- function(S,
                      Jmat, 
                      maxAge=41,
                      Njuv_Natural=40000,
                      Nadult_Natural=10000, 
                      Njuv_Hatchery=40000,
                      Nadult_Hatchery=1, 
                      EGGS=40000,
                      nyears=100)
{
  # [1] SET UP ARRAYS TO HOLD STATE VALUES
  J<-SP<- R1<- R2<- R3<- R4<- array(0,dim=c(maxAge,nyears,2))
  
  
  # [2] INITIALIZE MATRICES AT T=1
  ## [2.1] NATURAL ORIGIN JUVENILE FISH
  x<-S # SURVIVAL VECTOR
  x[-c(1:9)]<- 0	
  x[c(1:9)]<-cumprod(x[c(1:9)])
  ini<- rmultinom(1,Njuv_Natural,x)	
  sp<- rbinom(maxAge,ini,Jmat)	
  J[,1,1]<- ini-sp
  
  ## [2.2] HATCHERY ORIGIN JUVENILE FISH
  x<-S # SURVIVAL VECTOR
  x[-c(1:9)]<- 0	
  x[c(1:9)]<-cumprod(x[c(1:9)])
  x<- x/sum(x)
  ini<- rmultinom(1,Njuv_Hatchery,x)	
  sp<- rbinom(maxAge,ini,Jmat)	
  J[,1,2]<- ini-sp	
  
  
  # [3] INITIAL VALUES FOR ADULT STAGES: SP, R1, R2, R3 R4
  
  ## [3.1] NATURALLY PRODUCED FISH    
  x<-S # SURVIVAL VECTOR	
  x[c(1:9)]<- 0
  x[-c(1:9)]<-cumprod(x[-c(1:9)])
  ini<- rmultinom(1,Nadult_Natural,x)	
  # ALLOCATE ADULTS AMONG SPAWNING AND RECRDUCESCENT STAGES
  states<- t(sapply(1:length(ini), function(x) rmultinom(1,ini[x],c(1,1,1,1,1))))
  
  SP[,1,1]<- states[,1]
  R1[,1,1]<- states[,2]
  R2[,1,1]<- states[,3]
  R3[,1,1]<- states[,4]
  R4[,1,1]<- states[,5]
  
  ## [3.2] HATCHERY PRODUCED FISH    
  x<-S # SURVIVAL VECTOR	
  x[c(1:9)]<- 0
  x[-c(1:9)]<-cumprod(x[-c(1:9)])
  ini<- rmultinom(1,Nadult_Hatchery,x)	
  states<- t(sapply(1:length(ini), function(x) rmultinom(1,ini[x],c(1,1,1,1,1))))
  
  SP[,1,2]<- states[,1]
  R1[,1,2]<- states[,2]
  R2[,1,2]<- states[,3]
  R3[,1,2]<- states[,4]
  R4[,1,2]<- states[,5]		
  
  # INTIALIZE MATRICES FOR MATRIX OPERATIONS
  A<- matrix(0,nrow=maxAge,ncol=maxAge)
  A[cbind(c(2:maxAge),c(1:(maxAge-1)))]<-1
  fec<- matrix(0,nrow=maxAge,ncol=maxAge)
  fec[1,]<- EGGS	
  
  return(list(J=J, A=A, fec=fec, SP=SP, R1=R1, R2=R2, R3=R3,R4=R4))
}




simulate<- function(nyears=100,nreps=50,
                    stocked = c(0,0),
                    # ABUDNANCE
                    N_juv_Natural=c(0,500,1000),
                    N_adult_Natural=c(3750,4000,4250),
                    N_juv_Hatchery=c(18000,21500,25000),
                    N_adult_Hatchery=c(12500,14500,16750),
                    # RATES
                    sex_ratio=c(0.33*0.8,0.33,0.33*1.2),
                    S_egg_embryo=c(0.0000019*0.8,0.0000019,0.0000019*1.2),
                    S0=c(0.02,0.051,0.1),
                    S1=c(0.06,0.686,0.75),
                    S2=c(0.9,0.922,0.95),
                    stoch_in=FALSE)
{## A FUNCTION TO SIMULATE POPULATION DYNAMICS OVER A PERIOD OF TIME	
  outp<-data.frame()
  parms<- data.frame()	
  
  #von Bertalanffy Curve
  maxAge=41		
  Linf=1683
  K=0.036
  t0=-5.9
  sigma=93.77
  FL<-Linf*(1-exp(-K*(c(1:maxAge)-t0)))
  
  # ANNUAL STOCKING VALUES	
  ## [3.4] NUMBER OF AGE-0 FISH STOCKED
  AGE_0_STOCKED<- stocked[1] 
  ## [3.5] NUMBER OF AGE-1 FISH PRODUCED
  JUV_STOCKED<- stocked[2] 	
  
  for(i in 1:nreps)
  {
    ## [1.2] DEMOGRAPHIC RATES AND VALUES
    S0<- ifelse(unique(S0)>1, 
                rtriangle(1,a=S0[1] , b=S0[3], c=S0[2]),
                S0[1])
    S0<- year0(S0=S0,stoch=stoch_in) 
    S_embryo_free_embryo		<- S0[1]
    S_free_embryo_exo_larvae	<- S0[2]
    S_exo_larvae_age1			<- S0[3] 
    S1_ini<- rtriangle(1,a=S1[1], b=S1[3], c=S1[2])
    S2_ini<- rtriangle(1,a=S2[1], b=S2[3], c=S2[2])
    
    Jmat<- c(rep(0,6),0.25,0.5,rep(1,33))# PROBABILITY OF BECOMING SEXUALLY MATURE 
    
    # PROBABILITY EGG BECOMES EMBRYO [CALIBTRATED TO KEEP ~STABLE AGE STRUC]0.000019 
    S_egg_embryo_ini<- ifelse(length(unique(S_egg_embryo))>1, 
                              rtriangle(1,a=S_egg_embryo[1],b=S_egg_embryo[3],c=S_egg_embryo[2]),
                              S_egg_embryo[1])
    
    
    #pop_influx<- 12000
    #allee_rate<- 0.0004
    #S_egg_embryo<-  max_s_egg_embryo/(1+exp(-allee_rate*(pop - pop_influx)))
    EGGS_age<- 3.48*10^-8 *FL^4.05 #  + 110056*log(FL)# EGGS AT LENGTH 
    
    S<- c(S1_ini,rep(S2_ini,maxAge-1))#	
    
    ## [1.3] STATE INITIAL VALUES
    ### [1.3.1] NATURAL ORIGIN FISH
    N_juv_Natural_ini<- round(rtriangle(1,a=N_juv_Natural[1],b=N_juv_Natural[3],c=N_juv_Natural[2]),0) 
    N_adult_Natural_ini<- round(rtriangle(1,a=N_adult_Natural[1],b=N_adult_Natural[3],c=N_adult_Natural[2]) ,0)
    
    
    
    ### [1.3.2] HATCHERY ORIGIN FISH
    N_juv_Hatchery_ini<- round(rtriangle(1,a=N_juv_Hatchery[1],b=N_juv_Hatchery[3],c=N_juv_Hatchery[2]),0)
    N_adult_Hatchery_ini<- round(rtriangle(1,a=N_adult_Hatchery[1],b=N_adult_Hatchery[3],c=N_adult_Hatchery[2]),0)
    sex_ratio_ini<-  rtriangle(1,a=sex_ratio[1],b=sex_ratio[3],c=sex_ratio[2])
    
    
    
    # [3] SIMULATE POPULATION DYNAMICS
    ## [3.1] INITIALIZE MODEL at t=1 GIVEN INPUTS AT STABLE AGE DISTRIBUTION	
    inits<- initialize(S=S, 
                       Jmat=Jmat,
                       maxAge=maxAge,
                       Njuv_Natural=N_juv_Natural_ini,
                       Nadult_Natural=N_adult_Natural_ini,
                       Njuv_Hatchery=N_juv_Hatchery_ini,
                       Nadult_Hatchery=N_adult_Hatchery_ini,
                       EGGS=EGGS_age) 
    J<- as.array(inits$J)
    A<- as.matrix(inits$A)
    SP<- as.array(inits$SP)
    fec<- as.matrix(inits$fec)
    R1<- as.array(inits$R1)
    R2<- as.array(inits$R2)
    R3<- as.array(inits$R3)
    R4<- as.array(inits$R4)		
    
    # SIMULATE DYNAMICS		
    for(type in 1:2)
    {
      for(yr in 2:nyears)
      {  
        ## [1.1] NUMBER OF PST RECRUITED TO AGE 1
        EGGS_NAT<- round(sum(fec %*% (SP[,yr-1,1]+SP[,yr-1,2])*sex_ratio_ini),0) # egg produced
        
        ## [1.2] NUMBER OF EGGS THAT WERE SUCCESSFULLY FERTILIZED
        EMBRYOS_NAT<- rbinom(1,EGGS_NAT,S_egg_embryo_ini)
        
        ## [1.3] FREE EMBRYO
        FREE_EMBRYOS_NAT<- rbinom(1,EMBRYOS_NAT,S_embryo_free_embryo)
        
        ## [1.4] EXOGENOUSLY FEEDING LARVAE
        EXO_LARVAE_NAT<- rbinom(1,FREE_EMBRYOS_NAT,S_free_embryo_exo_larvae)
        AGE1_NAT<- rbinom(1,EXO_LARVAE_NAT,S_exo_larvae_age1)
        
        ## [1.2] JUVENILE STAGE
        tmp<- A %*% rbinom(maxAge,J[,yr-1,type],S) # fish surviving
        tmp[1]<- ifelse(type==1,AGE1_NAT,JUV_STOCKED)
        
        mat<- rbinom(maxAge,tmp,Jmat)
        
        ## [1.3] JUVENILES REMAINING SEXUALLY IMMATURE
        J[,yr,type]<- tmp-mat
        
        ## [1.4] SPAWNERS AND POST-SPAWNING
        srv_spn<- A %*% rbinom(maxAge,SP[,yr-1,type],S)
        R1[,yr,type]<- srv_spn
        
        ## [1.5]  R1
        srv_R1<- A %*% rbinom(maxAge,R1[,yr-1,type],S)
        spn_R1<- rbinom(maxAge,srv_R1,0.1)
        R2[,yr,type]<- srv_R1-spn_R1
        
        ## [1.6]  R2
        srv_R2 <-A %*% rbinom(maxAge,R2[,yr-1,type],S) 
        spn_R2<- rbinom(maxAge,srv_R2,0.5)
        R3[,yr,type]<- srv_R2-spn_R2  
        
        ## [1.7] R3
        srv_R3 <-A %*% rbinom(maxAge,R3[,yr-1,type],S)
        spn_R3<- rbinom(maxAge,srv_R3,0.75)
        R4[,yr,type]<- srv_R3-spn_R3
        
        ## [1.6]  R4 
        srv_R4 <- A %*% rbinom(maxAge,R4[,yr-1,type],S) 
        spn_R4<- rbinom(maxAge,srv_R4,1)
        
        SP[,yr,type]<- (mat+spn_R1+spn_R2+spn_R3+spn_R4)	
      }  # END i
    } # END type
    
    juveniles<- apply(J,c(2:3),sum)
    adults<- SP+R1+R2+R3+R1
    adults<- apply(adults,c(2:3),sum)
    out<- as.data.frame(cbind(juveniles, adults))
    names(out)<-c("jn","jh","an","ah")
    out$rep<- i
    out$year<- c(1:nyears)
    outp<- rbind(outp,out)
    vals<- c(prod(S0), S_egg_embryo_ini, S_embryo_free_embryo, S_free_embryo_exo_larvae,
             S_exo_larvae_age1, S1_ini, S2_ini, AGE_0_STOCKED,sum(JUV_STOCKED),
             N_juv_Natural_ini, N_adult_Natural_ini, N_juv_Hatchery_ini, N_adult_Hatchery_ini,sex_ratio_ini)	
    nams<- c("S: age-0 to age-1", 'S: egg to embryo', 'S: embryo to free embryo', 'S: free: embryo to exo larvae',
             'S: exo. larvae to age-1', 'S: age-1 to age-2', 'S: greater than age-1', 'Stocked: age-0','Stocked: age-1',
             "Juvenile abundance (natural)", "Adult abundance (natural)"," Juvenile abundance (hatchery)", "Adult abundance (hatchery)",
             "Sex ratio")	
    parms_app<- data.frame(rep=rep(i,length(vals)),parm=nams,vals=vals)
    parms<- rbind(parms, parms_app)
    print(i)
  }	# END REPS
  return(list(outp=outp, parms=parms))
}	


simulate()
