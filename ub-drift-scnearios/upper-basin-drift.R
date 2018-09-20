library(RODBC)

chan<-odbcConnectExcel2007("UMORdrift_forPopModel_v1.xlsx")
sqlTables(chan)
saklevels<-sqlFetch(chan,"SakLevels")
scen1<-sqlFetch(chan,"0pt5mps")
dat<-scen1
scen2<-sqlFetch(chan,"0pt7mps")
dat<-rbind(dat,scen2)
scen3<-sqlFetch(chan,"0pt9mps")
dat<-rbind(dat,scen3)

## SUMMARIZE THE PROPORTION OF AGE-0 RETAINED IN SYSTME
saklevels$id<-1:nrow(saklevels)

plot(Percentile~RM,dat,
    subset=U_mps==combos$U_mps[9] &
        temp_C==combos$temp_C[9],
    xlim=c(1200,1761))
abline(v=1761)# milk
abline(v=c(1565))# milk
abline(v=c(1507))# milk



dd<-subset(dat,(U_mps==combos$U_mps[9]&
    temp_C==combos$temp_C[9]))
plot(Percentile~RM,dd,
    xlim=c(1200,1761))
abline(v=1761)# milk
abline(v=c(1565))# milk
abline(v=c(1507))# milk    
pp<-approxfun(x=dd$RM,
    y=dd$Percentile,rule=2)

pp(1400)


combos<- expand.grid(U_mps=c(0.5,0.7,0.9),
    temp_C=c(14,16,18),
    id=saklevels$id,
    p0=seq(0.000001,0.001,length.out=50))
combos<-merge(combos,saklevels,by="id")


for(i in 1:nrow(combos))
    {
    dd<-subset(dat,(U_mps==combos$U_mps[i]&
        temp_C==combos$temp_C[i]))
    pp<-approxfun(x=dd$RM,
        y=dd$Percentile,rule=2)
    retained<-pp(combos$RM[i])     
    maxAge<-41
    ageMat<-9
    fec<- 19064
    p<- 0.922
    spnInt<- 1/3
    p0<- combos$p0[i] # 0.00001
    p1<-0.68
    S<-c(p1,rep(p,maxAge-2))
    A<-matrix(0,maxAge,maxAge)
    A[1,ageMat:maxAge]<- fec*spnInt*p0*retained
    A[cbind(2:maxAge,1:(maxAge-1))]<-S
    combos$lambda[i]<-as.numeric(eigen(A)$values[1])
    }

lattice::levelplot(lambda~RM+p0,combos,
    subset=U_mps==0.5) 
library(plyr)
library(reshape2)
pp<- expand.grid(U_mps=c(0.5,0.7,0.9),
    temp_C=c(14,16,18))
    
#par(mfrow=c(3,3),mar=c(2,1,1,0),
#    oma=c(2,2,1,1))

pdf(file="lambdas.pdf")
for(i in 1:nrow(pp))
    {
    xx<-dcast(combos,RM~p0,value.var="lambda",
        mean,subset=.(U_mps==pp$U_mps[i]&
        temp_C==pp$temp_C[i]))
    x<-xx[,1]
    y<-sort(unique(combos$p0))
    z<-as.matrix(xx[,-1]) 
    tit<-paste("Velocity= ",
        combos$U_mps[i],"mps", 
        " & Temperature = ", 
        combos$temp_C[i],"C",sep="")
    contour(x=x,y=y,z=z,
        xlab="Lake Sakakawea Level (River mile)",
        ylab="Age-0 survival",
        main=tit,cex.main=1.5,cex.lab=1.3)
    }
dev.off()    

    
    
    
    
    
    
    
plot(lambda~RM,combos,
    subset=U_mps==0.5)    


N<-40000
for(i in 2:40)
    {
    N<-c(N,N[i-1]*lambda)    
    }
