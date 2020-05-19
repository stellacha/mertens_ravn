setwd("~/repo/psvar")
library(R6)
library(vars)

# Name of Variables:"APITR","ACITR","PITB","CITB","GOV","RGDP","DEBT"
NameThatMakesSense <- R6Class("NameThatMakesSense",
   public=list(
      vars=NA,
      proxies=NA,
      p=NA,
      irhor=NA,
      x=NA,
      y=NA,
      shock=NA,
      v7=NA,
      coef=NA,
      bet=NA,
      res=NA,
      k=NA,
      n=NA,
      T=NA,
      m=NA,
      Sigma=NA,
      phib=NA,
      phib11=NA,
      phib21=NA,
      b21ib11=NA,
      Sig11=NA,
      Sig21=NA,
      Sig22=NA,
      ZZp=NA,
      b12b12p=NA,
      b11b11p=NA,
      b22b22p=NA,
      b12ib22=NA,
      b11iSig=NA,
      b21iSig=NA,
      SigmaTSigmaTp=NA,
     
      SigmaT=NA,
      b1=NA,
      irs=NA,
      datav7 = NA,
      datares = NA,
      dataproxies = NA,
      initialize=function(...) {
         DATASET = read.csv("MR_AER_DATASET.csv", header=TRUE)
         DATASET = ts(DATASET,frequency=4)
         self$vars = DATASET[, 6:12]
         self$proxies = DATASET[,4:5]
         self$p = 4         
         self$irhor = 20
         self$x =ts(self$vars , start=c(1950,1), end=c(2006,4), frequency=4)
         self$y =self$vars[-1:-4, ]
         ## "shock identifies the shock hitting the system. Takes the value cit or pit."
      },
      estimate=function() {
         
         
         self$v7 =VAR(self$x, p=4, type = "const")
         self$coef =coef(self$v7)
         self$bet = do.call(cbind, lapply(1:7, function(x) self$coef[[x]][,1]))
         self$res = residuals(self$v7)
         self$k  = ncol(self$proxies)
         self$n = ncol(self$y)
         self$T = nrow(self$y)
         self$m <-ginv(cbind(rep(1, self$T), self$proxies[c(-1:-4),]))
         self$Sigma <- cov(self$res) 
         self$phib <- self$m %*% (self$res)
         self$phib <- self$phib[-1,]
         self$phib11 <-self$ phib[,1:2] # Hard coded?
         self$phib21 <- self$phib[,3:7] # Hard coded?
         #self$b21ib11 <- t(solve(self$phib11, self$phib21))
         tryCatch(self$b21ib11 <-t(solve(self$phib11, self$phib21)), error=function(e) NULL)
         self$Sig11 <- self$Sigma[(1:self$k),(1:self$k)]
         self$Sig21 <- self$Sigma[(self$k+1):self$n,1:self$k]
         self$Sig22 <- self$Sigma[(self$k+1):self$n,(self$k+1):self$n]
         self$ZZp   <- self$b21ib11 %*% self$Sig11 %*% t(self$b21ib11) - (self$Sig21 %*% t(self$b21ib11) + self$b21ib11 %*% t(self$Sig21)) + self$Sig22
         self$b12b12p <- t(self$Sig21- self$b21ib11%*%self$Sig11) %*% (ginv(self$ZZp) %*% (self$Sig21- self$b21ib11%*%self$Sig11))
         self$b11b11p <- self$Sig11 - self$b12b12p
         self$b22b22p <- self$Sig22 + self$b21ib11 %*% (self$b12b12p - self$Sig11) %*% t(self$b21ib11)
         self$b12ib22 <- (t(self$Sig21- self$b21ib11 %*% self$Sig11) + self$b12b12p %*% t(self$b21ib11)) %*% solve(t(self$b22b22p))
         self$b11iSig <- diag(1, self$k) %*% (solve(diag(1, self$k) -self$b12ib22 %*% self$b21ib11))
         self$b21iSig <- self$b21ib11 %*% self$b11iSig
         self$SigmaTSigmaTp <- (solve(self$b11iSig) %*% self$b11b11p) %*% t(solve(self$b11iSig))
         
          
         
       },
      firstEstimation = function() {
         self$estimate()
         self$datav7 <- self$v7
         self$datares <- self$res
         self$dataproxies <- self$proxies
      },
      newData = function() {
         rr <- sample(c(0,1), size=self$T, replace=TRUE)
         varbs <- rbind(self$vars[1:4,],(fitted(self$datav7) + self$datares*rr))
         self$vars <-ts(varbs,start=c(1950,1),end=c(2006,4), frequency=4)
         #print( self$vars)
         self$proxies <-rbind(self$dataproxies[c(1:4),],self$dataproxies[c(-1:-4),]*rr)
         #print(self$proxies)
     
       },
      validate.shock = function() {
         if (!(self$shock %in% c("cit", "pit"))) {
            stop("shock needs to be either 'cit' or 'pit'")
         }
      },
      ## "orderFirst specifies the first variable in the ordering. The first variable affects the others contemporaneously,
      ##  a but is not ffected by the others contemporaneously."
         
      ## cit: corporate income tax is ordered first
      ## pit: personal income tax is ordered first
      validate.orderFirst = function() {
         if (!(self$orderFirst %in% c("cit", "pit"))) {
            stop("order.first needs to be either 'cit' (corporate income tax ordered first) or 'pit' (personal income tax ordered first)")
         }
      },
      set.sigmaT = function(conf) {
         if (conf$orderFirst=="pit") {
            s1 <- sqrt(self$SigmaTSigmaTp[1,1])
            a  <- self$SigmaTSigmaTp[2,1]/s1
            s2 <- sqrt(self$SigmaTSigmaTp[2,2]-(a^2))
            self$SigmaT <- rbind(c(s1, 0), c(a ,s2))
            # print("location 1")
            # print(self$SigmaT)
         } else if (conf$orderFirst=="cit") {
            s2 <- sqrt(self$SigmaTSigmaTp[2,2])
            b <- self$SigmaTSigmaTp[1,2]/s2
            s1 <- sqrt(self$SigmaTSigmaTp[1,1]-(b^2))
            self$SigmaT <- rbind(c(s1, b), c(0 ,s2))
            #cat("self$SigmaTSigmaTp[1,1]", self$SigmaTSigmaTp[1,1], "\n")
            #cat("s1", s1, "\n")
            #cat("s2", s2, "\n")
            #cat("b", b, "\n")
            #print("location 2")
            #print(self$SigmaT)
         } else {
            stop("conf$first has to be 'pit' or 'cit' you dumb mfer")
         }
         
         },
      irf.computation = function(conf) {
         self$set.sigmaT(conf)
         self$b1 <- rbind(self$b11iSig, self$b21iSig) %*% self$SigmaT
         self$irs <- matrix(0, nrow=(self$irhor+self$p), ncol=ncol(self$vars)) 
         i <- if (conf$shock=="pit") { 1 } else if (conf$shock=="cit") { 2 }
         self$irs[self$p+1,] <- -self$b1[,i]/self$b1[i,i]
         for (tt in 2:self$irhor) {
            l <- (self$p+tt-1):tt
            lvars <- as.vector(t(self$irs[l,]))  
            self$irs[self$p+tt,] = t(lvars) %*% self$bet[1:(self$p*self$n),]     
         }
         
         return(self$irs[c(-1:-self$p),]) 
      }))
       


config <-  list(
   "shock: pit, ordered first: pit"=list(shock="pit", orderFirst ="pit"),
   "shock: cit, ordered first: pit"=list(shock="cit", orderFirst ="pit"),
   "shock: pit, ordered first: cit"=list(shock="pit", orderFirst ="cit"),
   "shock: cit, ordered first: cit"=list(shock="cit", orderFirst ="cit"))

IRFBS <-lapply(config, function(x) {
   struct <- NameThatMakesSense$new()
   struct$firstEstimation()
   struct$irf.computation(x)
   set.seed(3801)
   replicate(10000, {
      struct$newData()
      struct$estimate()
      struct$irf.computation(x)
      
   } )
} )

irfbs.temp <- lapply(1:4, function(y) {lapply((1:dim(IRFBS[[y]])[2]),function(x)(IRFBS[[y]][,x,]))})
irfbs <-lapply(1:4, function(y) {vapply((1:dim(IRFBS[[y]])[2]), function(x) apply(irfbs.temp[[y]][[x]],1, mean), numeric(20))})

irfbs.h = lapply(1:4, function(y) {lapply(1:7, function(x) apply(irfbs.temp[[y]][[x]], 1, quantile, prob = 0.025))})
irfbs.l = lapply(1:4, function(y) {lapply(1:7, function(x) apply(irfbs.temp[[y]][[x]], 1, quantile, prob = 0.975))})
  
saveRDS(irfbs, file="~/repo/psvar/irfbs.RDS")
saveRDS(irfbs.h, file="~/repo/psvar/irfbsh.RDS")
saveRDS(irfbs.l, file="~/repo/psvar/irfbsl.RDS")

############### irfs  for shock Average personal income tax and Average Corporate income tax ##################################


IRF <-lapply(config, function(x) {
   struct <- NameThatMakesSense$new()
   struct$firstEstimation()
   struct$irf.computation(x)
     } )
saveRDS(IRF, file="~/repo/psvar/irf.RDS")

