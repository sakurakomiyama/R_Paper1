
time2hourMin <- function(x){
  #x <- 8.523
  hour <- floor(x)
  min <- floor((x-hour)*60)
  as.data.frame(cbind(hour,min))
}


### TEsting
test <- function(){
  x <- read.table("../Data/FinalData98.dat",header=T)
  x <- x[x$strata=="year=1998month=11dyp=300lat=1",]
  x$Catch <- x$KgHour
  x$t <- x$localtime
  x$stratum <- paste0("year=",x$year,"month=",x$month,"day=",x$day,"Vessel=",x$VesselID)
  x$row.id <- 1:nrow(x)
###
  x$stratum <- paste(x$year,x$month,x$day,x$VesselID)
  tmp <- x
  
    ## I have called it tmp.dat
    #tmp <- read.table("tmp.dat",header=T)
    tmp$x <- tmp$loct
    tmp$catch <- log(tmp$CPUE)
    stratum.var <- function(x){
	y <- numeric(length(x))
	for(i in 1:length(unique(x))){
	    n.i <- 1:length(unique(x))
	    y[x==unique(as.character(x))[i]] <- n.i[i]
	}
	y
    }
    tmp$stratum <- stratum.var(tmp$stratum)
    tmp$zero.exclude <- rep(FALSE,nrow(tmp))
    utc.tmp <- cbind(tmp$year,tmp$month,tmp$day,time2hourMin(tmp$localtime)["hour"],time2hourMin(tmp$localtime)["min"],tmp$longitudeStart,-abs(tmp$latitudeStart),tmp$catch,tmp$stratum,tmp$zero.exclude, tmp$row.id )
    names(utc.tmp) <- c("year","month","day","hour","min","lon","lat","catch","stratum","zero.exclude","row.id")
    #head(utc.tmp) <- utc.local(utc.tmp,tolocal=F)
#    browser()
    tmpTime <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]
    
    
    ## Altitude of Sun
    utc.tmp$x <- as.vector(unlist(alt.of.sun(x=utc.tmp)[1]))
    tmpSun <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]

    est.nls(tmpTime,a.fix=F,b.fix=F,timebased=T,Alpha=0.96,Beta=8)
    est.nls(utc.tmp,a.fix=T,b.fix=F,timebased=T,Alpha=0.96,Beta=8)
    
    ## Denne
    est.nls(utc.tmp,a.fix=T,b.fix=F,timebased=F,Alpha=0.08,Beta=3)
    
    
}

### myInput
myInput <- function(tmp){
    tmp$row.id <- 1:end(tmp)
    tmp$stratum <- paste(tmp$year,tmp$month,tmp$day,tmp$VesselID)
    tmp$x <- tmp$localtime  ## TIME
    tmp$catch <- log(tmp$CPUE)
    stratum.var <- function(x){
	y <- numeric(length(x))
	for(i in 1:length(unique(x))){
	    n.i <- 1:length(unique(x))
	    y[x==unique(as.character(x))[i]] <- n.i[i]
	}
	y
    }
    tmp$stratum <- stratum.var(tmp$stratum)
    tmp$zero.exclude <- rep(FALSE,end(tmp))
    utc.tmp <- cbind(tmp$year,tmp$month,tmp$day,time2hourMin(tmp$localtime)["hour"],
		    time2hourMin(tmp$localtime)["min"],tmp$longitudeStart,-abs(tmp$latitudeStart))
    names(utc.tmp) <- c("year","month","day","hour","min","lon","lat")
    utc.tmp <- utc.local(utc.tmp,tolocal=F)

    tmpTime <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]
    
    ## Altitude of Sun
    tmp$x <- as.vector(unlist(alt.of.sun(x=utc.tmp)[1]))
    tmpSun <- tmp[,(1:dim(tmp)[2])[match.all(names(tmp),c("catch","stratum","x","zero.exclude","row.id"))]]
#   browser() 
    outT <- est.nls(tmpTime,a.fix=F,b.fix=F,timebased=T,Alpha=0.96,Beta=6.60)	## Sjekk alpha (a.fix)
    outS <- est.nls(tmpSun,a.fix=F,b.fix=F,timebased=F,Alpha=0.079,Beta=3.12)	## Sjekk alpha (a.fix)
    
    density <- mean0(log(tmp$std.CPUE))
    mean.depth <- mean0(tmp$depth)
    mean.lat <- -abs(mean0(tmp$latitudeStart))
    nokept <- length(tmp$year)
    s.out <- c(as.character(tmp$strata[1]),tmp$year[1],tmp$month[1],density,mean.depth,mean.lat,nokept,
		outS$abd.pars[1,2],outS$abd.pars[1,3],outS$abd.pars[1,4],
		outS$abd.pars[2,2],outS$abd.pars[2,3],outS$abd.pars[2,4],
		outS$abd.pars[3,2],outS$abd.pars[3,3],outS$abd.pars[3,4])
    
    t.out <- c(as.character(tmp$strata[1]),tmp$year[1],tmp$month[1],density,mean.depth,mean.lat,nokept,
		outT$abd.pars[1,2],outT$abd.pars[1,3],outT$abd.pars[1,4],
		outT$abd.pars[2,2],outT$abd.pars[2,3],outT$abd.pars[2,4],
		outT$abd.pars[3,2],outT$abd.pars[3,3],outT$abd.pars[3,4])

     write(t.out, file = "outputTime.dat", append = TRUE, ncolumns=length(t.out))
     write(s.out, file = "outputSun.dat", append = TRUE, ncolumns=length(s.out))
#browser()
    
}


"%inword%" <- function(char, word) length(grep(char, word)) > 0    
#z1#z2====est.nls==============================================================
# Function for estimating Alpha, Beta, Dd and mu in the function logit.nls 
# Input:
# Data: a data.frame that must contain the following columns:
# 	catch, stratum, x, zero.exclude, row.id
# a.fix, Alpha: ALpha=C where C is a number. If a.fix=TRUE, Alpha is not 
#	estimated, but set equal to C. If a.fix=FALSE, C is used as start value
#	for Alpha in the nls algorithm.
# b.fix, Beta: same as for a.fix, Alpha.
# timebased: TRUE: local time of day is used. FALSE: altitude of sun is used.
#
est.nls <- function(Data, a.fix, b.fix, Alpha, Beta, timebased, coch.orc.val=1)
{
    Data <- Data[!Data$zero.exclude,]
    
    #Data1 <- read.table("tmp4.dat",header=T)
    #browser()
    x <- Data$x
    catch <- Data$catch

    #z2 calculates initial parameter values for mu and Dd
    # ----- Dd
    night <- Data$x<8 | Data$x>16
    if (sum(night)>0 && sum(!night)>0)
        Dd <- mean(Data$catch[!night]) - mean(Data$catch[night])
    else
        Dd <- 0

    # ----- mu
    ordr <- compress(unique(Data$stratum))# to get correct order in towsprstrat
    towsprstrat <- tapply(Data$stratum, Data$stratum, length)[ordr]
    mu <- tapply(Data$catch, Data$stratum, mean)[ordr] + Dd/2

    ctrl <- list(tolerance=0.01, minscale=0.001, maxiter=100)
    dat <- data.frame(Data[,c("catch","x")])
    towsprstrat.nls <<- towsprstrat
    timebased.nls <<- timebased

    pos <- match("Autoloads",search())

    #z2 estimation
    Afree <- !a.fix
    Bfree <- !b.fix
    crashed <- !FALSE
    if (Afree&&Bfree){
        strt <- list(Alpha=Alpha, Beta=Beta, Dd=Dd, mu=mu)
	
        print("Working....")
        est <- try(nls(catch~logit.nls(mu, Alpha, Beta, Dd, x, 
		timebased.nls, towsprstrat.nls), 
		start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "none"
	if (crashed) print("nls failed with a and b free. Tries with a fixed")
    }
    if (crashed|(a.fix&&Bfree)){
        assign("Alpha", Alpha, pos=pos)
        strt <- list(Beta=Beta, Dd=Dd, mu=mu)
	print("Working....")
        est <- try(nls(catch~logit.nls(mu, Alpha=Alpha, Beta, Dd, x, 
		timebased.nls, towsprstrat.nls), 
		start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "a"
	if (crashed) print(paste("nls failed with a fixed and b free.",
	    "Tries with b fixed and a free"))
    }
    if ((crashed&&Afree&&Bfree)|(Afree&&b.fix)){
        assign("Beta", Beta, pos=pos)
        strt <- list(Alpha=Alpha, Dd=Dd, mu=mu)
	print("Working....")
        est <- try(nls(catch~logit.nls(mu, Alpha, Beta=Beta, Dd, x,
		timebased.nls, towsprstrat.nls), 
		start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "b"
	if (crashed) print(paste("nls failed with a free and b fixed.",
	    "Tries with a and b fixed"))
    }
    if (crashed|(a.fix&&b.fix)){
        assign("Beta", Beta, pos=pos)
        assign("Alpha", Alpha, pos=pos)
        strt <- list(Dd=Dd, mu=mu)
	print("Working....")
	est <- try(nls(catch~logit.nls(mu, Alpha=Alpha, Beta=Beta, Dd, x,
		timebased.nls, towsprstrat.nls), 
		start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "ab"
    }

    #z2 cochran-orcutt info
    eps <- residuals(est)
    ahat <- ar(eps, aic=F, or=1)$ar
    if (abs(ahat)>coch.orc.val) tkmessageBox(message=paste(
        "The Cochran Orcutt procedure only works with alpha and beta fixed.\n",
	"Choose COP limit >= 1 to avoid this message box.\n\n",
	"(Activate the R Console and press <ESC> to stop the program)."))

    #z2 parameter extraction
    summary <- summary(est)
    pars <- est$m$getPars()
    if ("b" %inword% fixedpars){
        pars <- c(Beta, pars)
        names(pars)[1] <- 'Beta'
    }
    if ("a" %inword% fixedpars){
        pars <- c(Alpha, pars)
        names(pars)[1] <- 'Alpha'
    }
    Alpha <- pars["Alpha"]
    Beta <- pars["Beta"]
    Dd <- pars["Dd"]
    mu <- pars[grep("mu",names(pars))]

    #z2 value and standard error matrix
    tmp <- signif(summary(est)$parameters,5)
    abd.pars <- data.frame(matrix(nr=3, nc=4))
    names(abd.pars)[1:4] <- c("Parameter","Value","Sd.Error","p-value")
    abd.pars[,1] <- c("Dd", "Beta", "Alpha")
    #browser()
    abd.pars[,2] <- (c(Dd, Beta, Alpha))
    abd.pars[1,3:4] <- tmp[row.names(tmp)=="Dd", c("Std. Error","Pr(>|t|)")]
    if ("b" %inword% fixedpars) 
      abd.pars[2,3:4] <- "-"
    else 
      abd.pars[2,3:4] <- tmp[row.names(tmp)=="Beta",c("Std. Error","Pr(>|t|)")]
    if ("a" %inword% fixedpars) 
      abd.pars[3,3:4] <- "-"
    else 
      abd.pars[3,3:4] <- tmp[row.names(tmp)=="Alpha",c("Std. Error","Pr(>|t|)")]
    abd.pars <- abd.pars[3:1,]
    
    eps <- summary(est)$res
    fit <- est$m$predict()
    catch.min.mu <- Data$catch-rep(mu,towsprstrat)
    f.val <- logit(x, Alpha, Beta, Dd, timebased)

    #---- f.night
    f.night <- rep(-Dd,length(f.val))

    #z2 R^2
    Rsq <- c(cor(catch, fit)^2,		# total variation explained by model
            cor(catch.min.mu, f.val)^2)	# within-strata var expl by model

    # (zero catch rows are not included in results)
    results <- data.frame(row.id=Data$row.id, stratum=Data$stratum, 
        x, fit, eps, catch.min.mu, f.val, f.night)
 
    list (Alpha=Alpha, Beta=Beta, Dd=Dd, mu=mu, abd.pars=abd.pars, est=est, 
    	results=results, Rsq=Rsq, fixedpars=fixedpars, timebased=timebased,
	ahat=ahat, summary=summary)
#browser()
}

#z1#z2====est.nls.covs=========================================================
# Function for estimating alpha, beta, D in the function logit (above).
# alpha, beta: if called with alpha=C where C is a number, alpha is not
# 	estimated, but set equal to C. Same for beta D is always estimated.
#
est.nls.covs <- function(Data, a.fix, b.fix, Alpha, Beta, timebased,
	covb, covD, DprDstrat, DprDstrat.var, coch.orc.val=0)
{
    #z2 excludes zero catches and extracts columns
    covD <- data.frame1(covD,!Data$zero.exclude)
    covb <- data.frame1(covb,!Data$zero.exclude)
    Data <- Data[!Data$zero.exclude,]
    n <- dim(Data)[1]
    x <- Data$x
    catch <- Data$catch
    if (DprDstrat==0)
        Dstrat <- NA
    else {
	Dstrat <- Data$Dstrat
    }

    #z2 calculates initial parameter values
    # ----- D
    night <- x<8 | x>16
    if (sum(night)>0 && sum(!night)>0)
        D <- mean(Data$catch[!night]) - mean(Data$catch[night])
    else
        D <- 0

    # ----- mu
    ordr <- compress(unique(Data$stratum))# to get correct order in towsprstrat
    towsprstrat <- tapply(Data$stratum, Data$stratum, length)[ordr]
    mu <- tapply(Data$catch, Data$stratum, mean)[ordr] + D/2

    # ----- Dpars
    if (!is.null(covD)){
	covD <- data.frame(interc=rep(1, n), covD)
        Dpars <- c(D,rep(0, dim(covD)[2]-1))
    }
    else if (DprDstrat==1){
	covD <- NA
	Dpars <- rep(1,length(unique(Dstrat)))
    }
    else {
	covD <- NA
        Dpars <- D
    }

    # ----- bpars
    if (is.null(covb)){
        bpars <- Beta
	covb <- NA
    }
    else {
	covb <- data.frame(interc=rep(1, n), covb)
        bpars <- c(Beta,rep(0, dim(covb)[2]-1))
    }

    ctrl <- list(tolerance=0.01, minscale=0.001, maxiter=100)
    dat <- data.frame(Data[,c("catch","x")])
    Dstrat.nls<<-Dstrat
    covb.nls<<-covb
    covD.nls<<-covD
    towsprstrat.nls <<- towsprstrat
    timebased.nls <<- timebased

    pos <- match("Autoloads",search())

    #z2 estimation
    Afree <- !a.fix
    Bfree <- !b.fix
    crashed <- FALSE
    if (Afree&&Bfree){
        strt <- list(mu=mu, Alpha=Alpha, bpars=bpars, Dpars=Dpars)
        est <- try(nls(catch~logit.nls.covs(mu, Alpha, bpars, Dpars, x,
	    timebased.nls, towsprstrat.nls, Dstrat.nls, covb.nls, covD.nls), 
	    start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "none"
	if (crashed) catn("nls failed with a and b free. Tries with a fixed")
    }
    if (crashed|(a.fix&&Bfree)){
        assign("Alpha", Alpha, pos=pos)
        strt <- list(mu=mu, bpars=bpars, Dpars=Dpars)
        est <- try(nls(catch~logit.nls.covs(mu, Alpha=Alpha, bpars, Dpars, x,
	    timebased.nls, towsprstrat.nls, Dstrat.nls, covb.nls, covD.nls), 
	    start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "a"
	if (crashed) catn(paste("nls failed with a fixed and b free.\n",
	    "Tries with b fixed and a free"))
    }
    if ((crashed&&Afree&&Bfree)|(Afree&&b.fix)){
        assign("bpars", bpars, pos=pos)
        strt <- list(mu=mu, Alpha=Alpha, Dpars=Dpars)
        est <- try(nls(catch~logit.nls.covs(mu, Alpha, bpars=bpars, Dpars, x,
	    timebased.nls, towsprstrat.nls, Dstrat.nls, covb.nls, covD.nls), 
	    start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "b"
	if (crashed) catn(paste("nls failed with a free and b fixed.\n",
	    "Tries with a and b fixed"))
    }
    if (crashed|(a.fix&&b.fix)){
        bpars <- Beta
	covb.nls <<- NA
        assign("bpars", bpars, pos=pos)
        assign("Alpha", Alpha, pos=pos)
        strt <- list(mu=mu, Dpars=Dpars)
        est <- try(nls(catch~logit.nls.covs(mu,Alpha=Alpha,bpars=bpars,Dpars,x,
	    timebased.nls, towsprstrat.nls, Dstrat.nls, covb.nls, covD.nls), 
	    start=strt, data=dat, contr=ctrl), silent=T)
	crashed <- !(any(names(summary(est))=="formula"))
        fixedpars <- "ab"
    }

    #z2 cochran-orcutt info
    eps <- residuals(est)
    ahat <- ar(eps, aic=F, or=1)$ar
    if (abs(ahat)>coch.orc.val) tkmessageBox(message=paste(
        "The Cochran Orcutt procedure only works with alpha and beta fixed.\n",
	"Choose COP limit >= 1 to avoid this message box"))

    #z2 parameter extraction - abd.pars
    pars <- signif(est$m$getPars(),5)
    a.fixed <- "a" %inword% fixedpars
    b.fixed <- "b" %inword% fixedpars
    if (b.fixed){
        pars <- c(bpars=Beta, pars)
	bpars <- Beta
    }
    if (a.fixed){
        pars <- c(Alpha=Alpha, pars)
    }
    mu <- pars[grep("mu",names(pars))]
    tmp <- signif(summary(est)$parameters,5)
    abd.pars <- data.frame(matrix(nr=length(pars)-length(mu), nc=4+DprDstrat))
    names(abd.pars)[1:4] <- c("Parameter","Value","Sd.Error","p-value")

    # ---- Alpha
    abd.pars[1,1:2] <- c("Alpha",pars["Alpha"])
    if (a.fixed) abd.pars[1,3:4] <- c("-","-")
    else abd.pars[1,3:4] <- tmp[grep("Alpha",row.names(tmp)), 
    	c("Std. Error", "Pr(>|t|)")] 

    # ---- Beta
    tst <- 2:(1+length(bpars))
    abd.pars[tst,2] <- pars[grep("bpars",names(pars))]
    if (b.fixed) abd.pars[2,c(1,3:4)] <- c("Beta","-","-")
    else abd.pars[tst,3:4] <- tmp[grep("bpars",row.names(tmp)), 
    	c("Std. Error", "Pr(>|t|)")] 
    if (length(tst)==1) abd.pars[tst,1] <- "Beta"
    else abd.pars[tst,1] <- paste0("b_",names(covb))

    # ---- D
    tst <- (2+length(bpars)):dim(abd.pars)[1]
    abd.pars[tst,c(2:4)] <- tmp[grep("Dpars",row.names(tmp)),
    	c("Estimate", "Std. Error", "Pr(>|t|)")] 
    if (!is.data.frame(covD)) abd.pars[tst,1] <- "D"
    else abd.pars[tst,1] <- paste0("D_",names(covD))

    D <- an(abd.pars$Value[abd.pars$Par=="D"])

    #---- adds a column with values of Dstrat
    if (DprDstrat==1){
        names(abd.pars)[5] <- DprDstrat.var
	abd.pars[tst,5] <- unique(Data$Dstrat)
	nn <- c(n,n,tapply(Data$Dstrat,Data$Dstrat,length))
	abd.pars <- cbind(abd.pars, nn)
	names(abd.pars)[6] <- "n"
    }
    
    eps <- summary(est)$res
    fit <- est$m$predict()
    tmps <- Data$stratum
    mu <- coef(est)[grep("mu",names(coef(est)))]
    mu.long <- rep(mu, tapply(tmps, tmps, length)[unique(compress(tmps))])
    catch.min.mu <- Data$catch - mu.long
    f.val <- fit - mu.long

    #z2 R^2
    Rsq <- c(cor(catch, fit)^2,		# total variation explained by model
            cor(catch.min.mu, f.val)^2)	# within-strata var expl by model

    #---- f.night pr stratum
    f.night <- f0 <- rep(-1,n)
    Dpos <- 2+length(bpars)
    if (DprDstrat==1){ 			# D pr Dstrat
	tmp <- abd.pars[-(2:Dpos-1),c(2,5)]
        for (i in unique(Data$Dstrat)){
            tst <- Data$Dstrat==i
    	    f.night[tst] <- f0[tst]*an(tmp[tmp[,2]==i,1])
	}
    }
    else if (!is.null(dim(covD))){	# regression on D
        f.night <- f0*an(abd.pars[Dpos,2])
        for (i in 2:dim(covD)[2]) 
            f.night <- f.night + f0*an(abd.pars[Dpos+i-1,2])*covD[,i]
    }
    else				
        f.night <- f0*an(abd.pars[Dpos,2])

    # (zero catch rows are not included in results)
    results <- data.frame(row.id=Data$row.id, stratum=Data$stratum, 
  	x, fit, eps, catch.min.mu, f.val, f.night)

    #z2 regression curves if only one covariate

    covmatD <- NULL
    if (!is.null(covD) && !is.null(dim(covD)) && length(unique(unlist(strsplit(
    		names(covD[-1]),"\\."))[1:dim(covD[-1])[2]*2-1]))==1){
	tmp <- summary(est)$cov.unscaled
	tmp <- tmp[grep("Dpars", row.names(tmp)),grep("Dpars", colnames(tmp))]
	tmp <- tmp * summary(est)$sigma^2
	if (DprDstrat){
	    xvals <- Dstrat <- NULL
	    for (i in unique(an(Data$Dstrat))){
	        tst <- Data$Dstrat==i
		xvals <- c(xvals, unique(covD[tst,1]))
		Dstrat <- c(Dstrat, rep(i,length(unique(covD[tst,1]))))
	    }
	}
	else{
	    xvals <- unique(covD[,2])	# covD[,1]==1 for all rows
	    Dstrat <- rep(1, length(xvals))
	}
	xvals <- data.frame(xvals, Dstrat)
	covmatD <- list(value=as.numeric(abd.pars[grep("D_",abd.pars$Par),2]), 
	    cov=tmp, xvals=xvals, 
	    name=unlist(strsplit(names(covD[-1]),"\\."))[1])
    }

    covmatb <- NULL
    if (!is.null(covb)&&!is.na(covb)&&!b.fixed&&length(unique(unlist(strsplit(
    		names(covb[-1]),"\\."))[1:dim(covb[-1])[2]*2-1]))==1){
	tmp <- summary(est)$cov.unscaled
	tmp <- tmp[grep("bpars", row.names(tmp)),grep("bpars", colnames(tmp))]
	tmp <- tmp * summary(est)$sigma^2
	covmatb <- list(value=as.numeric(abd.pars[grep("b_",abd.pars$Par),2]), 
	    cov=tmp, xvals=unique(covb[,2]), 
	    name=unlist(strsplit(names(covb[-1]),"\\."))[1])
    }

    list (D=D, abd.pars=abd.pars, est=est, results=results, Rsq=Rsq,
    	fixedpars=fixedpars, timebased=timebased, covmatD=covmatD,
	covmatb=covmatb, Dstrat=Dstrat, ahat=ahat)
}

#z1#z2====estimate.dv==========================================================
# Main function. Estimation, adjusting, bootstrapping, plotting is done from
# here.
estimate.dv <- function(Data, 
	y.transf, boxcox.val, yadd.val,
	y.choise, ystand.var,
	zero.choise, zero.pct,
	catch.var,
	strat.vars, move.singles,
	covD, covD.vars, covD.elim,
	covb, covb.vars,
	nboot, seed, adj.choise,
	utc.choise,
	midhaul.choise, midhaul.var,
	suntime.choise,
	func.choise,
	a.fix, a.val, b.fix, b.val,
	DprDstrat, DprDstrat.var, coch.orc.val)
{
    if (covD==0) covD.vars <- ""
    if (covb==0) covb.vars <- ""

    nr <- dim(Data)[1]
    nc <- dim(Data)[2]
    catchcols <- catch.var.to.catchvars(catch.var, names(Data))
    allcols <- unlist(catchcols)
    
    #z2 adjusted catches; filled below by results from adjust.data
    adj.do <- adj.choise!=""
    if (adj.do){
        data.adj <- Data
        data.adj[,allcols] <- NA
        data.adj.boot <- data.frame(matrix(nr=nr*nboot, nc=length(allcols)))
        names(data.adj.boot) <- names(Data)[allcols]
    }
    else 
        data.adj <- data.adj.boot <- NULL

    #z2 various results; expanded at the end of the loop
    results <- abd.pars <- Rsq <- Rsq.elim <- ests <- ahats <- list()
    if (DprDstrat) Dstrat.values <- sort(unique(Data[,DprDstrat.var]))
    nDstrat <- ifelse(DprDstrat,length(Dstrat.values),1)
    D <- data.frame(matrix(nr=nboot+2, nc=length(catchcols)*nDstrat))
    mean.catch <- data.frame(matrix(nr=nboot+2, nc=length(catchcols)*nDstrat))

    nrows <- ceiling(sqrt(length(catchcols)))
    titles <- character(length(catchcols))

    # open graphical devices 
    if (tclvn(doplot.reusewin)==0){
	dev.new()
        par(mfrow=c(nrows, nrows), mar=c(5,4,4,2))
	dev.main.no <<- dev.cur()
	if (covD==1 && tclvn(doplot.regcurve)==1){
	    dev.new()
            par(mfrow=c(nrows, nrows), mar=c(5,4,4,2))
	    dev.regr.no <<- dev.cur()
	}
    }
    else {
	if (length(dev.list())==0) dev.new()
	if (length(dev.list())==1 && tclvn(doplot.regcurve)==1) dev.new()
	for (i in 1:length(dev.list())){
	    dev.set(i+1)
            plot(0,0,axes=F,xlab="",ylab="",type="n")
            par(mfrow=c(nrows, nrows), mar=c(5,4,4,2))
	}
	dev.main.no <<- 2
	dev.regr.no <<- 3
    } 
    bootno <<- 0
    j <- 0
    n <- length(catchcols)

    #z2 loop over catch variables
    for (i in catchcols[1:n]){ 
        cols <- unlist(i)
	dec <- ifelse(any(Data[,cols]%%1!=0),5,0) 	# only integer input?
	title <- names(Data)[cols[1]]
	if (length(cols)>1) title <- paste0(title,"-",names(Data)[last(cols)])
	catn("===================\ncurrent variable:",title)
	j <-j+1
	try(tkwm.title(base, 
 	    paste("DIVA: catch var:",j,"of",n)),silent=T)
	#z3 prepare data: catch,stratum,x,zero.exclude,row.id/covD.mat/covb.mat
        data0 <- prepare.data(Data, cols, y.transf, boxcox.val, yadd.val,
		y.choise, ystand.var, zero.choise, zero.pct, 
		catch.var, strat.vars, move.singles, covD.vars, covb.vars, 
		utc.choise, midhaul.choise, midhaul.var,
		suntime.choise, DprDstrat, DprDstrat.var) 
	orig.ordr <- order(data0$Data$row.id)
	if (DprDstrat) 
	    Dstrat.orig <- data0$Data$Dstrat[orig.ordr]
	else
	    Dstrat.orig <- NULL

	#z3 estimate
	if (func.choise=="Logistic" && a.fix + b.fix < 2 && 
	    (DprDstrat || covD || covb))			# nls, regr
	    est <- est.nls.covs(data0$Data, 
	        a.fix, b.fix, a.val, b.val, data0$timebased,
		data0$covb.mat, data0$covD.mat, DprDstrat, DprDstrat.var,
		coch.orc.val)
	else if (func.choise=="Logistic" && a.fix + b.fix < 2)	# nls
	    est <- est.nls(data0$Data, 
	        a.fix, b.fix, a.val, b.val, data0$timebased, coch.orc.val)
	else 							# lm
	    est <- est.lm(data0$Data, data0$covD.mat, a.val, b.val,
	    	func.choise, data0$timebased, DprDstrat==1, DprDstrat.var,
		coch.orc.val)

	#z3 backward elimination
	if (covD.elim==1) Rsq.tmp <- backw.elim(data0$Data, 
	    data0$covD.mat, a.val, b.val, func.choise, data0$timebased, 
	    coch.orc.val)

	#z3 adjust
	if (adj.do){
            adj <- adjust.data(data0$Data$catch[orig.ordr], 
	        data.frame(Data[,cols]), adj.choise, est, 
		y.transf, boxcox.val, yadd.val, y.choise, ystand.var, dec=dec)
	    data.adj[,cols] <- adj$Data
	}

        #z3 storing estimated D with sd.error in D[nboot+1:2,]
	tst <- ((j-1)*max(1,nDstrat)+1):(j*max(1,nDstrat))
	if (covD + covb == 0 && nboot>0){
	    tst1 <- est$abd.pars$Par=="D"
	    if (DprDstrat)
	        tst2 <- Dstrat.values %in% est$abd.pars[tst1, 5]
	    else
	        tst2 <- TRUE
	    D[nboot+1,tst[tst2]] <- an(est$abd.pars[tst1,"Value"])
            D[nboot+2,tst[tst2]] <- an(est$abd.pars[tst1,"Sd.Error"])
	}

        #z3 storing mean (un)adjusted catch in mean.catch[nboot+1:2,]
	tst <- ((j-1)*max(1,nDstrat)+1):(j*max(1,nDstrat))
	for (k in 1:nDstrat){
	    if (DprDstrat==0) 
		tst3 <- TRUE
	    else
		tst3 <- Dstrat.orig==unique(Dstrat.orig)[k]
	    mean.catch[nboot+2,tst[k]] <- sum(mean0(Data[tst3,cols]))
	    if (adj.do)
		mean.catch[nboot+1,tst[k]] <- sum(mean0(adj$Data[tst3,]))
	}

	#z3 plot
	if (adj.do) tmp <- adj$add else tmp <- rep(0, dim(est$results)[1])
	plot.est(est, tmp, func.choise, covD, covb, title)
	plot.linreg(est$covmatD, title, nrows, first=j==1, nDstrat)

	#z3 bootstrap
	rows <- est$results$row.id
	covD.mat <- data.frame1(data0$covD.mat, rows)
	covb.mat <- data.frame1(data0$covb.mat, rows)
	tmp <- boot.data(lgrno=j, nlgr=n, nboot, seed, est,
	        covD.mat, covb.mat, func.choise, a.fix, b.fix, a.val, b.val, 
		catch.obs=data0$Data$catch[orig.ordr], 
		data.obs=data.frame1(Data[,cols]), 
		Dstrat.obs=Dstrat.orig,
		adj.choise, y.transf, boxcox.val, yadd.val, y.choise, 
		ystand.var, DprDstrat, DprDstrat.var, coch.orc.val, dec=dec)
        if (adj.do) data.adj.boot[,names(Data)[cols]]<-tmp$adjvals
	if (nboot>0){
	    mean.catch[1:nboot,tst] <- tmp$mean.catch
	    if (covD + covb == 0) D[1:nboot,tst[tst2]] <- tmp$D
	}

	#z3 list of results and abd.pars
	results[[j]] <- est$results; names(results)[j] <- title
	abd.pars[[j]] <- est$abd.pars; names(abd.pars)[j] <- title
	Rsq[[j]] <- est$Rsq; names(Rsq)[j] <- title
	if (covD.elim==1){
	    Rsq.elim[[j]] <- Rsq.tmp; names(Rsq.elim)[j] <- title
	}
	else
	    Rsq.elim <- NULL
	ests[[j]] <- est$est
	ahats[[j]] <- est$ahat
	titles[j] <- title
    }
    #z2 diagnostic plots
##    tmp <- tkmessageBox(message="See diagnostic plots?", type="yesno",
##        icon="question")
##    if (tclv(tmp)=="yes") 
    if (tclvn(doplot.diagnost)==1)    
	plot.diagnost(ests, results, titles, covD.mat, nplot=j)

    list(results=results, abd.pars=abd.pars, D=D, mean.catch=mean.catch,
    	Rsq=Rsq, Rsq.elim=Rsq.elim, ahats=ahats,
	data.adj=data.adj, data.adj.boot=data.adj.boot)
#browser()
}

#z1#z2====load.input==========================================================
# function for reading input parameters into the GUI
load.input <- function(fnt, outfile.name) 
{
local({
    # browse for filename
    browse <- function(){
        tclvalue(filename) <- tkgetOpenFile(filetypes= 
	    "{{Input files} {.inp}} {{All files} *}")
    }

    # main function
    do <- function(){
        input.params <<- read.table(tclvalue(filename),sep="@",comment="?")
	tkdestroy(base.o)
	tkfocus(base); tkraise(base)
    }

    # GUI
    base.o <- tktoplevel()
    tkwm.title(base.o,"Load input parameters from file")
    child1 <- tkframe(base.o, borderwidth=2)
    child2 <- tkframe(base.o, borderwidth=2)

    filename <- tclVar()
    tkpack(tklabel(child1,text="file name:",font=fnt),side="left")
    inp.entry <- tkentry(child1, textvar=filename, width=40,
        font=fnt, bg=bgcol, takefocus=1)
    tkpack(inp.entry,side="left")
    tkpack(tkbutton(child2,text="Browse",command=browse,font=fnt),side="left")
    tkpack(tkbutton(child2, text="OK", font=fnt, command=do), side="left")
    tkpack(tkbutton(child2, text="Cancel", font=fnt, command=function(){ 
        tkdestroy(base.o) # input is read into the GUI!!
	tkfocus(base); tkraise(base)
    }), side="left")
    tkpack(child1, child2)
    tkfocus(base.o); tkraise(base.o)
    tkwait.window(base.o)
})
}

#z1=======logit, logit.nls, logit.nls.covs====================================
# functions called by est.lm, est.nls, est.nls.covs
logit <- function(x, alpha, beta, d, sym=T) 
{
   if (sym) x <- x - 2*(x-12)*(x>12) # reflected around 12
   tmp <- exp((x-beta)*alpha)
   tmp12 <- exp((12-beta)*alpha)
   if (sym) corr <- tmp12/(1+tmp12)*d else corr <- d
   tmp/(1+tmp)*d - corr #ifelse(sym, tmp12/(1+tmp12)*d, d)
   #browser()
}

#---------------------------------------------------------------------------
logit.nls <- function(mu, Alpha, Beta, Dd, x, timebased, towsprstrat.nls){
    logit(x,Alpha,Beta,Dd,timebased.nls) +  rep(mu, towsprstrat.nls)
   # browser()
}

#---------------------------------------------------------------------------
logit.nls.covs <- function(mu, Alpha, bpars, Dpars, x, timebased.nls, 
	towsprstrat.nls, Dstrat.nls, covb.nls, covD.nls)
{
    if (!all(is.na(covD.nls)))		Dd <- t(Dpars %*% t(covD.nls))
    else if (!all(is.na(Dstrat.nls)))	Dd <- spread(Dpars, Dstrat.nls)
    else				Dd <- Dpars

    if (!all(is.na(covb.nls)))		Beta <- t(bpars %*% t(covb.nls))
    else				Beta <- bpars
    y <- logit(x,Alpha,Beta,Dd,timebased.nls) +  rep(mu, towsprstrat.nls)
    y
}


#z1#z2====plot.est=============================================================
# function for plotting catch, adjusted catch and fitted values against 
# time of day / altitude of sun
# par(mfrow) is set in estimate.dv()
plot.est <- function(est, adj, func.choise, covD, covb, title)
{
    # various preparations
    res <- est$results
    catch <- res$catch.min.mu
    pars <- est$abd.pars
    logmod <- func.choise=="Logistic" 
    D <- as.numeric(pars[pars$Par=="D","Value"])
    nD <- length(D)
    if (logmod) a <- as.numeric(pars[pars$Par=="Alpha","Value"])
    if (logmod) b <- as.numeric(pars[pars$Par=="Beta","Value"])
    afix <- ifelse("a"%inword%est$fixedpars,"(f)","")
    bfix <- ifelse("b"%inword%est$fixedpars,"(f)","")
    sd.a <- sd.b <- sd.D <- p.D <- NULL
    if (afix=="") sd.a <- as.numeric(pars[pars$Par=="Alpha","Sd.Error"])
    if (bfix=="") sd.b <- as.numeric(pars[pars$Par=="Beta","Sd.Error"])
    if (nD>=1) sd.D <- as.numeric(pars[pars$Par=="D","Sd.Error"])
    if (nD>=1) p.D <- as.numeric(pars[pars$Par=="D","p-value"])
    timebased <- est$timebased
    if (timebased) xr <- c(0,24) else xr <- range(res$x)
    yr <- c(0,0)
    if (tclvn(doplot.obs.vals)==1) yr <- range(yr, catch)
    if (tclvn(doplot.adj.vals)==1 && any(adj!=0)) yr <- range(yr, adj)
    if (tclvn(doplot.fit.vals)==1) yr <- range(yr, frac(res$f.val,c(-0.2,1.2)))
    if (tclvn(doplot.confints)==1 && nD>=1) yr <- range(yr, frac(c(-D+sd.D*2,
	-D-sd.D*2),c(-0.2,1.2)))
    cex <- max(0.1,1-(length(catch)%/%1000)/10)
    subtit <- ""
    if (logmod && length(a)==1) subtit <- paste0(subtit,"a",afix,"=",r20(a))
    if (logmod && length(b)==1) subtit <- paste0(subtit," b",bfix,"=",r20(b))
    if (nD==1) subtit <- paste0(subtit," D=",r20(D))

    # data points 
    dev.set(dev.main.no); try(bringToTop(), silent=T)
    plot(xr, yr, main=title, type="n", axes=F,
        xlab=ifelse(timebased,"time","altitude of sun"), 
	ylab="standardized catch", pch=16, cex=.6)
    box(); axis(2)
    if (timebased) axis(1, at=0:6*4) else axis(1)
    title(line=0.5, cex.main=0.9, main=subtit)
    if (tclvn(doplot.obs.vals)==1){
	points(res$x, catch, col=1, cex=cex)
	text(frac(xr,0), frac(yr,0.98), "observed", col=1, adj=0, cex=1)
    }

    # adjusted catches
    if (any(adj!=0) && tclvn(doplot.adj.vals)==1){
        points(res$x, catch+adj, col=4, cex=cex)
        text(frac(xr,0), frac(yr,0.90), "adjusted", col=4, adj=0, cex=1)
    }

    # fitted function
    if (tclvn(doplot.fit.vals)==1){
	if (dim(pars)[1]==3 || (!covD&&!covb)) {	# no regression of D/b
	    xx <- seq(min(res$x), max(res$x), len=100)
	    for (i in 1:nD)
		if (logmod)
		    lines(xx, logit(xx, a, b, D[i], timebased), col=2, lwd=3)
		else 
		    lines(xx, sinf(xx, D[i]), col=2, lwd=3)
	}
	else 
	   points(res$x, res$f.val, col=2, cex=cex)
	text(frac(xr,1), frac(yr,0.98), "fitted", col=2, adj=1, cex=1)
    }

    # confidence interval D
    if (nD>=1 && tclvn(doplot.confints)==1){
        for (i in 1:nD) {
	    if (i <= nD/2+0.5 || !timebased || nD<6)
	        x0 <- frac(xr,0.03*i)
	    else
	        x0 <- frac(xr,1-0.03*(nD-i+1))
	    y0 <- -D[i]
	    col <- ifelse(p.D[i] < 0.05, "orange","green")
            lines(rep(x0,2), y0+sd.D[i]*c(-1.96,1.96), lwd=3, col=col)
	    lines(x0+c(-1,1)*.7, y0+rep( sd.D[i]*1.96,2), lwd=3, col=col)
	    lines(x0+c(-1,1)*.7, y0+rep(-sd.D[i]*1.96,2), lwd=3, col=col)
	    lines(x0+c(-1,1)*.3, y0+rep(            0,2), lwd=3, col=col)
	}
	tmp <- names(est$abd.pars)[5]
	if (nD>1){
	    tmp1 <- paste(ac(sort(unique(est$Dstrat))),collapse=" ")
	    text(xr[1], yr[1], paste0(tmp, ":", tmp1), adj=0)
	}
	abline(h=0, lty=2)
    }

    # confidence interval b
    if (logmod && length(b)==1 && !is.null(sd.b) && tclvn(doplot.confints)==1){
        y0 <- diff(range(catch))/20
	if (is.null(sd.D)) D <- -mean(est$results$f.night)
        lines(b+sd.b*c(-2,2), rep(-min(D)/2,2), col="green", lwd=3)
        lines(rep(b+sd.b*2,2), -min(D)/2+y0*c(-1,1), col="green", lwd=3) 
        lines(rep(b-sd.b*2,2), -min(D)/2+y0*c(-1,1), col="green", lwd=3) 
    }

    # "legend": confidence interval
    if ((nD>=1) || (!is.null(sd.b) && length(b)==1)&&tclvn(doplot.confints)==1){
        text(frac(xr,1), frac(yr,0.90), "significant 95% CI", col="orange", 
	    adj=1, cex=1, lwd=2)
        text(frac(xr,1), frac(yr,0.82), "non-signif. 95% CI", col="green", 
	    adj=1, cex=1)
    }
}
#z1#z2===============================================================

##z2-compress------------------------------------------------------------------
## returns a "compressed" version z of an integer vector x
## where z[min(x)] = 1, z[max(x)] = length(unique(x)). Example:
##  in: 6 8 9 9 12 6 12 8 14 
## out: 1 2 3 3  4 1  4 2  5
#
compress <- function(x){
    if (any(x%%1 != 0)) 
        warning("### Non-integer input to function compress ###")
    o <- order(x)
    y <- x[o]
    d <- c(y[1],diff(y))
    b <- cumsum(pmax(d-1,0))
    z <- numeric(length(x))
    z[o] <- y-b
    z <- z - min(z) + 1
    z
}

##z2-utc.local-----------------------------------------------------------------
## Calculates local tim from UTC (tolocal=T) or vice versa (tolocal=F)
## x is a data.frame with variables year, month, day, hour, min
#
utc.local <- function(x, tolocal=T){
    days <- c(0,31,28+x$year%%4==0,31,30,31,30,31,31,30,31,30,31)
    cumday <- x$day + cumsum(days)[x$month]
    dlta <- (-0.4083*sin(0.0172*(cumday-80))
             -1.7958*cos(0.0172*(cumday-80))
             +2.4875*sin(0.0344*(cumday-80)))/15
    add <- x$lon/15 + dlta
    if (!tolocal) add <- -add

    if (any(is.na(x$lon))) stop(paste("\n",
       "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
       "Cannot calculate local time because of missing values in longitude.\n",
       "To eliminate these observations, write '!is.na(lon)' in the \n",
       "'Selection' field\n",
       "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))



    adjust.time(x, add)
}

##z2-alt.of.sun----------------------------------------------------------------
## Calculates the altitude of the sun in a given position at a given time,
## and the time for sunrise in this position the given day.
## Time must be UTC; time, lat and lon must be decimal numbers.
#
alt.of.sun <- function(min=x$min, hour=x$hour, day=x$day, month=x$month, 
	lat=x$lat, lon=x$lon, x=NULL)
{
    # altitude of sun
    UTC <- hour+min/60
    CET <- (UTC + 1) %% 24
    dayadd <- cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30,31))
    cumday <- day + dayadd[month]
    K1 <- (lon - 15 - 0.4083 * sin(0.0172 * (cumday-80)) 
		    - 1.7958 * cos(0.0172 * (cumday-80))
		    + 2.4875 * sin(0.0344 * (cumday-80)))
    SST <- ((CET*15) + K1) / (180/pi)
    dkl <- asin(0.3979 * sin((0.0172 * (cumday - 80))
             + 0.03346 * (sin(0.0172 * cumday) - 0.98112)))
    Brq <- lat/(180/pi)
    sinush <- (sin(dkl)*sin(Brq)) - (cos(dkl)*cos(Brq)*cos(SST))
    alt.of.sun <- asin(sinush) * (180/pi)
 
    # time when altitude of sun = asun.0
    asun.0 <- 0		# 0: sunrise
    K2 <- (sin(dkl)*sin(Brq) - sin(asun.0/(180/pi))) / (cos(dkl)*cos(Brq))
    K2[K2 < (-1)] <- -1	# polar night
    K2[K2 > ( 1)] <-  1	# midnight sun
    SST0 <- acos(K2)
    CET0 <- (SST0 * (180/pi) - K1) / 15
    UTC0 <- (CET0 - 1) + 24*(CET0 < 1)
    sun.rise <- UTC0%%24
    list(alt.of.sun=alt.of.sun, sun.rise=sun.rise)
}


##z2-adjust.time---------------------------------------------------------------
## Adds <add> hours to the time and updates year, month, day, hour, min
#
adjust.time <- function(x, add){

    tmp <- ISOdatetime(x$year, x$month, x$day, x$hour, x$min, 0)+add*3600
    x$year <-  as.numeric(substring(tmp,1,4))
    x$month <- as.numeric(substring(tmp,6,7))
    x$day <-   as.numeric(substring(tmp,9,10))
    x$hour <-  as.numeric(substring(tmp,12,13))
    x$min <-   as.numeric(substring(tmp,15,16))
    x
}

