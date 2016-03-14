
library(shiny)
library(knitr)
library(Distance)

shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat <- read.csv2(inFile$datapath, header=TRUE, sep=input$sep)
    head(dat, 10)
    
  }, include.rownames=FALSE)

  
###########################################################

my_dat <- reactive({
  
  inFile <- input$file1
  if (is.null(inFile)){
    return(NULL)      
                      }
  
  read.csv2(inFile$datapath, header=TRUE, sep=input$sep)
})

    
  
############################################################
  
  output$distPlot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    dat <- my_dat()
      
    x <- dat$Linjeavstand  # Distance data
    
    # draw the histogram with the specified number of bins
    hist(x, col = input$histcol, border = 'white', xlab="Linjeavstand",ylab="Antall observasjoner", 
         main="", nclass=input$bins1, cex=2)
    
  })

  
#####################################################################
  
  output$Deskr <- renderTable({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    dat <- my_dat()
    
    temp <- sort(unique(dat$Linjenavn))
    temp_1 <- length(temp)+1
    desc <- as.data.frame(matrix(ncol=4, nrow=temp_1))
    
    
    for(i in 1:length(temp)){
      tempo <- subset(dat, Linjenavn==temp[i])
      desc[i,1] <- temp[i]
      desc[i,2] <- tempo$Linjelengde[1]
      tempo1 <- subset(tempo, Antall>0)
      desc[i,3] <- dim(tempo1)[1]
      desc[i,4] <- sum(tempo1$Antall)
      
    }

    colnames(desc) <- c("Linjenavn", paste("Linjelengde i ", (input$Taks), sep=" "), "Antall obs", "Antall individer observert")
    desc[temp_1,1] <- paste(length(temp), "linjer", sep=" ")
    desc[temp_1,2] <- sum(desc[,2], na.rm=T)
    desc[temp_1,3] <- sum(desc[,3], na.rm=T)
    desc[temp_1,4] <- sum(desc[,4], na.rm=T)
    
    
    colnames(desc) <- c("Linjenavn", paste("Linjelengde i ", (input$Taks), sep=" "), "Antall obs", "Antall individer observert")
    #rownames(desc) <- c(rep(NULL, length(desc)[1]-1), "Totalt")
    
    desc1 <- switch(input$desc1,
      lin={colnames(desc) <- c("Linjenavn", paste("Linjelengde i ", (input$Taks), sep=" "), "Antall obs", "Antall individer observert")
            desc <- desc[1:length(temp),]},
      saml={colnames(desc) <- c("Antall linjer", paste("Linjelengde i ", (input$Taks), sep=" "), "Antall obs", "Antall individer observert")
      desc <- desc[temp_1,]}
      
    )
    
    desc1
  }, include.rownames=FALSE)
  

###########################################################  
  
  
  Distance1 <- eventReactive(input$go_analyse, { 
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    dat <- my_dat()
    
    colnames(dat) <- c("Sample.Label", "Obs.nr", "distance", "size", "Effort", "Region.Label")

    d_la <- ifelse(input$LiAv=="km", 1000,1)
    d_e <- ifelse(input$Taks=="km", 1000,1)
    D_1 <- ifelse(input$Dens=="km", 1000,1)
    
    t_e <- d_e/D_1 
    t_la <- d_la/D_1
    
    dat$Effort <- dat$Effort*t_e
    dat$distance <- dat$distance*t_la
      
    
    ## Data table
    temp1 <- subset(dat, distance!="NA")
    temp1$object <- seq(1:dim(temp1)[1])
    temp1 <- temp1[, c("object", "distance", "size")]
    Data_tab <- temp1
    
    
    ## Region table
    Reg_tab <- as.data.frame(matrix(ncol=2, nrow=1))
    Reg_tab[1,1] <- dat$Region.Label[1]
    Reg_tab[1,2] <- 1000
    colnames(Reg_tab) <- c("Region.Label", "Area")
    
    ## Sample table
    temp <- sort(unique(dat$Sample.Label))
    
    Samp_tab <- as.data.frame(matrix(ncol=3, nrow=length(temp)))
    colnames(Samp_tab) <- c("Sample.Label", "Region.Label", "Effort")
    for(i in 1:length(temp)){
      tempo <- subset(dat, Sample.Label==temp[i])
      Samp_tab[i,1] <- temp[i]
      Samp_tab[i,2] <- dat$Region.Label[1]
      Samp_tab[i,3] <- tempo$Effort[1]
      
    }
    
    ## Obs table
    temp1 <- subset(dat, distance!="NA")
    temp1$object <- seq(1:dim(temp1)[1])
    
    obs_tab <- temp1[,c("object", "Region.Label", "Sample.Label")]
    
    
    
    
    ds.model1 <-ds(Data_tab, region.table=Reg_tab, sample.table=Samp_tab, 
                   obs.table=obs_tab, 
                   adjustment=NULL, transect="line", truncation="10%", 
                   formula= ~1, key="hn")
    
  })
  
  

##########################################
    
 
output$Distance <- renderTable({     
    
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
    ds.model1 <- Distance1()
     
    res <- matrix(ncol=5, nrow=3)
    
    res[1,1] <- as.numeric(paste(ds.model1$dht$individuals$D[2]))
    res[1,2] <- as.numeric(paste(ds.model1$dht$individuals$D[3]))
    res[1,3] <- as.numeric(paste(ds.model1$dht$individuals$D[4]))
    res[1,4] <- as.numeric(paste(ds.model1$dht$individuals$D[5]))
    res[1,5] <- as.numeric(paste(ds.model1$dht$individuals$D[6]))
    
    res[2,1] <- as.numeric(paste(ds.model1$dht$clusters$D[2]))
    res[2,2] <- as.numeric(paste(ds.model1$dht$clusters$D[3]))
    res[2,3] <- as.numeric(paste(ds.model1$dht$clusters$D[4]))
    res[2,4] <- as.numeric(paste(ds.model1$dht$clusters$D[5]))
    res[2,5] <- as.numeric(paste(ds.model1$dht$clusters$D[6]))
    
    res[3,1] <- as.numeric(paste(ds.model1$dht$Expected.S[1,2]))
    res[3,2] <- as.numeric(paste(ds.model1$dht$Expected.S[1,3]))
    res[3,3] <- as.numeric(paste(res[3,2]/res[3,1]))
    
    colnames(res) <- c("Estimat", "SE", "CV", "Nedre 95% CL", "Øvre 95% CL")
    rownames(res) <- c("Total tetthet", "Tetthet av grupper", "Gjennomsnittlig gruppestørrelse")
    res
 
  })

######################################################################

  output$Distance2 <- renderPlot({ 

    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    ds.model1 <- Distance1()

    plot(ds.model1)
    
  }) 
############################  

  
######################################################################
  
  output$Distance3 <- renderPlot({ 
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    model1 <- ds.model1 <- Distance1()
    klasser<-input$bins2
    
    model <- model1$ddf
    ltmodel <- model$ds 
    width <- model$meta.data$width 
    left <- model$meta.data$left 
    ddfobj <- ltmodel$aux$ddfobj 
    point <- ltmodel$aux$point 
    int.range <- ltmodel$aux$int.range
    Nhat <-model$Nhat
    
    breaks <- seq(left,width, length.out=klasser) 
    nc <- length(breaks)-1
    selected <- rep(TRUE,nrow(ddfobj$xmat))
    xmat <- ddfobj$xmat
    expected.counts <- (breaks[2:(nc+1)]-breaks[1:nc])*(Nhat/breaks[nc+1])
    
    hdat <- xmat$distance
    hist.obj <- hist(hdat, breaks=breaks, plot=FALSE) 
    hist.obj$density <- hist.obj$counts/expected.counts 
    hist.obj$density[expected.counts==0] <- 0 
    freq <- hist.obj$density 
    hist.obj$equidist <- FALSE 
    
    
    sigma <- exp( summary(ds.model1)$ds$coef$key.scale$estimate) *1000
    sigma_upper <- exp( (summary(ds.model1)$ds$coef$key.scale$estimate)+(summary(ds.model1)$ds$coef$key.scale$se) )*1000
    sigma_lower <- exp( (summary(ds.model1)$ds$coef$key.scale$estimate)-(summary(ds.model1)$ds$coef$key.scale$se) )*1000
    
    
    ### ESTIMATION OF ESW: 
    
    E_P <- summary(ds.model1)$ds$average.p
    ESW <- as.integer(E_P*as.numeric(width)*1000)
    ESW_cv <- summary(ds.model1)$ds$average.p.se/summary(ds.model1)$ds$average.p
    ESW_se <- as.integer(ESW*ESW_cv)
    
    
    ### Cramer von misen p verdi
    
    
    ### ESTIMATION OF g(x) FOR HALF-NORMAL MODEL; 
    
    x <- seq(left*1000, width*1000, length.out=1000)
    
    x2 <- x^2
    sigma2 <- sigma^2
    sigma2_lower <- sigma_lower^2
    sigma2_upper <- sigma_upper^2
    
    gx <- exp(-x2/(2*sigma2))
    gx_lower <- exp(-x2/(2*sigma2_lower))
    gx_upper <- exp(-x2/(2*sigma2_upper))  
    
    
    
    par(mfrow=c(1,2))
    
    #### Plottig gx and observations
    
    par(bty="l")
    plot(x=c(0, 0), y=c(0,0), xlim=c(0, max(breaks*1000)), ylim=c(0, max(hist.obj$density)+0.1), 
         type="n", xlab="Linjeavstand", ylab="Oppdagbarhet")  
    
    
    for(i in 1:nc){
      temp1 <- hist.obj$density[i]
      temp2 <- breaks[c(i, i+1)]
      temp2 <- temp2*1000
      polygon(x=c(temp2[1], temp2[1], temp2[2], temp2[2]), y=c(0, temp1, temp1, 0), col="slategray2")    
    }
    
    lines(x, gx, lwd=3, col="dark red")
    
    
    #### plotting gx - with se
    par(bty="l")
    plot(x, gx, ylim=c(0,1), xlim=c(left, width*1000), lwd=3, type="l", xlab="Linjeavstand", ylab="Oppdagbarhet")
    lines(x, gx_lower)
    lines(x, gx_upper)
    polygon(x=c(x, rev(x)), y=c(gx_upper, rev(gx_lower)), col=adjustcolor("blue", alpha=0.1), border=NA)  
    
    
  }) 
  ############################    
    
  
  
})