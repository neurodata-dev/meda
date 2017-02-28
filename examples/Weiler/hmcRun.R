#! /usr/local/bin/Rscript
require(meda)
require(rhdf5)

n <- 1e4
seed <- 1234
fresh <- TRUE

if(fresh){
  rhdf5::h5ls("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5")
  datH5 <- rhdf5::h5read("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5",
                         name = "F0")
  datEX <- t(datH5[,,1])
  
  namesEx <- read.csv("~/neurodata/synaptome-stats/Code/Python/chan.csv", head=FALSE, stringsAsFactors = FALSE)[,1]
  
  set.seed(seed)
  datRawEx <- as.data.frame(datEX[sample(nrow(datEX), n), ])
  names(datRawEx) <- as.vector(namesEx)
  
  d <- c(3,26,1,25, 27:29)
  d <- c(d,setdiff(1:29, d))
  datEx <- datRawEx[,d]
  
  ccolEx <- 
    c("#197300", "#197300", "#197300", "#cc0000", "#cc0000", "#cc0000", 
      "#cc0000", "#197300", "#0000cd", "#197300", "#197300", "#197300", 
      "#0000cd", "#0000cd", "#197300", "#cc0000", "#cc0000", "#cc0000", 
      "#197300", "#0000cd", "#197300", "#0000cd", "#0000cd", "#0000cd", 
      "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd")
  
  fac <- factor(ccolEx, levels = c("#197300", "#cc0000", "#0000cd"), 
         ordered = TRUE)
  
  datEx <- datEx[, order(fac)]
  ccolEx <- ccolEx[order(fac)]
  
  datRaw <- scale(datEx, center = TRUE, scale = FALSE)
  datLog <- scale(transformData(datEx, type = c("log10"))$log10, center = TRUE, scale = FALSE)
  dat01e3 <- scale(transformData(datEx, type = c("1e3"))$d01e3, center = TRUE, scale = FALSE)
  
  l4 <- hmcTree(datRaw)
  l5 <- hmcTree(datLog)
  l6 <- hmcTree(dat01e3)
  
  Ls <- list(Raw = l4, Log = l5, "01e3" =l6)
  save(Ls, file = paste0("weilerHMC_seed", seed,"_size", n,".Rdata"))
  
  p <- list()
  
  p[[1]] <- p.stackM(Ls[[1]], ccol = ccolEx, centered = TRUE)
  p[[2]] <- p.stackM(Ls[[2]], ccol = ccolEx, centered = TRUE)
  p[[3]] <- p.stackM(Ls[[3]], ccol = ccolEx, centered = TRUE)
  
  for(i in 1:length(p)){
    png(paste0("figs/weiler", names(Ls)[i],"_seed", seed,"_size", n, ".png"), height = 720, width = 1280) 
    print(p[[i]] + ggtitle(paste0("Weiler ",names(Ls)[i], ", Using seed ", seed, " Size = ", n))) 
    dev.off()
  }
}
  
hmcL <- Ls[[1]]


bic <- lapply(Ls, function(hmcL) hmcL$Get("bic", format = list, traversal = 'level', filterFun = isNotLeaf))
bic <- hmcL$Get("bic", format = list, traversal = 'level', filterFun = isNotLeaf)

setwd("~/neurodata/GH-pages/meda-gh/examples/Weiler/")
for(i in 1:length(bic)){
  pdf(paste0("bic317_size10000/bicWRaw_Node", names(bic)[i], ".pdf"))
  plot(bic[[i]]) 
  dev.off()
}

dev.off()


