library(UBL)
library(writexl)
library(tidyverse)

hospital <- read.csv("new.csv")
rownames(hospital) <- 1:nrow(hospital)
hospital_names <- read.csv("new.csv", check.names = F)
hospital_names <- colnames(hospital_names)
hospital_names <- hospital_names[-1]

generate <- function(data=hospital, target='hospital_length_of_stay', 
                     seeds=NULL, num=1, thr.rel=0.77, y_split='auto'){
  
  if (is.null(seeds)) {
    seeds <- sample(100:999, num)
  }
  
  return_dict <- {}
  
  form <- as.formula(paste(target, "~ ."))
  tgt <- which(names(data) == as.character(form[[2]]))
  
  if (tgt < ncol(data)) {
    orig.order <- colnames(data)
    cols <- 1:ncol(data)
    cols[c(tgt, ncol(data))] <- cols[c(ncol(data), tgt)]
    data <- data[, cols]
  }
  y <- data[, ncol(data)]
  attr(y, "names") <- rownames(data)
  s.y <- sort(y)
  pc <- phi.control(y, method = "extremes")
  y.relev <- phi(s.y, pc)
  bumps <- c()
  
  for (t in 1:length(thr.rel)) {
    for (h in 1:(length(y) - 1)) {
      if ((y.relev[h] >= thr.rel[t] && y.relev[h + 1] < thr.rel[t]) ||
          (y.relev[h] < thr.rel[t] && y.relev[h + 1] >= thr.rel[t])) {
        bumps <- c(bumps, h)
      }
    }
    
    nbump <- length(bumps) + 1
    obs.ind <- as.list(rep(NA, nbump))
    last <- 1
    for (b in 1:length(bumps)) {
      obs.ind[[b]] <- s.y[last:bumps[b]]
      last <- bumps[b] + 1
    }
    obs.ind[[nbump]] <- s.y[last:length(s.y)]
    
    for (i in 1:length(seeds)) {
      set.seed(seeds[i])
      
      for (j in 1:length(y_split)) {
        
        if (y_split[j] == 'auto'){
          B <- round(nrow(data)/nbump, 0)
          rescale <- nbump * B/sum(B^2/sapply(obs.ind, length))
          obj <- round((B^2/sapply(obs.ind, length)) * rescale, 2)
          C_perc <- list()
          for (p in seq_along(obj)) {
            C_perc[[p]] <- round(obj[p] / sapply(obs.ind, length)[p], 1)
          }
        } else {
          low_count <- round(nrow(data) * y_split[j], 2)
          high_count <- nrow(data) - low_count
          high_scale <- round(high_count / length(obs.ind[[2]]), 1)
          C_perc <- list(y_split[j], high_scale)
        }
        
        newdata <- SMOGNRegress(
          form = form,
          dat = data,
          dist = as.character('Manhattan'),
          thr.rel = thr.rel[t],
          C.perc = C_perc,
          k = 2,
          repl = FALSE,
        )
        
        newdata <- newdata %>% 
          select(-X)
        colnames(newdata) <- hospital_names
        
        name <- paste0("Seed", seeds[i], "_thr", thr.rel[t], "_perc", C_perc[j])
        return_dict[[name]] <- newdata
      }
    }
  }
  return(return_dict)
}

dict1 <- generate(seeds = c(410, 527, 222, 326, 832), num = 5)

write_xlsx(dict1, "SMOTERegressionCases.xlsx")
