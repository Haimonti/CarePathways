library(UBL)
library(philentropy)

hospital <- read.csv("new.csv")
rownames(hospital) <- 1:nrow(hospital)

generate <- function(data=hospital, target='hospital_length_of_stay', num=1, 
                     thr.rel=0.77, y_split='auto'){
  
  seeds <- sample(100:999, num)
  return_df <- data.frame(
    seed = numeric(),
    C.perc = I(list()),
    thr.rel = numeric(),
    HLOS = I(list())
  )
  
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
    for (i in 1:(length(y) - 1)) {
      if ((y.relev[i] >= thr.rel[t] && y.relev[i + 1] < thr.rel[t]) ||
          (y.relev[i] < thr.rel[t] && y.relev[i + 1] >= thr.rel[t])) {
        bumps <- c(bumps, i)
      }
    }
  
    nbump <- length(bumps) + 1
    obs.ind <- as.list(rep(NA, nbump))
    last <- 1
    for (i in 1:length(bumps)) {
      obs.ind[[i]] <- s.y[last:bumps[i]]
      last <- bumps[i] + 1
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
          for (i in seq_along(obj)) {
            C_perc[[i]] <- round(obj[i] / sapply(obs.ind, length)[i], 1)
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
        
        newrow <- data.frame(seed = seeds[i], C.perc = I(list(C_perc)), 
                             thr.rel = thr.rel[t],
                             HLOS = I(list(newdata[[target]])))
        return_df <- rbind(return_df, newrow)
      }
    }
  }
  return(return_df)
}

test_df <- generate(num = 5, thr.rel = c(0.5, 0.8), y_split = c(0.3, 0.5))

test_df
