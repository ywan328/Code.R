library(progress)
library(Matrix)
img_path<-'img/'
plot_part <- function(X,nc,type="l", sav = NULL){
  temp <- sort(unique(X[,nc]))
  X.test <- Matrix(0,length(temp),ncol(X),sparse = T)
  X.test[,nc] <- temp
  tau.hat <- predict(model, X.test, estimate.variance = T)
  sigma.hat <- sqrt(tau.hat$variance.estimates)
  up <- tau.hat$predictions + 1.96 * sigma.hat
  lw <- tau.hat$predictions - 1.96 * sigma.hat

  plot(temp , tau.hat$predictions, ylim = range(lw,up),xlim = range(temp), xlab = colnames(X)[nc],
       ylab = "treatment effect", type = type)
  lines(temp , lw, col = 2, lty = 2)
  lines(temp , up, col = 3, lty = 2)
  lines(temp , rep(0,length(temp)), col = 4, lty = 3)
  if(is.character(sav)) {
    dev.print(pdf, str_c(img_path,sav))
  }
}

plot_part_avg <- function(dat,nc,type="l", sav = NULL){
  rec <- dat[[nc]]
  plot(rec$level, rec$effect, ylim = range(rec$eff_lb, rec$eff_ub),
       xlim = range(rec$level), xlab = nc, ylab = "treatment effect", type = type)
  lines(rec$level, rec$eff_lb, col = 2, lty = 2)
  lines(rec$level, rec$eff_ub, col = 3, lty = 2)
  lines(rec$level, rep(0,length(rec$level)), col = 4, lty = 3)
  if(is.character(sav)){
    dev.print(pdf, str_c(img_path,sav))
  }
}

avg_treat <- function(X){
  cat("Prediction started.\n")
  model_hat <- predict(model, estimate.variance = TRUE)
  dat <- as.data.frame(X) %>% mutate_if(is.character, as.factor)
  dat$predest <- model_hat$predictions
  dat$predvar <- sqrt( model_hat$variance.estimates)
  dat$predlb <- dat$predest - 1.96*dat$predvar
  dat$predub <- dat$predest + 1.96*dat$predvar
  cat("Dataframe initialized.\n")
  pb <- progress_bar$new( format = " processing [:bar] :percent eta: :eta", total = ncol(X))
  treat <- list()
  for (x in  colnames(X)){
    treat[[x]] <- aggregate(list(effect = dat$predest,eff_lb = dat$predlb,eff_ub = dat$predub),
                           by = list(level = dat[[x]]), function(v) round(mean(v), 2))
    pb$tick()
  }
  treat
}

avg_box <- function(dat,ncs, sav = NULL){
  
  comb <- function(dat,ncs){
    ls <- dat[ncs]
    res <- lapply( ncs, function(x) {
      (ls[[x]] %>% mutate(variable=x)) [c(5,1:4)]
    })
    do.call(rbind, res)
  }
  treat <- comb(dat,ncs)
  
  ggplot(treat , aes(x = level, y = effect, color = variable)) +
    theme_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text.y = element_text(colour = "black"),
      strip.background = element_rect(colour = NA, fill = NA),
      legend.position = "none"
    ) +
    geom_point() +
    geom_errorbar( aes(ymin = eff_lb, ymax = eff_ub), width = 1, alpha = 0.5 ) +
    geom_hline(yintercept = 0, linetype = 3) +
    facet_grid( variable~., scales = "free_y") +
    coord_flip()
  if(is.character(sav)) ggsave(str_c(img_path,sav))
}