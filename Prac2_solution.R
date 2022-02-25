## ----setup, include=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ---- echo=TRUE, fig.width=4, fig.height=4---------------------------------------------
library(Distance)
data(ducknest)
# Check data OK
head(ducknest, n=3)
# How many observations (note: detections on all lines)
nrow(ducknest)
# Histogram with 8 bins
brks <- seq(from=0, to=2.4, by=0.3)
hist(ducknest$distance, breaks=brks, xlab="Distance (m)",
     main="Perpendicular distances duck nests")


## ---- echo=T, message=FALSE------------------------------------------------------------
conversion.factor <- convert_units("Meter", "Kilometer", "Square Kilometer")
# Half-normal with no adjustments
nest.hn <- ds(ducknest, key="hn", adjustment=NULL,
              convert_units=conversion.factor)
summary(nest.hn)


## ---- message=FALSE--------------------------------------------------------------------
nest.uf.cos <- ds(ducknest, key="unif", adjustment="cos",
                  convert_units=conversion.factor)
nest.hr.herm <- ds(ducknest, key="hr", adjustment="herm", 
                  convert_units=conversion.factor)


## ---- echo=T---------------------------------------------------------------------------
gof_ds(nest.hn, plot=FALSE)


## ---- echo=T---------------------------------------------------------------------------
# Summarise gof statistics
knitr::kable(summarize_ds_models(nest.hn, nest.uf.cos, nest.hr.herm, output="plain"), 
               caption="Model results for ducknest data set.", digits=3)


## ---- echo=F---------------------------------------------------------------------------
# This chunk of code is fiddly and needed only to harvest results
#     and make them look pretty in the output.  Not important for your
#     understanding of performing distance sampling estimation.
nest.tab <- data.frame(Model=1:3, 
                       DetectionFunction=c("Half-normal, no adjustments",
                                           "Uniform, cosine adjustments",
                                           "Hazard rate, no adjustments "),
                       Density=rep(NA,3), LowerCI=rep(NA,3), UpperCI=rep(NA,3))

get.results.f <- function(fit.model) {   
  return(c(D=fit.model$dht$individuals$D$Estimate,
         lCL=fit.model$dht$individuals$D$lcl,
         uCL=fit.model$dht$individuals$D$ucl))
}
nest.tab[1,3:5] <- get.results.f(nest.hn)
nest.tab[2,3:5] <- get.results.f(nest.uf.cos)
nest.tab[3,3:5] <- get.results.f(nest.hr.herm)
knitr::kable(nest.tab, 
             caption="Density estimates and confidence intervals for three fitted models.", 
             digits = 2)


## ---- echo=T, out.width="33%", fig.show="hold", fig.width=6, fig.height=6--------------
plot(nest.hn, nc=8, main="Half normal, no adjustments")
plot(nest.uf.cos, nc=8, main="Uniform, cosine adjustments")
plot(nest.hr.herm, nc=8, main="Hazard rate, no adjustments")

