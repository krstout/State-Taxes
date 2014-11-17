rm(list = ls())

library(descr)
library(lme4)
library(plyr)
library(ggplot2)
library(party)
library(caret)
library(arm)
library(blme)
library(arm)
library(PtProcess)
library(gtable)
library(gridExtra)
library(googleVis)
library(maps)
library(RColorBrewer)

setwd("C://Users//Sheryl//Documents//PSC 631 Adv. Stats//State Taxes")

data <- read.csv("StateTaxesData.csv")
  
## Dummy for tax increase, decrease, no change

# Turn taxes into millions

data$sales.mil <- data$sales * 1000000
data$inctax.mil <- data$inctax * 1000000
data$corpinc.mil <- data$corpinc * 1000000
data$cigtob.mil <- data$cigtob * 1000000
data$motofuel.mil <- data$motofuel * 1000000
data$alkie.mil <- data$alkie * 1000000
data$others.mil <- data$others * 1000000
data$fees.mil <- data$fees * 1000000
data$totals.mil <- data$totals * 1000000

# Turn budget to millions

data$total_expenditure.mil <- data$total_expenditure * 1000

# Divide taxes by total expenditures (total budget?)

data$n.sales <- data$sales.mil/data$total_expenditure.mil
data$n.inctax <- data$inctax.mil/data$total_expenditure.mil
data$n.corpinc <- data$corpinc.mil/data$total_expenditure.mil
data$n.cigtob <- data$cigtob.mil/data$total_expenditure.mil
data$n.motofuel <- data$motofuel.mil/data$total_expenditure.mil
data$n.alkie <- data$alkie.mil/data$total_expenditure.mil
data$n.others <- data$others.mil/data$total_expenditure.mil
data$n.fees <- data$fees.mil/data$total_expenditure.mil
data$n.totals <- data$totals.mil/data$total_expenditure.mil



# Total taxes
data$taxtotal_increase <- ifelse(data$totals > 0, 1, 0)
data$taxtotal_decrease <- ifelse(data$totals < 0, 1, 0)
data$taxtotal_nochange <- ifelse(data$totals == 0, 1, 0)

data$taxtotal_increase[data$taxtotal_decrease == 1] <- NA
data$taxtotal_decrease[data$taxtotal_increase == 1] <- NA

#data$taxtotal_direction <- ifelse(data$taxtotal_increase == 1, "Tax Increase",
#                             ifelse(data$taxtotal_decrease == 1, "Tax Decrease",
#                                    ifelse(data$taxtotal_nochange == 1, "Tax No Change", "NA")))

# Sales tax
data$sales_increase <- ifelse(data$sales > 0, 1, 0)
data$sales_decrease <- ifelse(data$sales < 0, 1, 0)
data$sales_nochange <- ifelse(data$sales == 0, 1, 0)

data$sales_increase[data$sales_decrease == 1] <- NA
data$sales_decrease[data$sales_increase == 1] <- NA

#data$sales_direction <- ifelse(data$sales_increase == 1, "Sales Tax Increase",
#                             ifelse(data$sales_decrease == 1, "Sales Tax Decrease",
#                                    ifelse(data$sales_nochange == 1, "Sales Tax No Change", "NA")))

# Income tax
data$inctax_increase <- ifelse(data$inctax > 0, 1, 0)
data$inctax_decrease <- ifelse(data$inctax < 0, 1, 0)
data$inctax_nochange <- ifelse(data$inctax == 0, 1, 0)

data$inctax_increase[data$inctax_decrease == 1] <- NA
data$inctax_decrease[data$inctax_increase == 1] <- NA

#data$inctax_direction <- ifelse(data$inctax_increase == 1, "Income Tax Increase",
#                             ifelse(data$inctax_decrease == 1, "Income Tax Decrease",
#                                    ifelse(data$inctax_nochange == 1, "Income Tax No Change", "NA")))

# Corporate tax
data$corp_increase <- ifelse(data$corpinc > 0, 1, 0)
data$corp_decrease <- ifelse(data$corpinc < 0, 1, 0)
data$corp_nochange <- ifelse(data$corpinc == 0, 1, 0)

data$corp_increase[data$corp_decrease == 1] <- NA
data$corp_decrease[data$corp_increase == 1] <- NA

#data$corp_direction <- ifelse(data$corp_increase == 1, "Corporate Tax Increase",
#                             ifelse(data$corp_decrease == 1, "Corporate Tax Decrease",
#                                    ifelse(data$corp_nochange == 1, "Corporate Tax No Change", "NA")))

# Tobacco tax
data$cigtob_increase <- ifelse(data$cigtob > 0, 1, 0)
data$cigtob_decrease <- ifelse(data$cigtob < 0, 1, 0)
data$cigtob_nochange <- ifelse(data$cigtob == 0, 1, 0)

data$cigtob_increase[data$cigtob_decrease == 1] <- NA
data$cigtob_decrease[data$cigtob_increase == 1] <- NA

#data$cigtob_direction <- ifelse(data$cigtob_increase == 1, "Tobacco Tax Increase",
#                             ifelse(data$cigtob_decrease == 1, "Tobacco Tax Decrease",
#                                    ifelse(data$cigtob_nochange == 1, "Tobacco Tax No Change", "NA")))

# Motor fuel tax
data$motofuel_increase <- ifelse(data$motofuel > 0, 1, 0)
data$motofuel_decrease <- ifelse(data$motofuel < 0, 1, 0)
data$motofuel_nochange <- ifelse(data$motofuel == 0, 1, 0)

data$motofuel_increase[data$motofuel_decrease == 1] <- NA
data$motofuel_decrease[data$motofuel_increase == 1] <- NA

#data$motofuel_direction <- ifelse(data$motofuel_increase == 1, "Gas Tax Increase",
#                             ifelse(data$motofuel_decrease == 1, "Gas Tax Decrease",
#                                    ifelse(data$motofuel_nochange == 1, "Gas Tax No Change", "NA")))

# Alkie tax
data$alkie_increase <- ifelse(data$alkie > 0, 1, 0)
data$alkie_decrease <- ifelse(data$alkie < 0, 1, 0)
data$alkie_nochange <- ifelse(data$alkie == 0, 1, 0)

data$alkie_increase[data$alkie_decrease == 1] <- NA
data$alkie_decrease[data$alkie_increase == 1] <- NA

#data$alkie_direction <- ifelse(data$alkie_increase == 1, "Alkie Tax Increase",
#                             ifelse(data$alkie_decrease == 1, "Alkie Tax Decrease",
#                                    ifelse(data$alkie_nochange == 1, "Alkie Tax No Change", "NA")))

# Others
data$others_increase <- ifelse(data$others > 0, 1, 0)
data$others_decrease <- ifelse(data$others < 0, 1, 0)
data$others_nochange <- ifelse(data$others == 0, 1, 0)

data$others_increase[data$others_decrease == 1] <- NA
data$others_decrease[data$others_increase == 1] <- NA
# Way to code for multinomial logit/probit

#data$others_increase <- as.numeric(data$others_increase)

#data$others_direction <- ifelse(data$tax_increase == 1, "Other Tax Increase",
#                             ifelse(data$tax_decrease == 1, "Other Tax Decrease",
#                                    ifelse(data$tax_nochange == 1, "Other Tax No Change", "NA")))

# Fees
data$fees_increase <- ifelse(data$fees > 0, 1, 0)
data$fees_decrease <- ifelse(data$fees < 0, 1, 0)
data$fees_nochange <- ifelse(data$fees == 0, 1, 0)

data$fees_increase[data$fees_decrease == 1] <- NA
data$fees_decrease[data$fees_increase == 1] <- NA

#data$fees_direction <- ifelse(data$fees_increase == 1, "Fee Increase",
#                             ifelse(data$fees_decrease == 1, "Fee Decrease",
#                                    ifelse(data$fees_nochange == 1, "Fee No Change", "NA")))


# Easier gov party variable

data$govrd[data$govparty_c == 0] <- "Republican"     # 576 Republican years
data$govrd[data$govparty_c == 1] <- "Democrat"       # 530 Democratic Years
data$govrd[data$govparty_c == 0.5] <- "Independent"  # 21 Independent years

# Tables comparing R vs. D by tax area

rep.data <- subset(data, data$govparty_c == 0)
dem.data <- subset(data, data$govparty_c == 1)
ind.data <- subset(data, data$govparty_c == 0.5)

# Tables

r.tax <- prop.table(table(rep.data$tax_direction))
d.tax <- prop.table(table(dem.data$tax_direction))

r.sales <- prop.table(table(rep.data$sales_direction))
d.sales <- prop.table(table(dem.data$sales_direction))

r.inctax <- prop.table(table(rep.data$inctax_direction))
d.inctax <- prop.table(table(dem.data$inctax_direction))

r.corp <- prop.table(table(rep.data$corp_direction))
d.corp <- prop.table(table(dem.data$corp_direction))

r.cigtob <- prop.table(table(rep.data$cigtob_direction))
d.cigtob <- prop.table(table(dem.data$cigtob_direction))

r.motofuel <- prop.table(table(rep.data$motofuel_direction))
d.motofuel <- prop.table(table(dem.data$motofuel_direction))

r.alkie <- prop.table(table(rep.data$alkie_direction))
d.alkie <- prop.table(table(dem.data$alkie_direction))

r.others <- prop.table(table(rep.data$others_direction))
d.others <- prop.table(table(dem.data$others_direction))

r.fees <- prop.table(table(rep.data$fees_direction))
d.fees <- prop.table(table(dem.data$fees_direction))

r.header <- c("Decrease", "Increase", "No Change")

rbind(r.sales, d.sales)

rep.gov <- rbind(r.tax, r.sales, r.inctax, r.corp, r.cigtob, r.motofuel, r.others, r.fees)
dem.gov <- rbind(d.tax, d.sales, d.inctax, d.corp, d.cigtob, d.motofuel, d.others, d.fees)

# Models

taxtotal.i <- bglmer(taxtotal_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                     family = binomial, data = data)
taxtotal.d <- bglmer(taxtotal_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                     family = binomial, data = data)

sales.i <- bglmer(sales_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                  family = binomial, data = data)
sales.d <- bglmer(sales_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                  family = binomial, data = data)

inctax.i <- bglmer(inctax_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)
inctax.d <- bglmer(inctax_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)

corp.i <- bglmer(corp_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                 family = binomial, data = data)
corp.d <- bglmer(corp_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                 family = binomial, data = data)

cigtob.i <- bglmer(cigtob_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)
cigtob.d <- bglmer(cigtob_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)

motofuel.i <- bglmer(motofuel_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                     family = binomial, data = data)
motofuel.d <- bglmer(motofuel_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                     family = binomial, data = data)

others.i <- bglmer(others_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)
others.d <- bglmer(others_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                   family = binomial, data = data)

fees.i <- bglmer(fees_increase ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                 family = binomial, data = data)
fees.d <- bglmer(fees_decrease ~ govparty_c + (1 | JoinState) + (1 | JoinYear),
                 family = binomial, data = data)



# govparty_c:
# R = 0
# D = 1

### Predicted Probabilities

dem.prob <- function(x) {
  prob <- plogis(fixef(x)[1] + fixef(x)[2])
}

rep.prob <- function(x) {
  prob <- plogis(fixef(x)[1])
}

# Increase

prob.dem.sales.inc <- dem.prob(sales.i)
prob.rep.sales.inc <- rep.prob(sales.i)

prob.dem.inctax.inc <- dem.prob(inctax.i)
prob.rep.inctax.inc <- rep.prob(inctax.i)

prob.dem.corp.inc <- dem.prob(corp.i)
prob.rep.corp.inc <- rep.prob(corp.i)

prob.dem.cigtob.inc <- dem.prob(cigtob.i)
prob.rep.cigtob.inc <- rep.prob(cigtob.i)

prob.dem.motofuel.inc <- dem.prob(motofuel.i)
prob.rep.motofuel.inc <- rep.prob(motofuel.i)

prob.dem.others.inc <- dem.prob(others.i)
prob.rep.others.inc <- rep.prob(others.i)

prob.dem.fees.inc <- dem.prob(fees.i)
prob.rep.fees.inc <- rep.prob(fees.i)

prob.dem.taxtotal.inc <- dem.prob(taxtotal.i)
prob.rep.taxtotal.inc <- rep.prob(taxtotal.i)

# Decrease

prob.dem.sales.dec <- dem.prob(sales.d)
prob.rep.sales.dec <- rep.prob(sales.d)

prob.dem.inctax.dec <- dem.prob(inctax.d)
prob.rep.inctax.dec <- rep.prob(inctax.d)

prob.dem.corp.dec <- dem.prob(corp.d)
prob.rep.corp.dec <- rep.prob(corp.d)

prob.dem.cigtob.dec <- dem.prob(cigtob.d)
prob.rep.cigtob.dec <- rep.prob(cigtob.d)

prob.dem.motofuel.dec <- dem.prob(motofuel.d)
prob.rep.motofuel.dec <- rep.prob(motofuel.d)

prob.dem.others.dec <- dem.prob(others.d)
prob.rep.others.dec <- rep.prob(others.d)

prob.dem.fees.dec <- dem.prob(fees.d)
prob.rep.fees.dec <- rep.prob(fees.d)

prob.dem.taxtotal.dec <- dem.prob(taxtotal.d)
prob.rep.taxtotal.dec <- rep.prob(taxtotal.d)

# Y-Axis for plot
y.axis <- c("Sales Tax", "Income Tax", "Coporate Tax", "Tobacco Tax", "Gas Tax", "Others", 
               "Fees", "Total Taxes")
better.levels <- c("Coporate Tax", "Income Tax", "Sales Tax", "Others", "Gas Tax", "Tobacco Tax",
                   "Fees", "Total Taxes")


# Democratic Tax Increases
prob.dem.inc <- rbind(prob.dem.sales.inc, prob.dem.inctax.inc, prob.dem.corp.inc, prob.dem.cigtob.inc,
                      prob.dem.motofuel.inc, prob.dem.others.inc, prob.dem.fees.inc, prob.dem.taxtotal.inc)
prob.dem.inc <- cbind(y.axis, prob.dem.inc)
prob.dem.inc <- data.frame(prob.dem.inc)
prob.dem.inc <- rename(prob.dem.inc, c("X.Intercept." = "Probability"))
prob.dem.inc$Probability <- as.numeric(as.character(prob.dem.inc$Probability))

# Republican Tax Increases
prob.rep.inc <- rbind(prob.rep.sales.inc, prob.rep.inctax.inc, prob.rep.corp.inc, prob.rep.cigtob.inc, 
                      prob.rep.motofuel.inc, prob.rep.others.inc, prob.rep.fees.inc, prob.rep.taxtotal.inc)
prob.rep.inc <- cbind(y.axis, prob.rep.inc)
prob.rep.inc <- data.frame(prob.rep.inc)
prob.rep.inc <- rename(prob.rep.inc, c("X.Intercept." = "r.Probability"))
prob.rep.inc$r.Probability <- as.numeric(as.character(prob.rep.inc$r.Probability))

# Democratic Tax Decreases
prob.dem.dec <- rbind(prob.dem.sales.dec, prob.dem.inctax.dec, prob.dem.corp.dec, prob.dem.cigtob.dec, 
                      prob.dem.motofuel.dec, prob.dem.others.dec, prob.dem.fees.dec, prob.dem.taxtotal.dec)
prob.dem.dec <- cbind(y.axis, prob.dem.dec)
prob.dem.dec <- data.frame(prob.dem.dec)
prob.dem.dec <- rename(prob.dem.dec, c("X.Intercept." = "Probability"))
prob.dem.dec$Probability <- as.numeric(as.character(prob.dem.dec$Probability))

# Republican Tax Decreases
prob.rep.dec <- rbind(prob.rep.sales.dec, prob.rep.inctax.dec, prob.rep.corp.dec, prob.rep.cigtob.dec, 
                      prob.rep.motofuel.dec, prob.rep.others.dec, prob.rep.fees.dec, prob.rep.taxtotal.dec)
prob.rep.dec <- cbind(y.axis, prob.rep.dec)
prob.rep.dec <- data.frame(prob.rep.dec)
prob.rep.dec <- rename(prob.rep.dec, c("X.Intercept." = "r.Probability"))
prob.rep.dec$r.Probability <- as.numeric(as.character(prob.rep.dec$r.Probability))

### Risk Ratio

risk.ratio <- function(x) {
  hi <- plogis(fixef(x)[1] + fixef(x)[2])
  lo <- plogis(fixef(x)[1])
  rr <- hi/lo
  return(rr)
}

taxtotal.r <- c(risk.ratio(taxtotal.i), risk.ratio(taxtotal.d))

sales.r <- c(risk.ratio(sales.i), risk.ratio(sales.d))

inctax.r <- c(risk.ratio(inctax.i), risk.ratio(inctax.d))

corp.r <- c(risk.ratio(corp.i), risk.ratio(corp.d))

cigtob.r <- c(risk.ratio(cigtob.i), risk.ratio(cigtob.d))

motofuel.r <- c(risk.ratio(motofuel.i), risk.ratio(motofuel.d))

others.r <- c(risk.ratio(others.i), risk.ratio(others.d))

fees.r <- c(risk.ratio(fees.i), risk.ratio(fees.d))

rr.table <- data.frame(rbind(sales.r, inctax.r, corp.r, cigtob.r, 
                             motofuel.r, others.r, fees.r, taxtotal.r))
names(rr.table) <- c("Increase, D/R", "Decrease, D/R")

rr.table

# Confidence Interval

# Risk Ratio for Increase

rr.table.i <- rr.table[,1]
rr.table.i <- cbind(y.axis, rr.table.i)

rr.sims <- function(x) {
  sims <- sim(x, n.sims = 1000)
  sc <- fixef(sims)
  lo <- plogis(sc[,1])
  hi <- plogis(sc[,1] + sc[,2])
  rr <- hi/lo
  ci <- quantile(rr, c(.05, .95))
  return(ci)
}

ci.sales.i <- rr.sims(sales.i)
ci.inctax.i <- rr.sims(inctax.i)
ci.corp.i <- rr.sims(corp.i)
ci.cigtob.i <- rr.sims(cigtob.i)
ci.motofuel.i <- rr.sims(motofuel.i)
ci.others.i <- rr.sims(others.i)
ci.fees.i <- rr.sims(fees.i)
ci.taxtotal.i <- rr.sims(taxtotal.i)

ci.lower.i <- c(ci.sales.i[1], ci.inctax.i[1], ci.corp.i[1], ci.cigtob.i[1], ci.motofuel.i[1], 
                ci.others.i[1], ci.fees.i[1], ci.taxtotal.i[1])
ci.upper.i <- c(ci.sales.i[2], ci.inctax.i[2], ci.corp.i[2], ci.cigtob.i[2], ci.motofuel.i[2],
                ci.others.i[2], ci.fees.i[2], ci.taxtotal.i[2])
rr.table.i <- data.frame(cbind(rr.table.i, ci.lower.i, ci.upper.i))
rr.table.i$rr.table.i <- as.numeric(as.character(rr.table.i$rr.table.i))
rr.table.i$ci.lower.i <- as.numeric(as.character(rr.table.i$ci.lower.i))
rr.table.i$ci.upper.i <- as.numeric(as.character(rr.table.i$ci.upper.i))

# Risk Ratio for Decrease

rr.table.d <- rr.table[,2]
rr.table.d <- cbind(y.axis, rr.table.d)

ci.sales.d <- rr.sims(sales.d)
ci.inctax.d <- rr.sims(inctax.d)
ci.corp.d <- rr.sims(corp.d)
ci.cigtob.d <- rr.sims(cigtob.d)
ci.motofuel.d <- rr.sims(motofuel.d)
ci.others.d <- rr.sims(others.d)
ci.fees.d <- rr.sims(fees.d)
ci.taxtotal.d <- rr.sims(taxtotal.d)

ci.lower.d <- c(ci.sales.d[1], ci.inctax.d[1], ci.corp.d[1], ci.cigtob.d[1], ci.motofuel.d[1],
                ci.others.d[1], ci.fees.d[1], ci.taxtotal.d[1])
ci.upper.d <- c(ci.sales.d[2], ci.inctax.d[2], ci.corp.d[2], ci.cigtob.d[2], ci.motofuel.d[2],
                ci.others.d[2], ci.fees.d[2], ci.taxtotal.d[2])
rr.table.d <- data.frame(cbind(rr.table.d, ci.lower.d, ci.upper.d))
rr.table.d$rr.table.d <- as.numeric(as.character(rr.table.d$rr.table.d))
rr.table.d$ci.lower.d <- as.numeric(as.character(rr.table.d$ci.lower.d))
rr.table.d$ci.upper.d <- as.numeric(as.character(rr.table.d$ci.upper.d))


### First Difference

fd <- function(x) {
  fd.hi <- plogis(fixef(x)[2] + fixef(x)[1])
  fd.lo <- plogis(fixef(x)[1])
  fd <- fd.hi - fd.lo
  return(fd)
}

taxtotal <- c(fd(taxtotal.i), fd(taxtotal.d))

sales <- c(fd(sales.i), fd(sales.d))

inctax <- c(fd(inctax.i), fd(inctax.d))

corp <- c(fd(corp.i), fd(corp.d))

cigtob <- c(fd(cigtob.i), fd(cigtob.d))

motofuel <- c(fd(motofuel.i), fd(motofuel.d))

others <- c(fd(others.i), fd(others.d))

fees <- c(fd(fees.i), fd(fees.d))

fd.table <- data.frame(rbind(sales, inctax, corp, cigtob, motofuel, others, fees, taxtotal))
names(fd.table) <- c("Increase, D-R", "Decrease, D-R")

# Confidence Interval

# First Difference for Increase

fd.table.i <- fd.table[,1]
fd.table.i <- cbind(y.axis, fd.table.i)

fd.sims <- function(x) {
  sims <- sim(x, n.sims = 1000)
  sc <- fixef(sims)
  lo <- plogis(sc[,1])
  hi <- plogis(sc[,1] + sc[,2])
  fd <- hi - lo
  ci <- quantile(fd, c(.05, .95))
  return(ci)
}

fd.ci.sales.i <- fd.sims(sales.i)
fd.ci.inctax.i <- fd.sims(inctax.i)
fd.ci.corp.i <- fd.sims(corp.i)
fd.ci.cigtob.i <- fd.sims(cigtob.i)
fd.ci.motofuel.i <- fd.sims(motofuel.i)
fd.ci.others.i <- fd.sims(others.i)
fd.ci.fees.i <- fd.sims(fees.i)
fd.ci.taxtotal.i <- fd.sims(taxtotal.i)

fd.ci.lower.i <- c(fd.ci.sales.i[1], fd.ci.inctax.i[1], fd.ci.corp.i[1], fd.ci.cigtob.i[1], fd.ci.motofuel.i[1], 
                fd.ci.others.i[1], fd.ci.fees.i[1], fd.ci.taxtotal.i[1])
fd.ci.upper.i <- c(fd.ci.sales.i[2], fd.ci.inctax.i[2], fd.ci.corp.i[2], fd.ci.cigtob.i[2], fd.ci.motofuel.i[2],
                fd.ci.others.i[2], fd.ci.fees.i[2], fd.ci.taxtotal.i[2])
fd.table.i <- data.frame(cbind(fd.table.i, fd.ci.lower.i, fd.ci.upper.i))
fd.table.i$fd.table.i <- as.numeric(as.character(fd.table.i$fd.table.i))
fd.table.i$fd.ci.lower.i <- as.numeric(as.character(fd.table.i$fd.ci.lower.i))
fd.table.i$fd.ci.upper.i <- as.numeric(as.character(fd.table.i$fd.ci.upper.i))

# First Difference for Decrease

fd.table.d <- fd.table[,2]
fd.table.d <- cbind(y.axis, fd.table.d)

fd.ci.sales.d <- fd.sims(sales.d)
fd.ci.inctax.d <- fd.sims(inctax.d)
fd.ci.corp.d <- fd.sims(corp.d)
fd.ci.cigtob.d <- fd.sims(cigtob.d)
fd.ci.motofuel.d <- fd.sims(motofuel.d)
fd.ci.others.d <- fd.sims(others.d)
fd.ci.fees.d <- fd.sims(fees.d)
fd.ci.taxtotal.d <- fd.sims(taxtotal.d)

fd.ci.lower.d <- c(fd.ci.sales.d[1], fd.ci.inctax.d[1], fd.ci.corp.d[1], fd.ci.cigtob.d[1], fd.ci.motofuel.d[1], 
                   fd.ci.others.d[1], fd.ci.fees.d[1], fd.ci.taxtotal.d[1])
fd.ci.upper.d <- c(fd.ci.sales.d[2], fd.ci.inctax.d[2], fd.ci.corp.d[2], fd.ci.cigtob.d[2], fd.ci.motofuel.d[2],
                   fd.ci.others.d[2], fd.ci.fees.d[2], fd.ci.taxtotal.d[2])
fd.table.d <- data.frame(cbind(fd.table.d, fd.ci.lower.d, fd.ci.upper.d))
fd.table.d$fd.table.d <- as.numeric(as.character(fd.table.d$fd.table.d))
fd.table.d$fd.ci.lower.d <- as.numeric(as.character(fd.table.d$fd.ci.lower.d))
fd.table.d$fd.ci.upper.d <- as.numeric(as.character(fd.table.d$fd.ci.upper.d))
fd.table.d


### Change Over Time

#plot(ranef(taxtotal.i)[["JoinYear"]][,1], type = "l")

#time.sales.i <- ranef(sales.i)[["JoinYear"]][,1]

years <- c(1989:2008, 2010:2012)

rep.time <- function(model) {
  re <- ranef(model)[["JoinYear"]][,1]
  intercept <- fixef(model)[1]
  time.est <- plogis(intercept + re)
  return(time.est)
}

over.time <- function(type.i, type.d, taxtype) {
  # Increase
  re.i <- ranef(type.i)[["JoinYear"]][,1]
  intercept.i <- fixef(type.i)[1]
  dem.i <- fixef(type.i)[2]
  republican.i <- plogis(intercept.i + re.i)
  democrat.i <- plogis(intercept.i + dem.i + re.i)
  # Decrease
  re.d <- ranef(type.d)[["JoinYear"]][,1]
  intercept.d <- fixef(type.d)[1]
  dem.d <- fixef(type.d)[2]
  republican.d <- plogis(intercept.d + re.d)
  democrat.d <- plogis(intercept.d + dem.d + re.d)
  # Graph Data
  years <- data.frame(c(1989:2008, 2010:2012))
  rep.inc <- data.frame(cbind(years, republican.i))
  dem.inc <- data.frame(cbind(years, democrat.i))
  rep.dec <- data.frame(cbind(years, republican.d))
  dem.dec <- data.frame(cbind(years, democrat.d))
  # Increase Graph
  inc.plot <- ggplot() + geom_line(data = rep.inc, aes(x = years, y = republican.i), color = "red", size = 1) +
    geom_line(data = dem.inc, aes(x= years, y = democrat.i), color = "blue", size = 1) + ggtitle("Increase") +
    ylab(taxtype) + xlab("Years") + theme_classic()
  # Decrease Graph
  dec.plot <- ggplot() + geom_line(data = rep.dec, aes(x = years, y = republican.d), color = "red", size = 1) +
    geom_line(data = dem.dec, aes(x = years, y = democrat.d), color = "blue", size = 1) + ggtitle("Decrease") +
    ylab(taxtype) + xlab("Years") + theme_classic()
  
  # Get Graphs Side by Side
  gplot.a <- ggplot_gtable(ggplot_build(inc.plot))
  gplot.b <- ggplot_gtable(ggplot_build(dec.plot))
  
  figure <- grid.arrange(gplot.a, gplot.b, ncol = 2)
  return(figure)
}

sales.graph <- over.time(sales.i, sales.d, "Sales")
inctax.graph <- over.time(inctax.i, inctax.d, "Income Tax")
corp.graph <- over.time(corp.i, corp.d, "Corporatate Tax")
cigtob.graph <- over.time(cigtob.i, cigtob.d, "Tobacco Tax")
motofuel.graph <- over.time(motofuel.i, motofuel.d, "Gas Tax")
others.graph <- over.time(others.i, others.d, "Other Taxes")
fees.graph <- over.time(fees.i, fees.d, "Fees")
taxtotal.graph <- over.time(taxtotal.i, taxtotal.d, "Total Taxes")



### Maps of State Random Effects

state.eff <- function(type.i, type.d, title) {
  # Increase
  sre.i <- ranef(type.i)[["JoinState"]][,1]

  # Decrease
  sre.d <- ranef(type.d)[["JoinState"]][,1]
  
  # Data organization
  region <- data.frame(tolower(state.name))
  all.states <- map_data("state")
  state.data <- data.frame(cbind(region, sre.i, sre.d))
  state.data <- rename(state.data, c(tolower.state.name. = "region"))
  state.data <- join(state.data, all.states, by = "region", match = "first")
  
  # Plot on Map
  map.i <- ggplot() + geom_map(data = state.data, aes(map_id = region, fill = sre.i), map = all.states) +
    geom_path(data = all.states, aes(x = long, y = lat, group = group)) +
    coord_fixed() + theme_bw() + scale_fill_gradientn(colours = brewer.pal(5, "Oranges"), name = "Increase") +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank())
  map.d <- ggplot() + geom_map(data = state.data, aes(map_id = region, fill = sre.d), map = all.states) +
    geom_path(data = all.states, aes(x = long, y = lat, group = group)) +                               
    coord_fixed() + theme_bw() + scale_fill_gradientn(colours = brewer.pal(5, "Purples"), name = "Decrease") +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank())
  map.a <- ggplot_gtable(ggplot_build(map.i))
  map.b <- ggplot_gtable(ggplot_build(map.d))
  
  twomap <- grid.arrange(map.a, map.b, nrow = 2, main = title)
}  

map.sales <- state.eff(sales.i, sales.d, "State Effects for Sales Tax")
map.inctax <- state.eff(inctax.i, inctax.d, "State Effects for Income Tax")
map.corp <- state.eff(corp.i, corp.d, "State Effects for Corporate Tax")
map.cigtob <- state.eff(cigtob.i, cigtob.d, "State Effects for Tobacco Tax")
map.motofuel <- state.eff(motofuel.i, motofuel.d, "State Effects for Gas Tax")
map.others <- state.eff(others.i, others.d, "State Effects for Other Taxes")
map.fees <- state.eff(fees.i, fees.d, "State Effects for Fees")
map.taxtotal <- state.eff(taxtotal.i, taxtotal.d, "State Effects for Total Taxes")

### FIGURE 1: Plot of Predicted Probabilities
#############################################

plot.a <- ggplot() + geom_point(data = prob.dem.inc, aes(Probability, y.axis), size = 4, color = "Blue") +
  geom_point(data = prob.rep.inc, aes(r.Probability, y.axis), size = 4, color = "Red", pch = 17) +
  scale_x_continuous(limits = c(0, 1)) + geom_hline(yintercept = c(1, 2, 3, 4, 5, 6, 7, 8), 
                                                    linetype = "dashed", size = 0.01, alpha = .2) +
  theme_classic() + ggtitle("Tax Increase") + xlab("Predicted Probability") + ylab("Tax Type") 
  
  
  #theme(axis.title.x = element_text(vjust = 0),
   #    axis.title.y = element_text(vjust = 1.5), 
    #   plot.title = element_text(vjust = 2))

plot(plot.a)

plot.b <- ggplot() + geom_point(data = prob.dem.dec, aes(Probability, y.axis), size = 4, color = "Blue") +
  geom_point(data = prob.rep.dec, aes(r.Probability, y.axis), size = 4, color = "Red", pch = 17) +
  scale_x_continuous(limits = c(0, 1)) + geom_hline(yintercept = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                    linetype = "dashed", size = 0.01, alpha = .2) +
  theme_classic() + ggtitle("Tax Decrease") + xlab("Predicted Probability") + ylab("Tax Type")  

plot(plot.b)

# Get plots side by side

gplot.a <- ggplot_gtable(ggplot_build(plot.a))
gplot.b <- ggplot_gtable(ggplot_build(plot.b))

#newWidth = unit.pmax(gplot.a$widths[2:3], gplot.b$widths[2, 3])

#gplot.a[2:3] <- as.list(newWidth)
#gplot.b[2:3] <- as.list(newWidth)

figure1 <- grid.arrange(gplot.a, gplot.b, ncol = 2)


### FIGURE 2: Risk Ratio
########################

plot.rr.i <- ggplot() + coord_flip() + geom_point(data = rr.table.i, aes(y.axis, rr.table.i),
                                                  size = 4) +
  geom_errorbar(data = rr.table.i, aes(y.axis, ymin = ci.lower.i, ymax = ci.upper.i),
                width = 0, size = 1) + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() + ggtitle("Tax Increase") + ylab("Risk Ratio") + xlab("Tax Type") +
  scale_y_continuous(limit = c(0, 2), breaks = seq(0, 2, .25))

plot.rr.i


plot.rr.d <- ggplot() + coord_flip() + geom_point(data = rr.table.d, aes(y.axis, rr.table.d),
                                                  size = 4) +
  geom_errorbar(data = rr.table.d, aes(y.axis, ymin = ci.lower.d, ymax = ci.upper.d),
                width = 0, size = 1) + geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() + ggtitle("Tax Decrease") + ylab("Risk Ratio") + xlab("") +
  scale_y_continuous(limit = c(0, 2), breaks = seq(0, 2, .25)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

plot.rr.d

gplot.rr.i <- ggplot_gtable(ggplot_build(plot.rr.i))
gplot.rr.d <- ggplot_gtable(ggplot_build(plot.rr.d))

figure2 <- grid.arrange(gplot.rr.i, gplot.rr.d, ncol = 2)

### FIGURE 3: First Differences
###############################

plot.fd.i <- ggplot() + coord_flip() + geom_point(data = fd.table.i, aes(y.axis, fd.table.i),
                                                  size = 4) +
  geom_errorbar(data = fd.table.i, aes(y.axis, ymin = fd.ci.lower.i, ymax = fd.ci.upper.i),
                width = 0, size = 1) + geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() + ggtitle("Tax Increase") + ylab("First Difference") + xlab("Tax Type") +
  scale_y_continuous(breaks = seq(0, 2, .25))

plot.fd.i


plot.fd.d <- ggplot() + coord_flip() + geom_point(data = fd.table.d, aes(y.axis, fd.table.d),
                                                  size = 4) +
  geom_errorbar(data = fd.table.d, aes(y.axis, ymin = fd.ci.lower.d, ymax = fd.ci.upper.d),
                width = 0, size = 1) + geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() + ggtitle("Tax Decrease") + ylab("First Difference") + xlab("") +
  scale_y_continuous(breaks = seq(0, 2, .25)) 

plot.fd.d

gplot.fd.i <- ggplot_gtable(ggplot_build(plot.fd.i))
gplot.fd.d <- ggplot_gtable(ggplot_build(plot.fd.d))

figure3 <- grid.arrange(gplot.fd.i, gplot.fd.d, ncol = 2)



### Interactive Motion Chart
############################

int.data <- data[, c(19, 20, 355:363, 391)]

mo.chart <- gvisMotionChart(int.data, idvar = "JoinState", timevar = "JoinYear")

plot(mo.chart)



#hier.logit.model <- bglmer(others_decrease ~ 
#                      govparty_c +
#                      (1 | JoinState) + (1 | JoinYear), 
#                      family = binomial, data = data)

# coef = fixef
# 1 - risk ratio gives increase or decrease
# Do for 16 models (2 per tax)

#summary(hier.logit.model)

#table(data$years_left_in_term)

#comp.data <- data[complete.cases(data[,c("tax_increase", "leg_cont_tax_super", "leg_cont_budg_super",
#                                         "per_leg_of_govs_pty", "years_left_before_limit",
#                                         "govparty_c", "hou_chamber", "sen_chamber")]),]

#forest <- cforest(tax_increase ~ leg_cont_tax_super + leg_cont_budg_super + per_leg_of_govs_pty +
#                    years_left_before_limit + govparty_c + hou_chamber + sen_chamber, data = comp.data,
#                  controls = cforest_unbiased(mtry = 4, ntree = 250))
#imp <- varimp(forest, conditional = F)
#ord <- order(imp)
#ord

####
#hier.linear.model <- lmer(totals ~ leg_cont_tax_super + leg_cont_budg_super + 
#                            per_leg_of_govs_pty +  years_left_before_limit + Lame_Duck_Last_Term + 
#                            Lame_Duck_Last_Year +  Lame_Duck_Last_2nd_To_Last_Year + govparty_c + 
#                            hou_chamber + sen_chamber + (1 | JoinState) + (1 | JoinYear), data = data)

#summary(hier.linear.model)
