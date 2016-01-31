## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
setwd("/home/malsburg/Documents/Uni/Workshops/201511_MCMCglmm/MCMCglmm-intro")
# load("data/models.Rda")
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
d <- read.table("data/data.tsv", sep="\t", head=T)

d$pronoun <- as.logical(d$pronoun)

head(d)
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
summary(d)
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
x <- with(d, tapply(pronoun, list(a, b, c), mean))
dimnames(x) <- list(c("not-a", "a"), c("not-b", "b"), c("not-c", "c"))
x
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
with(d, table(a, b, c))
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Design%20of%20the%20study%20and%20data][Design\ of\ the\ study\ and\ data:1]]
library(dplyr)

x <- d %>%
    group_by(subject) %>%
    summarize(nc = length(unique(paste(a,b,c))))

table(x$nc)
## Design\ of\ the\ study\ and\ data:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
subject.means <- d %>%
    group_by(subject, c, a, b) %>%
    summarize(prop = mean(pronoun))

condition.means <- subject.means %>%
    group_by(c, a, b) %>%
    summarize(mean = mean(prop),
              se   = sd(prop)/sqrt(n()))

library(ggplot2)

ggplot(condition.means, aes(x=interaction(c, b), fill=factor(a), y=mean)) +
  geom_bar(stat="identity", pos="dodge", colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=.5, width=.2, position=position_dodge(.9)) +
  ylim(c(0,1)) +
  theme_bw(base_size=12) +
  ylab("Proportion of pronouns")
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
library(lme4)
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
m1 <- glmer(pronoun ~  (a + b + c)^3            +
                      ((a + b + c)^3 | subject) + 
                      ((a + b    )^2 | item),
            data=d, family="binomial")
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
summary(m1)
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
m2 <- glmer(pronoun ~ (a + b + c)^3 +
                      (0 + a:b:c|subject) + 
                      (0 + a:b:c|item),
            data=d, family="binomial")
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Attempt%20to%20model%20the%20data%20with%20lme4][Attempt\ to\ model\ the\ data\ with\ lme4:1]]
summary(m2)
## Attempt\ to\ model\ the\ data\ with\ lme4:1 ends here

## [[file:README.org::*Using%20MCMCglmm][Using\ MCMCglmm:1]]
library(MCMCglmm)
## Using\ MCMCglmm:1 ends here

## [[file:README.org::*Using%20MCMCglmm][Using\ MCMCglmm:1]]
set.seed(14)
prior.m3 <- list(
  R=list(V=1, n=1, fix=1),
  G=list(G1=list(V        = diag(8),
                 n        = 8,
                 alpha.mu = rep(0, 8),
                 alpha.V  = diag(8)*25^2),
         G2=list(V        = diag(4),
                 n        = 4,
                 alpha.mu = rep(0, 4),
                 alpha.V  = diag(4)*25^2)))

m3 <- MCMCglmm(pronoun ~ (a + b + c)^3,
                       ~ us(1 + (a + b + c)^3):subject +
                         us(1 + (a + b    )^2):item,
               data   = d,
               family = "categorical",
               prior  = prior.m3,
               thin   = 1,
               burnin = 3000,
               nitt   = 4000)
## Using\ MCMCglmm:1 ends here

## [[file:README.org::*Using%20MCMCglmm][Using\ MCMCglmm:1]]
summary(m3$Sol)
## Using\ MCMCglmm:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
par(mfrow=c(8,2), mar=c(2,2,1,0))
plot(m3$Sol, auto.layout=F)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
set.seed(1)
par(mfrow=c(1,2), mar=c(2,2,1,0))
x <- rnorm(1000)
plot(3001:4000, x, t="l", main="Trace of x")
plot(density(x), main="Density of x")
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow=c(ceiling(n/2),2), mar=c(3,2,3,0))
  for (i in 1:n) {
    acf(x[,i], lag.max=100, main=colnames(x)[i])
    grid()
  }
}
plot.acfs(m3$Sol)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
set.seed(1)
m4 <- MCMCglmm(pronoun ~ (a + b + c)^3,
                       ~ us(1 + (a + b + c)^3):subject +
                         us(1 + (a + b    )^2):item,
               data   = d,
               family = "categorical",
               prior  = prior.m3,
               thin   = 20,
               burnin = 3000,
               nitt   = 23000)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
trace.plots <- function(x) {
  n <- dim(x)[2]
  par(mfrow=c(ceiling(n/2),2), mar=c(0,0.5,1,0.5))
  for (i in 1:n) {
    plot(as.numeric(x[,i]), t="l", main=colnames(x)[i], xaxt="n", yaxt="n")
  }
}
trace.plots(m4$Sol)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
plot.acfs(m4$Sol)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
prior.m5 <- list(
  R=list(V=1, n=1, fix=1),
  G=list(G1=list(V        = diag(2),
                 n        = 2,
                 alpha.mu = rep(0, 2),
                 alpha.V  = diag(2)*25^2),
         G2=list(V        = diag(2),
                 n        = 2,
                 alpha.mu = rep(0, 2),
                 alpha.V  = diag(2)*25^2)))

m5 <- MCMCglmm(pronoun ~ (a + b + c)^3,
                       ~ us(1 + a : b : c):subject +
                         us(1 + a : b    ):item,
               data   = d,
               family = "categorical",
               prior  = prior.m5,
               thin   = 20,
               burnin = 3000,
               nitt   = 23000)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
trace.plots(m5$Sol)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Diagnosing%20the%20results%20using%20plots][Diagnosing\ the\ results\ using\ plots:1]]
plot.acfs(m5$Sol)
## Diagnosing\ the\ results\ using\ plots:1 ends here

## [[file:README.org::*Gelman-Rubin%20criterion][Gelman-Rubin\ criterion:1]]
library(parallel)

set.seed(1)
m6 <- mclapply(1:4, function(i) {
  MCMCglmm(pronoun ~ (a + b + c)^3,
                   ~us(1 + a : b : c):subject +
                    us(1 + a : b)      :item,
           data   = d,
           family = "categorical",
           prior  = prior.m5,
           thin   = 20,
           burnin = 3000,
           nitt   = 23000)
}, mc.cores=4)

m6 <- lapply(m6, function(m) m$Sol)
m6 <- do.call(mcmc.list, m6)
## Gelman-Rubin\ criterion:1 ends here

## [[file:README.org::*Gelman-Rubin%20criterion][Gelman-Rubin\ criterion:1]]
library(coda)

par(mfrow=c(4,2), mar=c(2,2,1,2))
gelman.plot(m6, auto.layout=F)
## Gelman-Rubin\ criterion:1 ends here

## [[file:README.org::*Gelman-Rubin%20criterion][Gelman-Rubin\ criterion:1]]
gelman.diag(m6)
## Gelman-Rubin\ criterion:1 ends here

## [[file:README.org::*Gelman-Rubin%20criterion][Gelman-Rubin\ criterion:1]]
par(mfrow=c(8,2), mar=c(2, 1, 1, 1))
plot(m6, ask=F, auto.layout=F)
## Gelman-Rubin\ criterion:1 ends here

## [[file:README.org::*Results][Results:1]]
summary(m6)
## Results:1 ends here

## [[file:README.org::*Results][Results:1]]
plot.estimates <- function(x) {
  if (class(x) != "summary.mcmc")
    x <- summary(x)
  n <- dim(x$statistics)[1]
  par(mar=c(2, 7, 4, 1))
  plot(x$statistics[,1], n:1,
       yaxt="n", ylab="",
       xlim=range(x$quantiles)*1.2,
       pch=19,
       main="Posterior means and 95% credible intervals")
  grid()
  axis(2, at=n:1, rownames(x$statistics), las=2)
  arrows(x$quantiles[,1], n:1, x$quantiles[,5], n:1, code=0)
  abline(v=0, lty=2)
}

plot.estimates(m6)
## Results:1 ends here
