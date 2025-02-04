#rm(list = ls())

install.packages("tidyr", INSTALL_opts = '--no-lock')
install.packages("ggplot2", INSTALL_opts = '--no-lock')
install.packages("lavaan", INSTALL_opts = '--no-lock')
install.packages("semPlot", INSTALL_opts = '--no-lock')
library(tidyr)
library(ggplot2)
library(lavaan)
library(semPlot)

# Set up folder path to save data files
filepath <- "C:/myfiles"

###############################################
# Simulate data with constrained coupling paths
###############################################

###########
# Group = 0
###########

#set random number seed
seed_0 <- 011287

#Set sample size for data matrix;
n_0 <- 260

#Define correlation matrix among intercepts and slopes;
r_0 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of intercepts and slopes;
ds_0 <- diag(c(.7, .1, .6, .2))

#Compute covariance matrix
s_0 <- ds_0%*%r_0%*%ds_0
w_0 <- nrow(s_0)

#Compute Cholesky decomp for transformation
t_0 <- chol(s_0)

#Create data vector with random number seed and IID;
set.seed(seed_0)
x_0 = matrix(rnorm(n_0*w_0,mean=0,sd=1), n_0, w_0)
means_0 <- print(c(mean(x_0[,1]), mean(x_0[,2]), mean(x_0[,3]), mean(x_0[,4])))

#Transform using covariance structure;
cov_0 <- x_0%*%t_0
print(cor(cov_0))

#Create data frame with the covariance structure
dat1_0 <- data.frame(cov_0)
names(dat1_0) <- c("m0_0", "m1_0", "y0_0", "y1_0")

# Preallocate the columns for X, M, and Y
dat2_0 <- cbind(dat1_0, matrix(0, nrow = n_0, ncol = 11))
colnames(dat2_0)[5:15] <- c("x", paste0("m", 1:5), paste0("y", 1:5))

# Initialize arrays
x <- numeric(1)
ym <- numeric(5)
mm <- numeric(5)
yl <- numeric(5)
ml <- numeric(5)
ye <- numeric(5)
me <- numeric(5)
dy <- numeric(5)
dm <- numeric(5)

#Define parameters
mu_y0 <- 7
mu_ys <- -.5
alpha_y <- 1
beta_y <- .1
sig2_ye <- .101
sigma_ye <- sqrt(sig2_ye)
mu_m0 <- 6
mu_ms <- 0.9
alpha_m <- 1
beta_m <- -.05
sig2_me <- .025
sigma_me <- sqrt(sig2_me)
#gamma_ym is coupling from M to Y (b path)
gamma_ym <- -.130
b_a <- -0.4
b_cp <- 0.1
x_mu <- 0
x_var <- 1

#Set seed for reproduction
set.seed(seed_0)

# Generate raw data
for (i in 1:n_0) {
  x <- x_mu + sqrt(x_var) * rnorm(1)
  y0 <- mu_y0 + dat2_0[i, "y0_0"]
  ys <- mu_ys + dat2_0[i, "y1_0"]
  ye[1] <- sigma_ye * rnorm(1)
  yl[1] <- y0
  ym[1] <- yl[1] + ye[1]
  
  m0 <- mu_m0 + dat2_0[i, "m0_0"]
  ms <- mu_ms + dat2_0[i, "m1_0"]
  me[1] <- sigma_me * rnorm(1)
  ml[1] <- m0
  mm[1] <- ml[1] + me[1]
  
  for (t in 2:5) {
    dy[t] <- alpha_y * ys + beta_y * yl[t-1] + gamma_ym * ml[t-1] + b_cp * x
    ye[t] <- sigma_ye * rnorm(1)
    yl[t] <- yl[t-1] + dy[t]
    ym[t] <- yl[t] + ye[t]
    
    dm[t] <- alpha_m * ms + beta_m * ml[t-1] + b_a * x
    me[t] <- sigma_me * rnorm(1)
    ml[t] <- ml[t-1] + dm[t]
    mm[t] <- ml[t] + me[t]
  }
  
  # Append generated data to NormalData
  dat2_0[i, paste0("y", 1:5)] <- ym
  dat2_0[i, paste0("m", 1:5)] <- mm
  dat2_0[i, "x"] <- x
}


#Create ID variable
dat2_0$id <- 1:n_0

#Create grouping variable
dat2_0$z <- 0

#Select subset of data for plotting
dat3_0 <- dat2_0[1:100, ]

#Convert data to long format for plotting
data_longy_0 <- gather(dat3_0, t, y, y1:y5, factor_key=TRUE)
data_longm_0 <- gather(dat3_0, t, m, m1:m5, factor_key=TRUE)

#plot M and Y trajectories
ggplot(data_longy_0, aes(x = t, y = y, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "y", title = "Trajectory for Y, group = 0")

ggplot(data_longm_0, aes(x = t, y = m, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "m", title = "Trajectory for M, group = 0")

###########
# Group = 1
###########

#set random number seed
seed_1 <- 012992

#Set sample size for data matrix;
n_1 <- 260

#Define correlation matrix among intercepts and slopes;
r_1 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of intercepts and slopes;
ds_1 <- diag(c(.7, .1, .6, .2))

#Compute covariance matrix
s_1 <- ds_1%*%r_1%*%ds_1
w_1 <- nrow(s_1)

#Compute Cholesky decomp for transformation
t_1 <- chol(s_1)

#Create data vector with random number seed and IID;
set.seed(seed_1)
x_1 = matrix(rnorm(n_1*w_1,mean=0,sd=1), n_1, w_1)
means_1 <- print(c(mean(x_1[,1]), mean(x_1[,2]), mean(x_1[,3]), mean(x_1[,4])))

#Transform using covariance structure;
cov_1 <- x_1%*%t_1
print(cor(cov_1))

#Create data frame with the covariance structure
dat1_1 <- data.frame(cov_1)
names(dat1_1) <- c("m0_0", "m1_0", "y0_0", "y1_0")

# Preallocate the columns for X, M, and Y
dat2_1 <- cbind(dat1_1, matrix(0, nrow = n_1, ncol = 11))
colnames(dat2_1)[5:15] <- c("x", paste0("m", 1:5), paste0("y", 1:5))

# Initialize arrays
x <- numeric(1)
ym <- numeric(5)
mm <- numeric(5)
yl <- numeric(5)
ml <- numeric(5)
ye <- numeric(5)
me <- numeric(5)
dy <- numeric(5)
dm <- numeric(5)

#Define parameters
#All M and Y parameters are the same, only 'a' path differs in sign (not magnitude); XM interaction
mu_y0 <- 7
mu_ys <- -.5
alpha_y <- 1
beta_y <- .1
sig2_ye <- .101
sigma_ye <- sqrt(sig2_ye)
mu_m0 <- 6
mu_ms <- 0.9
alpha_m <- 1
beta_m <- -.05
sig2_me <- .025
sigma_me <- sqrt(sig2_me)
#gamma_ym is coupling from M to Y (b path)
gamma_ym <- -.130
b_a <- 0.4
b_cp <- 0.1
x_mu <- 0
x_var <- 1

#Set seed for reproduction
set.seed(seed_1)

# Generate raw data
for (i in 1:n_1) {
  x <- x_mu + sqrt(x_var) * rnorm(1)
  y0 <- mu_y0 + dat2_1[i, "y0_0"]
  ys <- mu_ys + dat2_1[i, "y1_0"]
  ye[1] <- sigma_ye * rnorm(1)
  yl[1] <- y0
  ym[1] <- yl[1] + ye[1]
  
  m0 <- mu_m0 + dat2_1[i, "m0_0"]
  ms <- mu_ms + dat2_1[i, "m1_0"]
  me[1] <- sigma_me * rnorm(1)
  ml[1] <- m0
  mm[1] <- ml[1] + me[1]
  
  for (t in 2:5) {
    dy[t] <- alpha_y * ys + beta_y * yl[t-1] + gamma_ym * ml[t-1] + b_cp * x
    ye[t] <- sigma_ye * rnorm(1)
    yl[t] <- yl[t-1] + dy[t]
    ym[t] <- yl[t] + ye[t]
    
    dm[t] <- alpha_m * ms + beta_m * ml[t-1] + b_a * x
    me[t] <- sigma_me * rnorm(1)
    ml[t] <- ml[t-1] + dm[t]
    mm[t] <- ml[t] + me[t]
  }
  
  # Append generated data to NormalData
  dat2_1[i, paste0("y", 1:5)] <- ym
  dat2_1[i, paste0("m", 1:5)] <- mm
  dat2_1[i, "x"] <- x
}

#Create ID variable
dat2_1$id <- 1:n_1

#Create grouping variable
dat2_1$z <- 1

#Select subset of data for plotting
dat3_1 <- dat2_1[1:100, ]

#Convert data to long format for plotting
data_longy_1 <- gather(dat3_1, t, y, y1:y5, factor_key=TRUE)
data_longm_1 <- gather(dat3_1, t, m, m1:m5, factor_key=TRUE)

#plot M and Y trajectories
ggplot(data_longy_1, aes(x = t, y = y, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "y", title = "Trajectory for Y, group = 1")

ggplot(data_longm_1, aes(x = t, y = m, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "m", title = "Trajectory for M, group = 1")

#####################
# Combining data sets
#####################

#Combine data into multiple group dataset
mg <- rbind(dat2_0, dat2_1)

#Create ID variable for full dataset
mg$fullid <- 1:(n_0+n_1)

#Re-order, rename, and drop columns in mg
mg <- mg[,c(18,16,17,5:15,1:4)]
mg <- subset(mg, select = -c(m0_0,m1_0,y0_0,y1_0))

#Drop ID columns
mgwrite <- subset(mg, select = -c(fullid,id))

#Create directory & folder for constrained coupling models
constr_path <- paste0(filepath, "/Constrained Path Results")
if (!dir.exists(constr_path)) {
  dir.create(constr_path, recursive = TRUE)
}

#Save data for analysis in Mplus

#For Mplus analysis purposes, variable order in mgwrite is as follows:
#z x m1-m5 y1-y5

write.table(mgwrite, file=paste0(constr_path, "/", "CIELCS_constr.dat"), row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)

# Rename variables for lavaan analysis 
names(mgwrite) <- c("z", "x", "m2", "m3", "m4", "m5", "m6", "y2", "y3", "y4", "y5", "y6")

####################################################
# Simulate data with freely estimated coupling paths
####################################################

###########
# Group = 0
###########

#set random number seed
seed_0 <- 011223

#Set sample size for data matrix;
n_0 <- 260

#Define correlation matrix among intercepts and slopes;
r_0 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of intercepts and slopes;
ds_0 <- diag(c(.7, .1, .6, .2))

#Compute covariance matrix
s_0 <- ds_0%*%r_0%*%ds_0
w_0 <- nrow(s_0)

#Compute Cholesky decomp for transformation
t_0 <- chol(s_0)

#Create data vector with random number seed and IID;
set.seed(seed_0)
x_0 = matrix(rnorm(n_0*w_0,mean=0,sd=1), n_0, w_0)
means_0 <- print(c(mean(x_0[,1]), mean(x_0[,2]), mean(x_0[,3]), mean(x_0[,4])))

#Transform using covariance structure;
cov_0 <- x_0%*%t_0

#Create data frame with the covariance structure
dat1_0 <- data.frame(cov_0)
names(dat1_0) <- c("m0_0", "m1_0", "y0_0", "y1_0")

# Preallocate the columns for X, M, and Y
dat2_0 <- cbind(dat1_0, matrix(0, nrow = n_0, ncol = 11))
colnames(dat2_0)[5:15] <- c("x", paste0("m", 1:5), paste0("y", 1:5))

#Define parameters
#All M and Y parameters are the same, only 'a' path differs in sign (not magnitude); XM interaction
mu_y0 <- 7
mu_ys <- -.5
alpha_y <- 1
beta_y <- .1
sig2_ye <- .101
sigma_ye <- sqrt(sig2_ye)
mu_m0 <- 6
mu_ms <- 0.9
alpha_m <- 1
beta_m <- -.05
sig2_me <- .025
sigma_me <- sqrt(sig2_me)
#gamma_ym is coupling from M to Y (b path), slightly increases over time
gamma_ym <- c(-.23,-.18,-.13,-.08)
b_a <- -0.4
b_cp <- 0.1
x_mu <- 0
x_var <- 1

# Initialize arrays
x <- numeric(1)
ym <- numeric(5)
mm <- numeric(5)
yl <- numeric(5)
ml <- numeric(5)
ye <- numeric(5)
me <- numeric(5)
dy <- numeric(5)
dm <- numeric(5)

#Set seed for reproduction
set.seed(seed_0)

# Generate raw data
for (i in 1:n_0) {
  x <- x_mu + sqrt(x_var) * rnorm(1)
  y0 <- mu_y0 + dat2_0[i, "y0_0"]
  ys <- mu_ys + dat2_0[i, "y1_0"]
  ye[1] <- sigma_ye * rnorm(1)
  yl[1] <- y0
  ym[1] <- yl[1] + ye[1]
  
  m0 <- mu_m0 + dat2_0[i, "m0_0"]
  ms <- mu_ms + dat2_0[i, "m1_0"]
  me[1] <- sigma_me * rnorm(1)
  ml[1] <- m0
  mm[1] <- ml[1] + me[1]
  
  for (t in 2:5) {
    g <- t - 1
    dy[t] <- alpha_y * ys + beta_y * yl[t-1] + gamma_ym[g] * ml[t-1] + b_cp * x
    ye[t] <- sigma_ye * rnorm(1)
    yl[t] <- yl[t-1] + dy[t]
    ym[t] <- yl[t] + ye[t]
    dm[t] <- alpha_m * ms + beta_m * ml[t-1] + b_a * x
    me[t] <- sigma_me * rnorm(1)
    ml[t] <- ml[t-1] + dm[t]
    mm[t] <- ml[t] + me[t]
  }
  
  # Append generated data to NormalData
  dat2_0[i, paste0("y", 1:5)] <- ym
  dat2_0[i, paste0("m", 1:5)] <- mm
  dat2_0[i, "x"] <- x
}

#Create ID variable
dat2_0$id <- 1:n_0

#Create grouping variable
dat2_0$z <- 0

#Select subset of data for plotting
dat3_0 <- dat2_0[1:100, ]

#Convert data to long format for plotting
data_longy_0 <- gather(dat3_0, t, y, y1:y5, factor_key=TRUE)
data_longm_0 <- gather(dat3_0, t, m, m1:m5, factor_key=TRUE)

#plot M and Y trajectories
ggplot(data_longy_0, aes(x = t, y = y, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "y", title = "Trajectory for Y, group = 0")

ggplot(data_longm_0, aes(x = t, y = m, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "m", title = "Trajectory for M, group = 0")

###########
# Group = 1
###########

#set random number seed
seed_1 <- 012923

#Set sample size for data matrix;
n_1 <- 260

#Define correlation matrix among intercepts and slopes;
r_1 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of intercepts and slopes;
ds_1 <- diag(c(.7, .1, .6, .2))

#Compute covariance matrix
s_1 <- ds_1%*%r_1%*%ds_1
w_1 <- nrow(s_1)

#Compute Cholesky decomp for transformation
t_1 <- chol(s_1)

#Create data vector with random number seed and IID;
set.seed(seed_1)
x_1 = matrix(rnorm(n_1*w_1,mean=0,sd=1), n_1, w_1)
means_1 <- print(c(mean(x_1[,1]), mean(x_1[,2]), mean(x_1[,3]), mean(x_1[,4])))

#Transform using covariance structure;
cov_1 <- x_1%*%t_1

#Create data frame with the covariance structure
dat1_1 <- data.frame(cov_1)
names(dat1_1) <- c("m0_0", "m1_0", "y0_0", "y1_0")

# Preallocate the columns for X, M, and Y
dat2_1 <- cbind(dat1_1, matrix(0, nrow = n_1, ncol = 11))
colnames(dat2_1)[5:15] <- c("x", paste0("m", 1:5), paste0("y", 1:5))

#Define parameters
#All M and Y parameters are the same, only 'a' path differs in sign (not magnitude); XM interaction
mu_y0 <- 7
mu_ys <- -.5
alpha_y <- 1
beta_y <- .1
sig2_ye <- .101
sigma_ye <- sqrt(sig2_ye)
mu_m0 <- 6
mu_ms <- 0.9
alpha_m <- 1
beta_m <- -.05
sig2_me <- .025
sigma_me <- sqrt(sig2_me)
#gamma_ym is coupling from M to Y (b path), slightly increases over time
gamma_ym <- c(-.23,-.18,-.13,-.08)
b_a <- 0.4
b_cp <- 0.1
x_mu <- 0
x_var <- 1

# Initialize arrays
x <- numeric(1)
ym <- numeric(5)
mm <- numeric(5)
yl <- numeric(5)
ml <- numeric(5)
ye <- numeric(5)
me <- numeric(5)
dy <- numeric(5)
dm <- numeric(5)

#Set seed for reproduction
set.seed(seed_1)

# Generate raw data
for (i in 1:n_1) {
  x <- x_mu + sqrt(x_var) * rnorm(1)
  y0 <- mu_y0 + dat2_1[i, "y0_0"]
  ys <- mu_ys + dat2_1[i, "y1_0"]
  ye[1] <- sigma_ye * rnorm(1)
  yl[1] <- y0
  ym[1] <- yl[1] + ye[1]
  
  m0 <- mu_m0 + dat2_1[i, "m0_0"]
  ms <- mu_ms + dat2_1[i, "m1_0"]
  me[1] <- sigma_me * rnorm(1)
  ml[1] <- m0
  mm[1] <- ml[1] + me[1]
  
  for (t in 2:5) {
      g <- t - 1
      dy[t] <- alpha_y * ys + beta_y * yl[t-1] + gamma_ym[g] * ml[t-1] + b_cp * x
      ye[t] <- sigma_ye * rnorm(1)
      yl[t] <- yl[t-1] + dy[t]
      ym[t] <- yl[t] + ye[t]
      dm[t] <- alpha_m * ms + beta_m * ml[t-1] + b_a * x
      me[t] <- sigma_me * rnorm(1)
      ml[t] <- ml[t-1] + dm[t]
      mm[t] <- ml[t] + me[t]
    }
    
  
  # Append generated data to NormalData
  dat2_1[i, paste0("y", 1:5)] <- ym
  dat2_1[i, paste0("m", 1:5)] <- mm
  dat2_1[i, "x"] <- x
  }

#Create ID variable
dat2_1$id <- 1:n_1

#Create grouping variable
dat2_1$z <- 1

#Select subset of data for plotting
dat3_1 <- dat2_1[1:100, ]

#Convert data to long format for plotting
data_longy_1 <- gather(dat3_1, t, y, y1:y5, factor_key=TRUE)
data_longm_1 <- gather(dat3_1, t, m, m1:m5, factor_key=TRUE)

#plot M and Y trajectories
ggplot(data_longy_1, aes(x = t, y = y, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "y", title = "Trajectory for Y, group = 1")

ggplot(data_longm_1, aes(x = t, y = m, group = id)) +
  geom_line() +
  theme_minimal() +
  labs(x = "t", y = "m", title = "Trajectory for M, group = 1")

#####################
# Combining data sets
#####################

#Combine data into multiple group dataset
mgfree <- rbind(dat2_0, dat2_1)

#Create ID variable for full dataset
mgfree$fullid <- 1:(n_0+n_1)

#Re-order, rename, and drop columns in mg
mgfree <- subset(mgfree, select = -c(m0_0,m1_0,y0_0,y1_0,id,fullid))
mgfree <- mgfree[,c(12,1:11)]

#Create directory & folder for freely estimated coupling models
free_path <- paste0(filepath, "/Freely Estimated Results")
if (!dir.exists(free_path)) {
  dir.create(free_path, recursive = TRUE)
}

#Save data for future use
#For Mplus analysis purposes, variable order is as follows:
#z x m1-m5 y1-y5

write.table(mgfree, file=paste0(free_path, "/", "CIELCS_free.dat"), row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)

# Rename variables for lavaan analysis 
names(mgfree) <- c("z", "x", "m2", "m3", "m4", "m5", "m6", "y2", "y3", "y4", "y5", "y6")

#############################################################
# Analyze data with constrained coupling paths - Cov approach
#############################################################

# create interaction term 
mgwrite$xz <- mgwrite$x * mgwrite$z

#Model Specification - X Z M Y with interaction
cie_lcs_constr <- ' 

#Y

  #latent variable nodes (loadings = 1)
    ly2 =~ 1*y2
    ly3 =~ 1*y3
    ly4 =~ 1*y4
    ly5 =~ 1*y5
    ly6 =~ 1*y6
    
  #autoregressions (fixed = 1)
    ly3 ~ 1*ly2
    ly4 ~ 1*ly3
    ly5 ~ 1*ly4
    ly6 ~ 1*ly5
    
  #difference score on latent level (latent change scores) (fixed = 1)
    dy3 =~ 1*ly3
    dy4 =~ 1*ly4
    dy5 =~ 1*ly5
    dy6 =~ 1*ly6
    
  #proportional effects (constrained equal)
    dy3 ~ start(.1)*pcy * ly2
    dy4 ~ start(.1)*pcy * ly3
    dy5 ~ start(.1)*pcy * ly4
    dy6 ~ start(.1)*pcy * ly5

  #slope (constant change factor) (loadings = 1)
    sy =~ 1*dy3 + 
          1*dy4 + 
          1*dy5 + 
          1*dy6    
    
  #latent true score means (initial free, others = 0)  
    ly2 ~ start(7)*1
    ly3 ~ 0*1
    ly4 ~ 0*1
    ly5 ~ 0*1
    ly6 ~ 0*1

  #latent true score variances (initial free, others = 0)
    ly2 ~~ start(0)*ly2
    ly3 ~~ 0*ly3
    ly4 ~~ 0*ly4
    ly5 ~~ 0*ly5
    ly6 ~~ 0*ly6

  #observed intercepts (fixed to 0)
    y2 ~ 0*1
    y3 ~ 0*1
    y4 ~ 0*1
    y5 ~ 0*1
    y6 ~ 0*1

  #observed residual variances (constrained to equality)
    y2 ~~ sigma2_y*y2
    y3 ~~ sigma2_y*y3
    y4 ~~ sigma2_y*y4
    y5 ~~ sigma2_y*y5
    y6 ~~ sigma2_y*y6

  #latent change score means (constrained to 0)  
    dy3 ~ 0*1
    dy4 ~ 0*1
    dy5 ~ 0*1
    dy6 ~ 0*1

  #latent change score variances (constrained to 0)  
    dy3 ~~ 0*dy3
    dy4 ~~ 0*dy4
    dy5 ~~ 0*dy5
    dy6 ~~ 0*dy6

  #slope mean
    sy ~ start(-.2)*1

 #slope variance
    sy ~~ sy
   
#M

  #latent variable nodes (loadings = 1)
    lm2 =~ 1*m2
    lm3 =~ 1*m3
    lm4 =~ 1*m4
    lm5 =~ 1*m5
    lm6 =~ 1*m6
    
  #autoregressions (fixed = 1)
    lm3 ~ 1*lm2
    lm4 ~ 1*lm3
    lm5 ~ 1*lm4
    lm6 ~ 1*lm5
    
  #difference score on latent level (latent change scores) (fixed = 1)
    dm3 =~ 1*lm3
    dm4 =~ 1*lm4
    dm5 =~ 1*lm5
    dm6 =~ 1*lm6
    
  #proportional effects (constrained equal)
    dm3 ~ start(-.05)*pcm * lm2
    dm4 ~ start(-.05)*pcm * lm3
    dm5 ~ start(-.05)*pcm * lm4
    dm6 ~ start(-.05)*pcm * lm5

  #slope (constant change factor) (loadings = 1)
    sm =~ 1*dm3 + 
          1*dm4 + 
          1*dm5 + 
          1*dm6    
    
  #latent true score means (initial free, others = 0)  
    lm2 ~ start(6)*1
    lm3 ~ 0*1
    lm4 ~ 0*1
    lm5 ~ 0*1
    lm6 ~ 0*1

  #latent true score variances (initial free, others = 0)
    lm2 ~~ start(0)*lm2
    lm3 ~~ 0*lm3
    lm4 ~~ 0*lm4
    lm5 ~~ 0*lm5
    lm6 ~~ 0*lm6

  #observed intercepts (fixed to 0)
    m2 ~ 0*1
    m3 ~ 0*1
    m4 ~ 0*1
    m5 ~ 0*1
    m6 ~ 0*1

  #observed residual variances (constrained equal)
    m2 ~~ sigma2_m*m2
    m3 ~~ sigma2_m*m3
    m4 ~~ sigma2_m*m4
    m5 ~~ sigma2_m*m5
    m6 ~~ sigma2_m*m6

  #latent change score means (constrained to 0)  
    dm3 ~ 0*1
    dm4 ~ 0*1
    dm5 ~ 0*1
    dm6 ~ 0*1

  #latent change score variances (constrained to 0)  
    dm3 ~~ 0*dm3
    dm4 ~~ 0*dm4
    dm5 ~~ 0*dm5
    dm6 ~~ 0*dm6

  #slope mean
    sm ~ start(.9)*1

 #slope variance
    sm ~~ sm
    
 #X on slope Y (c prime)
    sy ~ start(.11)*cp * x
    
 #X on slope M (a_x)
    sm ~ start(.04)*a_x * x
    
 #Z on slope M (a_z)
   sm ~ start(0)*a_z * z
   
 #XZ on slope M (a_xz) interaction
   sm ~ start(0.8)*a_xz * xz 
    
 #X Z & interaction on M & Y intercept
    ly2 ~ x
    lm2 ~ x
    ly2 ~ z
    lm2 ~ z
    lm2 ~ xz
    ly2 ~ xz
    
 #x & z intercept & variance 
 #differs from Mplus - all ints, vars, covars must be included in code
 x ~ 1
 z ~ 1
 xz ~ 0
 x ~~ x
 z ~~ z
 x ~~ z
 xz ~~ xz
 x ~~ xz
 z ~~ xz
   
  #coupling from M to Y (b) (constrained equal)
    dy3 ~ start(-.05)*b * lm2
    dy4 ~ start(-.05)*b * lm3
    dy5 ~ start(-.05)*b * lm4
    dy6 ~ start(-.05)*b * lm5
    
  #correlated residuals
    y2 ~~ cor*m2
    y3 ~~ cor*m3
    y4 ~~ cor*m4
    y5 ~~ cor*m5
    y6 ~~ cor*m6
    
  #parameter covs
   sm ~~ lm2
   sy ~~ ly2
   lm2 ~~ ly2
   sm ~~ sy
   lm2 ~~ sy
   ly2 ~~ sm
   
   #Define mediated effects
   ab0 := (a_x*b)+(a_xz*b*0)
   ab1 := (a_x*b)+(a_xz*b*1)
    
' #closing quote

fit_cie_lcs <- lavaan(cie_lcs_constr,
                      data = mgwrite,
                      meanstructure = TRUE,
                      estimator = "ML",
                      missing = "fiml",
                      fixed.x = FALSE,
                      se = "boot", bootstrap = 500,
                      mimic="mplus",
                      control=list(iter.max=500),
                      verbose=FALSE)

summary(fit_cie_lcs, fit.measures=TRUE)

# Wald test of equality for ab when Z = 0 vs. 1
lavTestWald(fit_cie_lcs, constraints = "ab0 == ab1")

# See bootstrapped CIs for conditional indirect effects
parameterEstimates(fit_cie_lcs, remove.nonfree=TRUE)

# See technical output (comparable to Mplus tech1)
lavInspect(fit_cie_lcs)

# See fit indices
fitMeasures(fit_cie_lcs)

##################################################################
# Analyze data with freely estimated coupling paths - Cov approach
#################################################################

# create interaction term 
mgfree$xz <- mgfree$x * mgfree$z

#Model Specification - X Z M Y with interaction
cie_lcs_free <- ' 

#Y

  #latent variable nodes (loadings = 1)
    ly2 =~ 1*y2
    ly3 =~ 1*y3
    ly4 =~ 1*y4
    ly5 =~ 1*y5
    ly6 =~ 1*y6
    
  #autoregressions (fixed = 1)
    ly3 ~ 1*ly2
    ly4 ~ 1*ly3
    ly5 ~ 1*ly4
    ly6 ~ 1*ly5
    
  #difference score on latent level (latent change scores) (fixed = 1)
    dy3 =~ 1*ly3
    dy4 =~ 1*ly4
    dy5 =~ 1*ly5
    dy6 =~ 1*ly6
    
  #proportional effects (constrained equal)
    dy3 ~ start(.1)*pcy * ly2
    dy4 ~ start(.1)*pcy * ly3
    dy5 ~ start(.1)*pcy * ly4
    dy6 ~ start(.1)*pcy * ly5

  #slope (constant change factor) (loadings = 1)
    sy =~ 1*dy3 + 
          1*dy4 + 
          1*dy5 + 
          1*dy6    
    
  #latent true score means (initial free, others = 0)  
    ly2 ~ start(7)*1
    ly3 ~ 0*1
    ly4 ~ 0*1
    ly5 ~ 0*1
    ly6 ~ 0*1

  #latent true score variances (initial free, others = 0)
    ly2 ~~ start(0)*ly2
    ly3 ~~ 0*ly3
    ly4 ~~ 0*ly4
    ly5 ~~ 0*ly5
    ly6 ~~ 0*ly6

  #observed intercepts (fixed to 0)
    y2 ~ 0*1
    y3 ~ 0*1
    y4 ~ 0*1
    y5 ~ 0*1
    y6 ~ 0*1

  #observed residual variances (constrained to equality)
    y2 ~~ sigma2_y*y2
    y3 ~~ sigma2_y*y3
    y4 ~~ sigma2_y*y4
    y5 ~~ sigma2_y*y5
    y6 ~~ sigma2_y*y6

  #latent change score means (constrained to 0)  
    dy3 ~ 0*1
    dy4 ~ 0*1
    dy5 ~ 0*1
    dy6 ~ 0*1

  #latent change score variances (constrained to 0)  
    dy3 ~~ 0*dy3
    dy4 ~~ 0*dy4
    dy5 ~~ 0*dy5
    dy6 ~~ 0*dy6

  #slope mean
    sy ~ start(-.2)*1

 #slope variance
    sy ~~ sy
   
#M

  #latent variable nodes (loadings = 1)
    lm2 =~ 1*m2
    lm3 =~ 1*m3
    lm4 =~ 1*m4
    lm5 =~ 1*m5
    lm6 =~ 1*m6
    
  #autoregressions (fixed = 1)
    lm3 ~ 1*lm2
    lm4 ~ 1*lm3
    lm5 ~ 1*lm4
    lm6 ~ 1*lm5
    
  #difference score on latent level (latent change scores) (fixed = 1)
    dm3 =~ 1*lm3
    dm4 =~ 1*lm4
    dm5 =~ 1*lm5
    dm6 =~ 1*lm6
    
  #proportional effects (constrained equal)
    dm3 ~ start(-.05)*pcm * lm2
    dm4 ~ start(-.05)*pcm * lm3
    dm5 ~ start(-.05)*pcm * lm4
    dm6 ~ start(-.05)*pcm * lm5

  #slope (constant change factor) (loadings = 1)
    sm =~ 1*dm3 + 
          1*dm4 + 
          1*dm5 + 
          1*dm6    
    
  #latent true score means (initial free, others = 0)  
    lm2 ~ start(6)*1
    lm3 ~ 0*1
    lm4 ~ 0*1
    lm5 ~ 0*1
    lm6 ~ 0*1

  #latent true score variances (initial free, others = 0)
    lm2 ~~ start(0)*lm2
    lm3 ~~ 0*lm3
    lm4 ~~ 0*lm4
    lm5 ~~ 0*lm5
    lm6 ~~ 0*lm6

  #observed intercepts (fixed to 0)
    m2 ~ 0*1
    m3 ~ 0*1
    m4 ~ 0*1
    m5 ~ 0*1
    m6 ~ 0*1

  #observed residual variances (constrained equal)
    m2 ~~ sigma2_m*m2
    m3 ~~ sigma2_m*m3
    m4 ~~ sigma2_m*m4
    m5 ~~ sigma2_m*m5
    m6 ~~ sigma2_m*m6

  #latent change score means (constrained to 0)  
    dm3 ~ 0*1
    dm4 ~ 0*1
    dm5 ~ 0*1
    dm6 ~ 0*1

  #latent change score variances (constrained to 0)  
    dm3 ~~ 0*dm3
    dm4 ~~ 0*dm4
    dm5 ~~ 0*dm5
    dm6 ~~ 0*dm6

  #slope mean
    sm ~ start(.9)*1

 #slope variance
    sm ~~ sm
    
 #X on slope Y (c prime)
    sy ~ start(.11)*cp * x
    
 #X on slope M (a_x)
    sm ~ start(.04)*a_x * x
    
 #Z on slope M (a_z)
   sm ~ start(0)*a_z * z
   
 #XZ on slope M (a_xz) interaction
   sm ~ start(0.8)*a_xz * xz 
    
 #X Z & interaction on M & Y intercept
    ly2 ~ x
    lm2 ~ x
    ly2 ~ z
    lm2 ~ z
    lm2 ~ xz
    ly2 ~ xz
    
 #x & z intercept & variance 
 #differs from Mplus - all ints, vars, covars must be included in code
 x ~ 1
 z ~ 1
 xz ~ 0
 x ~~ x
 z ~~ z
 x ~~ z
 xz ~~ xz
 x ~~ xz
 z ~~ xz
   
  #coupling from M to Y (b) (freely estimated)
    dy3 ~ start(-.23)*b3 * lm2
    dy4 ~ start(-.18)*b4 * lm3
    dy5 ~ start(-.13)*b5 * lm4
    dy6 ~ start(-.08)*b6 * lm5
    
  #correlated residuals
    y2 ~~ cor*m2
    y3 ~~ cor*m3
    y4 ~~ cor*m4
    y5 ~~ cor*m5
    y6 ~~ cor*m6
    
  #parameter covs
   sm ~~ lm2
   sy ~~ ly2
   lm2 ~~ ly2
   sm ~~ sy
   lm2 ~~ sy
   ly2 ~~ sm
   
   #Define mediated effects
   ab0_3 := (a_x*b3)+(a_xz*b3*0)
   ab0_4 := (a_x*b4)+(a_xz*b4*0)
   ab0_5 := (a_x*b5)+(a_xz*b5*0)
   ab0_6 := (a_x*b6)+(a_xz*b6*0)
   ab1_3 := (a_x*b3)+(a_xz*b3*1)
   ab1_4 := (a_x*b4)+(a_xz*b4*1)
   ab1_5 := (a_x*b5)+(a_xz*b5*1)
   ab1_6 := (a_x*b6)+(a_xz*b6*1)
    
' #closing quote

free_cie_lcs <- lavaan(cie_lcs_free,
                      data = mgfree,
                      meanstructure = TRUE,
                      estimator = "ML",
                      missing = "fiml",
                      fixed.x = FALSE,
                      se = "boot", bootstrap = 1000,
                      mimic="mplus",
                      control=list(iter.max=500),
                      verbose=FALSE)

summary(free_cie_lcs, fit.measures=TRUE)

# Wald test of equality for ab when Z = 0 vs. 1
con = '
   ab0_3 == ab1_3
   ab0_4 == ab1_4
   ab0_5 == ab1_5
   ab0_6 == ab1_6
'
lavTestWald(free_cie_lcs, constraints = con)

# See bootstrapped CIs for conditional indirect effects
parameterEstimates(free_cie_lcs, remove.nonfree=TRUE)

# See technical output (comparable to Mplus tech1)
lavInspect(free_cie_lcs)

# See fit indices
fitMeasures(free_cie_lcs)
