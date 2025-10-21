#rm(list = ls())

install.packages("tidyr")
install.packages("ggplot2")
install.packages("MplusAutomation")
library(tidyr)
library(ggplot2)
library(MplusAutomation)

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

#Define correlation matrix among initial levels and additive components;
r_0 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of initial levels and additive components;
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

#Define correlation matrix among initial levels and additive components;
r_1 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of initial levels and additive components;
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

#Define correlation matrix among initial levels and additive components;
r_0 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of initial levels and additive components;
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

#Define correlation matrix among initial levels and additive components;
r_1 <- matrix(c(1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1, .5, .5, .5, .5, 1),4,4)

#Define vector of standard deviations of initial levels and additive components;
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
#For Mplus analysis purposes, variable order in mgwrite is as follows:
#z x m1-m5 y1-y5

write.table(mgfree, file=paste0(free_path, "/", "CIELCS_free.dat"), row.names=FALSE, col.names = FALSE, sep="\t", quote=FALSE)




