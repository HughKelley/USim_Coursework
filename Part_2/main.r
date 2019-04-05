
get_functions <- source('helpers.r', echo = FALSE)

a = package_loader(1)

boundaries = "http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson"
commute_data_source = "https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1"
code_dictionary = "https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1"
pop_and_income_data = "https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1"

london_data <- data_loader(a = boundaries, commute_data_source, code_dictionary, pop_and_income_data)

rm(boundaries, commute_data_source, code_dictionary, pop_and_income_data, a)

mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(london_data$Total)

vi1_mu <- london_data$vi1_origpop^mu
wj2_alpha <- london_data$wj2_destsal^alpha
dist_beta <- london_data$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)


# calcualte estimates, round, and store in new data frame for results
results <- data.frame(actual_flows = london_data$Total, estimate_1 = round(k*vi1_mu*wj2_alpha*dist_beta,0))

est_1_r_2 <-  CalcRSquared(results$actual_flows, results$estimate_1)
est_1_RMSE <- CalcRMSE(results$actual_flows, results$estimate_1)
cat("R2: ", est_1_r_2,'\n',"RMSE: ", est_1_RMSE, sep = "")

# so that all works, now do a real, calibrated model

# estimate poisson regression for parameters

unconstrained_model <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)

results$estimate_2 <- round(fitted(unconstrained_model))

# check results 
# sum(results$estimate_2)
# sum(results$actual_flows)
# slightly off because of rounding

est_2_r_2 <- CalcRSquared(results$actual_flows, results$estimate_2)
est_2_RMSE <- CalcRMSE(results$actual_flows, results$estimate_2)
cat("R2: ", est_2_r_2,'\n',"RMSE: ", est_2_RMSE, sep = "")

#################
# Production Constrained Model


# passing "Orig" as a predictor variable
# glm() interprets that as a dummy variable, which gets a vector of parameter estimates
#wj2_destsal is a numerical vector of salaries which gets a single parameter value
production_constrained_model <- glm(Total ~ Orig+log(wj2_destsal)+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)

# adjust contrast parameters for making dummy variable if need be
# options(contrasts=c('contr.SAS','contr.SAS'))
# default is 
# options(contrasts=c('contr.treatment','contr.treatment'))

# add flow totals for each borough

london_data <- O_and_D_flow_totals(london_data)


# get coefficients, first one is k then mu is for each borough n -1
# and the borough without a coefficient is the reference borough and should be 1

coefs <- as.data.frame(production_constrained_model$coefficients)
coefs <- dplyr::rename(coefs, param_ests = 1)
london_data$mu_i <- ifelse(is.na(london_data$mu_i),0,london_data$mu_i)

k <- production_constrained_model$coefficients[1]
mu_i <- production_constrained_model$coefficients[2:33]
alpha <- production_constrained_model$coefficients[34]
beta <- production_constrained_model$coefficients[35]

# add to London data




# in the whatif scenarios, the balancing parameters have to be changed to keep total flows constant






