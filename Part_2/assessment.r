
get_functions <- source('helpers.r', echo = FALSE)

a = package_loader(1)

boundaries = "http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson"
commute_data_source = "https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1"
code_dictionary = "https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1"
pop_and_income_data = "https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1"

london_data <- data_loader(a = boundaries, commute_data_source, code_dictionary, pop_and_income_data)

# add in a local file loader

# add function here to select only specific boroughs to check correctness against practical

# toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")

# london_data <- subsetter(london_data, toMatch)

rm(boundaries, commute_data_source, code_dictionary, pop_and_income_data, a)


# estimate poisson regressions for each model's parameters

unconstrained_model <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)

double_constrained_model <- glm(Total ~ Orig+Dest+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)


# fit 

# to save the estimates
results <- data.frame(actual_flows = london_data$Total)

results$unconstrained_estimates <- round(fitted(unconstrained_model))
results$double_constrained_estimates <- round(fitted(double_constrained_model))

# And save to a csv

# test goodness of fit

total_r_2 =  CalcRSquared(results$actual_flows, results$unconstrained_estimates)
total_rmse = CalcRMSE(results$actual_flows, results$unconstrained_estimates)

double_r_2 = CalcRSquared(results$actual_flows, results$double_constrained_estimates)
double_rmse = CalcRMSE(results$actual_flows, results$double_constrained_estimates)

# get parameters for each of the models. 

# unconstrained parameters
un_k <- unconstrained_model$coefficients[1]
un_mu <- unconstrained_model$coefficients[2]
un_alpha <- unconstrained_model$coefficients[3]
un_beta <- unconstrained_model$coefficients[4]


# double constrained parameters
# need to go into the dataframe 
double_parameters <- as.data.frame(double_constrained_model$coefficients)
# order is intercept, coefs for origins, coefs for dests, coef for dist
double_intercept <- double_parameters[1,1]
distance_parameter <- double_parameters[66,1]
# and take them out
double_parameters <- double_parameters[-c(1,66),,drop=F]
# results in 64 parameters 32 * 2. 
# but what about the baseline borough that doesn't get a parameter?

# now convert long to wide
names(double_parameters) <- 'coefficients'
double_parameters <- rownames_to_column(double_parameters,'row') 

double_parameters <- separate(double_parameters, row,into=c('type','id'),sep = 4)
double_parameters <- spread(double_parameters, type, coefficients)

# https://stackoverflow.com/questions/55764089/regex-solution-for-dataframe-rownames

# now match them into the london_data object

london_data$mu_i <- double_parameters$Orig[match(london_data$Orig,double_parameters$id)]
london_data$alpha_j <- double_parameters$Dest[match(london_data$Dest,double_parameters$id)]

# the reference borough doesn't get a coefficient, it's 1. 
# so replace NA's where there was no match with 1
london_data$mu_i <- ifelse(is.na(london_data$mu_i),1,london_data$mu_i)
london_data$alpha_j <- ifelse(is.na(london_data$alpha_j),1,london_data$alpha_j)



# a chunk of the above should be a function for org purposes


# print results
cat("total constrained model", '\n', "R2: ", total_r_2,'\n',"RMSE: ", total_rmse, sep = "", '\n')
cat("double constrained model", '\n', "R2: ", double_r_2,'\n',"RMSE: ", double_rmse, sep = "", '\n')

# now create a new data for scenario estimates 
# outer_boroughs <- c(Hilingdon, Harrow, Barnet, Enfield, Waltham forest, Redbridge, Havering, Bexley, Bromley, Croydon, Sutton, Kingston, Richmond, Hounslow)
# this basically has to be done manually
outer_borough_codes <- c('E09000017','E09000015','E09000003','E09000010','E09000031','E09000026','E09000016','E09000004','E09000006','E09000008','E09000029','E09000021','E09000027','E09000018')

scenario_data <- dplyr::select(london_data, OrigCodeNew, DestCodeNew, Orig, Dest, distances)

# set distances between outer boroughs to min distance. 
scenario_data$hypo_distances <- ifelse((scenario_data$DestCodeNew %in% outer_borough_codes) & (scenario_data$OrigCodeNew %in% outer_borough_codes), 2080, scenario_data$distances)

# calculate estimates for hypothetical data for both models. 

# cdatasub$prodsimest2 <- exp(k+(cdatasub$mu_i)+(alpha*log(cdatasub$wj3_destsalScenario))+(beta*log(cdatasub$dist)))
# cdatasub$prodsimest2 <- round(cdatasub$prodsimest2,0)


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

results$prod_con_est_1 <- round(fitted(production_constrained_model))

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

# The code above can be done with fitted() but that doesn't allow one to 
# change the parameters by hand as is required in the assessment



# add to London data




# in the whatif scenarios, the balancing parameters have to be changed to keep total flows constant

# Double constrained model

double_constrained_model <- glm(Total ~ Orig+Dest+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)

summary(double_constrained_model)

# Calculate goodness of fit statistics

# use either inverse power law or negative exponential function

est_2_r_2 <- CalcRSquared(results$actual_flows, results$estimate_2)
est_2_RMSE <- CalcRMSE(results$actual_flows, results$estimate_2)
cat("R2: ", est_2_r_2,'\n',"RMSE: ", est_2_RMSE, sep = "")



