
get_functions <- source('helpers.r', echo = FALSE)

a = package_loader(1)

boundaries = "http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson"
commute_data_source = "https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1"
code_dictionary = "https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1"
pop_and_income_data = "https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1"

london_data <- data_loader(a = boundaries, commute_data_source, code_dictionary, pop_and_income_data)

# to do a subset of boroughs as in the practical
# toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG")
# london_data <- subsetter(london_data, toMatch)

# clean up environment a little
rm(boundaries, commute_data_source, code_dictionary, pop_and_income_data, a)


# estimate poisson regressions for each model's parameters
unconstrained_model <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)
double_constrained_model <- glm(Total ~ Orig+Dest+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)


# fit and save the estimates
results <- dplyr::select(london_data, Orig, Dest, Total)
results$unconstrained_estimates <- round(fitted(unconstrained_model))
results$double_constrained_estimates <- round(fitted(double_constrained_model))

# save to a csv

# test goodness of fit
total_r_2 =  CalcRSquared(results$Total, results$unconstrained_estimates)
total_rmse = CalcRMSE(results$Total, results$unconstrained_estimates)
double_r_2 = CalcRSquared(results$Total, results$double_constrained_estimates)
double_rmse = CalcRMSE(results$Total, results$double_constrained_estimates)
double_adj_r_2 = 1-(1-double_r_2)*(1055/990)
# get parameters for each of the models. 

# unconstrained parameters
un_k <- unconstrained_model$coefficients[1]
un_mu <- unconstrained_model$coefficients[2]
un_alpha <- unconstrained_model$coefficients[3]
un_beta <- unconstrained_model$coefficients[4] *(-1)


# double constrained parameters
# need to go into the dataframe 
double_parameters <- as.data.frame(double_constrained_model$coefficients)
# order is intercept, coefs for origins, coefs for dests, coef for dist
double_intercept <- double_parameters[1,1]
distance_parameter <- double_parameters[66,1]
distance_parameter <- distance_parameter*(-1)
# and take out intercept and distance parameter
double_parameters <- double_parameters[-c(1,66),,drop=F]
# results in 64 parameters 32 * 2. Thats the 32 boroughs plus the city of london 
# minus the reference borough

# now convert long to wide parsiung the row names
# as per https://stackoverflow.com/questions/55764089/regex-solution-for-dataframe-rownames
names(double_parameters) <- 'coefficients'
double_parameters <- rownames_to_column(double_parameters,'row') 
double_parameters <- separate(double_parameters, row,into=c('type','id'),sep = 4)
double_parameters <- spread(double_parameters, type, coefficients)



# match them into the london_data object
london_data$mu_i <- double_parameters$Orig[match(london_data$Orig,double_parameters$id)]
london_data$alpha_j <- double_parameters$Dest[match(london_data$Dest,double_parameters$id)]

# the reference borough doesn't get a coefficient, it's 0. 
# so replace NA's where there was no match with 1
london_data$mu_i <- ifelse(is.na(london_data$mu_i),0,london_data$mu_i)
london_data$alpha_j <- ifelse(is.na(london_data$alpha_j),0,london_data$alpha_j)

# now calculate estimates by hand to be sure of calculation
results$unconstrained_estimate_calc <- exp(un_k+(un_mu*log(london_data$vi1_origpop))+(un_alpha*log(london_data$wj2_destsal))-(un_beta*log(london_data$distances)))
results$unconstrained_estimate_calc <- round(results$unconstrained_estimate_calc,0)


results$double_estimate_calc <- exp(double_intercept+(london_data$mu_i)+(london_data$alpha_j)-(distance_parameter*log(london_data$distances)))
results$double_estimate_calc <- round(results$double_estimate_calc)


#########################################################################
# now see if estimates can be reproduced with seniors algorithm

balance_data <- flow_totals(london_data)
balance_data$beta <- distance_parameter
balance_data <- dplyr::select(balance_data, Orig, Dest, OrigCodeNew, DestCodeNew, vi1_origpop, wj2_destsal, D_j, O_i, distances, beta)

balance_data <- balancing_factor_calc(balance_data)

# exp(double_intercept+(london_data$mu_i)+(london_data$alpha_j)-(distance_parameter*log(london_data$distances)))
# (cdatasub$O_i*cdatasub$Ai*cdatasub$D_j*cdatasub$Bj*exp(log(cdatasub$dist)*cdatasub$beta))
balance_data$estimate <- balance_data$O_i*balance_data$Ai*balance_data$D_j*balance_data$Bj*exp(log(balance_data$distances)*balance_data$beta)



# balance_data$estimate <- exp(double_intercept+(balance_data$O_i*balance_data$Ai)+(balance_data$D_j*balance_data$Bj)-(distance_parameter*log(balance_data$distances)))
# balance_data$fit <- round(fitted(double_constrained_model),0)

# balance_data$estimates <- balance_data$O_i*balance_data$Ai*balance_data$D_j*balance_data$Bj*exp(log(balance_data$distances)*balance_data$beta)
# balance_data$estimates <- round(balance_data$estimates, 0)
# results$Double_manual_estimates <- balance_data$estimates

# flow_matrix <- dcast(balance_data, Orig ~ Dest, sum, value.var = "estimates", margins = c("Orig", "Dest"))
# total_matrix <- dcast(balance_data, Orig ~ Dest, sum, value.var = "Total", margins = c("Orig", "Dest"))


############################################################################
# make the scenario data

scenario_data <- balance_data
outer_borough_codes <- c('E09000017','E09000015','E09000003','E09000010','E09000031','E09000026','E09000016','E09000004','E09000006','E09000008','E09000029','E09000021','E09000027','E09000018')
scenario_data$distances <- ifelse((scenario_data$DestCodeNew %in% outer_borough_codes) & (scenario_data$OrigCodeNew %in% outer_borough_codes), 2.08, scenario_data$distances)


# now use the function for calculating balancing factors

scenario_data <- balancing_factor_calc(scenario_data)

scenario_data$estimate <- scenario_data$O_i*scenario_data$Ai*scenario_data$D_j*scenario_data$Bj*exp(log(scenario_data$distances)*scenario_data$beta)
scenario_data$estimate <- round(scenario_data$estimate,0)

results$scenario_double_estimates <- scenario_data$estimate


# calculate scenario estimates for the unconstrained model. 

scenario_data$unconstrained_estimate <- exp(un_k+(un_mu*log(scenario_data$vi1_origpop))+(un_alpha*log(scenario_data$wj2_destsal))-(un_beta*log(scenario_data$distances)))
scenario_data$unconstrained_estimate <- round(scenario_data$unconstrained_estimate,0)
reduction_ratio <- sum(results$Total)/sum(scenario_data$unconstrained_estimate)
scenario_data$unconstrained_estimate <- round(scenario_data$unconstrained_estimate * reduction_ratio,0)
results$unconstrained_scenario_estimate <- scenario_data$unconstrained_estimate

# matrix form
actual_flows <- dcast(results, Orig ~ Dest, sum, value.var = "Total", margins = c("Orig", "Dest"))
unconstrained_scenario <- dcast(results, Orig ~ Dest, sum, value.var = "unconstrained_scenario_estimate", margins = c("Orig", "Dest"))
double_constrained_scenario <- dcast(results, Orig ~ Dest, sum, value.var = "scenario_double_estimates", margins = c("Orig", "Dest"))

# write to csv

write.csv(actual_flows, "actual_flows.csv")
write.csv(unconstrained_scenario, "unconstrained_scenario_flows.csv")
write.csv(double_constrained_scenario, "double_scenario_flows.csv")

