
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
results <- data.frame(actual_flows = london_data$Total)
results$unconstrained_estimates <- round(fitted(unconstrained_model))
results$double_constrained_estimates <- round(fitted(double_constrained_model))

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

# the reference borough doesn't get a coefficient, it's 1. 
# so replace NA's where there was no match with 1
london_data$mu_i <- ifelse(is.na(london_data$mu_i),1,london_data$mu_i)
london_data$alpha_j <- ifelse(is.na(london_data$alpha_j),1,london_data$alpha_j)


# now use the function for calculating balancing factors

# create origin and destination flow totals for each borough

balance_data <- flow_totals(london_data)
balance_data$beta <- distance_parameter * (-1)
balance_data <- dplyr::select(balance_data, Orig, Dest, D_j, O_i, distances, beta)

# create scenario dataframe
scenario_data <- balance_data
outer_borough_codes <- c('E09000017','E09000015','E09000003','E09000010','E09000031','E09000026','E09000016','E09000004','E09000006','E09000008','E09000029','E09000021','E09000027','E09000018')
scenario_data$hypo_distances <- ifelse((scenario_data$DestCodeNew %in% outer_borough_codes) & (scenario_data$OrigCodeNew %in% outer_borough_codes), 2080, scenario_data$distances)


# now use the function for calculating balancing factors

# check an estimate by hand

balance_data <- balancing_factor_calc(balance_data)

balance_data$estimates <- balance_data$O_i*balance_data$Ai*balance_data$D_j*balance_data$Bj*exp(log(balance_data$distances)*balance_data$beta)
balance_data$estimates <- round(balance_data$estimates, 0)
flow_matrix <- dcast(balance_data, Orig ~ Dest, sum, value.var = "estimates", margins = c("Orig", "Dest"))
total_matrix <- dcast(balance_data, Orig ~ Dest, sum, value.var = "Total", margins = c("Orig", "Dest"))

# with the regular distance decay function, estimates come out to a LOT of zeros
# try the other one maybe it'll come out more reasonably. 


# a chunk of the above should be a function for org purposes


# print results
cat("total constrained model", '\n', "R2: ", total_r_2,'\n',"RMSE: ", total_rmse, sep = "", '\n')
cat("double constrained model", '\n', "R2: ", double_r_2,'\n',"RMSE: ", double_rmse, sep = "", '\n')

# now create a new data for scenario estimates 
# outer_boroughs <- c(Hilingdon, Harrow, Barnet, Enfield, Waltham forest, Redbridge, Havering, Bexley, Bromley, Croydon, Sutton, Kingston, Richmond, Hounslow)
# this basically has to be done manually
outer_borough_codes <- c('E09000017','E09000015','E09000003','E09000010','E09000031','E09000026','E09000016','E09000004','E09000006','E09000008','E09000029','E09000021','E09000027','E09000018')


# get from london data borough codes and names for match
# data: total flow for check, distances, mu parameters, alpha parameters
scenario_data <- dplyr::select(london_data, Total, OrigCodeNew, DestCodeNew, Orig, Dest, distances, mu_i, alpha_j)

# set distances between outer boroughs to min distance. 
scenario_data$hypo_distances <- ifelse((scenario_data$DestCodeNew %in% outer_borough_codes) & (scenario_data$OrigCodeNew %in% outer_borough_codes), 2080, scenario_data$distances)

scenario_data <- balancing_factor_calc(scenario_data)



