
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

attraction_constrained_model <- glm(Total~ DestCodeNew + log(vi1_origpop)+log(distances), na.action = na.exclude, family = poisson(link = "log"), data = london_data)

results <- dplyr::select(london_data, Orig, Dest, OrigCodeNew, DestCodeNew, Total)

results$fitted_estimates <- round(fitted(attraction_constrained_model),0)


# now do the same thing manually to check correctness

coefficients <- as.data.frame(attraction_constrained_model$coefficients)
names(coefficients) <- "coefficients"
intercept = attraction_constrained_model$coefficients[1]
beta = attraction_constrained_model$coefficients[35]
mu <- attraction_constrained_model$coefficients[34]
london_data$alpha_j <- coefficients$coefficients[match(london_data$OrigCodeNew,sub(".*DestCodeNew","", rownames(coefficients)))]
london_data$alpha_j <- ifelse(is.na(london_data$alpha_j),0,london_data$alpha_j)

results$calculated_estimates <- exp(intercept+(london_data$alpha_j)+(mu*log(london_data$vi1_origpop))+(beta*log(london_data$distances)))

