## Creating a new dataframe with the 100,000 individuals
set.seed(4006)


libraries <- c("tidyverse", "mc2d")

install.packages(libraries)

for (lib in libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

#--------------------------------------
# Number of iterations and data frame
#--------------------------------------
simulation_results <- data.frame(iteration = numeric(), 
                                 proportion_test = numeric())

# Number of iterations
num_iterations = 10000

# Progress bar
pb <- winProgressBar(title = "Simulation progress",
                     label = "Percentage complete",
                     min = 0, max = num_iterations,
                     initial = 0,
                     width = 300L)

init <- numeric(num_iterations)
end <- numeric(num_iterations)


#-------------------------
# Initialising data frame
#-------------------------

for (iter in 1:num_iterations) {
  
  pctg <- paste(round(iter/num_iterations *100, 0), "% completed")
  setWinProgressBar(pb, iter, label = pctg) # The label will override the label set on the
  
  
  
  # Patient numbers
  patient_numbers <- 1:10000
  
  # Data frame creation
  patients_df <- data.frame(patients = patient_numbers)
  
  head(patients_df)
  
  # Assignment of sex column to the population
  
  # Define the desired proportions
  prop_male <- runif(1, 0.693, 0.74151)
  prop_female <- 1 - prop_male
  
  # Calculation of the number of proportions
  num_observations <- nrow(patients_df)
  
  # Calculate the number of males and females
  num_male <- round(num_observations * prop_male)
  num_female <- num_observations - num_male
  
  # Generate the sex column
  sex <- c(rep("Male", num_male), rep("Female", num_female))
  patients_df$sex <- sample(sex, size = num_observations)
  
  # Verify the proportions of males and females
  prop.table(table(patients_df$sex))
  
  # Define the proportion range for assigning MSM status to males
  prop_msm_range <- c(0.5, 0.7)
  
  # Identify the indices of males
  male_indices <- which(patients_df$sex == "Male")
  
  # Calculate the number of males to assign MSM status
  num_msm <-
    round(length(male_indices) * runif(1, prop_msm_range[1], prop_msm_range[2]))
  
  # Generate the indices for males assigned MSM status
  msm_indices <- sample(male_indices, size = num_msm)
  
  # Assign MSM or Hetero status to males in patients_df
  patients_df$status <- ifelse(patients_df$sex == "Male",
                               ifelse(row.names(patients_df) %in% msm_indices, "MSM", "Hetero"),
                               NA)
  
  # Verify the proportions of MSM and Hetero
  prop.table(table(patients_df$status))
  
  #Assign Females Heterosexual status
  patients_df$status[patients_df$sex == "Female"] <- "Hetero"
  
  # Define the proportion range for Indigenous population
  prop_indigenous_range <- c(0.12, 0.30)
  
  # Calculate the number of Indigenous individuals
  num_indigenous <-
    round(nrow(patients_df) * runif(1, prop_indigenous_range[1], prop_indigenous_range[2]))
  
  # Generate the indigenous column
  patients_df$indigenous <-
    ifelse(seq_len(nrow(patients_df)) <= num_indigenous,
           "Indigenous",
           "Non-Indigenous")
  
  # Calculate the number of Indigenous individuals who are MSM
  num_msm_indigenous <- round(num_indigenous * runif(1, 0.01, 0.05))
  
  # Find the indices of Indigenous individuals
  indigenous_indices <- which(patients_df$indigenous == "Indigenous")
  
  # Randomly select the indices for MSM Indigenous individuals
  msm_indigenous_indices <-
    sample(indigenous_indices, size = num_msm_indigenous)
  
  # Assign MSM status to the selected Indigenous individuals
  patients_df$status[msm_indigenous_indices] <- "MSM"
  
  # Verify the proportions of Indigenous and MSM
  prop.table(table(patients_df$indigenous))
  prop.table(table(patients_df$status))
  prop.table(table(patients_df$status[(patients_df$indigenous == "Indigenous") &
                                        (patients_df$sex == "Male")]))
  
  percentage_male_indigenous_msm <-
    with(
      patients_df,
      sum(sex == "Male" &
            indigenous == "Indigenous" &
            status == "MSM") / nrow(patients_df) * 100
    )
  
  ######################
  # Diagnoses category #
  ######################
  
  # Generate the diagnoses column
  patients_df$diagnoses <- NA
  
  ## MSM ##
  prop_1_range <- c(0.583, 0.683)
  prop_2_to_4_range <- c(0.281, 0.381)
  prop_5_to_9_range <- c(0.031, 0.041)
  prop_10_range <- c(0.000, 0.0006)
  
  # Calculate the number of individuals in each diagnosis category
  num_1 <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "MSM") * runif(1, prop_1_range[1], prop_1_range[2])
    )
  num_2_to_4 <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "MSM") * runif(1, prop_2_to_4_range[1], prop_2_to_4_range[2])
    )
  num_5_to_9 <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "MSM") * runif(1, prop_5_to_9_range[1], prop_5_to_9_range[2])
    )
  num_10 <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "MSM") * runif(1, prop_10_range[1], prop_10_range[2])
    )
  
  
  # Assign values to the diagnoses column based on the proportion ranges
  patients_df$diagnoses[patients_df$sex == "Male" &
                          patients_df$status == "MSM"] <- 0
  
  indices_1 <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "MSM" &
        seq_along(patients_df$diagnoses) <= num_1
    )
  patients_df$diagnoses[indices_1] <- 1
  
  indices_2_to_4 <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "MSM" &
        seq_along(patients_df$diagnoses) > num_1 &
        seq_along(patients_df$diagnoses) <= (num_1 + num_2_to_4)
    )
  patients_df$diagnoses[indices_2_to_4] <-
    sample(2:4, size = length(indices_2_to_4), replace = TRUE)
  
  indices_5_to_9 <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "MSM" &
        seq_along(patients_df$diagnoses) > (num_1 + num_2_to_4) &
        seq_along(patients_df$diagnoses) <= (num_1 + num_2_to_4 + num_5_to_9)
    )
  patients_df$diagnoses[indices_5_to_9] <-
    sample(5:9, size = length(indices_5_to_9), replace = TRUE)
  
  indices_10 <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "MSM" &
        seq_along(patients_df$diagnoses) > (num_1 + num_2_to_4 + num_5_to_9) &
        seq_along(patients_df$diagnoses) <= (num_1 + num_2_to_4 + num_5_to_9 + num_10)
    )
  patients_df$diagnoses[indices_10] <- 10
  
  # Verify the proportions of each diagnosis category
  prop.table(table(patients_df$diagnoses))
  
  ## Hetero Male ##
  
  # Define the probabilities for each diagnosis category
  prop_1_range_hetero <- c(0.86, 0.96)
  prop_2_to_4_range_hetero <- c(0.04, 0.14)
  prop_5_to_9_range_hetero <- c(0.001, 0.006)
  prop_10_range_hetero <- c(0.0001, 0.0006)
  
  
  # Calculate the number of individuals in each diagnosis category for Hetero males
  num_1_hetero <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "Hetero") * runif(1, prop_1_range_hetero[1], prop_1_range_hetero[2])
    )
  num_2_to_4_hetero <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "Hetero") * runif(1, prop_2_to_4_range_hetero[1], prop_2_to_4_range_hetero[2])
    )
  num_5_to_9_hetero <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "Hetero") * runif(1, prop_5_to_9_range_hetero[1], prop_5_to_9_range_hetero[2])
    )
  num_10_hetero <-
    round(
      sum(patients_df$sex == "Male" &
            patients_df$status == "Hetero") * runif(1, prop_10_range_hetero[1], prop_10_range_hetero[2])
    )
  
  # Assign values to the diagnoses column for Hetero males
  indices_1_hetero <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "Hetero" &
        seq_along(patients_df$diagnoses) <= num_1_hetero
    )
  patients_df$diagnoses[indices_1_hetero] <- 1
  
  indices_2_to_4_hetero <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "Hetero" &
        seq_along(patients_df$diagnoses) > num_1_hetero &
        seq_along(patients_df$diagnoses) <= (num_1_hetero + num_2_to_4_hetero)
    )
  patients_df$diagnoses[indices_2_to_4_hetero] <-
    sample(2:4,
           size = length(indices_2_to_4_hetero),
           replace = TRUE)
  
  indices_5_to_9_hetero <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "Hetero" &
        seq_along(patients_df$diagnoses) > (num_1_hetero + num_2_to_4_hetero) &
        seq_along(patients_df$diagnoses) <= (num_1_hetero + num_2_to_4_hetero + num_5_to_9_hetero)
    )
  patients_df$diagnoses[indices_5_to_9_hetero] <-
    sample(5:9,
           size = length(indices_5_to_9_hetero),
           replace = TRUE)
  
  indices_10_hetero <-
    which(
      patients_df$sex == "Male" &
        patients_df$status == "Hetero" &
        seq_along(patients_df$diagnoses) > (num_1_hetero + num_2_to_4_hetero + num_5_to_9_hetero) &
        seq_along(patients_df$diagnoses) <= (
          num_1_hetero + num_2_to_4_hetero + num_5_to_9_hetero + num_10_hetero
        )
    )
  patients_df$diagnoses[indices_10_hetero] <- 10
  
  # Verify the proportions of each diagnosis category for Hetero males
  prop.table(table(patients_df$diagnoses[patients_df$sex == "Male" &
                                           patients_df$status == "Hetero"]))
  
  # -----------------
  # Hetero females #
  # -----------------
  
  
  # Define the probabilities for each diagnosis category
  prop_1_range_hetero_female <- c(0.754, 0.854)
  prop_2_to_4_range_hetero_female <- c(0.137, 0.237)
  prop_5_to_9_range_hetero_female <- c(0.004, 0.014)
  prop_10_range_hetero_female <- c(0.00001, 0.00006)
  
  # Calculate the number of individuals in each diagnosis category for Hetero Females
  num_1_hetero_female <-
    round(
      sum(patients_df$sex == "Female" &
            patients_df$status == "Hetero") * runif(1, prop_1_range_hetero_female[1], prop_1_range_hetero_female[2])
    )
  num_2_to_4_hetero_female <-
    round(
      sum(patients_df$sex == "Female" &
            patients_df$status == "Hetero") * runif(
              1,
              prop_2_to_4_range_hetero_female[1],
              prop_2_to_4_range_hetero_female[2]
            )
    )
  num_5_to_9_hetero_female <-
    round(
      sum(patients_df$sex == "Female" &
            patients_df$status == "Hetero") * runif(
              1,
              prop_5_to_9_range_hetero_female[1],
              prop_5_to_9_range_hetero_female[2]
            )
    )
  num_10_hetero_female <-
    round(
      sum(patients_df$sex == "Female" &
            patients_df$status == "Hetero") * runif(
              1,
              prop_10_range_hetero_female[1],
              prop_10_range_hetero_female[2]
            )
    )
  
  # Assign values to the diagnoses column for Hetero Females
  indices_1_hetero_female <-
    sample(
      which(patients_df$sex == "Female" &
              patients_df$status == "Hetero"),
      num_1_hetero_female
    )
  patients_df$diagnoses[indices_1_hetero_female] <- 1
  
  indices_2_to_4_hetero_female <-
    sample(
      which(patients_df$sex == "Female" &
              patients_df$status == "Hetero"),
      num_2_to_4_hetero_female
    )
  patients_df$diagnoses[indices_2_to_4_hetero_female] <-
    sample(2:4,
           size = length(indices_2_to_4_hetero_female),
           replace = TRUE)
  
  indices_5_to_9_hetero_female <-
    sample(
      which(patients_df$sex == "Female" &
              patients_df$status == "Hetero"),
      num_5_to_9_hetero_female
    )
  patients_df$diagnoses[indices_5_to_9_hetero_female] <-
    sample(5:9,
           size = length(indices_5_to_9_hetero_female),
           replace = TRUE)
  
  indices_10_hetero_female <-
    sample(
      which(patients_df$sex == "Female" &
              patients_df$status == "Hetero"),
      num_10_hetero_female
    )
  patients_df$diagnoses[indices_10_hetero_female] <- 10
  
  # Verify the proportions of each diagnosis category for Hetero Females
  prop.table(table(patients_df$diagnoses[patients_df$sex == "Female" &
                                           patients_df$status == "Hetero"]))
  
  
  # Assigning missing (skewed towards 1)
  
  # Generate random numbers between 1 and 10 with skew towards 1
  random_values <- sample(c(1, 2:9, 10), size = sum(is.na(patients_df$diagnoses)), 
                          replace = TRUE, 
                          prob = c(0.25, rep(0.05, 8), 0.001))
  
  # Assign random values to missing values in the diagnoses column
  patients_df$diagnoses[is.na(patients_df$diagnoses)] <- random_values
  
  # -------------------------------------
  # Age distribution of the population
  # -------------------------------------
  
  # Define the percentage ranges for each age category
  prop_less_than_19_range <- c(0.0745, 0.1885)
  prop_20_to_29_range <- c(0.39, 0.44)
  prop_greater_than_30_range <- c(0.38, 0.46)
  
  # Calculate the number of individuals in each age category
  num_less_than_19 <-
    round(nrow(patients_df) * runif(1, prop_less_than_19_range[1], prop_less_than_19_range[2]))
  num_20_to_29 <-
    round(nrow(patients_df) * runif(1, prop_20_to_29_range[1], prop_20_to_29_range[2]))
  num_greater_than_30 <-
    round(
      nrow(patients_df) * runif(1, prop_greater_than_30_range[1], prop_greater_than_30_range[2])
    )
  
  # Create the age_classification column
  patients_df$age_classification <- ""
  
  # Assign values to the age_classification column
  patients_df$age_classification[sample(nrow(patients_df), num_less_than_19)] <-
    "<19"
  patients_df$age_classification[sample(nrow(patients_df), num_20_to_29)] <-
    "20-29"
  patients_df$age_classification[sample(nrow(patients_df), num_greater_than_30)] <-
    ">30"
  
  # Check for missing age classifications
  missing_indices <- which(patients_df$age_classification == "")
  
  # Assign missing age classifications iteratively until all are filled
  while (length(missing_indices) > 0) {
    missing_count <- length(missing_indices)
    
    # Calculate the number of individuals needed in each age category
    num_needed_less_than_19 <-
      round(missing_count * prop_less_than_19_range[2])
    num_needed_20_to_29 <-
      round(missing_count * prop_20_to_29_range[2])
    num_needed_greater_than_30 <-
      missing_count - num_needed_less_than_19 - num_needed_20_to_29
    
    # Sample missing indices for each age category
    less_than_19_indices <- sample(missing_indices, num_needed_less_than_19)
    missing_indices <- setdiff(missing_indices, less_than_19_indices)
    patients_df$age_classification[less_than_19_indices] <- "<19"
    
    twenty_to_29_indices <- sample(missing_indices, num_needed_20_to_29)
    missing_indices <- setdiff(missing_indices, twenty_to_29_indices)
    patients_df$age_classification[twenty_to_29_indices] <- "20-29"
    
    greater_than_30_indices <- sample(missing_indices, num_needed_greater_than_30)
    missing_indices <- setdiff(missing_indices, greater_than_30_indices)
    patients_df$age_classification[greater_than_30_indices] <- ">30"
  }
  
  # Verify the proportions of each age category
  prop.table(table(patients_df$age_classification))
  

  ######################
  ## concurrent STI ####    
  ######################
  
  # Calculate the number of rows to assign 'yes' for concurrent STI
  num_yes <- round(runif(1, 0.23, 0.33) * nrow(patients_df))
  
  # Assign 'yes' randomly to a subset of the dataframe
  patients_df$concurrent_STI <- ifelse(seq_len(nrow(patients_df)) %in% sample(seq_len(nrow(patients_df)), num_yes), 'yes', 'no')
  
  # Checking the proportions
  prop.table(table(patients_df$concurrent_STI))
  
  
  # ----------------------------
  # Number of sexual partners 
  # ----------------------------
  
  # Calculate the number of rows for each category of sexual partners
  
  # Uncertainty plus minus 5% for each proportion
  
  # Define the proportions with uncertainty
  proportions <- c(0.387, 0.225, 0.021)
  uncertainty <- 0.05
  
  # Calculate the adjusted proportions with uncertainty
  adjusted_proportions <- proportions * (1 + runif(length(proportions), -uncertainty, uncertainty))
  
  # Normalize the adjusted proportions to sum up to 1
  adjusted_proportions <- adjusted_proportions / sum(adjusted_proportions)
  
  # Generate the categories of sexual partners
  categories <- c("0-1 partners", "2-5 partners", "6+ partners")
  
  # Assign sexual partners to females based on adjusted proportions
  female_rows <- patients_df$sex == "Female"
  num_females <- sum(female_rows)
  
  # Calculate the number of females for each category
  num_categories <- length(categories)
  num_per_category <- round(num_females * adjusted_proportions)
  
  # Initialize the column with NA values for females
  patients_df$sexual_partners[female_rows] <- NA
  
  # Assign sexual partners based on the calculated numbers
  start_idx <- 1
  for (i in 1:num_categories) {
    end_idx <- start_idx + num_per_category[i] - 1
    patients_df$sexual_partners[female_rows][start_idx:end_idx] <- categories[i]
    start_idx <- end_idx + 1
  }
  
  
  #--------------------
  # Assigning males
  # -------------------
  
  
  # Define the proportions with uncertainty for males
  male_proportions <- c(0.282, 0.37, 0.041)
  male_uncertainty <- 0.05
  
  # Calculate the adjusted proportions with uncertainty for males
  adjusted_male_proportions <- male_proportions * (1 + runif(length(male_proportions), 
                                                             -male_uncertainty, male_uncertainty))
  
  # Normalize the adjusted proportions for males to sum up to 1
  adjusted_male_proportions <- adjusted_male_proportions / sum(adjusted_male_proportions)
  
  # Generate the categories of sexual partners
  categories <- c("0-1 partners", "2-5 partners", "6+ partners")
  
  
  # Assign sexual partners to males based on adjusted proportions
  male_rows <- patients_df$sex == "Male"
  num_males <- sum(male_rows)
  num_male_categories <- length(male_proportions)
  num_male_per_category <- round(num_males * adjusted_male_proportions)
  
  # Initialize the column with NA values for males
  patients_df$sexual_partners[male_rows] <- NA
  
  # Assign sexual partners to males based on the calculated numbers
  start_idx <- 1
  for (i in 1:num_male_categories) {
    end_idx <- start_idx + num_male_per_category[i] - 1
    patients_df$sexual_partners[male_rows][start_idx:end_idx] <- categories[i]
    start_idx <- end_idx + 1
  }
  
  # Checking proportions
  prop.table(table(patients_df$sexual_partners[patients_df$sex == "Male"]))
  
  
  #-----------------------------
  # Support tools [information]
  #-----------------------------
  
  # Calculate the number of rows for support tool "present"
  num_present <- round(runif(1, 0.10* num_rows, 
                             0.26 * num_rows))
  
  # Generate the support_tool column
  patients_df$support_tool <- NA
  
  # Assign values to support_tool based on the proportions
  patients_df$support_tool[seq(num_present)] <- "present"
  patients_df$support_tool[(num_present + 1):num_rows] <- "absent"
  
  # Checking proportions
  prop.table(table(patients_df$support_tool))
  
  # Checking for missing
  sum(is.na(patients_df$support_tool))
  
  #---------------------
  # HIV Status
  #---------------------
  
  # Assigning 4-8% of MSM and male HIV status
  
  male_msm_rows <- patients_df$sex == "Male" & patients_df$status == "MSM"
  num_assign_hiv <- round(sum(male_msm_rows) * runif(1, 0.04, 0.08))
  
  # Generate indices of individuals to assign HIV status
  hiv_indices <- sample(which(male_msm_rows), num_assign_hiv)
  
  # Create the "HIV" column in the dataframe and assign values
  patients_df$HIV <- "No"
  patients_df$HIV[hiv_indices] <- "Yes"
  
  #----------------------------
  # Calculation of patient risk
  #----------------------------
  
  # Risk magnitude calculation
  
  # Calculation of standard deviations for random distribution calculations
  # Used to pick numbers from a distribution 
  
  #----------------------------
  # Clinican Factors
  #----------------------------
  
  # Assigning agents [Default = 0.70/0.25/0.05]
  patients_df$agent <- sample(c("A", "B", "C"), nrow(patients_df), 
                              replace = TRUE, prob = c(0.70, 0.25, 0.05))
  

  # Check the proportions of the 'agent' column
  agent_proportions <- table(patients_df$agent) / nrow(patients_df)
  
  
  # Agent adherence value (prop. of how many would be missed)
  ad_A <- runif(n = 1, min = 0.40, max = 0.60)
  ad_B <- runif(n = 1, min = 0.80, max = 1.00)
  ad_C <- runif(n = 1, min = 0.01, max = 0.10)
  
  # Clinical/electronic decision support tools
  
  # Function to calculate 'eclintool'
  calculate_eclintool <- function(support_tool) {
    if (support_tool == "present") {
      return(rpert(1, min = 1.1, mean = 2, max = 3.8))
    } else if (support_tool == "absent") {
      return(0)
    } else {
      return(NA)  # Handle other cases if needed
    }
  }
  
  # Apply the function to create the 'eclintool' column
  patients_df$eclintool <- sapply(patients_df$support_tool, 
                                  calculate_eclintool)
  
  # Education/experience of clinician (estimation)
  patients_df$clinknow <- rpert(nrow(patients_df),
                                min = 0.5, 
                                mean = 1.0,
                                max = 1.5)
  
  # Modify clinknow values if less than 1
  patients_df$clinknow <- ifelse(patients_df$clinknow < 1, 
                                 -1*(1-patients_df$clinknow), 
                                 patients_df$clinknow)
  
  # Previous diagnoses
  patients_df <- patients_df %>%
    mutate(
      individual_risk = case_when(
        status == "MSM" & diagnoses %in% 0:1 ~ runif(n(), 0.01, 0.10),
        status == "MSM" & diagnoses %in% 2:4 ~ rpert(n(), min = 1.07, mean = 1.59, max = 2.37),
        status == "MSM" & diagnoses %in% 5:9 ~ rpert(n(), min = 0.71, mean = 1.85, max = 4.82),
        status == "MSM" & diagnoses >= 10 ~ rpert(n(), min = 0.636, mean = 2.32, max = 6.55),
        status == "Hetero" & diagnoses %in% 0:1 ~ runif(n(), 0.01, 0.025),
        status == "Hetero" & diagnoses %in% 2:4 ~ rpert(n(), min = 0.37, mean = 0.94, max = 2.37),
        status == "Hetero" & diagnoses %in% 5:9 ~ runif(n(),0.01, 0.10),
        status == "Hetero" & diagnoses >=10 ~ rpert(n(), min = 0.636, mean = 2.32, max = 6.55),
      )
    )
  
  #------------------------
  # Calculation of values
  #------------------------
  phi_individual <- function(theta_i) {
    return(1 / (1 + exp(-theta_i)))
  }
  
  phi_agent <- function(tau_i, ad_value) {
    return(1 / (1 + exp(-(ad_value*(tau_i)))))
  }
  
  
  # Creating adherence values
  patients_df <- patients_df %>%
    mutate(
      adherence_value = case_when(agent == "A" ~ ad_A,
                                  agent == "B" ~ ad_B, 
                                  agent == "C" ~ ad_C)
    )
  
  # Patient theta value
  theta_i = patients_df$individual_risk
  
  # Clinician tau value 
  # If modifying add patients_df$eclintool
  tau_i = patients_df$clinknow
  
  # Creating phi values for both agents and individual (patients)
  patients_df$phipatients = phi_individual(theta_i = theta_i) # patients
  
  # If missing then no risk
  patients_df$phipatients <- ifelse(
    is.na(patients_df$phipatients),
    runif(nrow(patients_df), min = 0, max = 0.05),
    patients_df$phipatients
  )
  
  # For agents
  patients_df$phitau = phi_agent(tau_i = tau_i,
                                 ad_value = patients_df$adherence_value)
  
  #------------------------------
  # Calculation of final values
  #------------------------------
  
  # Calculation of probability
  patients_df$probability = (patients_df$phipatients * patients_df$phitau)
  
  # Assigning 0 value if negative (cannot have negative probability)
  patients_df$probability <-
    ifelse(patients_df$probability < 0, 0, patients_df$probability)
  
  # Function to determine test status
  head(patients_df$probability)
  
  # Binomial assignment function
  binomial_assignment <- function(probabilities) {
    n_trials <- 1 # Number of trials for the binomial distribution
    results <- ifelse(runif(length(probabilities)) < probabilities, "test", "no test")
    return(results)
  }
  
  # Apply the function to create the 'columntest' column
  patients_df$columntest <- binomial_assignment(patients_df$probability)
  
  # Test status calculated
  patients_df$teststatus <- ifelse(patients_df$columntest == "test", "test", "no test")
  
  # Viewing and calculating the proportions
  proportion_table <- prop.table(table(patients_df$teststatus))
  
  # Print proportion table
  print(proportion_table)
  
  
  # adherence_value is already assigned based on clinician type
  patients_df$policy_test <- sapply(patients_df$adherence_value, function(adherence) {
    # Probabilistically assign "test" based on adherence value
    ifelse(runif(1) <= adherence, "test", "no test")
  })
  
  # After assigning policy_test outcomes
  proportion_policy_test_table <- prop.table(table(patients_df$policy_test))
  
  # Record the results in the simulation_results dataframe
  simulation_results <-
    rbind(
      simulation_results,
      data.frame(iteration = iter, proportion_test = proportion_table[[2]],
                 policy_test = proportion_policy_test_table[[2]])
    )
  
}

# Closing the progress bar
close(pb)

# View the simulation_results dataframe
head(simulation_results)


simulation_results %>% 
  ggplot(aes(x = proportion_test)) +
  geom_density(fill = "lightblue") +
  theme_Publication()


# Data frame saving results
write.csv(simulation_results, file = "scenario_1.csv",
          row.names = FALSE)


