kf_model <- function(HR, CTstart) {
    # Inputs:
    #   HR = A vector of minute to minute HR values.
    #   CTstart = Core Body Temperature at time 0.
    # Outputs:
    #   CT = A vector of minute to minute CT estimates
    #
    # Reference:
    #   Buller, Mark J., et al. "Estimation of human core temperature from 
    #     sequential heart rate observations." Physiological measurement 34.7 
    #     (2013): 781.

    # Extended Kalman Filter Parameters
    a <- 1
    gamma <- 0.022 ^ 2
    b_0 <- -7887.1
    b_1 <- 384.4286
    b_2 <- -4.5714
    sigma <- 18.88 ^ 2
    
    # Initialize Kalman filter
    CT <- as.numeric(NULL)
    x <- CTstart
    v <- 0                         # v = 0 assumes confidence with start value.
    
    # Iterate through HR time sequence
    for (t in 1:length(HR)) {
        # Time Update Phase
        x_pred <- a * x                                                   # Eq3
        v_pred <- (a ^ 2) * v + gamma                                     # Eq4
        
        # Observation Update Phase
        z <- HR[t]
        c_vc <- 2 * b_2 * x_pred + b_1                                    # Eq5
        k <- (v_pred * c_vc) / ((c_vc ^ 2) * v_pred + sigma)              # Eq6
        x <- x_pred + k * (z - (b_2 * (x_pred ^ 2) + b_1 * x_pred + b_0)) # Eq7
        v <- (1 - k * c_vc) * v_pred                                      # Eq8
        CT[t] <- x
    }
    
    return(CT)
}

