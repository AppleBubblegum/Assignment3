best <- function(state, outcome){
    possible.outcomes <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcome.col <- possible.outcomes[[outcome]]
    
    if (is.null(outcome.col))
        stop("invalid outcome")
    
    hospital.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
    hospital.df[, outcome.col] <- suppressWarnings(sapply(hospital.df[, outcome.col], as.numeric))
    
    if (!state %in% unique(hospital.df[,7]))
        stop("invalid state")
    
    #Find best outcome
    outcome.min <- min(hospital.df[hospital.df$State == state, outcome.col], na.rm = T)
    
    #Get list of names of best hospitals in state for outcome
    best.list <- hospital.df[hospital.df$State == state &
                                 (hospital.df[, outcome.col] == outcome.min) &
                                 !is.na(hospital.df[, outcome.col]), 2]
    
    sort(best.list)[1]
}