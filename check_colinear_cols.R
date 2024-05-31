check_colinear_cols <- function(model,data){
  # Get formula, fixed.only = T for mixed models (otherwise get errors)
  # Can add other exceptions for more general models if needed
  formula <- formula(model, fixed.only = TRUE)
  
  # Create a model matrix
  # This is what R does under the hood in for functions like lm
  model_matrix <- model.matrix(as.formula(formula),data=data)
  #QR decomposition, used for getting the rank (# of linearly indep cols)
  
  qr_decomp <- qr(model_matrix)
  rank <- qr_decomp$rank
  # Get the location of columns that exceed the rank
  # The workhorse here is again the model.matrix function that throws
  # linearly dependent columns to the end
  # We essentially want to get those that are thrown to the end
  linear_dependent <- colnames(model_matrix)[rank + 1:ncol(model_matrix)]
  
  #Final step: get the non NA colnames (the ones at the end that exceed the rank
  # Hence are linearly dependent)
  # The !is.na is used to get the names that are not NA
  dependents <-linear_dependent[!is.na(linear_dependent)]
  
  #Returns: If the dependents is empty, no linearly dependent
  if (length(dependents)==0){
    return("No Linearly Dependent")
  #if nonzero length, return names of the linearly dependent
  }else{
    return(dependents)
  }
