# Function to compute the hereditary base representation as a string
hereditary_base_string <- function(number, base) {
  if (number == 0) return("0")
  
  terms <- list()
  power <- 0
  
  while (number > 0) {
    
    remainder <- number %% base
    
    if (remainder > 0) {
      
      if (power == 0) {
        # Coefficient only
        terms = c(terms, as.character(remainder))
        
      } else if (power == 1) {
        
        # Coefficient * base
        if (remainder == 1) {
          terms = c(terms, paste0(base))
        } else {
          terms = c(terms, paste0(remainder, "*", base))
        }
        
      } else {
        
        # Coefficient * base^power
        power_rep <- hereditary_base_string(power, base)
        if (remainder == 1) {
          terms <- c(terms, paste0(base, "^(", power_rep, ")"))
        } else {
          terms <- c(terms, paste0(remainder, "*", base, "^(", power_rep, ")"))
        }
      }
      
    }
    
    number = number %/% base
    power = power + 1
  }
  
  return(paste(rev(terms), collapse = " + "))
}


hereditary_base_string(226, 2)


# Example usage
# number <- 266
# base <- 2
# result <- hereditary_base_string(number, base)
# cat("Hereditary representation of", number, "in base", base, ":\n", result, "\n")



goodstein_sequence = function(num, iter){
  
  for(b in 2:iter){
    
    if(num == 0){
      print(num)
      break
    }
    
    print(num)
    
    current = hereditary_base_string(num, b)
    
    print(current)
    
    # increase base
    num = gsub(as.character(b), as.character(b+1), current)
    
    # evaluate and subtract by 1
    num = eval(parse(text=num)) - 1
    
    b = b + 1
  }
  
}

goodstein_sequence(13, 20)


