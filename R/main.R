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


# Test of hereditary_base_string function
# hereditary_base_string(226, 2)


goodstein_sequence = function(num, iter, hb = T, digits = T){
  
  for(b in 2:(iter+1)){
    
    # if(num == 0){
    #   print(num)
    #   break
    # }
    
    print(paste("Iteration", b-1, ":", num))
    
    current = hereditary_base_string(num, b)
    
    if(hb == T){
      # print(current)
      print(paste("Hereditary base", b, ":", current))
    }
    
    # increase base
    num = gsub(as.character(b), as.character(b+1), current)
    
    # evaluate and subtract by 1
    num = eval(parse(text=num)) - 1
  
    # Print number of digits
    if(digits == T){
      # print(current)
      if(num > 1){
        print(paste("Digits", ":", floor(log10(num)) + 1))
      } else {
        print(paste("Digits", ":", 1))
      }
      
    }
    
    b = b + 1
    
    # Leave an empty line to make the output easier to read
    if(hb == T | digits == T){
      cat(sep="\n\n")
    }
    
    if(num == -1){
      break
    }
    
  }
  
}
