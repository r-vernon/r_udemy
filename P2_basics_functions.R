# function syntax
funcName <- function(var1,var2,var3=defaultVal){
  myCode
  return(result) # [optional] value to return
}

# call a function
funcName(var1,var2)         # uses default for var3
funcName(var1,var2,var3)    # explicit var3
var3 <- funcName(var1,var2) # saves output

# NOTE:
addNum <- function(a,b){a+b}
sumNum <- addNum(1,2)
# no 'return' but sumNum will still be 3
# - by default functions return value of last calculation

#-----------------------------------------------------------
# Scope (below is bad practice, just demonstrative)
# Will show:
# - txt1: fcns can access global vars
# - txt2: vars modified locally (<-) don't affect global vars
# - txt3: vars modified w/ global modifier (<<-) affect global vars
# - txt4: fcns can access global vars even when created after fcn

# remove vars/fcn if exist so no ambiguity!
rm(list = Filter(exists,c('txt1','txt2','txt3','txt4','printTxt'))) 

# create some text vars
txt1 <- 'txt1'; txt2 <- 'txt2'; txt3 <- 'txt3'

# function to print text vars
printTxt <- function(){
  txt2 <- 'txtB'  # override txt2 *locally* (<-)
  txt3 <<- 'txtC' # override txt3 *globally* (<<-)
  print(paste(txt1,txt2,txt3,txt4,sep = ', '))
}

# one more text var.
txt4 <- 'txt4'

# INITIAL STATE, prints: "txt1, txt2, txt3, txt4"
print(paste(txt1,txt2,txt3,txt4,sep = ', '))
# CALL FUNCTION, prints: "txt1, txtB, txtC, txt4"
printTxt()
# FINAL STATE,   prints: "txt1, txt2, txtC, txt4"
print(paste(txt1,txt2,txt3,txt4,sep = ', '))

