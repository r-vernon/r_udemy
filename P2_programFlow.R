 # logical operators syntax
# & - AND
# | - OR
# ! - NOT

# if, else if and else statements
if (cond1){
  myCode1
} else if (cond2){
  myCode2
} else{
  myCode3
}

# while loops
while (cond) {
  if (skipCond) {next} # [optional] skip current iter.
  myCode
  if (breakCond) {break} # [optional] break
}

# for loops
# vars can be vector, list, matrix etc
# - for matrix, goes rows then cols ([1,1],...,[nr,1],[1,2],...,[nr,nc])
for (curVar in vars) {
  if (skipCond) {next} # [optional] skip current iter.
  myCode
  if (breakCond) {break} # [optional] break
}