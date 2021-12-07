using DelimitedFiles
using Statistics

data = readdlm("C:/RScripts/Advent-of-Code-2021/Day 7/input.txt", ',', Int)

sum = 0
for d in data
  p = abs(d - median(data))
  sum += p
end
sum

sum = 0
for d in data
  p = abs(d - floor(mean(data)))
  p2 = p*(p-1)/2
  p = p + p2
  sum += p
end
sum