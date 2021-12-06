# Day 1
using CSV
using DataFrames

data = CSV.File("C:/RScripts/Advent-of-Code-2021/Day 1/data.csv") |> DataFrame

counter = 0
for i = 2:nrow(data)
  if data[i, 1] > data[i-1, 1]
    global counter = counter + 1
  end
end
print(counter)
print("\n")

counter = 0
for i = 4:nrow(data)
  if data[i, 1] > data[i-3, 1]
    global counter = counter + 1
  end
end
print(counter)