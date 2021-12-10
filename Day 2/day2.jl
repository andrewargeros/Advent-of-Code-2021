using DataFrames
using CSV

data = CSV.File("C:/RScripts/Advent-of-Code-2021/Day 2/data.csv") |> DataFrame

data = DataFrame(reduce(vcat, permutedims.(split.(data.d, ' '))), [:direction, :amt])

data[:, :amt] = convert.(Int, data[!, :amt])

data