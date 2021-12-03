# Advent of Code 2021
The [Advent of Code](https://adventofcode.com/) is a yearly challenge held each December to effectively give programmers, data scientists, and engineers some good-ol'-fashioned superiority complexes. Since there is nothing I love more than raw, unbridled competition, here are my entries.

## Day 1 -- [Sonar Sweep](https://adventofcode.com/2021/day/1)

You're minding your own business on a ship at sea when the overboard alarm goes off! You rush to see if you can help. Apparently, one of the Elves tripped and accidentally sent the sleigh keys flying into the ocean!

Before you know it, you're inside a submarine the Elves keep ready for situations like this. It's covered in Christmas lights (because of course it is), and it even has an experimental antenna that should be able to track the keys if you can boost its signal strength high enough; there's a little meter that indicates the antenna's signal strength by displaying 0-50 stars.

## Day 2 -- [Dive!](https://adventofcode.com/2021/day/2)

Now, you need to figure out how to pilot this thing.

It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:

forward X increases the horizontal position by X units.
down X increases the depth by X units.
up X decreases the depth by X units.

## Day 3 -- [Binary Manipulation](https://adventofcode.com/2021/day/3)

The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly, can tell you many useful things about the conditions of the submarine. The first parameter to check is the power consumption.

The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon rate (9) produces the power consumption, 198.