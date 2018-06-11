import sys
import rank_lib as rank

if __name__ == "__main__":
	with open("input.txt") as f:
		strArr = f.read().split("\n")
		input = [i.split(" ") for i in strArr]
		input = [[float(j) for j in i] for i in input[:len(input) - 1]]
		rank.init(input, len(input))
