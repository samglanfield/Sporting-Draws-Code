# Sporting-Draws-Code
This repository includes the R code used to simulate both the uniform and sequential methods for both the 2026 World Cup group stage draw and the 2022/23 UEFA Champions League round of 16 draw.


All the relevant files of R code are self contained and as such to run and determine the probabilities associated with certain matchups the code can be run as presented. A seed can be set to produce reproducible results. An arbitrary seed was choose for the data files generated. 


The 2026 World Cup group stage draw features a major change in the format of the competition namely, 16 groups of 3 teams compared to the current system of 8 groups of 4 teams. This means that there is a greater number of qualification spots for each country and as such the current constraints may not be directly applicable as FIFA is still determining this. In order to perform the draw we assume that all the constraints are the same other than UEFA/EU teams can have at most 2 per group but not at least one as is the current constraint. However, as mentioned in the file for the FIFA code this can be adapted to have only 1 EU team per group. To do this make the sugested changes to the code and so the checkvalid function only allows 1 EU team. Further, introducing the sequence of commented code including eurplace in the function assignment automatically places the remaining EU teams when they are drawn to save computational time.

The Champions League code is self explanatory and if adapted to different seasons changes need to be made. The first change is update the pots of runner ups and group winners. Second, is for each team update the list of possible teams they can play and thus set, in the algorithm, the starting point of where all permutations of the draw are computed to the length of the smallest possible list less 1.

The files for the sequential method of each draw contains tests for normality and the hellinger estimator. The files for calculating the Total Variation distance require that draws are simulated and loaded before running the code.
