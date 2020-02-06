# TODO: Create a function that takes a fitted model (returned from bayesPF())
#       and aggregates the coefficients at the various combinations of levels.
#       E.g. Age, gender: Young, Middle, Old, Male, Female, Y-M, Y-F, M-M, M-F,
#       O-M, O-F -> a total of 11 + 1 (no groups) estimates each for slope and
#       intercept. Math part: Let G[i] represent a group (category). Then we
#       need 1 + \Sum_{i=1}^{n} ||G_i|| + \prod_{i=1}^{n} ||G_i|| estimates

