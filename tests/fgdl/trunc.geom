readone = r:
readtwo = r:
anchor = f[TTTT]
1{<readone><anchor><readtwo>}2{r:}
-> 1{trunc_to(<readone>, 2)trunc_to_left(<readtwo>, 2)}