umi = u[2]
bc = search_whitelist(b[3], $0, 1)

1{
    r:
    <umi>
    <bc>
    f[AAA]
    r:
}2{r:}
-> 1{<umi><bc>}
