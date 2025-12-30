# Test search_whitelist with followed_by linker validation
# Barcode must match whitelist AND be followed by CCC linker (with 1 mismatch allowed)
# Uses search_whitelist to find barcode, then validates linker follows
bc = search_whitelist(b[3], $0, 1, f[CCC], 1)

1{
    x[2]
    <bc>
    x[3]
    f[AAA]
    r:
}2{r:}
-> 1{<bc>}
