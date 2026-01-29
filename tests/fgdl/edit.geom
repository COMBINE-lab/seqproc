# Test edit distance matching
# Primer: CTACACGACGCTCTTCCGATCT (22bp)
# edit(f[...], 2) allows 2 edits (insertions, deletions, or substitutions)

primer = anchor_relative(edit(f[CTACACGACGCTCTTCCGATCT], 2))
bc = b[16]

1{<primer><bc>}
-> 1{<bc>}
