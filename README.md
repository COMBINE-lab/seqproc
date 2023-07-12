Areas which need work with `seqproc`

#todo
- [x] Parse number as `usize`
- [ ] Validate bounds and variables for functions (decide if functions are `padTO` or `padBY`)
- [x] Execute transformations, tie it to `collect_fastq`
- [ ] Execute stack 
	- [x] Propagate fixed size and ranged size to execute stack
- [x] Store stack in the rev direction, insert and remove `O(1)`
- [x] Force parse transformation if `->` is present
- [ ] Handle ANTISEQUENCE errors
- [ ] Better error checking FGDL
- [ ] Map should take a sequence or label as a fallback
- [x] Test order of functions

#todo 
ANTISEQUENCE functions to add:
- [x] normalize
- [x] pad
- [ ] trucate
- [ ] reverse
- [ ] reverse compliment
- [ ] map
