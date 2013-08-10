icfp2013
========

ICFP Contest 2013

http://icfpc2013.cloudapp.net/

team: THE DYNAMIC LEXICALS
peeps: awwaiid, lungching, ...


git:
* git pull --rebase
* git stash
* git stash pop
* git rec / git add / git commit
* git push

setup:
* cpanm carton
* carton install

-------------------------------------

NOTES (possible spoilers!)

for $n (1..40) { print "$n: " . `grep 'size":$n,' myproblems.json | wc -l` }
3: 20
4: 20
5: 20
6: 20
7: 20
8: 40
9: 40
10: 40
11: 60
12: 60
13: 60
14: 60
15: 60
16: 60
17: 60
18: 60
19: 60
20: 60
21: 60
22: 60
23: 60
24: 60
25: 60
26: 60
27: 60
28: 60
29: 60
30: 60


-----------------------------------

2013.08.09.19.51
We currently have 60 points by doing brute-force. Now we're working on bucketing solutions based on 256 random inputs, using the 'eval' API to get the outputs and then narrowing down our solutions to just the ones that match.

2013.08.10.12.15
At Bourbon. Researching SMT to see if that'll help ... bucketing stuff has helped and we are working on solving some of size-9 and 10 problems.

