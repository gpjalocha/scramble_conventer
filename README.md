# scramble_conventer
Conditional scrambler for 3x3BLD training

In this repository I'll share my scripts i use for 3x3BLD training. *conditional scrambler.*R allows to generate scrambles with given conditions, i.e.

* N twisted edges/corners
* Having specific 3-cycle in scramble(in first cycle)
* Having pairity/no pairity

My work reqiures packages like *permutations* that allows to represent permutations in form of cycles. 

It's totally non-user friendly. There is no UI for this and it requires to run socket server which allows to solve cube with two-phase solver

```cmd
python start_server.py 8080 20 2
```

...and in R I convert scrambled cubes from permutation form to string cube form, so it is fetching scrambles invoking:

```
http://localhost:8080/DRLUUBFBRBLURRLRUBLRDDFDLFUFUFFDBRDUBRUFLLFDDBFLUBLRBD
```

If someone is interested in developing it in better language(java maybe) i can give some tips about building features mentioned here without brute-force approach.
