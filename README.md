Dog Bunny Puzzle Solver
======================

## Algorithm Model

A Chinese blog: [https://kainwen.com/2022/12/27/dog_bunny_puzzle/](https://kainwen.com/2022/12/27/dog_bunny_puzzle/).

## How to run?

```
make
make shell

> io:format("~p~n", [dog_bunny_puzzle:solve("src/demo1227.txt")]).
```

## Model

The following is an example of `demo1226.txt`:

```
7
bone
null
tree
house
null
carrot
well
9
0~~1;:well
0-->2
1~~2
2~~3;bone
3~~4;well
5-->3
4~~5
6-->4
5~~6;house,tree
4,1,1
0,5,5
```

- The 1st line is the number of locations. Suppose it is `N`.
- Then the following `N` lines are each location's icon, null means no icon.
- The `N+2` th line is the number of paths. Suppose it is `K`.
- The following `K` lines are each path:
   * `~~` means not-directed path
   * `-->` means directed path
   * the 1st part between `;` and `:` are icons must have animals on them to enable the path
   * the 2nd part after `:` are icons must have no animals on them to enable the path
- Final two lines are:
  * the start location of `dog,bunny1,bunny2`
  * the end location of `dog,bunny1,bunny2`  