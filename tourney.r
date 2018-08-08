## (*) The game tree in R
##
## > print.tourney.tree("", f.sample("A", "B", 0, 0, 0, 0))
##  A B --> A C --> end
##  A B --> A C --> C B --> end
##  A B --> A C --> C B --> B A --> end
##  A B --> B C --> end
##  A B --> B C --> C A --> end
##  A B --> B C --> C A --> A B --> end

## (*) The more simple case where the tree is complete
##
## > print.tourney.tree("", f("A", "B", 0))
##  A B --> A C --> A B --> A C --> end
##  A B --> A C --> A B --> B C --> end
##  A B --> A C --> C B --> C A --> end
##  A B --> A C --> C B --> B A --> end
##  A B --> B C --> B A --> B C --> end
##  A B --> B C --> B A --> A C --> end
##  A B --> B C --> C A --> C B --> end
##  A B --> B C --> C A --> A B --> end

## --8<---------------cut here---------------start------------->8---
## The Lisp primitives
cons <- function (elem, ls) {
    c(elem, ls)
}

first <- function (ls) {
    ls[[1]]
}

rest <- function (ls) {
    ls[-1]
}

is.empty <- function (ls) {
    length(ls) == 0
}

empty <- function () {
    list()
}
## --8<---------------cut here---------------end--------------->8---

## --8<---------------cut here---------------start------------->8---
## A binary tree
make.tree <- function (root, left, right) {
    list(root, left, right)
}

tree.root <- function (tree) {
    tree[[1]]
}

tree.left <- function (tree) {
    tree[[2]]
}

tree.right <- function (tree) {
    tree[[3]]
}
## --8<---------------cut here---------------end--------------->8---

## --8<---------------cut here---------------start------------->8---
## (*) The sample space for the tourney problem

other <- function (p1, p2) {
    ## This function just produces the player who is not playing.
    
    if (toupper(p1) == "A" && toupper(p2) == "B")
        return("C")
    if (toupper(p1) == "A" && toupper(p2) == "C")
        return("B")
    if (toupper(p1) == "B" && toupper(p2) == "A")
        return("C")
    if (toupper(p1) == "B" && toupper(p2) == "C")
        return("A")
    if (toupper(p1) == "C" && toupper(p2) == "B")
        return("A")
    if (toupper(p1) == "C" && toupper(p2) == "A")
        return("B")
    "impossible"
}

f <- function (p1, p2, total) {
    ## This function produces the (complete) tree of games in a
    ## tourney that only ends after 4 games have been played, no
    ## matter what happens.  It's a simplification of the original
    ## problem which is encoded by f.sample() --- see below.
    if (total == 4)
        return (empty())

    make.tree(paste(p1, p2),
              f(p1, other(p1,p2), total + 1),
              f(p2, other(p1,p2), total + 1))
}

print.tourney.tree <- function (acc.str, tree) {
    ## This function prints the paths of the trees, being careful not
    ## to print paths which look the same.  Two different paths look
    ## the same when a node is a leaf.  (Because going to the left
    ## ends the tree, and going to the right also ends the tree.  So
    ## the paths are different, but they're only different in the last
    ## empty node.  Since we don't display empty nodes, these paths
    ## look the same.)
    
    if (length(tree) == 0) {
        return(write(paste(acc.str, "end"), stdout()))
    }

    if (is.empty(tree.left(tree)) && is.empty(tree.right(tree))) {
        ## Avoid displaying paths that look the same
        return(write(paste(acc.str, tree.root(tree), "--> end"), stdout()))
    }
    
    print.tourney.tree(paste(acc.str, tree.root(tree), "-->"), tree.left(tree))
    print.tourney.tree(paste(acc.str, tree.root(tree), "-->"), tree.right(tree))
}

f.sample <- function (p1, p2, total, a, b, c) {
    ## This is the original problem.  The tourney ends after 4 matches
    ## or after a player wins two matches in a row.  This function
    ## encodes the sample space, which is a tree and can be displayed
    ## by print.tourney.tree().
    if (total == 4)
        return(empty())
    if (a == 2 || b == 2 || c == 2)
        return(empty())

    make.tree(paste(p1,p2),
              # left 
              f.sample(p1, other(p1,p2), total + 1,
                       if (toupper(p1) == "A") a + 1 else a,
                       if (toupper(p1) == "B") b + 1 else b,
                       if (toupper(p1) == "C") c + 1 else c),
              # right
              f.sample(p2, other(p1,p2), total + 1,
                       if (toupper(p2) == "A") a + 1 else a,
                       if (toupper(p2) == "B") b + 1 else b,
                       if (toupper(p2) == "C") c + 1 else c))
}
## --8<---------------cut here---------------end--------------->8---
