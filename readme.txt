Implementation of Dinic max flow algorithm in Haskell.

------
Files:
------
Dinic.hs
    Contains the main functionality.

Main.hs
    Contains main function so that the graph can be read
    from a file.

Graph.hs
    Contains data structure definitions.

input.txt
    Sample input. Source node on the first line, sink node on the
    second line and edges on further lines. Note that nodes are
    represented as strings.

------
Usage:
------
The easiest way is to compile source files into a binary program via ghc:
> ghc -o main Main.hs
This way a correctly-formated file input.txt has to be located
in current directory.

Program is then executed without any arguments and reads from input.txt