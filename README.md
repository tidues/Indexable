# Indexable: Containers Subsetting and Slicing

This is a Haskell package mainly used for subsetting and slicing all kinds of containers or nested containers that can be different types.

This package defined a typeclass called "indexable", whose main operator is slicing operator (&). The folllowing is a small example

```haskell
import Indexable    -- import the package

lst = [0..10]       -- create a list
lst&[""]            -- select all elements
lst&["0,3,8"]       -- select elements of index 0, 3 and 8
lst&["2:"]          -- select all elements from index 2 to the end
lst&[":-1"]         -- select all elements except the last one
lst&["1:-1:2"]      -- select elements in range with step size 2
lst&["1:3,4:6,8"]   -- elements 1:3, 4:6 and the 8th.

```
