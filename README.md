# Indexable: Containers Subsetting and Slicing

This is a Haskell package mainly used for subsetting and slicing all kinds of containers or nested containers that may have different types.

This package defined a typeclass called "indexable", whose main operator is slicing operator `(&)`. The folllowing is a small example (in ghci)

```haskell ghci
import Indexable    -- import the package

lst = [0..10]       -- create a list
lst&[""]            -- select all elements
lst&["0,3,8"]       -- select elements of index 0, 3 and 8
lst&["2:"]          -- select all elements from index 2 to the end
lst&[":-1"]         -- select all elements except the last one
lst&["1:-1:2"]      -- select elements in range with step size 2
lst&["1:3,4:6,8"]   -- elements 1:3, 4:6 and the 8th.

```
the output in ghci is:

```haskell ghci
[0,1,2,3,4,5,6,7,8,9,10]
[0,3,8]
[2,3,4,5,6,7,8,9,10]
[0,1,2,3,4,5,6,7,8,9]
[1,3,5,7,9]
[1,2,4,5,8]
```

# One Dimension Slicing
A slicing request is a string with multiple slicing terms connected by separator ",", like

```haskell ghci
slicing_request1 = "1,3,5"                -- has three terms "1", "3" and "5"
slicing_request2 = "1:10:2,11:20,78,90"   -- has four terms "1:10:2", "11:20", "78", "90"
```

Each term is a string represents a list of keys or indexes in numbers. 

### Index Slicing (for list-like container)
The general version of a term is `"a:b:c"` where `a` is the starting index, `b` is the ending index, `c` is the step. The defualt value (when term is `":b:c"`) is `0`, default for `b` is the length of the container, default value for `c` is `1`.

Like in python, value of b can be negative, in this case it means how many elements you want to dissmiss at the end of the container.

### Key Slicing (for map-like container)
Slicing for map-like containers has only one requirement on the type of key, that is readable. Each slicing term is composed in the form

```haskell ghci
key_slice_term1 = "xxxx[1:10:2]xxxxx"
key_slice_term2 = "xxxx"
```
i.e. it can be a string represent the key, or a string with "[slicing term]" (not request, just term) inside. You can still connect terms together as a key slicing requrest like below

```haskell ghci
key_slice_request = "key[0:5], newkey1, oldkey2, other[3:8:2]key"
```

## Multiple Dimension Slicing

### Lift Nested Container into Composed Type
The multiple dimensional containers need to be constructed as a composed type. Below is an example of map of lists (in ghci).

```haskell ghci
import Indexable                        -- import indexable package
import Data.Map.Strict                  -- import the map container
import Data.Functor.Compose             -- import the compose functor

mlst = [(i,[i..i+3]) | i<-[0..3]]       -- an list of tuples with first value as key
                                        -- second as a list of numbers.

mmap = fromList mlst                    -- construct map of list with type (Map Integer [Integer])

cmap = Compose mmap                     -- lift nested container to composed type
                                        -- so we can use slicing operator (&) on it
```

### Apply Slicing on Composed Containers
We can slice on nested container with a list of slicing request, with the ith request perform on the ith dimension. Continue on the above example:

```haskell ghci

cmap&[":","0,1"]        -- all rows in the map, with first two entries of each list
cmap&["1:3","1:-1"]     -- rows (items in the map) 1 and 2, with each list from index 1 to len-1
cmap&["::2"]            -- all rows with step 2, and all elements in list of each row

```

the outputs are:

```
Compose (fromList [(0,[0,1]),(1,[1,2]),(2,[2,3]),(3,[3,4])])

Compose (fromList [(1,[2,3]),(2,[3,4])])

Compose (fromList [(0,[0,1,2,3]),(2,[2,3,4,5])])
```

For higher level nested containers, we need to first construct composed type with corresponding level. For example, the map of list of vector, say `mlv`, needs to composed twice, `cmlv = Compose $ Compose mlv` before applying `(&)` operator.

# Current Instances of Indexable

1. Data.List
1. Data.Map.Strict
1. Data.HashMap.Strict
1. Data.IntMap
1. Data.Sequence
1. Data.Vector

and all finite nested combination of above containers.

# Create New Instances

To add new containers `f` with key `b` as indexable, two minimal functions need to provide:

1. `(&?) :: f a -> b -> Maybe a` : a function safely return the element of type `a` by key of tyep `b`.
1. `fromLst :: (Typeable b) => [(b,a)] -> f a` : a function that construct container `f` from a list of key value pair tuples. (list-like containers should forget the keys in this function). 

After appling these two function, new container can be nested with other containers and using slicing operator `(&)`.

# Other Operators for Indexable

1. `(&?)` safe lookup
1. `(&!)` unsafe lookup


# How to Install

This package is currently in the candidate package pool of Hackage. You can download from there, or simply clone this git.
