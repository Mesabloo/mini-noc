This code is the result of experimenting with Haskell's unboxed/unlifted types in order to maximise the raw performance by avoiding allocations as much as possible.

The code was originally put into a gist ([https://gist.github.com/Mesabloo/96aef8da87903201ee76edd6c909c2ff](https://gist.github.com/Mesabloo/96aef8da87903201ee76edd6c909c2ff)) but has been migrated here and somehow broken also.

The goal is to implement a minimalistic virtual machine (VM) to run a very small subset of the [Noc language](https://github.com/noc-lang).
Performance is an issue.

Here are the last known performance results on my machine:

| `fact(15)` | `ack(3, 6)` |
| :--------: | :---------: |
|   ~5µs   |    ~31.5ms    |

-----------------------

:warning: This is very experimental.
Expect to see some weird and ugly very unsafe Haskell code, relying on GHC's own internal libraries and boot packages (and nothing else).
I am proud of what I have been able to do, though I would not recommend anybody doing this at all.

-----------------------

