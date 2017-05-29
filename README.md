# hanon

Simple anonimization software for text files (CSV for example)

Goal and status
---------------
 - Created as a simple formalization of the Highlighter/Mapper concept
 - Works but not production ready: no official support available, slow implementation.

Usage
------

First using `--scan` a list of translation items is found and put in a LevelDB.

You can inspect the translations by using `--list`

Apply the mapping by using `--map`


Adding mappers
----------

Open the Mapper.hs file

 - Create a highlighter
 - Create a MappingGenerator
 - Add the new InputPath to the inputPaths list

Then `stack build` and run.

Thanks
------

Thanks to people at the #haskell-beginners IRC channel for helping out, and [pull requests from other people](./pulls?q=is%3Aclosed).
