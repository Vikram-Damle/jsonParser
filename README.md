# JSON Parser

### - Vikram Damle

----------

A simple JSON parser library in Haskell. Can parse most simple JSON strings. 

The project follows the demonstration in [this video](https://www.youtube.com/watch?v=N9RUqGYuGfw). Apparently, this is a well established functional method of writing parser libraries. The power of this method is fascinating and it is impressive how it leverages the *Functor*, *Applicative* and *Alternative* typeclasses, which is what drew me into trying it out. 

The main code is in MyLib.hs and includes a JSON string parser `fromJson` (converts to an Abstract Syntax Tree internal to Haskell) and a method `toJson` to convert back from the AST to the JSON string format. The operation `toJson . fromJson` is (in theory) idempotent. 

To try out the parser interactively, use `cabal repl` (after running `cabal init`, requires cabal). 