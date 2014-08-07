# EasyJson

### A tiny, easy-to-use JSON library for Haskell

JSON is highly useful, and ubiquitous in many scenarios. As such, it is often desirable to be able to easily injest and query JSON data. There is a powerful JSON library for Haskell called Aeson, but it is more focused on data serialization/deserialization, and has a robust and somewhat complex API. This library focuses on being easy to use. There are a very small set of useful functions for querying JSON data, the use of which are obvious to even beginner Haskellers. (Note: the *manipulation* of JSON is another matter, and has not been implemented). In addition, easyjson has very light dependencies: most of them will already be covered in all but the most minimal packages.

## Example

EasyJson uses an `Either` monad which provides error reporting. Except for the parser, which uses strings, all string types are `Text`.

Note: EasyJson's parser is a bit closer to JavaScript's, as it allows strings to be surrounded with either `"` or `'`, and allows trailing commas in arrays and objects. All generated JSON uses the standard.

```haskell
>>> :set -XOverloadedStrings
>>> json "{'foo': 2}"
Right {"foo":2.0}
>>> json "{'foo': 2}" >>= key "foo"
Right 2.0
>>> json "{'foo': [1, 2, 3]}" >>= key "foo" >>= index 1
Right 2.0
>>> json "{'foo': [1, 2, 3]}" >>= key "foo" >>= index 0
Right 1.0
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 2
Right "hello"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 3
Left "Index 3 out of range"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "bar" >>= index 3
Left "No key 'bar'"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 3
Left "Index 3 out of range"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 2 >>= number
Left "Expected a number, but got string"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 1 >>= number
Right 2.0
>>> fmap (+2) $ json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 1 >>= number
Right 4.0
```

## Usage

Install:

```
$ cabal install easyjson
```

Use:

```haskell
import Data.Text (pack, unpack)
import Text.EasyJson

main = do putStrLn "Write some json:"
          input <- getLine
          putStrLn $ case json $ pack input of
            Left err -> unpack err
            Right j -> "Valid JSON! " ++ show j
```
