```haskell
>>> :set -XOverloadedStrings
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
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 3 >>= number
Left "Index 3 out of range"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 2 >>= number
Left "Expected a number, but got string"
>>> json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 1 >>= number
Right 2.0
>>> fmap (+2) $ json "{'foo': [1, 2, 'hello']}" >>= key "foo" >>= index 1 >>= number
Right 4.0
```
