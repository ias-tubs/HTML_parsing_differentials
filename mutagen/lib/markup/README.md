# HTML Tokenizer

Entry module is [Tokenizer](tokenizer.ml) which is based on test code from markup.ml.

## Origin

Substantial parts of this code (read: 99,999%) are taken from [markup.ml](https://github.com/aantron/markup.ml).

This should be credited accordingly.

Changes include:
- When parsing a tag and EOF occurs inside an attribute, the tag is emitted nevertheless.
- Duplicate attributes are added regardless
- Emit character references as is and don't decode them
- Add CData tokens in foreign content

