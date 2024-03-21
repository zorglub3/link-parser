# Link parser in Scale

This is a small project to implement a library for supporting a link 
grammar and parser for the English language.

This is work in progress. The basic grammar/parser functionality is much
in place, so if you are working on a similar project, this might be useful
for you. Otherwise it won't be useful.

## License and warranty

This software ain't fit for any particular purpose. It ain't finished yet, 
so don't use it in your project. It doesn't come with a license either. At
some point I'll publish it with some useful license, MIT- or GPL- or 
something like that.

## Stuff to do

- More sentence structures:
  1. Not just present and past, but also present- and past- participle
  2. Quesion words
  3. Adverbs and adjectives
  4. ... and more...
- Better graph structure represent sentence links. Use labelled, directed
  edges.
- Data structure for a logical representation of sentence "meaning". Also,
  some sentence generator (ie, word list) from such representation. And a
  graph-to-representation function

## References

- "Parsing English with a Link Grammar", Daniel D. K. Sleator and Davy
  Temperly, 1991
