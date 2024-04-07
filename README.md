# Link parser in Scale

This is a small project to implement a library for supporting a link 
grammar and parser for the English language.

This is work in progress. The basic grammar/parser functionality is much
in place, so if you are working on a similar project, this might be useful
for you. Otherwise it won't be useful.

## Is it any good?

Yes.

## License and warranty

This software ain't fit for any particular purpose. It ain't finished yet, 
so don't use it in your project. It doesn't come with a license either. At
some point I'll publish it with some useful license, MIT- or GPL- or 
something like that.

## Stuff to do

- More sentence structures. The tests include more sentences structures 
  than the parser can currently handle. This is intentional. It indicates
  the direction for future additions.
- Data structure for a logical representation of sentence "meaning". Also,
  some sentence generator (ie, word list) from such representation. And a
  graph-to-representation function

## References

- "Parsing English with a Link Grammar", Daniel D. K. Sleator and Davy
  Temperly, 1991
