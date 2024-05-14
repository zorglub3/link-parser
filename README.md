# Link parser in Scala

This is a small project to implement a library for supporting a link 
grammar and parser for the English language.

This is work in progress. The basic grammar/parser functionality is much
in place, so if you are working on a similar project, this might be useful
for you. Otherwise it won't be useful.

## Purpose

This library is made with several purposes in mind. It can be used for:

- The basis for an interface for interactive fiction. Instead of only 
  handling the traditional, simple verb-plus-noun sentences, a modern
  IF should understand way more sentence forms.
- An shell for an old-fashioned (read pre-transformer) expert system.
- An interface to work with a large language model.

The library does not work so well when faced with English sentences that
may not be grammatically correct. This makes it unsuitable for parsing, eg, 
Twitter/X posts or scraping other parts of the internet.

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
  the direction for future additions. Failing tests are commented out.
  Uncomment when adding new features.
- Data structure for a logical representation of sentence "meaning". Also,
  some sentence generator (ie, word list) from such representation. And a
  graph-to-representation function
- Part of speach, POS, can be inferred from what links apply to each word. 
  Currently, each words gets tagged with _all_ tags found for said word in
  the dictionary. This set of tags can be limited, if the tags are applied
  while processing the link rules.

## References

- "Parsing English with a Link Grammar", Daniel D. K. Sleator and Davy
  Temperly, 1991
- [The CMU link grammar natural language parser](https://github.com/opencog/link-grammar) is
  a C implementation based on the original CMU link parser. It has bindings
  to other languages including Java and thus Scala.
