package link.english

import link.english.lexicon._

trait StandardVerbs { self: EnglishLexiconBuilder =>
  addEntries(List(

    IntransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    IntransitiveVerb("walk", "walks", "walk", "walking", "walked", "walked"),
    IntransitiveVerb("crawl", "crawls", "crawl", "crawling", "crawled", "crawled"),
    IntransitiveVerb("run", "runs", "run", "running", "ran", "run"),
    IntransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),
    IntransitiveVerb("go", "goes", "go", "going", "went", "gone"),
    IntransitiveVerb("look", "looks", "look", "looking", "looked", "looked"),

    TransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    TransitiveVerb("take", "takes", "take", "taking", "took", "taken"),
    TransitiveVerb("pick_up", "picks_up", "pick_up", "picking_up", "picked_up", "picked_up"),
    TransitiveVerb("pick", "picks", "pick", "picking", "picked", "picked"),
    TransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),
    TransitiveVerb("see", "sees", "see", "seeing", "saw", "seen"),

    LinkVerb("look", "looks", "look", "looking", "looked", "looked"),
    LinkVerb("smell", "smells", "smell", "smelling", "smelled", "smelled"),
    LinkVerb("look", "looks", "look", "looking", "looked", "looked"),
    LinkVerb("sound", "sounds", "sound", "sounding", "sounded", "sounded"),

    HelpVerb("do", "does", "do", "doing", "did", "done"),
    HelpVerb("will", "will", "will", "willing", "would", "willed"),
    HelpVerb("can", "can", "can", "can", "could", "could"),

    ToBe()
  ))
}
