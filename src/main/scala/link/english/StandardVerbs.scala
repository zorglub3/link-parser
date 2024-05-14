package link.english

import link.english.lexicon._

trait StandardVerbs { self: EnglishLexiconBuilder =>
  addEntries(List(
    // linkingVerb('look, "look", "looks", "look", "looking", "looked", "looked"),
    // linkingVerb('smell, "smell", "smells", "smell", "smelling", "smelled", "smelled"),
    // linkingVerb('sound, "sound", "sounds", "sound", "sounding", "sounded", "sounded"),

    IntransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    IntransitiveVerb("walk", "walks", "walk", "walking", "walked", "walked"),
    IntransitiveVerb("crawl", "crawls", "crawl", "crawling", "crawled", "crawled"),
    IntransitiveVerb("run", "runs", "run", "running", "ran", "run"),
    IntransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),

    TransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    TransitiveVerb("take", "takes", "take", "taking", "took", "taken"),
    TransitiveVerb("pick_up", "picks_up", "pick_up", "picking_up", "picked_up", "picked_up"),
    TransitiveVerb("pick", "picks", "pick", "picking", "picked", "picked"),
    TransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),
    TransitiveVerb("see", "sees", "see", "seeing", "saw", "seen"),

    LinkVerb("look", "looks", "look", "looking", "looked", "looked"),
    LinkVerb("smell", "smells", "smell", "smelling", "smelled", "smelled"),

    ToBe()
  ))
}
