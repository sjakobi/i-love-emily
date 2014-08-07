Emily Howell is a computer program, created by the classical composer [David Cope][cope], that composes original music of surprising artistic quality. [Examples of her work][emily] can be found on [Cope's Youtube channel][davidhcope].

The goal of this project is to reimplement parts of this program in Haskell, in order to better understand its inner workings and creative algorithms. David Cope has kindly documented his work on algorithmic composition in many books and source code files, including

* [*Computer Models of Musical Creativity*.][cmmc] Cambridge, MA: MIT Press. 2006
* [Source code][cmmc-code] for *Computer Models of Musical Creativity*. Written in [Macintosh Common Lisp.][mac-clisp]

  [davidhcope]: https://www.youtube.com/user/davidhcope
  [cope]: http://artsites.ucsc.edu/faculty/cope/
  [emily]: https://www.youtube.com/watch?v=jLR-_c_uCwI
  [cmmc]: http://books.google.de/books?id=rnEJAQAAMAAJ
  [cmmc-code]: http://artsites.ucsc.edu/faculty/cope/cmmc.html
  [mac-clisp]: http://en.wikipedia.org/wiki/Macintosh_Common_Lisp


Bookmarks
---------

[*The Music Suite*](http://hackage.haskell.org/package/music-suite) — Hans Hoglund's Haskell DSL for description, analysis, composition and manipulation of music.

* [Reference documentation](http://music-suite.github.io/docs/ref/)
* [API haddocks](http://music-suite.github.io/docs/api/)
    * [`data Score a`](http://music-suite.github.io/docs/api/music-score/Music-Time-Score.html#t:Score)
    * Pitch literals (c,d,...)
      [`class IsPitch a`](http://music-suite.github.io/docs/api/music-pitch-literal/Music-Pitch-Literal-Pitch.html)
* [examples](https://github.com/music-suite/music-preludes/tree/master/examples)

[*HCodec*](http://hackage.haskell.org/package/HCodecs) — a library that provides functions to read, write and manipulate MIDI, WAVE and SoundFont2 multimedia files.

* [`Codec.Midi` module](http://hackage.haskell.org/package/HCodecs-0.5/docs/Codec-Midi.html)


