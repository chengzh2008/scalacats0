# Learning journey of Scala with Cats 
## Table of Content

[Chap 1 Introduction](./src/main/scala/sandbox/cats1)
- [Type Class Introduction](./src/main/scala/sandbox/cats1/TypeClassIntro.scala)
- [Exercise: Printable Library](./src/main/scala/sandbox/cats1/Printable.scala)
- [Exercise: Cat Show](./src/main/scala/sandbox/cats1/CatShow.scala)
- [Exercise: Equality, Liberty, and Felinity](./src/main/scala/sandbox/cats1/CatEq.scala)

[Chap 2 Monoids and Semigroups](./src/main/scala/sandbox/cats2)
- [Exercise: The Truth About Monoids](./src/main/scala/sandbox/cats2/BooleanMonoid.scala)
- [Exercise: All Set for Monoids](./src/main/scala/sandbox/cats2/SetMonoid.scala)
- [Exercise: Adding All The Things](./src/main/scala/sandbox/cats2/SuperAdder.scala)

[Chap 3 Functors](./src/main/scala/sandbox/cats3)
- [Exercise: Branching out with Functors](./src/main/scala/sandbox/cats3/TreeFunctor.scala)
- [Exercise: Showing off with Contramap and Transformative Thinking with imap](./src/main/scala/sandbox/cats3/Codec.scala)

[Chap 4 Monads](./src/main/scala/sandbox/cats4)
- [Exercise: Getting Func-y](./src/main/scala/sandbox/cats4/FunctorMonad.scala)
- [Exercise: Monadic Secret Identities](./src/main/scala/sandbox/cats4/IdMonad.scala)
- [Exercise: Abastracting](./src/main/scala/sandbox/cats4/Abstracting.scala)
- [Exercise: Safer Folding using Eval](./src/main/scala/sandbox/cats4/SafeFoldingEval.scala)

# Scala with Cats Code

Sandbox project for the exercises in the book [Scala with Cats][book].
Based on the [cats-seed.g8][cats-seed] template by [Underscore][underscore].

Copyright Anonymous Aardvark. Licensed [CC0 1.0][license].

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)][gitter]

## Getting Started

You will need to have Git, Java 8, and [SBT][sbt] installed.
You will also need an internet connection to run the exercises.
All other dependencies are either included with the repo
or downloaded on demand during compilation.

Start SBT using the `sbt` command to enter SBT's *interactive mode*
(`>` prompt):

```bash
$ sbt
[info] Loading global plugins from <DIRECTORY>
[info] Loading project definition from <DIRECTORY>
[info] Set current project to <PROJECT_NAME>

>
```

From the SBT prompt you can run the code in `Main.scala`:

```bash
> run
[info] Updating {file:<DIRECTORY>}cats-sandbox...
[info] Resolving jline#jline;2.14.4 ...
[info] Done updating.
[info] Compiling 1 Scala source to <DIRECTORY>...
[info] Running sandbox.Main
Hello Cats!
[success]
```

You can also start a *Scala console* (`scala>` prompt)
to play with small snippets of code:

```bash
> console
[info] Starting scala interpreter...
[info]
Welcome to Scala 2.12.3 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_112).
Type in expressions for evaluation. Or try :help.

scala> import cats._, cats.implicits._, cats.data._
import cats._
import cats.implicits._
import cats.data._

scala> "Hello " |+| "Cats!"
res0: String = Hello Cats!

scala>
```

Press `Ctrl+D` to quit the Scala console
and return to SBT interactive mode.

Press `Ctrl+D` again to quit SBT interactive mode
and return to your shell.

### Notes on Editors and IDEs

If you don't have a particular preference for a Scala editor or IDE,
we strongly recommend you do the exercises for this course using
the [Atom][atom] editor and a Linux or OS X terminal.
See the instructions below to get started.

If you want to use [Scala IDE][scala-ide] for Eclipse,
we recommend using [sbteclipse][sbteclipse].
Follow the instructions on the `sbteclipse` web page
to install it as a global SBT plugin.

If you want to use IntelliJ IDEA,
follow the instructions for [Importing an SBT Project][intellij-setup]
in the IntelliJ online documentation.

### Asking Questions

If you want to discuss the content or exercises with the authors,
join us in our chat room on [Gitter][gitter].

[cats-seed]: https://github.com/underscoreio/cats-seed.g8
[underscore]: https://underscore.io
[book]: https://underscore.io/books/advanced-scala
[license]: https://creativecommons.org/publicdomain/zero/1.0/
[sbt]: http://scala-sbt.org
[gitter]: https://gitter.im/underscoreio/scala?utm_source=essential-scala-readme&utm_medium=badge&utm_campaign=essential-scala
[atom]: https://atom.io
[scala-ide]: http://scala-ide.org
[sbteclipse]: https://github.com/typesafehub/sbteclipse
[intellij-idea]: https://www.jetbrains.com/idea
[intellij-setup]: https://www.jetbrains.com/help/idea/2016.1/getting-started-with-sbt.html#import_project
