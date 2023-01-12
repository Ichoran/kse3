# Kse3

This repository contains the Kerr Standard Extensions to Scala 3.

These are intended to cover everything that the standard library forgot that
and is commonly used for general-purpose programming and data analysis.

Kse3 has no pretenses of being an idiomatic toolchain.  It is designed for
high-productivity programming for people who like direct style, good error
handling, and care about performance.  There is no particular attempt to
take a monadic or other functional approach, except inasmuch as it helps
productivity.  When there is a tradeoff between enabling good user code and
writing "good" library code (DRY, etc.), Kse3 favors the user.  Kse is
supposed to take care of any necessary ugly stuff so you don't have to.

**WARNING: Kse3 ONLY WORKS ON Scala 3.1 DUE TO USE OF MACROS**
**THIS IS SUBJECT TO CHANGE--IN PARTICULAR, Kse3 ONLY HAS A GOAL OF WORKING
WITH ONE Scala X.Y VERSION AT ANY TIME.  MORE IS NICE, NOT A GUARANTEE.**

## How do I get it?

This is an exploratory release, so it is a mystery!

## Subprojects

The structure of packages is still being determined.  In general, they will
be `kse.` something--a name collision with the original Kse for Scala 2, but
you shouldn't use that with Scala 3 because Kse is actually still on Scala
2.12.  So it's all good.

## More to come

There is more to come, obviously.  This is just a stub to keep things from
being totally blank on GitHub.
