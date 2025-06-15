# **G4SSG** (in COBOL)
Star system generation in COBOL? Why not!
In other words, a bit of a proof of concept
for myself that "I can do it."

If the app has any use for *anyone*, including
myself, depends™.

### G4SSG?
G4SSG is a shorthand for "GURPS 4E Star System Generator".

## **Requirements**
You need:
* **GnuCOBOL** (v3.2.0+), which, IIRC, requires (or just really, really likes to have available):
    * **GMP** (libgmp); this one is mandatory, AFAIK.
    * **Berkeley DB** (libdb++-dev); optional but useful.
* **CMake** - ditto.
#### ...and...
* Ambient music, space themed.
* Coffee / energy drinks.
* Food (of your preferred sort).

## Files - there be files!
So, all source files sit in two places,
`cbl/` (main source files) and
`cpy/` (the copybook stuff). In the very root directory resides `WRAPPER.cbl` which
lets us have some semblance of "command line options" (of which there isn't many
at the moment), e.g. **`C`** and **`V`** (or `c` and `v`, if you hate caps).

# Running it ...
After you've managed to successfully execute a combination of
**`cmake config`** and
**`cmake --build .`**, you can issue **`bin/g4ssg`** and hope for the best!

## Command Line Options
There's no command line options per se, but you can use an environment variable
`PARM` for that, e.g. to make a star system in a core or cluster, you call:
`PARM='C' bin/g4ssg`, or if you want more verbose output,
`PARM='V' bin/g4ssg`,
or a combination of those (in no particular order),
`PARM='C,V' bin/g4ssg`.

# **Bugs?**
There's probably plenty of these, but I'll eventually™ try to weed
(most) of them out in the future with testprogs and such.

## Testing
### "test" directory
This one contains **.cbl** files which try to test things. Some might or might not work...
### "bin/testXYZ" files
Test executables will end up in **bin/** directory and
they follow (a rather cryptic?) naming scheme ;-) - check the CMakeLists.txt.

# **About** (Gnu)**COBOL**
Technically any "modern" COBOL variant would do,
but I've yet to test this in e.g. z/OS environment nor to
make any complicated build system variants.
And so, until otherwise stated, **GnuCOBOL** + **CMake** are the
toolset to use.
