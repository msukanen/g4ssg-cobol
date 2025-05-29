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

# **About** (Gnu)**COBOL**
Technically any "modern" COBOL variant would do,
but I've yet to test this in e.g. z/OS environment nor to
make any complicated build system variants.
And so, until otherwise stated, **GnuCOBOL** + **CMake** are the
toolset to use.

# **Bugs?**
There's probably plenty of these, but I'll eventually™ try to weed
(most) out in the future with testprogs and such.

## Testing
### "test" directory
This one contains **.cob** files that try to test things.
### "bin/testXYZ" files
Test executables will end up in **bin/** directory and
they follow (a rather cryptic?) naming scheme - all of the file names
begin with "test" ;-)

