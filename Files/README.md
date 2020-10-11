# Organization

The directory structure serves

- to avoid multiple files with the same name and
- to avoid too many files in the same directory.

For every other kind of relation, indices (forthcoming) may be consulted.

Design choices

- files are to be added to the structure in a sequence.
- each new file is placed at the next available slot.

# Zero-Maintenance Software

Don't change your position, ["Travel through space without moving,"](https://en.wikiquote.org/wiki/Dune_(film)) instead.

I do not intend ever to change any of these **R** files: It is not necessary. (I _do_ intend to revise documentation, including the present file).

If a correction is needed because `foo` in file `path1/foo.R` is in error, I may create another `foo` in file `path2/foo.R`.
Those who want to continue using `foo` in `path1/foo.R` rather than `path2/foo.R` may continue to do so.
