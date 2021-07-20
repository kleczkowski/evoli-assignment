# Evoli Assignment

This repository contains a solution to Evoli assignment.

## Architecture note & decisions

Whole project is written with Polysemy with _Clean Code Architecture_
to achieve good testability of the project. Project uses SQLite database, because
is simple and it fits into our use case.

I've assumed that the name of applicant is unique and that simplifies 
this assignment. Of course, there is no problem to give quotes/policies
a synthetic ID.

## Interactions with project

In sections above it will be described how to run or test the project.

### Running the project

1. Ensure that you have installed Cabal 3.4 at least and GHC 8.10.5
2. Run `cabal install` to install service binary
3. Run `evoli-assignment bootstrap` to initialise database.
4. Run `evoli-assignment run` to run the server.

### Testing the project

Project is equiped with test suite written in Tasty.
Run `cabal test` to run test suite or `cabal test -- --help` 
to get the help for test suite executable.

## Issues

If you noticed a bug or you want to issue a feature, create ticket at
[issue tracker](https://github.com/kleczkowski/evoli-assignment/issues).

## License

This code is shared under [MIT License](LICENSE).

## Contact

Feel free to contact me at <konrad.kleczkowski@gmail.com>
