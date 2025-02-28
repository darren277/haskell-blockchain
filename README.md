# Haskell Blockchain

## About

This is an experimental project for me to learn Haskell a little bit better.

The use case is a very simple blockchain along with a web server for interacting with it along with Postgres interactivity.

## Directory Structure

Here's a suggested directory structure:
```
haskell-blockchain/
├── app/
│   └── Main.hs
├── src/
│   └── Blockchain.hs
|   └── Web.hs
├── test/
│   └── Spec.hs
├── haskell-blockchain.cabal
├── stack.yaml
└── Setup.hs
```

`app/Main.hs`: The entry point of your application. It will import your blockchain module from src/ and run the main application logic.
`src/Blockchain.hs`: Contains the blockchain logic, including data structures and functions for creating and manipulating the blockchain.
`src/Web.hs`: Contains the REST Web API for interacting with our blockchain.
`test/Spec.hs`: For any tests you want to write using a framework like Hspec.
`haskell-blockchain.cabal`: Cabal file describing your application's properties, dependencies, and build information.
~~`stack.yaml`: (Optional) If you're using Stack, this file will configure the project's stack tool settings.~~
`Setup.hs`: Used by Cabal, typically a boilerplate file for simple projects.

## Initializing the Project (Done)

You can initialize the project using either Cabal or Stack. These tools help manage dependencies, build processes, and package configurations.

### Using Cabal
Install GHC and Cabal if you haven't already.
Navigate to your project directory and run:
`cabal init`

Follow the interactive prompts to set up your project. You can choose the executable type when prompted, as this project is an application.
Add your dependencies to the generated .cabal file. For this project, you might need base, time, and cryptonite for cryptographic functions.

### Using Stack
Install Stack if you haven't already.
Navigate to project directory and run:
`stack new haskell-blockchain simple`

This command creates a new project using the "simple" template.
Update the stack.yaml and haskell-blockchain.cabal files with your project's dependencies.

## Building and Running the Project

### Using Cabal

Build project:
`cabal build`

Run project:
`cabal run`

### ~~Using Stack~~

~~Build project:~~
~~`stack build`~~

~~Run project:~~
~~`stack exec haskell-blockchain-exe`~~
~~`stack run`~~

Replace haskell-blockchain-exe with the name of your executable defined in the Cabal file if you are using a different name.

## Assorted Notes

### Cautionary Tales

Dependency conflicts can be particularly brutal with Haskell.

After a whole lot of trial and error (mostly error) with `stack`, I would up switching to `cabal`.

Even with `cabal`, I went through a few learning lessons to get things off the ground.

One thing to take note of: The `build-depends` sections of both the `library` and `executable` have to match. I spent way too much time trying to figure out why some of my dependencies simply were not being recognized.

Also, take note of some of the CLI commands in the following section in case you need to use a specific version of GHC, etc.

### Some CLI Commands

```shell
stack exec -- ghc-pkg list

stack setup --resolver ghc-8.8.4

$env:GHC_PACKAGE_PATH = "C:\Users\Darren\AppData\Local\Programs\stack\x86_64-windows\ghc-9.10.1\lib\package.conf.d"
echo $env:GHC_PACKAGE_PATH

#### To unset:
$env:GHC_PACKAGE_PATH=""

stack exec -- cabal init
```

```shell
ghcup install ghc 8.8.4
ghcup set ghc 8.8.4
$env:PATH = "C:\ghcup\bin;" + $env:PATH
```

## Testing

`cabal repl`
`:l src/Database.hs`
`:t Envy.envMaybe "PG_PORT"`
