<h1 align="center">File System</h1>

## Overview

The principal idea behind my project implementation is to use the underlying custom-designed `FileSystem` structure, which conveniently represents the computer file system

### FileSystem

```haskell
data FileSystem =
    MkFile String String 
    | MkDirectory String [FileSystem]
    deriving (Eq, Show)
```

- `MkFile <name> <content>` - used for creating file instances
- `MkDirectory <name> <content>` - used for creating directory instances, where content could be comprised of files and directories

For my REPL (Read-Eval-Print Loop), I instantiated additional `Command` and `Eval` structures, which segregate the code by making it more readable

### Command

```haskell
data MKCommands = MkDir | Touch
    deriving (Eq, Show)

data Command = PWDCommand 
    | CDCommand 
    | LSCommand 
    | DIRCommand MKCommands
    | CATCommand 
    | RMCommand
    | SHOWCommand
    | QUITCommand
    deriving (Eq, Show)
```

### Eval

```haskell
data Eval = Continue (Maybe [FileSystem])
    | PWD
    | LS String
    | SHOW String
    | QUIT
```

Due to the provided implementation, I had to define `Eval` in order to obtain information whether or not the command is of I/O type or just `FileSystem` manipulation

## Project Structure

1. The provided `Parser.hs` logic plays a pivotal role in the way input is being processed throughout the whole project. Due to the complex logic, the code was partially borrowed from the repository linked down below
   - https://github.com/tsoding/haskell-json
   - https://github.com/fmi-fp-lab/fp-lab-2024-25/tree/master/exercises/09
2. The logic, concerned with `FileSystem` manipulation is segregated into distinct code files i.e `Add.hs`, `Remove.hs`, `Navigation.hs`, `Concat.hs`

## Installation

1. Clone this repository

```console
git clone https://github.com/Slavi15/File-System.git
# SSH: git clone git@github.com:Slavi15/File-System.git
```

2. Head to `src` directory

```console
cd src
```

3. Load the program

```console
ghci Main.hs
```

4. Run the program

```console
main
```

## Commands

By default the program runs with the following file system initially:

```haskell
fileSystem :: FileSystem
fileSystem = MkDirectory "/" []
```

- `pwd` - absolute path
- `cd <path>` - navigation to the provided subdirectory
- `cd ..` - navigation to the parent directory
- `ls <path | empty>` - prints the contents of the provided directory
- `cat <file_1> <file_2> ... <file_n> > <output_file>` - combines the contents of the provided files into the output file
- `touch <path | empty> <name_1> <content_1 ++ $> <name_2> <content_2 ++ $> ... <name_n> <content_n ++ $>` - creates files in the provided directory
- `mkdir <path | empty> <dir_1> <dir_2> ... <dir_n>` - creates directories in the provided directory
- `rm <file_1> <file_2> ... <file_n>` - removes the provided files from the current directory
- `show <file>` - prints the content of the provided file in the current directory
- `:q` - termination of the program

## Usage

```console
Microsoft Windows [Version 10.0.22631.4602]
(c) Microsoft Corporation. All rights reserved.

D:\File-System\src>ghci
GHCi, version 9.4.8: https://www.haskell.org/ghc/  :? for help
ghci> :load Main.hs
[ 1 of 13] Compiling Core.Command     ( Core\Command.hs, interpreted )
[ 2 of 13] Compiling Core.FileSystem  ( Core\FileSystem.hs, interpreted )
[ 3 of 13] Compiling Core.Eval        ( Core\Eval.hs, interpreted )
[ 4 of 13] Compiling Output           ( Output.hs, interpreted )
[ 5 of 13] Compiling Parser           ( Parser.hs, interpreted )
[ 6 of 13] Compiling Navigation       ( Navigation.hs, interpreted )
[ 7 of 13] Compiling Utility          ( Utility.hs, interpreted )
[ 8 of 13] Compiling Show             ( Show.hs, interpreted )
[ 9 of 13] Compiling Remove           ( Remove.hs, interpreted )
[10 of 13] Compiling Add              ( Add.hs, interpreted )
[11 of 13] Compiling Cat              ( Cat.hs, interpreted )
[12 of 13] Compiling Main             ( Main.hs, interpreted )
Ok, 12 modules loaded.
ghci> main
/> ls

/> mkdir dir
/> ls
Directory: dir

/> cd dir
/dir/> ls

/dir/> touch file1 Very cool project! $
/dir/> touch file2 Test concat! $
/dir/> ls
File: file1
File: file2

/dir/> cat file1 file2 > file3
/dir/> ls
File: file1
File: file2
File: file3

/dir/> show file3
File: file3
Content:
Very cool project! Test concat!

/dir/> rm file2
/dir/> ls
File: file1
File: file3

/dir/> cd ..
/> ls
Directory: dir

/> mkdir test
/> ls
Directory: dir
Directory: test

/> cd test
/test/> mkdir subtest
/test/> ls
Directory: subtest

/test/> cd subtest
/test/subtest/> ls

/test/subtest/> pwd
/test/subtest/

/test/subtest/> cd ..
/test/> cd ..
/> ls
Directory: dir
Directory: test

/> rm test
/> ls
Directory: dir
Directory: test

/> :q
Exit ...
ghci> :q
Leaving GHCi.

D:\File-System\src>
```