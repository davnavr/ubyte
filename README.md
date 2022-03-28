[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)

# Notice
This has now been superceded by [`SAILAR`](https://github.com/davnavr/SAILAR-lang), a rewrite of this project in the Rust programming language for performance reasons and changes in design.

# What is this?

Another virtual machine and bytecode format written for fun, whose language is in [static single assignment form](https://en.wikipedia.org/wiki/Static_single_assignment_form).

For an obligatory hello world example, [click here](../main/test/text/Example.HelloWorld/HelloWorld.txtmdl).

# Getting Started

## Building

First, download the [.NET 5 SDK](https://dotnet.microsoft.com/download) then navigate to the repository and run the following on your command line:
```bash
dotnet build -c Release 
```

## Writing Programs

The [`asmdl`](../main/src/asmdl/) tool compiles `.txtmdl` files into `.binmdl` files. For examples on the syntax of the text format, see the [examples directory](../main/test/text/).

## Running Programs

Compiled files are then interpreted with the [`runmdl`](../main/src/runmdl/) tool.
