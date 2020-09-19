## Gurobi

Installation of the `gurobi` package requires Gurobi optimizer and
libraries which can be acquired from the [Gurobi
website](https://www.gurobi.com). 

The source package can be built as usual at the shell prompt:

```
R CMD build gurobi
```

### Installation

Installation of the package requires knowledge of the `GUROBI_HOME`
environment variable.

- In a proper linux installation, `GUROBI_HOME` will be already set in
  a proper installation per Gurobi documentation.

- On macOS, the value of this environment variable can be obtained by
  examining the `gurobi.sh` file that is distributed with the
  optimizer; it is typically something like
  `/Library/gurobi903/mac64`.

- On windows, I was not able to use the instructions provided by
  Gurobi to install. Here too, you need the `GUROBI_HOME` environment
  variable which is usually something like `c:/gurobi903/win64`.  I
  believe there is some error in the instructions since my approach is
  pretty close to what they suggest: see `src/Makefile.win`.  Also,
  there is an issue involving architectures: only `win64` seems
  supported. 

To install the package at a shell prompt, execute

```
env GUROBI_HOME=<value> R CMD INSTALL gurobi_9.0.3.1.tar.gz
```

Inside R, use

```
Sys.setenv(GUROBI_HOME="<value>")
install.packages("/path/to/gurobi_9.0.3.1.tar.gz", repos = NULL)
```

### Development

To make changes to documentation, regenerate man pages etc. using
`devtools`, use:

```
Sys.setenv(GUROBI_HOME="<value>")
devtools::document()
## or devtools::install() etc.
```

### Documentation

The gurobi sources clearly have incomplete and wrong documentation. It
appears they used some automated process on their LaTeX sources that
did part of the job but screwed up a lot.

The version here has roxygenized documentation. Where the
documentation is screwed up, I've bunched them all together in a file
`R/gurobi_bad_doc.R`, so that Gurobi can easily correct it as needed. 

