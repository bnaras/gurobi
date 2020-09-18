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
  Gurobi to install. I believe there is some error in the
  instructions. 


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


