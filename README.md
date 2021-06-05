# MySQL login path reader, for R

This is a port of the Python package [myloginpath](https://pypi.org/project/myloginpath/). It can read an encrypted login path file.

## Install

Install [devtools](https://cran.r-project.org/web/packages/devtools/readme/README.html), then run the following R command:

```r
devtools::install_github("nickodell/rmyloginpath")
```

## Usage

Create login path file with mysql_config_editor shell command:

```sh
$ mysql_config_editor set --login-path=client --host=localhost --user=localuser --password
Enter password: <Type password here>
```

Use it within R:

```R
> library(rmyloginpath)
> login_parse("client")
$user
[1] "localuser"

$password
[1] "hunter2"

$host
[1] "localhost"
```
