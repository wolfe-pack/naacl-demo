# naacl-demo

## Setup

### Update wolfe in your local ivy repository

1. in the `wolfe` directory, do `sbt publish-local`

You may have to delete the wolfe directory in the ivy cache to make sure you get the newest version.

### Setup and Run Moro

Initialize sub-modules (1), setup configuration file (2), compile the project (3) and wolfe (4) and run moro (5-6).

1. `git submodule update --init --recursive`
2. `cp naacl-moro.conf moro/conf/application.conf`
3. `sbt compile`
4. `cd wolfe; sbt compile; cd ..`
5. `cd moro; git checkout master`
6. `sbt run`

Maybe (most definitely?) you'll need to clone htmlgen and scalaplot and install them to a local repository by running

    mvn clean install -Dgpg.skip=true

### Download Data
To download the OHHLA files

    scripts/download_ohhla j_live

## Live editing in IntelliJ

You can write code in IntelliJ and access it from moro after you compile it (either through IntelliJ or sbt)
