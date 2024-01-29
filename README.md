# Hiqa Reports Reader

Work in Progress...

## Build
To build, requires [clojure](https://clojure.org/guides/install_clojure)  and a version of the jvm with JavaFX support. 

Then, run:

``` sh
clj -T:build ci
```

which will build a jar file and output it  the `targets` directory.

## Usage

The UI is currently very minimal.

1. Select a folder with HIQA reports in `pdf`format.
2. Click "Run"

Depending on the amount of pdfs, the program may take a couple of minutes to run and will appear to 'hang'. Files are output to the same folder the progam is called from. It outputs three csv files:

- main csv - full table with all the data
- 'most recents' csv - filtered to only list the most recent inspection per centre.
- info csv - some metadata info about the data
