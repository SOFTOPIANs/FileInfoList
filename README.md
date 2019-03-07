# File Infomation Listing Tool v0.2.1 (20190307)

### required opam package

 - oasis
 
### How to build

 - install opam (by brew : `brew install opam`)
 - install oasis package (`opam install oasis`)
 - run `oasis setup`
 - run `make`
 - get the binary file : `main.byte` or `main.native`
 
### How to run

 - `./main.{byte|native} -t <dir_name>` : search every files in <dir_name>, listing files of unknown extension (categorized by extension).
 - `./main.{byte|native} <dir_name>` : search every files in <dir_name>, listring informations of files.

### Addtional options

 - `./main.{byte|native} ... -f <output_file>` : print out to <output_file>. (default : stdout)

### TODO

 - move hard-coded file type list to external file
 - fix bugs
