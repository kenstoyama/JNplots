## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Ken S. Toyama <ken.toyama@mail.utoronto.ca>'
  
  New submission

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [3s/11s] NOTE
  Maintainer: ‘Ken S. Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [3s/12s] NOTE
  Maintainer: ‘Ken S. Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

## About notes: 
# Three of them correspond to this being a new submission. One note ("...no 
# command 'tidy' found") is obtained when testing with rhub on Ubuntu and is, 
# as far as I know, caused by problems with the testing platform. The two notes
# obtained when testing on the Windows server seem to be common and also caused 
# by an issue with rhub
