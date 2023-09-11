# New submission

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Ken Toyama <ken.toyama@mail.utoronto.ca>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Curran (6:169)

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/23s] NOTE
  Maintainer: ‘Ken Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Curran (6:169)

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [6s/27s] NOTE
  Maintainer: ‘Ken Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Curran (6:169)

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

## About notes

Three of them correspond to this being a new submission. Also, "Curran" is a 
last name. One note ("...no command 'tidy' found") is obtained when testing with 
rhub on Ubuntu and is, as far as I know, caused by problems with the testing 
platform. The two notes obtained when testing on the Windows server seem to be 
common and also caused by an issue with rhub. Local R CMD check results in no 
errors, warnings, or notes.

## Comments made on previous submission and fixes

Please reduce the length of the title to less than 65 characters.
-Title was reduced.

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file...
-I added a reference in the description field.

Please write TRUE and FALSE instead of T and F.
-Fixed in both functions.

Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
-Added that information in the documentation.

Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
-The changes in par in the examples were removed to avoid changing those settings.


# First submission

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Ken Toyama <ken.toyama@mail.utoronto.ca>'
  
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
  checking CRAN incoming feasibility ... [3s/13s] NOTE
  Maintainer: ‘Ken Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [3s/16s] NOTE
  Maintainer: ‘Ken Toyama <ken.toyama@mail.utoronto.ca>’
  
  New submission

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

## About notes

Three of them correspond to this being a new submission. One note ("...no 
command 'tidy' found") is obtained when testing with rhub on Ubuntu and is, 
as far as I know, caused by problems with the testing platform. The two notes
obtained when testing on the Windows server seem to be common and also caused 
by an issue with rhub. Local R CMD check results in no errors, warnings, 
or notes.
