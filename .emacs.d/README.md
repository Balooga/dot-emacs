

# Dependencies

## Emacs for OSX
https://bitbucket.org/mituharu/emacs-mac/overview
https://github.com/railwaycat/homebrew-emacsmacport

```bash
$ brew tap railwaycat/emacsmacport
$ brew install emacs-mac
$ brew link emacs-mac
$ brew linkapps emacs-mac
```

Note: to determine the location of “emacs-mac”, use;

```bash
$ brew info emacs-mac
```

To debug the GUI Emacs.app from the command-line;
https://www.emacswiki.org/emacs/EmacsForMacOS#toc16

https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html


```bash
open /Applications/Emacs.app --args --debug-init

open /Applications/Emacs.app --args --no-init-file
```


## Cowsay 

```bash
$ brew install cowsay
```

## Fortune

```bash
$ brew install fortune
```

### Spelling

;; Aspell Setup:
;; 1. Install aspell from http://aspell.net/
;;    - Install using ./configure --prefix=~/usr_local/bin, make, make install
;; 2. Download the latest dictionary from ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
;;    and extract it.
;;    - Install the dictionary using ./configure, make, make install
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;;    http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;;    the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;;    save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;;     setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;;    variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'
;;
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html



## aspell

```bash
$ brew install aspell --with-lang-en
```

Download dictionary

```bash
$ ftp://ftp.gnu.org/gnu/aspell/dict/0index.html
```

## Latex

http://tug.org/mactex/

```bash
$ tlmgr update --list (to list all updates)
$ tlmgr update --all (to perform all updates)
$ tlmgr install pkgname (to install pkgname)
```
