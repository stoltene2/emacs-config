
# Run this config without touching your own
On OSX run the following in the current directory
```
Applications/Emacs.app/Contents/MacOS/Emacs -q --eval '(load-file "init.el")' --eval "(setq user-emacs-directory \"`pwd`\")"
```

## Design

To indicate packages you want el-get to install, create an
init-package-name.el file in the packages directory. These are
dual-purposed files. `init.el` instructs `el-get` the package name to
install. The file themselves provide mode specific configuration.
