
# Run this config without touching your own
On OSX run the following in the current directory
```
Applications/Emacs.app/Contents/MacOS/Emacs -q --eval '(load-file "init.el")' --eval "(setq user-emacs-directory \"`pwd`\")"
```

## Design

I switched from an `el-get` approach to `use-package`.
