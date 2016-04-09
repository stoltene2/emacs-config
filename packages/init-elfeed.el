(global-set-key (kbd "C-c f") 'elfeed)

(setf url-queue-timeout 120)

(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        ("http://nedroid.com/feed/" webcomic)
        ("http://feeds.feedburner.com/brainpickings/rss" learning)
        ("http://xkcd.com/atom.xml" webcomic)
        ("http://feeds.feedburner.com/StudyHacks" learning)
        ("https://www.farnamstreetblog.com/feed/" learning)))
