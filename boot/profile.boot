(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                [refactor-nrepl "1.2.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware*
       into '[cider.nrepl/cider-middleware
              refactor-nrepl.middleware/wrap-refactor])
