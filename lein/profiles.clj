{:user {:dependencies []
        :plugins [[lein-droid "0.4.0-alpha4"]
                  [lein-try "0.4.3"]]}

 :repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]
        :plugins [[refactor-nrepl "1.1.0"]
                  [cider/cider-nrepl "0.10.2"]]}

 :android-common {:dependencies [[cider/cider-nrepl "0.10.2"]]
                  :android {:aot-exclude-ns ["cider.nrepl.middleware.util.java.parser"
                                             "cider.nrepl" "cider-nrepl.plugin"]
                            :sdk-path "/home/fvaresi/opt/android-sdk-linux"}}}
