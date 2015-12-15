{:user {:dependencies []
        :plugins [[lein-droid "0.4.0-alpha4"]
                  [lein-try "0.4.3"]]}

 :repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]
        :plugins [[refactor-nrepl "2.0.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.11.0-SNAPSHOT"]]}

 :android-common {:dependencies [[cider/cider-nrepl "0.10.0"]]
                  :android {:aot-exclude-ns ["cider.nrepl.middleware.util.java.parser"
                                             "cider.nrepl" "cider-nrepl.plugin"]
                            :sdk-path "/home/fvaresi/opt/android-sdk-linux"}}}
