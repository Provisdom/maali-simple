(defproject provisdom/simple "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/spec.alpha "0.1.143"]
                 [org.clojure/core.async "0.3.465"]
                 [org.clojure/test.check "0.9.0"]
                 [rum "0.11.0"]
                 [thheller/shadow-cljs "2.2.1"]
                 [proto-repl "0.3.1"]
                 [provisdom/maali "0.0.1-SNAPSHOT"]]
  :repositories [["clojars" {:url "https://repo.clojars.org/"}] ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths ["src" "test" "resources"]
  :clean-targets ^{:protect false} [:target-path "out" "resources/public/cljs"]
  :cljsbuild {:builds [{:id           "dev"
                        :source-paths ["src" "test" "dev"]
                        :dependencies [thheller/shadow-cljs "2.1.28"]
                        :compiler     {:main "provisdom.simple.app"
                                       :asset-path "cljs/out"
                                       :output-to  "resources/public/cljs/main.js"
                                       :output-dir "resources/public/cljs/out"
                                       :verbose false
                                       :parallel-build true}}]})
