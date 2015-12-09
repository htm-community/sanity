;; Example start command:
;; lein run -m clojure.main repl.clj

(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.repl.browser)

(cljs.repl/repl (cljs.repl.browser/repl-env)
  :watch "src"
  :output-dir "log")
