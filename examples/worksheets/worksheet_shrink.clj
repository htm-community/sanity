;; gorilla-repl.fileformat = 1

;; **
;;; # Shrink your worksheet
;; **

;; @@
(ns comportexviz.worksheet-shrink
  (:require [clojure.data.codec.base64 :as base64]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:use [clojure.pprint]
        [clojure.stacktrace]))

(org.nfrac.comportex.repl/truncate-large-data-structures)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>"}
;; <=

;; **
;;; Extract any inline SVGs and inline PNGs from the worksheet, put them in files, and link to them in a new copy of the worksheet.
;; **

;; @@
;; Be careful! This will happily destroy your work if you tell it to.

(let [in-path "examples/worksheets/hello.clj"
      worksheet-out-filename "hello.faster.clj"
      
      date-dir (.format (java.text.SimpleDateFormat. "yyyy.MM.dd.HH.mm.ss") (java.util.Date.))
      out-dir (str "examples/worksheets/" date-dir)
      images-subfolder date-dir

      file-count (atom 0)
      worksheet-out-path (format "%s/%s" out-dir worksheet-out-filename)]
  (io/make-parents worksheet-out-path)
  (spit worksheet-out-path
        (-> (slurp in-path)
            (string/replace #"<svg(.*?)</svg>"
                            (fn [[_ innards]]
                              (let [contents (str "<svg version=\"1.1\" "
                                                  "xmlns=\"http://www.w3.org/2000/svg\" "
                                                  "xmlns:xlink=\"http://www.w3.org/1999/xlink\" "
                                                  (-> innards
                                                      (string/replace "\\\"" "\""))
                                                  "</svg>")
                                    img-filename (str (swap! file-count inc) ".svg")
                                    img-web-path (format "%s/%s" images-subfolder img-filename)
                                    img-file-path (format "%s/%s/%s"
                                                          out-dir images-subfolder img-filename)]
                                (io/make-parents img-file-path)
                                (spit img-file-path
                                      contents)
                                (format "<img src='%s' />" img-web-path))))
            (string/replace #"<img src='data\:image/png;base64,(.*?)' />"
                            (fn [[_ base64-str]]
                              (let [img-filename (str (swap! file-count inc) ".png")
                                    img-web-path (format "%s/%s" images-subfolder img-filename)
                                    img-file-path (format "%s/%s/%s"
                                                          out-dir images-subfolder img-filename)]
                                (io/make-parents img-file-path)
                                (with-open [out (io/output-stream
                                                  (io/file img-file-path))]
                                  (.write out (base64/decode (.getBytes base64-str))))
                                (format "<img src='%s' />" img-web-path))))))
  (println "Wrote to" worksheet-out-path))
;; @@
