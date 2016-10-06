(ns codekata13.core
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))

(defparser eol []
  (either (char \newline) (eof)))

(defparser any-string []
  (many (any-char)))

(defparser loc-parser []
  (let->> [line (either (>> (string "\\\\") (any-string) (eol))
                        (>> (any-string) (eol)))]

    (do (println (take 2 line))
    (if (= (apply str (take 2 line)) "\\\\")
      (always 0)
      (always 1)))))

(defn count-lines
  [lines-of-file]
  (run (loc-parser) lines-of-file))

(defn get-file
  [filename]
  (slurp filename))

(defn -main
  "Codekata 13"
  [& args]
  (->>
    (first args)
    get-file
    count-lines
    println))
