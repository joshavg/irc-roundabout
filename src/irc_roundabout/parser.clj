(ns irc-roundabout.parser
  (:require [clojure.string :refer [split starts-with?]]))

(defn nvl [v1 v2]
  (if v1 v1 v2))

(defn normalize-line [line]
  (if (starts-with? line ":")
      (str " " line)
      (str " :" line)))

(defn split-response [line]
  (split line #"\s:"))

(defn fetch-payload
  [line splits]
  (when (> (count splits) 2)
        (let [length-header (count (second splits))]
          (subs line (+ 3 length-header)))))

(def usr-pattern #"^([^!]+)!([^@]+)@(.+)$")

(defn fetch-usr-values [user-header]
  (if-let [parts (re-find usr-pattern user-header)]
    {:nick   (second parts)
     :user   (nth parts 2)
     :server (last parts)}
    {:nick nil :user nil :server nil}))

(defn fetch-server [header-split]
  (when-not (= 1 (count header-split))
            (first header-split)))

(defn fetch-command [header-split]
  (if (= 1 (count header-split))
      (first header-split)
      (second header-split)))

(defn fetch-target [header-split]
  (when (> (count header-split) 2)
        (nth header-split 2)))

(defn fetch-meta [header header-split]
  (if (> (count header-split) 3)
      (let [first-length  (count (first header-split))
            second-length (count (second header-split))
            third-length  (count (nth header-split 2))]
        (subs header (+ first-length second-length third-length 4)))))

(defn process-line [line split-line]
  (let [header        (second split-line)
        header-split  (split header #" ")

        payload       (fetch-payload line split-line)
        usr-values    (fetch-usr-values (first header-split))
        header-server (fetch-server header-split)
        cmd           (fetch-command header-split)
        target        (fetch-target header-split)
        meta          (fetch-meta header header-split)]
    {:payload payload
     :nick    (:nick usr-values)
     :user    (:user usr-values)
     :server  (nvl (:server usr-values) header-server)
     :cmd     cmd
     :target  target
     :meta    meta}))

(defn postprocess-privmsg [parsed]
  (let [target (:target parsed)
        nicked (assoc parsed :sender (:nick parsed))]
    (if (starts-with? target "#")
        (assoc parsed :respond_to (:target parsed))
        (assoc parsed :respond_to (:nick parsed)))))

(def postprocess-steps
  {"JOIN"    #(assoc % :channel (:payload %) :respond_to (:payload %))
   "PRIVMSG" #(postprocess-privmsg %)
   "PART"    #(assoc % :channel (:target %))
   "353"     #(assoc % :channel (second (:meta %)))
   "366"     #(assoc % :channel (:meta %))})

(defn postprocess [parsed]
  (if-let [step (get postprocess-steps (:cmd parsed))]
    (step parsed)
    parsed))

(defn parse-response [line]
  (if (starts-with? line "PING :")
      {:cmd "PING" :payload (subs line 6)}
      (->> line
           (normalize-line)
           (split-response)
           (process-line line)
           (postprocess))))
