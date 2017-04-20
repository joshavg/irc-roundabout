(ns irc-roundabout.parser-test
  (:require [clojure.test :refer :all]
            [irc-roundabout.parser :refer :all]))

(deftest test-fetch-payload
  (let [ar-line    ":header :payload"
        valid-line ":hybrid7.debian.local 372 leetbot :- -- Aurélien GÉRÔME <ag@roxor.cx>"]
    (testing "returns payload for artificial input"
             (is
               (= "payload"
                  (fetch-payload ar-line (split-response (normalize-line ar-line))))))
    (testing "returns payload for valid input"
             (is
               (= "- -- Aurélien GÉRÔME <ag@roxor.cx>"
                  (fetch-payload valid-line (split-response (normalize-line valid-line))))))
    (testing "returns nil for invalid input"
             (is (nil? (fetch-payload "dingel" (split-response "dingel")))))))

(deftest test-parse-response
  (testing "mode response is recognized"
           (let [parsed (parse-response ":nleetbot!leetbot@i.love.debian.org MODE leetbot :+i")]
             (is (= "MODE" (:cmd parsed)))
             (is (= "+i" (:payload parsed)))
             (is (= "nleetbot" (:nick parsed)))
             (is (= "leetbot" (:user parsed)))
             (is (= "i.love.debian.org" (:server parsed)))
             (is (= "leetbot" (:target parsed)))
             (is (nil? (:meta parsed)))))
  (testing "join response is recognized"
           (let [parsed (parse-response ":leetbot!leetbot@i.love.debian.org JOIN :#bots")]
             (is (= "#bots" (:payload parsed)))
             (is (= "#bots" (:channel parsed)))
             (is (= "#bots" (:respond_to parsed)))))
  (testing "ping is recognized"
           (let [parsed (parse-response "PING :123payload")]
             (is (= "123payload" (:payload parsed)))
             (is (= "PING" (:cmd parsed))))))
