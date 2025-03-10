(require 'ert)
(require 'jsonp)

(ert-deftest jsonp-resolve/test-nested-object ()
  "Resolves a value in a nested object structure."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}"))
        (expected 1))
    (should (equal (jsonp-resolve example "/a/b/0/c") expected))))

(ert-deftest jsonp-resolve/test-nested-object-2 ()
  "Resolves a different value in the same nested object structure."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}"))
        (expected 2))
    (should (equal (jsonp-resolve example "/a/b/1/c") expected))))

(ert-deftest jsonp-resolve/test-simple-value ()
  "Resolves a simple value directly within the root object."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}"))
        (expected "value"))
    (should (equal (jsonp-resolve example "/d") expected))))

(ert-deftest jsonp-resolve/test-nonexistent-pointer ()
  "Returns nil when the pointer does not exist."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should-error (jsonp-resolve example "/nonexistent"))))

(ert-deftest jsonp-resolve/test-empty-pointer ()
  "Returns the entire object when the pointer is empty."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should (equal (jsonp-resolve example "") example))))

(ert-deftest jsonp-resolve/test-nil-pointer ()
  "Returns the entire object when the pointer is nil."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should (equal (jsonp-resolve example nil) example))))

(ert-deftest jsonp-resolve/test-uri-fragment ()
  "Should allow a # prefix in pointer."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should (equal (jsonp-resolve example "#/a/b/0/c") 1))))

(ert-deftest jsonp-resolve/test-malformed ()
  "Should require a leading forward slash."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should-error (jsonp-resolve example "a/b/0"))
    (should-error (jsonp-resolve example "foo/a/b/0"))))

(ert-deftest jsonp-resolve/test-numeric-index ()
  "Resolves a value using a numeric index in an array."
  (let ((example (json-parse-string "{\"arr\": [10, 20, 30]}"))
        (expected 20))
    (should (equal (jsonp-resolve example "/arr/1") expected))))

(ert-deftest jsonp-resolve/test-out-of-bounds-index ()
    "Returns nil when the index is out of bounds."
    (let ((example (json-parse-string "{\"arr\": [10, 20, 30]}")))
      (should-error (jsonp-resolve example "/arr/3"))))

(ert-deftest jsonp-resolve/test-array-hyphen ()
    "Returns nil when the index is out of bounds."
    (let ((example (json-parse-string "{\"arr\": [10, 20, 30]}")))
      (should-error (jsonp-resolve example "/arr/-"))))

(ert-deftest jsonp-resolve/test-leading-slash ()
  "Handles pointers with a leading slash."
  (let ((example (json-parse-string "{\"a\": 1}")))
    (should-error (jsonp-resolve example "//a"))) ;; Double slash is treated as missing object.
  (let ((example (json-parse-string "{\"a\": 1}")))
    (should (equal (jsonp-resolve example "/a") 1))))

(ert-deftest jsonp-resolve/test-numeric-string-key ()
  "Resolves a value using a numeric string as a key in an object."
  (let ((example (json-parse-string "{\"123\": \"value\"}"))
        (expected "value"))
    (should (equal (jsonp-resolve example "/123") expected))))

;; TODO json-read-from-string converts numeric keys into numbers!
(ert-deftest jsonp-resolve/test-numeric-string-key-2 ()
  "Resolves a value using a numeric string as a key in an object."
  (let ((example (json-read-from-string "{\"123\": \"value\"}"))
        (expected "value"))
    (should (equal (jsonp-resolve example "/123") expected))))

(ert-deftest jsonp-resolve/test-nested-numeric-string-key ()
  "Resolves a value nested under a numeric string key."
  (let ((example (json-parse-string "{\"obj\": {\"123\": \"value\"}}"))
        (expected "value"))
    (should (equal (jsonp-resolve example "/obj/123") expected))))

(ert-deftest jsonp-resolve/test-numeric-string-and-regular-key ()
  "Resolves values with a mix of numeric string and regular keys."
  (let ((example (json-parse-string "{\"123\": {\"a\": \"value1\", \"456\": \"value2\"}}"))
        (expected "value1"))
    (should (equal (jsonp-resolve example "/123/a") expected)))
  (let ((example (json-parse-string "{\"123\": {\"a\": \"value1\", \"456\": \"value2\"}}"))
        (expected "value2"))
    (should (equal (jsonp-resolve example "/123/456") expected))))

(ert-deftest jsonp-resolve/test-numeric-string-array-index ()
  "Test array indexed by numeric string."
  (let ((example (json-parse-string "{\"arr\": {\"1\": \"value1\", \"2\": \"value2\"}}"))
        (expected "value1"))
    (should (equal (jsonp-resolve example "/arr/1") expected))))

;; TODO
(ert-deftest jsonp-resolve/test-non-unique ()
  "Non-unique names should error."
  (let ((example (json-parse-string "{\"a\": 1, \"a\": 2}")))
    (should-error (jsonp-resolve example "/a"))))

(ert-deftest jsonp-resolve/test-escaping ()
  "Resolves pointers including escape sequences ~0 and ~1."
  (let ((example (json-parse-string "{
      \"foo\": [\"bar\", \"baz\"],
      \"\": 0,
      \"a/b\": 1,
      \"c%d\": 2,
      \"e^f\": 3,
      \"g|h\": 4,
      \"i\\\\j\": 5,
      \"k\\\"l\": 6,
      \" \": 7,
      \"m~n\": 8
   }")))
    (should (equal (jsonp-resolve example "/m~0n") 8))
    (should (equal (jsonp-resolve example "/a~1b") 1))))

(ert-deftest jsonp-resolve/test-json-escaping ()
  "Resolves pointers including JSON escaped chars."
  (let ((example (json-parse-string "{
      \"foo\": [\"bar\", \"baz\"],
      \"\": 0,
      \"a/b\": 1,
      \"c%d\": 2,
      \"e^f\": 3,
      \"g|h\": 4,
      \"i\\\\j\": 5,
      \"k\\\"l\": 6,
      \" \": 7,
      \"m~n\": 8
   }")))
    (should (equal (jsonp-resolve example "/k\"l") 6))
    (should (equal (jsonp-resolve example "/i\\j") 5))))
