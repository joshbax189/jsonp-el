;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'jsonp)
(require 'el-mock)

;;; jsonp-resolve
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

(ert-deftest jsonp-resolve/test-nil-object ()
  "Should error when resolving in an empty object."
  (should-error (jsonp-resolve nil "/foo/bar")))

(ert-deftest jsonp-resolve/test-nil-object-nested ()
  "Should return an empty object."
  (let ((example (json-parse-string "{\"a\": {}}" :object-type 'alist)))
    (should (equal (jsonp-resolve example "/a") nil))))

(ert-deftest jsonp-resolve/test-pointer-prefix ()
  "Should error if the pointer has more tokens."
  (let ((example (json-parse-string "{\"a\": {\"b\": [{\"c\": 1}, {\"c\": 2}]}, \"d\": \"value\"}")))
    (should-error (jsonp-resolve example "/a/b/0/c/e"))))

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

(ert-deftest jsonp-resolve/test-strings-are-primitive ()
  "Should not index inside a string."
  (let ((example (json-parse-string "{\"arr\": \"foo\"}")))
    ;; should not treat the string as an array
    (should-error (jsonp-resolve example "/arr/0"))))

(ert-deftest jsonp-resolve/test-strings-symbol-compare ()
  "Should resolve when symbols are used in object."
  (let ((example '((key . "value"))))
    ;; should not treat the string as an array
    (should (equal (jsonp-resolve example "/key") "value"))))

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

(ert-deftest jsonp-resolve/test-long-pointer ()
  "Test array indexed by numeric string."
  (let* (;; circular list
        (example (list (cons 'a nil))))
    (setf (cdr (car example)) example)
    (should (equal (jsonp-resolve example "/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a") example))))

(ert-deftest jsonp-resolve/test-escaping ()
  "Resolves pointers including escape sequences."
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
    (should (equal (jsonp-resolve example "")           example))
    (should (equal (jsonp-resolve example "/foo")       ["bar" "baz"]))
    (should (equal (jsonp-resolve example "/foo/0")     "bar"))
    (should (equal (jsonp-resolve example "/")          0))
    (should (equal (jsonp-resolve example "/a~1b")      1))
    (should (equal (jsonp-resolve example "/c%d")       2))
    (should (equal (jsonp-resolve example "/e^f")       3))
    (should (equal (jsonp-resolve example "/g|h")       4))
    (should (equal (jsonp-resolve example "/i\\j")      5))
    (should (equal (jsonp-resolve example "/k\"l")      6))
    (should (equal (jsonp-resolve example "/ ")         7))
    (should (equal (jsonp-resolve example "/m~0n")      8))))

(ert-deftest jsonp-resolve/test-uri-escaping ()
  "Resolves pointers including escape sequences."
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
    (should (equal (jsonp-resolve example "#")            example))
    (should (equal (jsonp-resolve example "#/foo")        ["bar" "baz"]))
    (should (equal (jsonp-resolve example "#/foo/0")      "bar"))
    (should (equal (jsonp-resolve example "#/")           0))
    (should (equal (jsonp-resolve example "#/a~1b")       1))
    (should (equal (jsonp-resolve example "#/c%25d")      2))
    (should (equal (jsonp-resolve example "#/e%5Ef")      3))
    (should (equal (jsonp-resolve example "#/g%7Ch")      4))
    (should (equal (jsonp-resolve example "#/i%5Cj")      5))
    (should (equal (jsonp-resolve example "#/k%22l")      6))
    (should (equal (jsonp-resolve example "#/%20")        7))
    (should (equal (jsonp-resolve example "#/m~0n")       8))))

;;; Other
(ert-deftest jsonp/test-json-read-string-number-keys ()
  "Tests default behavior of `json-read-from-string'."
  (let ((result (json-read-from-string "{ \"1\": \"a\", \"3\": \"b\" }")))
    (should (equal (type-of (car (car result)))
                   'symbol))))

(ert-deftest jsonp/test-json-parse-string-number-keys ()
  "Tests default behavior of `json-parse-string'."
  (let ((result (json-parse-string "{ \"1\": \"a\", \"3\": \"b\" }")))
    (should (equal (type-of (car (car (map-pairs result))))
                   'string))))

;;; jsonp-resolve-remote
(ert-deftest jsonp-resolve-remote/test-absolute-uri ()
  "Test resolving from an absolute URI."
  (with-mock
    (stub jsonp--url-retrieve-default => "{\"key\": \"value\"}")
    (let ((result (jsonp-resolve-remote jsonp-remote-default "http://example.com/#/key")))
      (should (equal result "value")))))

(ert-deftest jsonp-resolve-remote/test-absolute-uri-with-custom-parser ()
  "Test resolving from an absolute URI with a custom JSON parser."
  (with-mock
    (mock (jsonp--url-retrieve-default *) => "{\"key\": \"value\"}")
    (mock (json-parse-string *) => '((key . "value")))
    (let* ((remote (jsonp-remote :json-parser #'json-parse-string))
           (result (jsonp-resolve-remote remote "http://example.com/")))
      (should (listp result)))))

(ert-deftest jsonp-resolve-remote/test-absolute-uri-whitelist ()
  "Test whitelisting an absolute URI."
  (with-mock
    (stub jsonp--url-retrieve-default => "{\"key\": \"value\"}")
    (let* ((remote (jsonp-remote :whitelist '("http://example\\.com/")))
           (result (jsonp-resolve-remote remote "http://example.com/#/key")))
      (should (equal result "value")))))

(ert-deftest jsonp-resolve-remote/test-absolute-uri-whitelist-fail ()
  "Test whitelisting an absolute URI that fails."
  (let ((remote (jsonp-remote :whitelist '("https://example\\.com/"))))
    (should-error (jsonp-resolve-remote remote "http://example.com/#/key"))))

(ert-deftest jsonp-resolve-remote/test-bare-fragment ()
  "Resolving a fragment with no object or uri should error."
    (should-error (jsonp-resolve-remote jsonp-remote-default "#/key")))

(ert-deftest jsonp-resolve-remote/test-pointer-uri-escaping ()
  "Test pointers are properly unescaped before resolving."
    (with-mock
      (mock (jsonp--url-retrieve-default "http://example.com/#/foo%20bar") => "{\"foo bar\": \"value\"}")
      (let ((result (jsonp-resolve-remote jsonp-remote-default "http://example.com/#/foo%20bar")))
        (should (equal result "value")))))

;;; jsonp-replace-refs
(ert-deftest jsonp-replace-refs/test ()
  "Should work on petstore example."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (should (equal
             (jsonp-replace-refs json)
             '(("api_key" . (("x" . 1) ("y" . 2))) ("keys" . (("foo" . (("x" . 1) ("y" . 2))))))))))

(ert-deftest jsonp-replace-refs/test-max-depth-0 ()
  "Should not replace any $ref when max-depth is 0."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (should (equal
             (jsonp-replace-refs json nil 0)
             json))))

(ert-deftest jsonp-replace-refs/test-max-depth-1 ()
  "Should not replace any $ref beyond limit."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": { \"$ref\": \"#/keys/foo/y\" },
      \"y\": 2
    }
  }
}" :object-type 'alist)))
    (message "%S" json)
    (should (equal
             (jsonp-replace-refs json nil 1)
             ;; TODO not quite right
             ;; replacement should not depend on key order...
             '((api_key (x ($ref . "#/keys/foo/y")) (y . 2)) (keys (foo (x . 2) (y . 2))))))))

(ert-deftest jsonp-replace-refs/test-max-depth-2 ()
  "Should not replace any $ref beyond limit."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": { \"$ref\": \"#/keys/foo/y\" },
      \"y\": 2
    }
  }
}" :object-type 'alist)))
    (message "%S" json)
    (should (equal
             (jsonp-replace-refs json nil 2)
             '((api_key (x . 2) (y . 2)) (keys (foo (x . 2) (y . 2))))))))

(ert-deftest jsonp-replace-refs/test-symbols ()
  "Should replace $ref as a symbol too."
  (let ((json (json-read-from-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}")))
    (should (equal
             (jsonp-replace-refs json)
             '((api_key . ((x . 1) (y . 2))) (keys . ((foo . ((x . 1) (y . 2))))))))))

(ert-deftest jsonp-replace-refs/test-remote ()
  "Should look up remote pointer."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"https://example.com/foo/bar/baz#/Bam\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (with-mock
      (mock (jsonp--url-retrieve-default "https://example.com/foo/bar/baz#/Bam") => "{ \"Bam\": \"fizz\" }")
      (should (equal
               (jsonp-replace-refs json json nil jsonp-remote-default)
               '(("api_key" . "fizz") ("keys" . (("foo" . (("x" . 1) ("y" . 2)))))))))))

(ert-deftest jsonp-replace-refs/test-remote-relative-uri ()
  "Should look up remote pointer when using a relative URI."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"./foo/bar/baz#/Bam\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": 1,
      \"y\": 2
    }
  }
}" :object-type 'hash-table)))
    (with-mock
      (mock (jsonp--url-retrieve-default "https://example.com/foo/bar/baz#/Bam") => "{ \"Bam\": \"fizz\" }")
      (should (equal
               (jsonp-replace-refs json json nil jsonp-remote-default "https://example.com")
               '(("api_key" . "fizz") ("keys" . (("foo" . (("x" . 1) ("y" . 2)))))))))))

;;; jsonp-nested-elt
(ert-deftest jsonp-nested-elt/test ()
  "Should traverse a local $ref."
  (let ((json (json-parse-string "{
  \"api_key\": {
    \"$ref\": \"#/keys/foo\"
  },
  \"keys\": {
    \"foo\": {
      \"x\": { \"$ref\": \"#/keys/foo/y\" },
      \"y\": 2
    }
  }
}" :object-type 'alist)))
    ;; TODO hmm what should happen here?
    ;; (should (equal
    ;;          (jsonp-nested-elt json '(keys foo x))
    ;;          2))
    (should (equal
             (jsonp-nested-elt json '("keys" "foo" "y"))
             2))
    (should (equal
             (jsonp-nested-elt json '(api_key y))
             2))))

(ert-deftest jsonp-nested-elt/primitives ()
  "Should match `map-elt' behavior."
  (should (equal
           (jsonp-nested-elt nil '("keys" "foo" "y"))
           nil))
  (should (equal
           (jsonp-nested-elt 1 '("keys" "foo" "y"))
           nil))
  (should (equal
           (jsonp-nested-elt '((a . 1)) nil)
           '((a . 1))))
  (should (equal
           (jsonp-nested-elt "foobar" '(1))
           nil))
  (should (equal
           (jsonp-nested-elt (vector 'a 'b) (list 1))
           'b)))
