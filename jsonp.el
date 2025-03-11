;;; jsonp.el --- Resolve JSON pointers -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.0.1
;; Package-Requires: (dependencies)
;; Homepage: github.com/joshbax189/jsonp
;; Keywords: programming


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Resolve JSON pointers locally and remotely.
;; See https://datatracker.ietf.org/doc/html/rfc6901

;;; Code:

(require 'map)
(require 'url)
(require 'json)

(defun jsonp-resolve (object pointer)
  "Resolve a JSON pointer in a parsed JSON object.

OBJECT is the parsed JSON object (e.g., the result of `json-read').
POINTER is a string representing the JSON pointer (e.g., \"/a/b/c\").
Backslashes are not removed from POINTER.

Returns the value at the given pointer, or signals an error if the
pointer is invalid or the value does not exist.  Returns OBJECT
itself if POINTER is an empty string.

If the pointer begins with a #, it is treated as a URL fragment and
first decoded before further resolving."
  (when (or (string-prefix-p "#/" pointer)
            (string-equal "#" pointer))
    (setq pointer (url-unhex-string (string-remove-prefix "#" pointer))))

  (if (or (not pointer)
          (string-equal pointer ""))
      object
    (unless (string-prefix-p "/" pointer)
      (error "Malformed JSON pointer %s" pointer))
    (let ((parts (split-string pointer "/")))
      (if (string-equal (car parts) "") ; Remove leading empty string
          (setq parts (cdr parts)))
      (jsonp-resolve-recursive object parts))))

(defun jsonp--unescape-token (token)
  "Replace the special sequences ~1 and ~0 in TOKEN string."
  (let* ((token-1 (string-replace "~1" "/" token))
         (token-2 (string-replace "~0" "~" token-1)))
    token-2))

;; TODO convert to while, see swelter--resolve-json-ref
(defun jsonp-resolve-recursive (object parts)
  "Recursive helper function for `jsonp-resolve'.
OBJECT is the parsed JSON object (e.g., the result of `json-read').
PARTS is a list of tokens from the pointer."
  (if (null parts)
      object
    (let ((part (jsonp--unescape-token (car parts))))
      (cond
       ;; NOTE According to spec this should error if there are
       ;;      duplicate keys in the object.
       ;;      Due to the variety of ways JSON can be decoded in
       ;;      elisp, this is hard to enforce.
       ((and (mapp object) (or (map-contains-key object part)
                               (map-contains-key object (intern part))))
        ;; covers props represented as a symbol or string
        (let ((key (cond ((map-contains-key object part) part)
                         ((map-contains-key object (intern part))
                          (intern part))
                         (t (error "Unreachable!")))))
          (jsonp-resolve-recursive (map-elt object key) (cdr parts))))
       ((and (string-match "^[0-9]+$" part)
             (or (vectorp object)
                 ;; careful, if arrays serialize to lists then you
                 ;; can't distinguish arrays and objects
                 ;; e.g. [[0,1], 2]
                 (listp object)))
        (let ((index (string-to-number part)))
          (if (and (>= index 0) (< index (length object)))
              (jsonp-resolve-recursive (seq-elt object index) (cdr parts))
            (error "JSON pointer index out of bounds: %s" part))))
       (t
        (error "JSON pointer key not found: %s" part))))))

(defun jsonp-resolve-safe (object pointer)
  "Resolve a JSON pointer, returning nil if it cannot be resolved.
OBJECT is the parsed JSON object.
POINTER is the string JSON pointer.
Returns the value at the pointer or nil if not resolved."
  (condition-case nil
      (jsonp-resolve object pointer)
    (error nil)))

;; TODO two uses for this:
;;      1. resolving a URI with no object, e.g. jsonp-open-uri
;;      2. following a ref in a document

(defun jsonp-resolve-remote (uri &optional base-uri whitelist json-parse-function)
  "Resolve a JSON pointer from a URI.

URI can be an absolute URI (downloads and parses JSON), a relative
URI (expanded using BASE-URI), or a path fragment URI (delegates to
`jsonp-resolve`).

BASE-URI, if provided, is used to expand relative URIs.

WHITELIST, if provided, is a list of allowed URI patterns (regexp).
Raises an error if the URI does not match any pattern.

JSON-PARSE-FUNCTION, if provided, is the function to use for parsing
the downloaded JSON (defaults to `json-read-from-string')."
  (when base-uri
    (setq base-uri (string-remove-suffix "/" base-uri)))
  (cond
   ;; Path fragment URI
   ;; TODO is this helpful?
   ((and (or (null base-uri) (string-empty-p base-uri))
         (string-match "^#" uri))
    (error "Cannot resolve json pointers with only path fragments without OBJECT"))
   ;; TODO cannot call with uri #/foo/bar and base-uri http://example.com/ -- correct?
   ((and base-uri (string-match "^/" uri)) ;; Relative URI with base URI
    (let ((absolute-uri (concat base-uri uri)))
      (jsonp-resolve-remote absolute-uri nil whitelist json-parse-function)))
   (t ;; Absolute URI
    (when whitelist
      (unless (seq-some (lambda (pattern) (string-match pattern uri))
                        whitelist)
        (error "URI not in whitelist: %s" uri)))
    (let* ((parsed-uri (url-generic-parse-url uri))
           (pointer (url-target parsed-uri))
           (pointer (url-unhex-string pointer))
           (parse-fn (or json-parse-function 'json-read-from-string))
           ;; assume host will ignore fragment specifier here
           (json-object (jsonp--url-retrieve uri parse-fn)))
      (jsonp-resolve json-object pointer)))))

(defun jsonp--url-retrieve (url json-parse-function)
  "Default fetch function.
Fetch URL and call JSON-PARSE-FUNCTION with response body as a string."
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (while (looking-at "^.") (forward-line))
    (forward-line)
    (funcall json-parse-function (buffer-substring (point) (point-max)))))

(provide 'jsonp)
;;; jsonp.el ends here
