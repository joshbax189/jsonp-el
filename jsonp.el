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
    (let* ((parts (split-string pointer "/"))
           (parts (if (string-equal (car parts) "") (cdr parts) parts)) ; Remove leading empty string
           (current-object object))
      (while (and parts current-object)
        (let ((part (jsonp--unescape-token (car parts))))
          (cond
           ;; NOTE According to spec this should error if there are duplicate keys in the object.
           ;;      Due to the variety of ways JSON can be decoded in elisp, this is hard to enforce.
           ((and (mapp current-object) (or (map-contains-key current-object part)
                                            (map-contains-key current-object (intern part))))
            ;; covers props represented as a symbol, or string
            (let ((key (cond ((map-contains-key current-object part) part)
                             ((map-contains-key current-object (intern part)) (intern part))
                             (t (error "Unreachable!")))))
              (setq current-object (map-elt current-object key))))
           ((and (string-match "^[0-9]+$" part)
                 (or (vectorp current-object)
                     ;; careful, if arrays serialize to lists then you can't distinguish arrays and objects
                     ;; e.g. [[0,1], 2]
                     (listp current-object)))
            (let ((index (string-to-number part)))
              (if (and (>= index 0) (< index (length current-object)))
                  (setq current-object (seq-elt current-object index))
                (error "JSON pointer index out of bounds: %s" part))))
           (t
            (error "JSON pointer key not found: %s" part)))
          (setq parts (cdr parts))))
      (if parts
          (error "Could not resolve all parts of JSON pointer")
        current-object))))

(defun jsonp--unescape-token (token)
  "Replace the special sequences ~1 and ~0 in TOKEN string."
  (let* ((token-1 (string-replace "~1" "/" token))
         (token-2 (string-replace "~0" "~" token-1)))
    token-2))

(defun jsonp-resolve-safe (object pointer)
  "Resolve a JSON pointer, returning nil if it cannot be resolved.
OBJECT is the parsed JSON object.
POINTER is the string JSON pointer.
Returns the value at the pointer or nil if not resolved."
  (condition-case nil
      (jsonp-resolve object pointer)
    (error nil)))

(defun jsonp-resolve-remote (uri &optional whitelist json-parse-function)
  "Resolve a JSON pointer from a URI.

URI should be an absolute URI (downloads and parses JSON), then
resolves any path fragment as a JSON pointer.

WHITELIST, if provided, is a list of allowed URI patterns (regexp).
Raises an error if the URI does not match any pattern.

JSON-PARSE-FUNCTION, if provided, is the function to use for parsing
the downloaded JSON (defaults to `json-read-from-string')."
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
    (jsonp-resolve json-object pointer)))

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
