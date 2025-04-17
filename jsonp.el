;;; jsonp.el --- Resolve JSON pointers -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/joshbax189/jsonp
;; Keywords: comm, tools


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
    (if (string-prefix-p "/" pointer)
        ;; Remove leading empty string
        (setq pointer (string-remove-prefix "/" pointer))
      (error "Malformed JSON pointer %s" pointer))
    (let ((parts (split-string pointer "/"))
          (current-object object))
      (while (and parts current-object)
        (let ((part (jsonp--unescape-token (car parts))))
          (cond
           ;; NOTE According to spec this should error if there are duplicate keys in the object.
           ;;      Due to the variety of ways JSON can be decoded in elisp, this is hard to enforce.
           ((and (mapp current-object)
                 ;; covers props represented as a symbol, or string
                 (when-let* ((key (jsonp--map-contains-key current-object part)))
                   (setq current-object (map-elt current-object key))
                   ;; return t in case the returned value is nil
                   t )))
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

(defun jsonp--map-contains-key (obj key)
  "Check if OBJ contains KEY either as a string or symbol.
Returns the key as the correct type if present, nil otherwise."
  (cond
   ((map-contains-key obj key) key)
   ((map-contains-key obj (intern key)) (intern key))
   (t nil)))

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

(defun jsonp-expand-relative-uri (uri base-uri)
  "Expands URI relative to BASE-URI, removing any dot components.
Assumes no query component in either URI.

For example
  (jsonp-expand-relative-uri
    \"../foo/bar#baz\"
    \"https://example.com/something\")
yields
  \"https://example.com/foo/bar#baz\""
  (let* ((base-parsed (url-generic-parse-url base-uri))
         (uri-parsed (url-generic-parse-url uri))
         (joined-paths (concat (url-filename base-parsed) "/" (url-filename uri-parsed)))
         (path-segments (split-string joined-paths "/"))
         (path-result nil))
    (when (cdr (url-path-and-query base-parsed))
      (error "Cannot merge URIs with query %s" base-uri))
    (when (cdr (url-path-and-query uri-parsed))
      (error "Cannot merge URIs with query %s" uri))
    ;; merge dot paths
    (while path-segments
      (let ((next (car path-segments)))
        (setq path-segments (cdr path-segments))
        (pcase next
          ((or "" "."))
          (".." (setq path-result (cdr path-result)))
          (_ (push next path-result)))))
    (setq path-result (concat "/" (string-join (reverse path-result) "/")))

    (setf (url-filename base-parsed) path-result
          (url-target base-parsed) (url-target uri-parsed))
    (url-recreate-url base-parsed)))

(defun jsonp-resolve-remote (uri &optional whitelist json-parse-function base-uri)
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

(defun jsonp-nested-elt (json-obj keys &optional allow-remote)
  "Get an element from JSON-OBJ traversing $refs and following KEYS.
Unlike other functions, it returns nil if some key in KEYS fails to match.

KEYS should be a list of string, symbol or number.  Strings will match symbol
keys and vice-versa, but numbers must be used to index arrays.
ALLOW-REMOTE if non-nil will resolve $ref by downloading URIs."
  (let ((root-obj json-obj)
        val)

    (cl-block nil
      ;; edge cases
      (unless keys
        (cl-return json-obj))

      (if (or (not (mapp json-obj))
              (stringp json-obj))
          (cl-return nil))

      (while keys
        (let* ((key (car keys))
               (key (if (numberp key)
                        key
                      (jsonp--map-contains-key json-obj key))))
          (setq keys (cdr keys)
                val (map-elt json-obj key))
          (cond
           ;; primitive values
           ((or (not (mapp val))
                (stringp val))
            (if keys
                (cl-return nil)
              (cl-return val)))
           ;; arrays
           ((vectorp val)
            ;; recurse
            (setq json-obj val))
           ;; objects
           (t
            (if-let* ((ref-key (jsonp--map-contains-key val "$ref"))
                      (ref-string (map-elt val ref-key))
                      (new-val (if (string-prefix-p "#" ref-string)
                                   (jsonp-resolve root-obj ref-string)
                                 ;; TODO might be relative urls too, should provide a base-uri?
                                 ;; TODO pass whitelist and json-parse-function
                                 (if allow-remote
                                     (jsonp-resolve-remote ref-string)
                                   (cl-return nil)))))
                (setq json-obj new-val)
              ;; otherwise recurse into a regular object
              (setq json-obj val))))))
      (if keys
          (cl-return nil)
        (cl-return val)))))

;; NOTE: json-schema explicitly disallows $refs from referring to other $refs
(defun jsonp-replace-refs (json-obj &optional root-obj max-depth allow-remote)
  "Given parsed JSON-OBJ expand any $ref.

Fragments are resolved in ROOT-OBJ, remote URIs are only resolved
if ALLOW-REMOTE is non-nil.
MAX-DEPTH is the maximum recursion depth when expanding refs.
Default is 10."
  (setq root-obj (or root-obj json-obj)
        max-depth (or max-depth 10))
  ;; TODO this always returns an alist, so it will coerce different types of parsed objects
  (map-apply
   (lambda (key val)
     (cond
      ;; primitive values
      ((or (not (mapp val))
           (stringp val))
       (cons key val))
      ;; arrays
      ((vectorp val)
       ;; recurse and rebuild vector
       (cons key (apply #'vector (map-values (jsonp-replace-refs val root-obj max-depth allow-remote)))))
      ;; objects
      (t
       (if-let* ((ref-key (jsonp--map-contains-key val "$ref"))
                 (ref-string (map-elt val ref-key))
                 (new-val (if (string-prefix-p "#" ref-string)
                              (jsonp-resolve root-obj ref-string)
                            ;; TODO might be relative urls too
                            ;; TODO pass whitelist and json-parse-function
                            (if allow-remote
                                (jsonp-resolve-remote ref-string)
                              (error "Bad JSON $ref %s" ref-string)))))
           (if (or (not (mapp new-val))
                   (stringp new-val))
               ;; do not replace in strings
               (cons key new-val)
             (cons key (jsonp-replace-refs new-val root-obj (1- max-depth) allow-remote)))
         ;; otherwise recurse into a regular object
         (cons key (jsonp-replace-refs val root-obj max-depth allow-remote))))))
   json-obj))

(provide 'jsonp)
;;; jsonp.el ends here
