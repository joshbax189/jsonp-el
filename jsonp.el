;;; jsonp.el --- Resolve JSON pointers in ELisp objects -*- lexical-binding: t -*-

;; Author: Josh Bax
;; Maintainer: Josh Bax
;; Version: 1.1.0
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

;; This library provides functions to resolve JSON pointers, as defined in
;; RFC6901 (https://datatracker.ietf.org/doc/html/rfc6901p),
;; within parsed JSON objects, also supporting remote resolution (fetching
;; JSON from a URI).

;; JSON pointers can be resolved whether your JSON is parsed as an alist,
;; plist or a hash table.

;; It also includes functions to replace JSON references ("$ref" properties)
;; commonly used in JSON schema and OpenAPI specs.

;;; Code:

(require 'map)
(require 'url)
(require 'json)
(require 'eieio)
(require 'cl-lib)

(define-error 'jsonp-error "JSONP error")
(define-error 'jsonp-resolution-error "JSONP resolution error" 'jsonp-error)
(define-error 'jsonp-remote-error "JSONP remote error" 'jsonp-error)

(defun jsonp-resolve (object pointer)
  "Resolve a JSON pointer in a parsed JSON object.

OBJECT is the parsed JSON object (e.g., the result of `json-read').

POINTER is a string representing the JSON pointer (e.g., \"/a/b/c\").

Returns the value at the given pointer, or signals `jsonp-resolution-error'
if the pointer is invalid or the value does not exist.  Returns OBJECT
itself if POINTER is an empty string or nil.

If the pointer begins with a #, it is treated as a URL encoded string
and is decoded before further resolving.  If the pointer does not
begin with a #, then no backslashes will be removed."
  (when (or (string-prefix-p "#/" pointer)
            (string-equal "#" pointer))
    (setq pointer (url-unhex-string (string-remove-prefix "#" pointer))))

  (if (or (not pointer)
          (string-equal pointer ""))
      object
    (if (string-prefix-p "/" pointer)
        ;; Remove leading empty string
        (setq pointer (string-remove-prefix "/" pointer))
      (signal 'jsonp-resolution-error
              (format "Malformed JSON pointer %s" pointer)))
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
                (signal 'jsonp-resolution-error
                        (format "JSON pointer index out of bounds: %s" part)))))
           (t
            (signal 'jsonp-resolution-error
                    (format "JSON pointer key not found: %s" part))))
          (setq parts (cdr parts))))
      (if parts
          (signal 'jsonp-resolution-error "Could not resolve all parts of JSON pointer")
        current-object))))

(defun jsonp--map-contains-key (obj key)
  "Non-nil if map-like OBJ has KEY as either a string, property or symbol.
Returns the key as the correct type if present, nil otherwise.

For example
  (jsonp--map-contains-key (:a 1) \"a\")
yields
  :a"
  (cond
   ((map-contains-key obj key) key)
   ((map-contains-key obj (intern key)) (intern key))
   ((map-contains-key obj (intern (concat ":" key))) (intern (concat ":" key)))))

(defun jsonp--unescape-token (token)
  "Replace the special sequences ~1 and ~0 in TOKEN string.
See https://datatracker.ietf.org/doc/html/rfc6901#section-4"
  (let* ((token-1 (string-replace "~1" "/" token))
         (token-2 (string-replace "~0" "~" token-1)))
    token-2))

(defun jsonp-resolve-safe (object pointer)
  "Resolve a JSON pointer, returning nil if it cannot be resolved.
OBJECT is the parsed JSON object.
POINTER is the string JSON pointer.
Returns the value at the pointer or nil if not resolved."
  (ignore-errors
      (jsonp-resolve object pointer)))

(defun jsonp-expand-relative-uri (uri base-uri)
  "Expands URI relative to BASE-URI, removing any dot components.
Assumes no query component in either URI and signals `jsonp-remote-error'
otherwise.

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
      (signal 'jsonp-remote-error
              (format "Cannot merge URIs with query %s" base-uri)))
    (when (cdr (url-path-and-query uri-parsed))
      (signal 'jsonp-remote-error
              (format "Cannot merge URIs with query %s" uri)))
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

(defun jsonp--url-retrieve-default (url)
  "Default JSONP fetch function based on `url-retrieve-synchronously'.
Fetches URL and returns response body as a string.

Signals `jsonp-remote-error' if the response's content-type is not
\"application/json\"."
  (message "JSONP: visiting %s" url)
  (with-current-buffer (url-retrieve-synchronously url t)
    (goto-char (point-min))
    (if (re-search-forward "^Content-Type: application/json\\(; ?.*\\)?$" nil t)
        (forward-line)
      (signal 'jsonp-remote-error "Expected HTTP response Content-Type to be application/json"))
    (while (looking-at "^.") (forward-line))
    (forward-line)
    (prog1 (buffer-substring (point) (point-max))
      (kill-buffer))))

(defclass jsonp-remote ()
  ((whitelist :initarg :whitelist
              :initform nil
              :type list
              :documentation "If provided, is a list of allowed URI patterns (regexp). Signals `jsonp-remote-error' if the URI does not match any pattern.")
   (json-parser :initarg :json-parser
                :initform #'json-parse-string
                :type function
                :documentation "The function to use for parsing the downloaded JSON. Defaults to `json-parse-string'.")
   (url-fetcher :initarg :url-fetcher
                :initform #'jsonp--url-retrieve-default
                :type function
                :documentation "The function to use to download a url. Defaults to `jsonp--url-retrieve-default'.")
   (url-cache :initarg :url-cache
              :initform nil
              :type list
              :documentation "Simple alist cache of urls and response body. See usage in `jsonp--url-retrieve'."))
  "Configuration data for resolving JSON pointers remotely.")

(cl-defmethod jsonp--url-retrieve ((remote jsonp-remote) url)
  "Fetch JSON from URL and parse using settings from `jsonp-remote' REMOTE."
  (let ((url-fetcher (slot-value remote 'url-fetcher))
        (json-parse-function (slot-value remote 'json-parser))
        (whitelist (slot-value remote 'whitelist))
        (cache (slot-value remote 'url-cache)))
    (when whitelist
      (unless (seq-some (lambda (pattern) (string-match pattern url))
                        whitelist)
        (signal 'jsonp-remote-error
                (format "URL not in whitelist: %s" url))))
    (let* ((url-for-cache (car (string-split url "#")))
           (cached-result (cdr (assoc-string url-for-cache cache)))
           (response (or cached-result
                         (funcall url-fetcher url)))
           (json-result (funcall json-parse-function response)))
      (unless (or (string-empty-p url-for-cache)
                  cached-result)
        (push (cons url-for-cache response) (oref remote url-cache)))
      json-result)))

(cl-defmethod jsonp-resolve-remote ((remote jsonp-remote) uri &optional base-uri)
  "Resolve a JSON pointer from a URI.

REMOTE is the `jsonp-remote' instance to use.

URI should be an absolute URI and must use either HTTP or HTTPS,
otherwise `jsonp-remote-error' is signalled.

BASE-URI is used to resolve URI if it is relative.
See `jsonp-expand-relative-uri' for details."
  (when (not (string-prefix-p "http" uri))
    (if (and base-uri (string-prefix-p "http" base-uri))
        (setq uri (jsonp-expand-relative-uri uri base-uri))
      (signal 'jsonp-remote-error
              (format "URI must use HTTP or HTTPS: %s" uri))))

  (let* ((parsed-uri (url-generic-parse-url uri))
         (pointer (url-target parsed-uri))
         (pointer (url-unhex-string pointer))
         ;; assume host will ignore fragment specifier here
         (json-object (jsonp--url-retrieve remote uri)))
    (jsonp-resolve json-object pointer)))

(defun jsonp-nested-elt (json-obj keys &optional remote base-uri)
  "Get an element from JSON-OBJ traversing $refs and following KEYS.
If some key in KEYS fails to match, signals `jsonp-resolution-error'.
Note that $refs are not replaced in the result.

KEYS should be a list of string, symbol or number.  Strings will match symbol
keys and vice-versa, but numbers must be used to index arrays.

REMOTE if non-nil should be an instance of `jsonp-remote' to use for downloading
URIs.

BASE-URI is used to resolve URI if it is relative.
See `jsonp-expand-relative-uri' for details."
  (let ((root-obj json-obj)
        val)

    (cl-block nil
      ;; edge cases
      (unless keys
        (cl-return json-obj))

      (when (jsonp--primitive-p json-obj)
        (signal 'jsonp-resolution-error "Cannot index primitive value"))

      (while keys
        (let* ((key (car keys))
               (key (if (numberp key)
                        key
                      (jsonp--map-contains-key json-obj key))))
          (setq keys (cdr keys)
                val (map-elt json-obj key))
          (cond
           ((jsonp--primitive-p val)
            (if keys
                (signal 'jsonp-resolution-error "Cannot index primitive value")
              (cl-return val)))
           ((jsonp--array-p val)
            ;; recurse
            (setq json-obj val))
           ;; objects
           (t
            (if-let* ((ref-key (jsonp--map-contains-key val "$ref"))
                      (ref-string (map-elt val ref-key))
                      (new-val (if (string-prefix-p "#" ref-string)
                                   (jsonp-resolve root-obj ref-string)
                                 (if remote
                                     (jsonp-resolve-remote remote ref-string base-uri)
                                   (signal 'jsonp-remote-error
                                           (format "Cannot resolve %s remote resolution disabled" ref-string))))))
                (setq json-obj new-val)
              ;; otherwise recurse into a regular object
              (setq json-obj val))))))
      (cl-return val))))

(defun jsonp--primitive-p (json-obj)
  "Non-nil if JSON-OBJ is a parsed boolean, string, number or null.

Note that `json-read-from-string' parses both the empty object and
the JSON \"null\" literal as nil, so in that case empty objects will
be treated as primitive."
  (cond
   ((stringp json-obj) t)
   ((numberp json-obj) t)
   ((null json-obj) t)
   ((eq t json-obj) t)
   ;; boolean special symbols
   ((eq :json-false json-obj) t)
   ((eq :false json-obj) t)
   ;; literal null used by json-parse-string
   ((eq :null json-obj) t)))

(defun jsonp--array-p (json-obj)
  "Non-nil if JSON-OBJ is a parsed JSON array.

If arrays are parsed as lists then empty arrays and objects may be
indistinguishable.  Therefore this returns nil when json-obj is nil."
  (cond
   ;; default for both json-parse-string and json-read-from-string
   ((vectorp json-obj) t)
   ;; when parsed as a non-empty list
   ((and (listp json-obj)
         json-obj
         ;; it should not be a plist
         (or (not (symbolp (car json-obj)))
             (not (string-prefix-p ":" (symbol-name (car json-obj)))))
         ;; it should not be an alist:
         ;; either the car is not a list
         (or (not (listp (car json-obj)))
             ;; or e.g. [{a: 1}] => (((a . 1)))
             ;; double car is not a symbol or string
             (not (symbolp (car (car json-obj))))))
    t)))

;; NOTE: json-schema explicitly disallows $refs from referring to other $refs
(defun jsonp-replace-refs (json-obj &optional root-obj max-depth remote base-uri)
  "Given parsed JSON-OBJ expand any $ref modifying JSON-OBJ in place.

Fragments are resolved in ROOT-OBJ, remote URIs are only resolved
if REMOTE is an instance of `jsonp-remote'.  Signals `jsonp-remote-error' if
a remote URI is encountered in a $ref prop, but REMOTE is nil.

MAX-DEPTH is the maximum recursion depth when expanding refs.
Default is 10.  If replacement reaches this maximum depth, a warning
is raised but the partial replacement is returned."
  (setq root-obj (or root-obj json-obj)
        max-depth (or max-depth 10))
  (cond
   ((<= max-depth 0)
    (warn "Recursion limit reached when expanding.  Some $refs may not be expanded.")
    json-obj)
   ((jsonp--primitive-p json-obj)
    json-obj)
   (t
    (dolist (keyval (map-pairs json-obj))
      (let ((key (car keyval))
            (val (cdr keyval)))
        (cond
         ((jsonp--primitive-p val)
          nil)
         ((jsonp--array-p val)
          ;; recurse and rebuild vector
          (let ((updated (jsonp-replace-refs val root-obj max-depth remote)))
            (map-put! json-obj key updated)))
         (t
          (if-let* ((ref-key (jsonp--map-contains-key val "$ref"))
                    (ref-string (map-elt val ref-key))
                    (new-val (if (string-prefix-p "#" ref-string)
                                 (jsonp-resolve root-obj ref-string)
                               (if remote
                                   (jsonp-resolve-remote remote ref-string base-uri)
                                 (signal 'jsonp-remote-error
                                         (format "Cannot resolve %s remote resolution disabled" ref-string))))))
              (if (jsonp--primitive-p new-val)
                  ;; do not replace in strings
                  (map-put! json-obj key new-val)
                (map-put! json-obj key (jsonp-replace-refs new-val root-obj (1- max-depth) remote)))
            ;; otherwise recurse into a regular object
            (map-put! json-obj key (jsonp-replace-refs val root-obj max-depth remote)))))))
    ;; return possibly modified object
    json-obj)))

(provide 'jsonp)
;;; jsonp.el ends here
