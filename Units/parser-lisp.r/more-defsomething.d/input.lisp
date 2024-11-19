;; Taken from https://github.com/atlas-engineer/nyxt.git

(defmethod url ((url quri:uri))
  url)

(deftype url-designator ()
  "Type for anything URL-like.
Means that `url' can be applied to it to get `quri:uri'."
  `(satisfies has-url-method-p))

(defclass renderer-scheme ()
  ()
  (:metaclass interface-class)
  (:documentation "Renderer-specific representation of the custom scheme.
Should be redefined by the renderer."))

(define-class scheme (renderer-scheme)
  ((name
    (alex:required-argument 'name)
    :documentation "The custom scheme name to handle.
HTTPS or FILE are examples of schemes.")
   (callback
    nil
    :type (or null function)
    :documentation "A function called on URL load that returns the page contents.

It takes the URL as an argument and returns up to 5 values:
- The data for page contents (either as string or as a unsigned byte array)
- The MIME type for the contents
- The status code for the request
- An alist of headers for the request
- A status reason phrase")
   (error-callback
    nil
    :type (or null function)
    :documentation "Callback to use when a condition is signaled.

Accepts only one argument: the signaled condition."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Representation of Nyxt-specific internal schemes.
Has `name' it can be accessed with. When accessed, runs `callback' to return
content. In case something goes wrong, runs `error-callback'.")
  (:metaclass user-class))

(defgeneric browser-schemes (browser)
  (:method-combination append)
  (:documentation "Return a list of schemes supported by BROWSER."))

(defparameter %buffer nil)

(nyxt:define-package :nyxt/renderer/gtk
    (:documentation "GTK renderer using direct CFFI bindings."))

(defstruct renderer-history-entry
  title
  url
  original-url
  gtk-object)

