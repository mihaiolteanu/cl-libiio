;;;; cl-libiio.lisp
(in-package :cl-libiio)

(define-foreign-library libiio
  (:unix "/usr/lib/libiio.so.0.18")
  (t (:default "libiio")))
(use-foreign-library libiio)

(defcfun "iio_create_scan_context" :pointer
  "Create a context besed on the backend (ip, usb or xml).
Use empty string for backend to search all of them."
  (backend :string) (flags :uint))

(defcfun "iio_scan_context_destroy" :void
  "Destroy the given scan context."
  (context :pointer))

;; This function returns 0 even when called directly from C and even when there are available
;; contexts (device connected to usb, for example). As a result, the rest of the functionality
;; for scan_context is not currently implemented.
(defcfun "iio_scan_context_get_info_list" :uint
  "Enumerate available contexts."
  (scan-context :pointer) (info (:pointer (:pointer :pointer))))

(defcfun "iio_create_context_from_uri" :pointer
  "Create a context from a URI description."
 (ip :string))

(defcfun "iio_context_get_devices_count" :uint
  "Enumerate the devices found in the given context."
  (ctx :pointer))

(defcfun "iio_context_get_device" :pointer
  "Get the device present at the given index."
  (ctx :pointer) (index :uint))

(defcfun "iio_context_find_device" :pointer
  "Try to find a device structure by its name of ID."
  (ctx :pointer) (name :string))

(defcfun "iio_device_get_name" :string
  "Retrieve the device name."
  (device :pointer))

(defcfun "iio_context_get_name" :string
  "Get the name of the given context."
  (ctx :pointer))

(defcfun "iio_context_get_description" :string
  "Get a description of the given context."
  (ctx :pointer))

(defcfun "iio_context_get_attrs_count" :uint
  "Get the number of context-specific attributes."
  (ctx :pointer))

(defcfun "iio_device_get_channels_count" :uint
  "Enumerate the channels of the given device."
  (device :pointer))

(defcfun "iio_device_get_channel" :pointer
  "Get the channel present at the given index."
  (device :pointer) (index :uint))

(defcfun "iio_device_find_channel" :pointer
  "Try to find a channel structure by its name of ID."
  (device :pointer) (name :string) (output :bool))

(defcfun "iio_channel_get_id" :string
  "Retrieve the channel ID."
  (channel :pointer))

(defcfun "iio_channel_get_name" :string
  "Retrieve the channel name."
  (channel :pointer))

(defcfun "iio_channel_get_attrs_count" :uint
  "Enumerate the channel-specific attributes of the given channel."
  (channel :pointer))

(defcfun "iio_channel_get_attr" :string
  "Get the channel-specific attribute present at the given index."
  (channel :pointer) (index :uint))

(defun iio-channel-attr-read (channel attr)
  "Read the content of the given channel-specific attribute."
  (let ((buf-len 64))
    (with-foreign-array (dest (make-array buf-len :initial-element 0)
                              `(:array :char ,buf-len))
      (foreign-funcall "iio_channel_attr_read"
                       :pointer channel
                       :string attr 
                       (:pointer :string) dest
                       :uint buf-len
                       :uint)
      (let ((raw-dest (foreign-array-to-lisp dest `(:array :char ,buf-len))))
        (octets-to-string
         (remove-if #'zerop raw-dest))))))

(defun iio-context-get-attr (context index)
  "Retrieve the name and value of a context-specific attribute."
  (let ((name  (foreign-alloc :char :count 100))
        (value (foreign-alloc :char :count 100)))
    (foreign-funcall "iio_context_get_attr"
                     :pointer context
                     :uint index
                     (:pointer :string) name
                     (:pointer :string) value)
    (prog1 (list (mem-aref name :string)
                 (mem-aref value :string))
      (foreign-free name)
      (foreign-free value))))

(defun context-attributes (context)
  "List of all the attributes and their values for the given context, as strings."
  (loop for i from 0 to (1- (iio-context-get-attrs-count context))
        collect (iio-context-get-attr context i)))

(defun devices (context)
  "List of all devices available for the given context, as strings."
  (loop for i from 0 to (1- (iio-context-get-devices-count *context*))
        collect
        (iio-device-get-name (iio-context-get-device *context* i))))

(defun channels (device)
  "List of all channels for the given device, as strings."
  (loop for i from 0 to (1- (iio-device-get-channels-count device))
        collect (iio-channel-get-id (iio-device-get-channel device i))))

(defun channel-attributes (channel)
  (loop for i from 0 to (1- (iio-channel-get-attrs-count channel))
        collect (iio-channel-get-attr channel i)))

(defun channel-attributes-and-values (channel)
  "Return a list of all attributes together with their values for the given channel."
  (let ((attributes (channel-attributes channel)))
    (mapcar (lambda (attr)
              (list attr (iio-channel-attr-read channel attr)))
            attributes)))
