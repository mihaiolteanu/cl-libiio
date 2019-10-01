;;;; cl-libiio.lisp
(in-package :cl-libiio)

(define-foreign-library libiio
  (:unix "/usr/lib/libiio.so.0.18")
  (t (:default "libiio")))
(use-foreign-library libiio)

;;; iiolib interface implementation.
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

(defun iio-library-get-version ()
  "Get the version of the libiio library."
  (with-foreign-objects ((major :uint 1)
                         (minor :uint 1)
                         (git-tag :char 8))
    (foreign-funcall "iio_library_get_version"
                     :pointer major
                     :pointer minor
                     :pointer git-tag)
    (list :major (mem-aref major :uint)
          :minor (mem-aref minor :uint)
          :git-tag (foreign-string-to-lisp git-tag :count 7))))

(defun iio-strerror (err)
  "Get a string description of the error code."
  (with-foreign-object (dst :char 256)
    (foreign-funcall "iio_strerror"
                     :int err
                     :pointer dst
                     :uint 256)
    (foreign-string-to-lisp dst)))

(defcfun "iio_has_backend" :bool
  "Check if the specified backend is available."
  (backend :string))

(defcfun "iio_get_backends_count" :uint
  "Get the number of available backends")

(defcfun "iio_get_backend" :string
  (index :uint))

(defun backends ()
  "Get all the avaliable backends, as strings."
  (loop for i from 0 to (1- (iio-get-backends-count))
      collect (iio-get-backend i)))


;;; Context functions.
(defcfun "iio_create_default_context" :pointer
  "Create a context from local or remote IIO devices.")

(defcfun "iio_create_local_context" :pointer
  "Create a context from local IIO devices (Linux only).")

(defcfun "iio_create_xml_context" :pointer
  "Create a context from a XML file"
  (xml-file :string))

(defcfun "iio_create_xml_context_mem" :pointer
  "Create a context from XML data in memory"
  (xml :string) (len :uint))

(defcfun "iio_create_network_context" :pointer
  "Create a context from the network"
  (host :string))

(defcfun "iio_create_context_from_uri" :pointer
  "Create a context from a URI description."
 (ip :string))

(defcfun "iio_context_clone" :pointer
  "Duplicate a pre-existing IIO context."
  (ctx :pointer))

(defcfun "iio_context_destroy" :void
  "Destroy the given context."
  (ctx :pointer))

(defun iio-context-get-version (context)
  "Get the version of the backend in use."
  (with-foreign-objects ((major :uint 1)
                         (minor :uint 1)
                         (git-tag :char 8))
    (foreign-funcall "iio_context_get_version"
                     :pointer context
                     :pointer major
                     :pointer minor
                     :pointer git-tag)
    (list :major (mem-aref major :uint)
          :minor (mem-aref minor :uint)
          :git-tag (foreign-string-to-lisp git-tag :count 7))))

(defcfun "iio_context_get_xml" :string
  "Obtain a XML representation of the given context."
  (ctx :pointer))

(defcfun "iio_context_get_name" :string
  "Get the name of the given context."
  (ctx :pointer))

(defcfun "iio_context_get_description" :string
  "Get a description of the given context."
  (ctx :pointer))

(defcfun "iio_context_get_attrs_count" :uint
  "Get the number of context-specific attributes."
  (ctx :pointer))

(defun iio-context-get-attr (context index)
  "Retrieve the name and value of a context-specific attribute."
  (with-foreign-objects ((name :char 256)
                         (value :char 256))
    (foreign-funcall "iio_context_get_attr"
                     :pointer context
                     :uint index
                     (:pointer :string) name
                     (:pointer :string) value)
    (list (mem-aref name :string)
          (mem-aref value :string))))

(defcfun "iio_context_get_attr_value" :string
  "Retrieve the value of a context-specific attribute."
  (ctx :pointer) (name :string))

(defcfun "iio_context_get_devices_count" :uint
  "Enumerate the devices found in the given context."
  (ctx :pointer))

(defcfun "iio_context_get_device" :pointer
  "Get the device present at the given index."
  (ctx :pointer) (index :uint))

(defcfun "iio_context_find_device" :pointer
  "Try to find a device structure by its name of ID."
  (ctx :pointer) (name :string))

(defcfun "iio_context_set_timeout" :int
  "Set a timeout for I/O operations."
  (ctx :pointer) (timeout-ms :uint))


;;; Device functions.
(defcfun "iio_device_get_context" :pointer
  "Retrieve a pointer to the iio_context structure."
  (device :pointer))

(defcfun "iio_device_get_id" :string
  "Retrieve the device ID."
  (device :pointer))

(defcfun "iio_device_get_name" :string
  "Retrieve the device name."
  (device :pointer))

(defcfun "iio_device_get_channels_count" :uint
  "Enumerate the channels of the given device."
  (device :pointer))

(defcfun "iio_device_get_attrs_count" :uint
  "Enumerate the device-specific attributes of the given device"
  (device :pointer))

(defcfun "iio_device_get_buffer_attrs_count" :uint
  "Enumerate the buffer-specific attributes of the given device."
  (device :pointer))

(defcfun "iio_device_get_channel" :pointer
  "Get the channel present at the given index."
  (device :pointer) (index :uint))

(defcfun "iio_device_get_attr" :string
  "Get the device-specific attribute present at the given index."
  (device :pointer) (index :uint))

(defcfun "iio_device_get_buffer_attr" :string
  (device :pointer) (index :uint))

(defcfun "iio_device_find_channel" :pointer
  "Try to find a channel structure by its name of ID."
  (device :pointer) (name :string) (output :bool))

(defcfun "iio_device_find_attr" :string
  "Try to find a device-specific attribute by its name"
  (device :pointer) (name :string))

(defcfun "iio_device_find_buffer_attr" :string
  "Try to find a buffer-specific attribute by its name."
  (device :pointer) (name :string))

(defun iio-device-attr-read (device attr)
  "Read the content of the given device-specific attribute."
  (with-foreign-object (dst :char 256)
    (foreign-funcall "iio_device_attr_read"
                     :pointer device
                     :string attr
                     :pointer dst
                     :uint 256)
    (foreign-string-to-lisp dst)))

(defun iio-device-attrs (device)
  "Read all device attributes. [EXTRA]"
  (loop for i from 0 to (1- (iio-device-get-attrs-count device))
        collect (iio-device-get-attr device i)))

(defun iio-device-attr-read-all (device)
  "Read the content of all device-specific attributes.
libiio function available, but we don't use that."
  (mapcar (lambda (attr)
            (list attr (iio-device-attr-read device attr)))
          (iio-device-attrs device)))

(defcfun "iio_device_attr_write" :int
  "Set the value of the given device-specific attribute."
  (device :pointer) (attr :string) (value :string))

(defun iio-device-buffer-attrs (device)
  "Return the device attributes as strings. [EXTRA]"
  (loop for i from 0 to (1- (iio-device-get-buffer-attrs-count device))
        collect (iio-device-get-buffer-attr device i)))

(defun iio-device-buffer-attr-read (device attr)
  (with-foreign-object (dst :char 256)
    (foreign-funcall "iio_device_buffer_attr_read"
                     :pointer device
                     :string attr
                     :pointer dst
                     :uint 256)
    (foreign-string-to-lisp dst)))

(defun iio-device-buffer-attr-read-all (device)
  "Read the content of all buffer-specific attributes.
libiio function available, but we don't use that."
  (mapcar (lambda (attr)
            (list attr (iio-device-buffer-attr-read device attr)))
          (iio-device-get-buffer-attrs-names device)))

(defcfun "iio_device_buffer_attr_write" :int
  (device :pointer) (attr :string) (value :string))

;; Trigger functions not tested.
(defun iio-device-get-trigger (device)
  "Retrieve the trigger of a given device."
  (with-foreign-object (trigger :pointer)
    (foreign-funcall "iio_device_set_trigger"
                     :pointer device
                     :pointer trigger)
    (mem-ref trigger :pointer)))

(defcfun "iio_device_get_trigger" :int
  "Associate a trigger to a given device."
  (device :pointer) (trigger :pointer))

(defcfun "iio_device_set_trigger" :int
  (device :pointer) (trigger :pointer))

(defcfun "iio_device_is_trigger" :bool
  "Return t if the given device is a trigger."
  (device :pointer))

(defcfun "iio_device_set_kernel_buffers_count" :int
  "Configure the number of kernel buffers for a device."
  (device :pointer) (nb-buffer :uint))


;; Channel functions.
(defcfun "iio_channel_get_device" :pointer
  "Retrieve a pointer to the iio_device structure."
  (channel :pointer))

(defcfun "iio_channel_get_id" :string
  "Retrieve the channel ID."
  (channel :pointer))

(defcfun "iio_channel_get_name" :string
  "Retrieve the channel name."
  (channel :pointer))

(defcfun "iio_channel_is_output" :bool
  "Return t if the given channel is an output channel."
  (channel :pointer))

(defcfun "iio_channel_is_scan_element" :bool
  "Return t if the given channel is a scan element."
  (channel :pointer))

(defcfun "iio_channel_get_attrs_count" :uint
  "Enumerate the channel-specific attributes of the given channel."
  (channel :pointer))

(defcfun "iio_channel_get_attr" :string
  "Get the channel-specific attribute present at the given index."
  (channel :pointer) (index :uint))

(defcfun "iio_channel_find_attr" :string
  "Try to find a channel-specific attribute by its name."
  (channel :pointer) (name :string))

(defcfun "iio_channel_attr_get_filename" :string
  "Retrieve the filename of an attribute."
  (channel :pointer) (name :string))

(defun iio-channel-attr-read (channel attr)
  "Read the content of the given channel-specific attribute."
  (with-foreign-object (dst :char 256)
    (foreign-funcall "iio_channel_attr_read"
                     :pointer channel
                     :string attr
                     :pointer dst
                     :uint 256)
    (foreign-string-to-lisp dst)))

(defun iio-channel-attrs (channel)
  "Return all channel attributes as strings. [EXTRA]"
  (loop for i from 0 to (1- (iio-channel-get-attrs-count channel))
        collect (iio-channel-get-attr channel i)))

(defun iio-channel-attr-read-all (channel)
  "Read the content of all channel-specific attributes.
libiio function available, but we don't use that."
  (mapcar (lambda (attr)
            (list attr (iio-channel-attr-read channel attr)))
          (iio-channel-attrs channel)))

(defun context-attributes-and-values (context)
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

