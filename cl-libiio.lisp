;;;; cl-libiio.lisp
(in-package :cl-libiio)

(define-foreign-library libiio
  (:unix "/usr/lib/libiio.so.0.18")
  (t (:default "libiio")))
(use-foreign-library libiio)

(defcfun "iio_create_context_from_uri" :pointer (ip :string))
(defcfun "iio_context_get_devices_count" :uint (ctx :pointer))
(defcfun "iio_context_get_device" :pointer (ctx :pointer) (index :uint))
(defcfun "iio_context_find_device" :pointer (ctx :pointer) (name :string))
(defcfun "iio_device_get_name" :string (device :pointer))
(defcfun "iio_context_get_name" :string (ctx :pointer))
(defcfun "iio_context_get_description" :string (ctx :pointer))
(defcfun "iio_context_get_attrs_count" :uint (ctx :pointer))
(defcfun "iio_device_get_channels_count" :uint (device :pointer))
(defcfun "iio_device_get_channel" :pointer (device :pointer) (index :uint))
(defcfun "iio_device_find_channel" :pointer (device :pointer) (name :string) (output :bool))
(defcfun "iio_channel_get_id" :string (channel :pointer))
(defcfun "iio_channel_get_name" :string (channel :pointer))
(defcfun "iio_channel_get_attrs_count" :uint (channel :pointer))
(defcfun "iio_channel_get_attr" :string (channel :pointer) (index :uint))

(defun iio-channel-attr-read (channel attr)
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
