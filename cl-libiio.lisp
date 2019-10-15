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
                     :pointer git-tag
                     :void)
    (list :major (mem-aref major :uint)
          :minor (mem-aref minor :uint)
          :git-tag (foreign-string-to-lisp git-tag :count 7))))

(defcvar ("errno" :read-only t) :int
  "errno.")

(defun iio-strerror (err)
  "Get a string description of the error code."
  (with-foreign-object (dst :char 256)
    (foreign-funcall "iio_strerror"
                     :int err
                     :pointer dst
                     :uint 256
                     :void)
    (foreign-string-to-lisp dst)))

(defcfun "iio_has_backend" :bool
  "Check if the specified backend is available."
  (backend :string))

(defcfun "iio_get_backends_count" :uint
  "Get the number of available backends")

(defcfun "iio_get_backend" :string
  (index :uint))

(defun iio-backends ()
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

(defun iio-success-p (return-code)
  "Return t if return-code is non-negative [EXTRA].  
Useful for libiio functions which signal an error by returning a
negative error code."
  (>= return-code 0))

(defmacro foreign-funcall-with-err-handle (name &body options-and-success)
  "Same as foreign-funcall, but there is an extra form at the end to
be evaluated. Its value is returned if the foreign call is succesful.
Otherwise, return the error code together with the error string."
  `(let ((return-code (foreign-funcall
                       ,name
                       ,@(butlast options-and-success))))
     (if (pointerp return-code)         ;A pointer is expected
         (if (null-pointer-p return-code)
             ;; If a null pointer was returned, an error has occured.
             (values *errno* (iio-strerror *errno*))
             ;; Return the pointer, otherwise.
             return-code)
         ;; Otherwise the error code is returned diretly by the function
         (if (iio-success-p return-code) ;No error.
             ;; Evaluate and return the last form in the body.
             ,(last-elt options-and-success)
             (values (abs return-code) (iio-strerror (abs return-code)))))
     ))

(defun iio-context-get-version (context)
  "Get the version of the backend in use."
  (with-foreign-objects ((major :uint 1)
                         (minor :uint 1)
                         (git-tag :char 8))
    (foreign-funcall-with-err-handle "iio_context_get_version"
      :pointer context
      :pointer major
      :pointer minor
      :pointer git-tag
      :int
      (list :major (mem-aref major :uint)
            :minor (mem-aref minor :uint)
            :git-tag (foreign-string-to-lisp git-tag :count 7)))))

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
    (foreign-funcall-with-err-handle "iio_context_get_attr"
      :pointer context
      :uint index
      (:pointer :string) name
      (:pointer :string) value
      :int
      (list (mem-aref name :string)
            (mem-aref value :string)))))

(defcfun "iio_context_get_attr_value" :string
  "Retrieve the value of a context-specific attribute."
  (ctx :pointer) (name :string))

(defun iio-context-get-attrs-and-values (context)
  "Return all the context's attributes and values [EXTRA]."
  (loop for i from 0 to (1- (iio-context-get-attrs-count context))
        collect (iio-context-get-attr context i)))

(defun iio-context-get-attrs (context)
  "Return all the context's attributes, as strings [EXTRA]."
  (mapcar #'first (iio-context-get-attrs-and-values context)))

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

(defun iio-context-get-devices-pointers (context)
  "Return all devices for the given context, as pointers [EXTRA]."
  (loop for i from 0 to (1- (iio-context-get-devices-count context))
        collect
        (iio-context-get-device context i)))

(defun iio-context-get-devices-str (context)
  "Return all devices for the given context, as strings [EXTRA]."
  (mapcar #'iio-device-get-name
          (iio-context-get-devices-pointers context)))

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
    (foreign-funcall-with-err-handle "iio_device_attr_read"
      :pointer device
      :string attr
      :pointer dst
      :uint 256
      :int
      (foreign-string-to-lisp dst))))

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
    (foreign-funcall-with-err-handle "iio_device_buffer_attr_read"
      :pointer device
      :string attr
      :pointer dst
      :uint 256
      :int
      (foreign-string-to-lisp dst))))

(defun iio-device-buffer-attr-read-all (device)
  "Read the content of all buffer-specific attributes.
libiio function available, but we don't use that."
  (mapcar (lambda (attr)
            (list attr (iio-device-buffer-attr-read device attr)))
          (iio-device-buffer-attrs device)))

(defun iio-device-buffer-attr-write (device attr value)
  (foreign-funcall-with-err-handle "iio_device_buffer_attr_write"
    :pointer device
    :string attr
    :string value
    :int
    ;; Return the number of bytes written on success.
    return-code))

;; Trigger functions not tested.
(defun iio-device-get-trigger (device)
  "Retrieve the trigger of a given device."
  (with-foreign-object (trigger :pointer)
    (foreign-funcall-with-err-handle "iio_device_set_trigger"
      :pointer device
      :pointer trigger
      :int
      (mem-ref trigger :pointer))))

(defcfun "iio_device_set_trigger" :int
  (device :pointer) (trigger :pointer))

(defcfun "iio_device_is_trigger" :bool
  "Return t if the given device is a trigger."
  (device :pointer))

(defcfun "iio_device_set_kernel_buffers_count" :int
  "Configure the number of kernel buffers for a device."
  (device :pointer) (nb-buffer :uint))

(defun iio-device-get-channels (device)
  "Return all channels for the given device, as pointers to channels [EXTRA]."
  (loop for i from 0 to (1- (iio-device-get-channels-count device))
        collect (iio-device-get-channel device i)))

(defun iio-device-get-channels-str (device)
  "Return all channels for the given device, as strings [EXTRA]."
  (mapcar #'iio-channel-get-id (iio-device-get-channels device)))

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
    (foreign-funcall-with-err-handle "iio_channel_attr_read"
      :pointer channel
      :string attr
      :pointer dst
      :uint 256
      :int
      (foreign-string-to-lisp dst))))

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

(defcfun "iio_channel_attr_write" :int
  "Set the value of the given channel-specific attribute."
  (channel :pointer) (attr :string) (value :string))

(defcfun "iio_channel_enable" :void
  "Enable the given channel."
  (channel :pointer))

(defcfun "iio_channel_disable" :void
  "Disable the given channel."
  (channel :pointer))

(defcfun "iio_channel_is_enabled" :bool
  "Return t if the channel is enabled."
  (channel :pointer))

(defun iio-channel-read (channel buffer len)
  "[Not implemented] Demultiplex and convert the samples of a given
channel."
  (with-foreign-object (dest :uint32 len)
    (foreign-funcall-with-err-handle "iio_channel_read"
      :pointer channel
      :pointer buffer
      :pointer dest
      :uint len
      :int
      (foreign-array-to-lisp dest :uint32))))

(defun iio-channel-write (channel buffer src len)
  "[Not implemented] Convert and multiplex the samples of a given
channel."
  (foreign-funcall-with-err-handle "iio_channel_write"
    :pointer channel
    :pointer buffer
    :pointer src
    :uint32 len
    :int
    return-code))

(defcfun "iio_channel_get_type" :uint
  "Get the type of the given channel."
  (channel :pointer))

(defcfun "iio_channel_get_modifier" :uint
  "Get the modifier type of the given channel."
  (channel :pointer))

;; Buffer functions.
(defcfun "iio_buffer_get_device" :pointer
  "Retrieve a pointer to the iio_device structure."
  (buffer :pointer))

(defcfun "iio_device_create_buffer" :pointer
  "Create an input or output buffer associated to the given device."
  (device :pointer) (samples-count :uint) (cyclic :bool))

(defun iio-device-enabled-channels (device)
  "Returns a list of enabled channels, as pointers. [EXTRA]"
  (remove-if (complement #'iio-channel-is-enabled)
             (iio-device-get-channels device)))

(defun iio-device-enabled-channels-count (device)
  "Return the number of enabled channels for the device. [EXTRA]"
  (length (iio-device-enabled-channels device)))

;; TODO: The actual format of the channel samples is not fixed for all
;; eternity and it differs from device to device (or so I guess). The
;; details are stored and can be inspected in the struct
;; iio_data_format. The reading and writing of buffers should depend
;; on the device sample format so that we can be as generic as
;; possible. See also the iio_info.c file in the libiio, where the
;; format is printed as "voltage0: (output, index: 0, format:
;; le:S16/16>>0), for pluto, for example (when called from the cli)."
(defun iio-buffer-collect-channel-samples (buffer channel)
  "Return the buffer values for a single channel."
  (let* ((device (iio-buffer-get-device buffer))
         (sample-size (iio-device-get-sample-size device))
         ;; How many bytes are there, for a channel, assuming all
         ;; channels have the same sample size.
         (size-per-channel
           (/ sample-size (iio-device-enabled-channels-count device)))
         (samples (/ (iio-buffer-samples-count buffer)
                     sample-size))
         (type (case size-per-channel
                 (1 :int8)
                 (2 :int16)
                 (4 :int32)
                 (8 :int64))))
    (loop for i from 0 to samples
          collect (mem-aref (iio-buffer-first buffer channel)
                            type i))))

(defun iio-buffer-collect-all-samples (buffer)
  "Return the buffer values for all the enabled channels. [EXTRA] 
The return values are lists where the first element is the channel id
and the second element is the samples read from the buffer."
  (let* ((device (iio-buffer-get-device buffer))
         (channels (iio-device-enabled-channels device)))
    (mapcar (lambda (channel)
              (list (iio-channel-get-id channel)
                    (iio-buffer-collect-channel-samples buffer channel)))
            channels)))

(defun iio-buffer-samples-count (buffer)
  "Return the number of samples currently in the buffer. [EXTRA]"
  (- (pointer-address (iio-buffer-end buffer))
     (pointer-address (iio-buffer-start buffer))))

;; TODO: implement such that destroying a buffer a second time has no
;; effect and just returns nil. The current implementation fails with
;; an "Unhandled memory fault at #x0" error.
(defcfun "iio_buffer_destroy" :void
  "Destroy the given buffer."
  (buffer :pointer))

(defun iio-buffer-get-poll-fd (buffer)
  (foreign-funcall-with-err-handle "iio_buffer_get_poll_fd"
    :pointer buffer
    :int
    ;; Return a valid file descriptor on success.
    return-code))

(defun iio-buffer-set-blocking-mode (buffer blocking)
  (foreign-funcall-with-err-handle "iio_buffer_set_blocking_mode"
    :pointer buffer
    :bool blocking
    :int
    ;; Returns 0 on success, actually (see libiio doc).
    return-code))

(defun iio-buffer-refill (buffer)
  "Fetch more samples from the hardware."
  (foreign-funcall-with-err-handle "iio_buffer_refill"
    :pointer buffer
    :int
    ;; Return the number of bytes read in case of success.
    return-code))

(defun iio-buffer-push (buffer)
  "Send the samples to the hardware."
  (foreign-funcall-with-err-handle "iio_buffer_push"
    :pointer buffer
    :int
    ;; Return the number of bytes written in case of success.
    return-code))

(defun iio-buffer-push-partial (buffer samples-count)
  "Send a given number of samples to the hardware."
  (foreign-funcall-with-err-handle "iio_buffer_push_partial"
    :pointer buffer
    :uint samples-count
    :int
    ;; Return the number of bytes written in case of success.
    return-code))

(defcfun "iio_buffer_cancel" :void
  (buffer :pointer))

(defcfun "iio_buffer_start" :pointer
  "Get the start address of the buffer"
  (buffer :pointer))

(defcfun "iio_buffer_first" :pointer
  "Find the first sample of a channel in a buffer."
  (buffer :pointer) (channel :pointer))

(defcfun "iio_buffer_step" :uint
  "Get the step size between two samples of one channel."
  (buffer :pointer))

(defcfun "iio_buffer_end" :pointer
  "Get the address that follows the last sample in a buffer."
  (buffer :pointer))

(defcfun "iio_buffer_foreach_sample" :int
  "[Not implemented] Call the supplied callback for each sample found
  in a buffer.")

;; Low-level functions.
(defun iio-device-get-sample-size (device)
  "Get the current sample size.
On success, the sample size in bytes is returned.
Warning! The sample size is not constant and will change when channels
get enabled or disabled, for example: If one channel is enabled and
the returned sample size is two (bytes); and if then another channel
is enabled and the returned sample size is four (bytes), and you
create a buffer with 10 samples count, then the buffer will be filled
with 40 bytes, where the first two bytes are the first channel sample,
the next two bytes are the second channel sample, and so on."
  (foreign-funcall-with-err-handle "iio_device_get_sample_size"
    :pointer device
    :int
    ;; Return the sample size on success.
    return-code))

(defcfun "iio_channel_get_index" :long
  "Get the index of the given channel"
  (channel :pointer))

(defcstruct iio-data-format
  (length          :uint)
  (bits            :uint)
  (shift           :uint)
  (is-signed       :bool)
  (is-full-defined :bool)
  (is-be           :bool)
  (with-scale      :bool)
  (scale           :double)
  (repeat          :uint))

(defcfun "iio_channel_get_data_format" (:pointer (:struct iio-data-format))
  "Get a pointer to a channel's data format structure"
  (channel :pointer))

(defun iio-data-format-to-plist (data)
  (convert-from-foreign data '(:struct iio-data-format)))

;; TODO: not sure how to name the extra functions. Should it be
;; iio-context-info or just context-info. The iio- is used for all
;; defcfun definitions since that is the name of the foreign
;; function. iio- was added to some other extra functions since it
;; keeps the naming consistent.
(defun context-info (context)
  "Return a list with all the context devices, channels, attributes.
In a word, return all the info available for the given context. [EXTRA]"
  (list :context
        (list :attrs (iio-context-get-attrs-and-values context))
        (cons :devices
              (mapcar #'device-info
                      (iio-context-get-devices-pointers context)))))

(defun device-info (device)
  "Return a list with the device attributes and all the channels and
their attributes. In a word, return all the info available for the
given device. [EXTRA]"
  (list (list :device
              (list :name (iio-device-get-name device))
              (list :attr (iio-device-attr-read-all device)))
        (cons :channels
              (mapcar #'channel-info
                      (iio-device-get-channels device)))))

(defun channel-info (channel)
  "Return a list with the channel name and all of its attributes
together with their values. In a word, return all the info available
for the given channel. [EXTRA]"
  (list (list :id
              (iio-channel-get-id channel))
        (list :attrs
              (iio-channel-attr-read-all channel))))

;; TODO: handle null pointer returns. For example,
;; (iio-create-context-from-uri "ip:192.168.2.1"), when the device is
;; not connected, a null pointer is returned. Maybe return nil instead.
