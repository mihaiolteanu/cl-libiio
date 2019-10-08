(defpackage :cl-libiio
  (:use :cl :cffi)
  (:import-from :alexandria :last-elt)
  (:export iio-create-scan-context
           iio-scan-context-destroy
           iio-scan-context-get-info-list
           iio-library-get-version
           iio-strerror
           iio-backends
           iio-has-backend
           iio-get-backends-count
           iio-get-backend
           iio-create-default-context
           iio-create-local-context
           iio-create-xml-context
           iio-create-xml-context-mem
           iio-create-network-context
           iio-create-context-from-uri
           iio-context-clone
           iio-context-destroy
           iio-success-p
           iio-context-get-version
           iio-context-get-xml
           iio-context-get-name
           iio-context-get-description
           iio-context-get-attrs-count
           iio-context-get-attr
           iio-context-get-attr-value
           iio-context-get-attrs-and-values
           iio-context-get-attrs
           iio-context-get-devices-count
           iio-context-get-device
           iio-context-find-device
           iio-context-set-timeout
           iio-context-get-devices
           iio-device-get-context
           iio-device-get-id
           iio-device-get-name
           iio-device-get-channels-count
           iio-device-get-attrs-count
           iio-device-get-buffer-attrs-count
           iio-device-get-channel
           iio-device-get-attr
           iio-device-get-buffer-attr
           iio-device-find-channel
           iio-device-find-attr
           iio-device-find-buffer-attr
           iio-device-attr-read
           iio-device-attrs
           iio-device-attr-read-all
           iio-device-attr-write
           iio-device-buffer-attrs
           iio-device-buffer-attr-read
           iio-device-buffer-attr-read-all
           iio-device-buffer-attr-write
           iio-device-get-trigger
           iio-device-get-trigger
           iio-device-set-trigger
           iio-device-is-trigger
           iio-device-set-kernel-buffers-count
           iio-device-get-channels
           iio-device-get-channels-str
           iio-channel-get-device
           iio-channel-get-id
           iio-channel-get-name
           iio-channel-is-output
           iio-channel-is-scan-element
           iio-channel-get-attrs-count
           iio-channel-get-attr
           iio-channel-find-attr
           iio-channel-attr-get-filename
           iio-channel-attr-read
           iio-channel-attr-read-all
           iio-channel-attrs
           iio-channel-attr-write
           iio-channel-enable
           iio-channel-disable
           iio-channel-is-enabled
           iio-channel-read
           iio-channel-write
           iio-channel-get-type
           iio-channel-get-modifier
           iio-buffer-get-device
           iio-device-create-buffer
           iio-device-enabled-channels
           iio-device-enabled-channels-count
           iio-buffer-collect-channel-samples
           iio-buffer-collect-all-samples
           iio-buffer-samples-count
           iio-buffer-get-poll-fd
           iio-buffer-set-blocking-mode
           iio-buffer-refill
           iio-buffer-push
           iio-buffer-push-partial
           iio-buffer-destroy
           iio-buffer-cancel
           iio-buffer-start
           iio-buffer-first
           iio-buffer-step
           iio-buffer-end
           iio-buffer-foreach-sample
           iio-device-get-sample-size
           iio-channel-get-index
           iio-channel-get-data-format))
