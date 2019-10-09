# Overview

The `cl-libiio` package provides complete Common Lisp API
implementation for Analog Device's libiio (in short: CL bindings via
CFFI for libiio)

You can find more info about libiio on their official pages: [about libiio](https://wiki.analog.com/resources/tools-software/linux-software/libiio_internals) and [libiio documentation](http://analogdevicesinc.github.io/libiio/index.html).

# API (exported functions)

Some extra functionality not provided by the libiio is exported and
mentioned as such in the documentation.

**iio-channel-get-type** *channel*

    Get the type of the given channel.

**iio-device-buffer-attr-read-all** *device*

    Read the content of all buffer-specific attributes.
    libiio function available, but we don't use that.

**iio-channel-attrs** *channel*

    Return all channel attributes as strings. [EXTRA]

**iio-buffer-get-poll-fd** *buffer*

    NIL

**iio-channel-get-id** *channel*

    Retrieve the channel ID.

**iio-device-get-channel** *device*

    Get the channel present at the given index.

**iio-channel-get-attr** *channel*

    Get the channel-specific attribute present at the given index.

**iio-device-set-kernel-buffers-count** *device*

    Configure the number of kernel buffers for a device.

**iio-channel-is-enabled** *channel*

    Return t if the channel is enabled.

**iio-channel-attr-read** *channel*

    Read the content of the given channel-specific attribute.

**iio-buffer-push-partial** *buffer*

    Send a given number of samples to the hardware.

**iio-backends** *NIL*

    Get all the avaliable backends, as strings.

**iio-context-get-xml** *ctx*

    Obtain a XML representation of the given context.

**iio-device-attr-read** *device*

    Read the content of the given device-specific attribute.

**iio-channel-is-scan-element** *channel*

    Return t if the given channel is a scan element.

**iio-create-scan-context** *backend*

    Create a context besed on the backend (ip, usb or xml).
    Use empty string for backend to search all of them.

**iio-device-buffer-attrs** *device*

    Return the device attributes as strings. [EXTRA]

**iio-device-buffer-attr-write** *device*

    NIL

**iio-context-get-attrs-count** *ctx*

    Get the number of context-specific attributes.

**iio-create-context-from-uri** *ip*

    Create a context from a URI description.

**iio-get-backends-count** *NIL*

    Get the number of available backends

**iio-device-get-sample-size** *device*

    Get the current sample size.
    On success, the sample size in bytes is returned.
    Warning! The sample size is not constant and will change when channels
    get enabled or disabled, for example: If one channel is enabled and
    the returned sample size is two (bytes); and if then another channel
    is enabled and the returned sample size is four (bytes), and you
    create a buffer with 10 samples count, then the buffer will be filled
    with 40 bytes, where the first two bytes are the first channel sample,
    the next two bytes are the second channel sample, and so on.

**iio-device-get-attrs-count** *device*

    Enumerate the device-specific attributes of the given device

**iio-context-find-device** *ctx*

    Try to find a device structure by its name of ID.

**iio-scan-context-get-info-list** *scan-context*

    Enumerate available contexts.

**iio-buffer-foreach-sample** *NIL*

    [Not implemented] Call the supplied callback for each sample found
  in a buffer.

**iio-device-get-buffer-attrs-count** *device*

    Enumerate the buffer-specific attributes of the given device.

**iio-channel-is-output** *channel*

    Return t if the given channel is an output channel.

**iio-device-get-channels** *device*

    Return all channels for the given device, as pointers to channels [EXTRA].

**iio-buffer-set-blocking-mode** *buffer*

    NIL

**iio-buffer-samples-count** *buffer*

    Return the number of samples currently in the buffer. [EXTRA]

**iio-device-get-name** *device*

    Retrieve the device name.

**iio-context-get-attr** *context*

    Retrieve the name and value of a context-specific attribute.

**iio-context-get-attrs-and-values** *context*

    Return all the context's attributes and values [EXTRA].

**iio-buffer-refill** *buffer*

    Fetch more samples from the hardware.

**iio-device-find-attr** *device*

    Try to find a device-specific attribute by its name

**iio-buffer-cancel** *buffer*

    NIL

**iio-context-get-devices-count** *ctx*

    Enumerate the devices found in the given context.

**iio-context-get-attr-value** *ctx*

    Retrieve the value of a context-specific attribute.

**iio-channel-get-data-format** *channel*

    Get a pointer to a channel's data format structure

**iio-device-get-buffer-attr** *device*

    NIL

**iio-device-attrs** *device*

    Read all device attributes. [EXTRA]

**iio-channel-write** *channel*

    [Not implemented] Convert and multiplex the samples of a given
channel.

**iio-create-xml-context** *xml-file*

    Create a context from a XML file

**iio-device-enabled-channels-count** *device*

    Return the number of enabled channels for the device. [EXTRA]

**iio-context-get-description** *ctx*

    Get a description of the given context.

**iio-device-get-attr** *device*

    Get the device-specific attribute present at the given index.

**iio-channel-enable** *channel*

    Enable the given channel.

**iio-channel-get-attrs-count** *channel*

    Enumerate the channel-specific attributes of the given channel.

**iio-context-clone** *ctx*

    Duplicate a pre-existing IIO context.

**iio-channel-find-attr** *channel*

    Try to find a channel-specific attribute by its name.

**iio-device-get-trigger** *device*

    Associate a trigger to a given device.

**iio-create-default-context** *NIL*

    Create a context from local or remote IIO devices.

**iio-strerror** *err*

    Get a string description of the error code.

**iio-channel-attr-get-filename** *channel*

    Retrieve the filename of an attribute.

**iio-device-find-buffer-attr** *device*

    Try to find a buffer-specific attribute by its name.

**iio-buffer-end** *buffer*

    Get the address that follows the last sample in a buffer.

**iio-device-enabled-channels** *device*

    Returns a list of enabled channels, as pointers. [EXTRA]

**iio-context-get-devices** *context*

    Return all devices for the given context, as strings [EXTRA].

**iio-context-destroy** *ctx*

    Destroy the given context.

**iio-create-network-context** *host*

    Create a context from the network

**iio-device-buffer-attr-read** *device*

    NIL

**iio-buffer-collect-channel-samples** *buffer*

    Return the buffer values for a single channel.

**iio-create-local-context** *NIL*

    Create a context from local IIO devices (Linux only).

**iio-channel-get-modifier** *channel*

    Get the modifier type of the given channel.

**iio-success-p** *return-code*

    Return t if return-code is non-negative [EXTRA].  
    Useful for libiio functions which signal an error by returning a
    negative error code.

**iio-channel-attr-read-all** *channel*

    Read the content of all channel-specific attributes.
    libiio function available, but we don't use that.

**iio-get-backend** *index*

    NIL

**iio-device-get-channels-str** *device*

    Return all channels for the given device, as strings [EXTRA].

**iio-context-set-timeout** *ctx*

    Set a timeout for I/O operations.

**iio-has-backend** *backend*

    Check if the specified backend is available.

**iio-create-xml-context-mem** *xml*

    Create a context from XML data in memory

**iio-context-get-name** *ctx*

    Get the name of the given context.

**iio-device-set-trigger** *device*

    NIL

**iio-device-create-buffer** *device*

    Create an input or output buffer associated to the given device.

**iio-scan-context-destroy** *context*

    Destroy the given scan context.

**iio-context-get-attrs** *context*

    Return all the context's attributes, as strings [EXTRA].

**iio-device-is-trigger** *device*

    Return t if the given device is a trigger.

**iio-device-attr-write** *device*

    Set the value of the given device-specific attribute.

**iio-context-get-version** *context*

    Get the version of the backend in use.

**iio-device-get-context** *device*

    Retrieve a pointer to the iio_context structure.

**iio-buffer-push** *buffer*

    Send the samples to the hardware.

**iio-channel-attr-write** *channel*

    Set the value of the given channel-specific attribute.

**iio-device-get-id** *device*

    Retrieve the device ID.

**iio-buffer-get-device** *buffer*

    Retrieve a pointer to the iio_device structure.

**iio-device-get-channels-count** *device*

    Enumerate the channels of the given device.

**iio-channel-disable** *channel*

    Disable the given channel.

**iio-library-get-version** *NIL*

    Get the version of the libiio library.

**iio-device-attr-read-all** *device*

    Read the content of all device-specific attributes.
    libiio function available, but we don't use that.

**iio-buffer-step** *buffer*

    Get the step size between two samples of one channel.

**iio-channel-get-index** *channel*

    Get the index of the given channel

**iio-context-get-device** *ctx*

    Get the device present at the given index.

**iio-buffer-collect-all-samples** *buffer*

    Return the buffer values for all the enabled channels. [EXTRA] 
    The return values are lists where the first element is the channel id
    and the second element is the samples read from the buffer.

**iio-buffer-destroy** *buffer*

    Destroy the given buffer.

**iio-buffer-start** *buffer*

    Get the start address of the buffer

**iio-buffer-first** *buffer*

    Find the first sample of a channel in a buffer.

**iio-channel-get-device** *channel*

    Retrieve a pointer to the iio_device structure.

**iio-channel-read** *channel*

    [Not implemented] Demultiplex and convert the samples of a given
    channel.

**iio-channel-get-name** *channel*

    Retrieve the channel name.

**iio-device-find-channel** *device*

    Try to find a channel structure by its name of ID.

