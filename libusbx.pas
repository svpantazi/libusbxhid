//******************************************************************************
//
//  * Project name:
//       Translation of libusb 1.0 header for pascal
//  * Copyright:
//       (C) 2010 Marko Medic <medamarko@gmail.com>
//  * Revision History:
//       20101126:
//        - initial release;
//  * Test Configuration:
//       - Lazarus IDE 0.9.28.2
//       - fpc 2.4
//       - few different USB devices based on PIC MCU
//
// source: https://forum.lazarus.freepascal.org/index.php?topic=11435.0
//        attachment: libusb.pp.gz   (https://forum.lazarus.freepascal.org/index.php?action=dlattach;topic=11435.0;attach=1292)
//
// source modified (Apr 2013) by S. V. Pantazi (svpantazi@gmail.com)
{
Aug 27, 2019 - added hotplug callback types and calls
Aug 23, 2019 - added timeval structure and libusb_handle_events_completed and libusb_handle_events_timeout_completed calls
}

//******************************************************************************

{literature links:
  http://www.usbmadesimple.co.uk/index.html

  https://www.socallinuxexpo.org/sites/default/files/presentations/scale_2017_usb.pdf

  http://www.beyondlogic.org/usbnutshell/usb1.shtml
API reference
  http://libusb.sourceforge.net/api-1.0/index.html
}

{library installation on linux:
ubuntu: sudo apt-get install libusb-1.0-0-dev
centos: sudo yum install libusb-1.0-0-devel}

{..$define STATIC}

{$MACRO on}

{$ifdef windows}
  {$define CALLING_CONV:=stdcall}
{warning - tread carefully - the win64 libusb libraries can be created to use used the cdecl call convention;
on linux most likely the call conv is going to be cdecl as well so this is going to fail to compile}

{$else}
  {$define CALLING_CONV:=cdecl}
{$endif}


unit libusbx;

interface

uses ctypes;{ seems simpler to use the ctypes instead of more aliases to basic pascal types}


{$PACKRECORDS C}

{$ifdef STATIC}
  {$LINKLIB /usr/lib/libusb-1.0.a}
  {$DEFINE LIB_NAME:=}
{$else}
  {$ifdef windows}
    {$ifdef win64}
      {$DEFINE LIB_NAME:='libusb-1.0_x64.dll'}
    {$else}
      {$DEFINE LIB_NAME:='libusb-1.0_x86.dll'}
    {$endif}
  {$else}
    {$DEFINE LIB_NAME:='libusb-1.0.so'}
  {$endif}
{$endif}

//------------------------------------------------------------------------------
// Constant
//------------------------------------------------------------------------------
{(* standard USB stuff *)}
{(* Device and/or Interface Class codes *)}
const LIBUSB_CLASS_PER_INTERFACE = 0;
const LIBUSB_CLASS_AUDIO         = 1;
const LIBUSB_CLASS_COMM          = 2;
const LIBUSB_CLASS_HID           = 3;
const LIBUSB_CLASS_PRINTER       = 7;
const LIBUSB_CLASS_PTP           = 6;
const LIBUSB_CLASS_MASS_STORAGE  = 8;
const LIBUSB_CLASS_HUB           = 9;
const LIBUSB_CLASS_DATA          = 10;
const LIBUSB_CLASS_WIRELESS      = $e0;
const LIBUSB_CLASS_APPLICATION   = $fe;
const LIBUSB_CLASS_VENDOR_SPEC   = $ff;

{(* Descriptor types as defined by the USB specification. *)}
const LIBUSB_DT_DEVICE    = $01;
const LIBUSB_DT_CONFIG    = $02;
const LIBUSB_DT_STRING    = $03;
const LIBUSB_DT_INTERFACE = $04;
const LIBUSB_DT_ENDPOINT  = $05;
const LIBUSB_DT_HID       = $21;
const LIBUSB_DT_REPORT    = $22;
const LIBUSB_DT_PHYSICAL  = $23;
const LIBUSB_DT_HUB       = $29;

{(* Descriptor sizes per descriptor type *)}
const LIBUSB_DT_DEVICE_SIZE         = 18;
const LIBUSB_DT_CONFIG_SIZE         = 9;
const LIBUSB_DT_INTERFACE_SIZE      = 9;
const LIBUSB_DT_ENDPOINT_SIZE       = 7;
const LIBUSB_DT_ENDPOINT_AUDIO_SIZE = 9;  {* Audio extension *}
const LIBUSB_DT_HUB_NONVAR_SIZE     = 7;
const LIBUSB_ENDPOINT_ADDRESS_MASK  = $0f;    {* in bEndpointAddress *}
const LIBUSB_ENDPOINT_DIR_MASK      = $80;

{* Endpoint direction. Values for bit 7 of the ref libusb_endpoint_descriptor.bEndpointAddress 'endpoint address' scheme. *}
const LIBUSB_ENDPOINT_IN  = $80;
const LIBUSB_ENDPOINT_OUT = $00;

const LIBUSB_TRANSFER_TYPE_MASK        = $03;    {* in bmAttributes *}

{* Endpoint transfer cType. Values for bits 0:1 of the ref libusb_endpoint_descriptor.bmAttributes 'endpoint attributes' field.*}
const LIBUSB_TRANSFER_TYPE_CONTROL     = 0;
const LIBUSB_TRANSFER_TYPE_ISOCHRONOUS = 1;
const LIBUSB_TRANSFER_TYPE_BULK        = 2;
const LIBUSB_TRANSFER_TYPE_INTERRUPT   = 3;

{* Standard requests, as defined in table 9-3 of the USB2 specifications *}
const LIBUSB_REQUEST_GET_STATUS        = $00;
const LIBUSB_REQUEST_CLEAR_FEATURE     = $01;
const LIBUSB_REQUEST_SET_FEATURE       = $03;
const LIBUSB_REQUEST_SET_ADDRESS       = $05;
const LIBUSB_REQUEST_GET_DESCRIPTOR    = $06;
const LIBUSB_REQUEST_SET_DESCRIPTOR    = $07;
const LIBUSB_REQUEST_GET_CONFIGURATION = $08;
const LIBUSB_REQUEST_SET_CONFIGURATION = $09;
const LIBUSB_REQUEST_GET_INTERFACE     = $0A;
const LIBUSB_REQUEST_SET_INTERFACE     = $0B;
const LIBUSB_REQUEST_SYNCH_FRAME       = $0C;

{* Request cType bits of the ref libusb_control_setup.bmRequestType 'bmRequestType' field in control transfers. *}
const LIBUSB_REQUEST_TYPE_STANDARD = $00 shl 5;  // $00
const LIBUSB_REQUEST_TYPE_CLASS    = $01 shl 5;  // $20
const LIBUSB_REQUEST_TYPE_VENDOR   = $02 shl 5;  // $40
const LIBUSB_REQUEST_TYPE_RESERVED = $03 shl 5;  // $60

{* Recipient bits of the ref libusb_control_setup.bmRequestType 'bmRequestType' field in control transfers. Values 4 through 31 are reserved. *}
const LIBUSB_RECIPIENT_DEVICE    = $00;
const LIBUSB_RECIPIENT_INTERFACE = $01;
const LIBUSB_RECIPIENT_ENDPOINT  = $02;
const LIBUSB_RECIPIENT_OTHER     = $03;

const LIBUSB_ISO_SYNC_TYPE_MASK  = $0C;

{* Synchronization cType for isochronous endpoints. Values for bits 2:3 of the ref libusb_endpoint_descriptor.bmAttributes 'bmAttributes' field in libusb_endpoint_descriptor. *}
const LIBUSB_ISO_SYNC_TYPE_NONE     = 0;
const LIBUSB_ISO_SYNC_TYPE_ASYNC    = 1;
const LIBUSB_ISO_SYNC_TYPE_ADAPTIVE = 2;
const LIBUSB_ISO_SYNC_TYPE_SYNC     = 3;

const LIBUSB_ISO_USAGE_TYPE_MASK    = $30;

{* Usage cType for isochronous endpoints. Values for bits 4:5 of the ref libusb_endpoint_descriptor.bmAttributes 'bmAttributes' field in libusb_endpoint_descriptor. *}
const LIBUSB_ISO_USAGE_TYPE_DATA     = 0;
const LIBUSB_ISO_USAGE_TYPE_FEEDBACK = 1;
const LIBUSB_ISO_USAGE_TYPE_IMPLICIT = 2;

{* Error codes. Most libusb functions result:= 0 on success or one of these codes on failure.*}
const LIBUSB_SUCCESS             = 0;
const LIBUSB_ERROR_IO            = -1;
const LIBUSB_ERROR_INVALID_PARAM = -2;
const LIBUSB_ERROR_ACCESS        = -3;
const LIBUSB_ERROR_NO_DEVICE     = -4;
const LIBUSB_ERROR_NOT_FOUND     = -5;
const LIBUSB_ERROR_BUSY          = -6;
const LIBUSB_ERROR_TIMEOUT       = -7;
const LIBUSB_ERROR_OVERFLOW      = -8;
const LIBUSB_ERROR_PIPE          = -9;
const LIBUSB_ERROR_INTERRUPTED   = -10;
const LIBUSB_ERROR_NO_MEM        = -11;
const LIBUSB_ERROR_NOT_SUPPORTED = -12;
const LIBUSB_ERROR_OTHER         = -99;


//------------------------------------------------------------------------------
// Structures
//------------------------------------------------------------------------------
{ * Transfer status codes */}
type libusb_transfer_status = (
  //** Transfer completed without error. Note that this does not indicate that the entire amount of requested data was transferred. */
  LIBUSB_TRANSFER_COMPLETED,
  //** Transfer failed */
  LIBUSB_TRANSFER_ERROR,
  //** Transfer timed out */
  LIBUSB_TRANSFER_TIMED_OUT,
  //** Transfer was cancelled */
  LIBUSB_TRANSFER_CANCELLED,
  //** For bulk/interrupt endpoints: halt condition detected (endpoint stalled). For control endpoints: control request not supported. */
  LIBUSB_TRANSFER_STALL,
  //** Device was disconnected */
  LIBUSB_TRANSFER_NO_DEVICE,
  //** Device sent more data than requested */
  LIBUSB_TRANSFER_OVERFLOW
);



{* A structure representing the standard USB device descriptor.
This descriptor is documented in section 9.6.1 of the USB 2.0 specification.
All multiple-byte fields are represented in host-endian format.*}
type
   libusb_device_descriptor = packed record {sizeof=18}
    bLength,
    bDescriptorType:    cuint8;// uint8_t;
    bcdUSB:             cuint16;  //uint16_t;
    bDeviceClass,
    bDeviceSubClass,
    bDeviceProtocol,
    bMaxPacketSize0:    cuint8;
    idVendor,
    idProduct,
    bcdDevice:          cuint16;
    iManufacturer,
    iProduct,
    iSerialNumber,
    bNumConfigurations: cuint8;
  end;
  Plibusb_device_descriptor = ^libusb_device_descriptor;
// PPlibusb_device_descriptor = ^Plibusb_device_descriptor;


{* A structure representing the standard USB endpoint descriptor. This descriptor is documented in section 9.6.3 of the USB 2.0 specification.
All multiple-byte fields are represented in host-endian format.*}
type
  libusb_endpoint_descriptor = packed record
    bLength,
    bDescriptorType,
    bEndpointAddress,
    bmAttributes:     cuint8;
    wMaxPacketSize:   cuint16;
    bInterval,
    bRefresh,
    bSynchAddress:    cuint8;
    extra:            pcchar;// PByte;
    extra_length:     cint;
  end;
  libusb_endpoint_descriptor_array=packed array[0..0] of libusb_endpoint_descriptor;
  Plibusb_endpoint_descriptor_array = ^libusb_endpoint_descriptor_array;

{* A structure representing the standard USB interface descriptor. This descriptor is documented in section 9.6.5 of the USB 2.0 specification.
All multiple-byte fields are represented in host-endian format.*}
type
  libusb_interface_descriptor = packed record
    bLength,
    bDescriptorType,
    bInterfaceNumber,
    bAlternateSetting,
    bNumEndpoints,
    bInterfaceClass,
    bInterfaceSubClass,
    bInterfaceProtocol,
    iInterface:         cuint8;
    endpoint:           Plibusb_endpoint_descriptor_array;
    extra:              pcchar;
    extra_length:       cint;
  end;
  libusb_interface_descriptor_array=packed array[0..0] of libusb_interface_descriptor;
  Plibusb_interface_descriptor_array = ^libusb_interface_descriptor_array;

{* A collection of alternate settings for a particular USB interface.*}
type
  libusb_interface = packed record
    altsetting:       Plibusb_interface_descriptor_array;
    num_altsetting:   cint;
  end;
  libusb_interface_array=packed array[0..0] of libusb_interface;
  Plibusb_interface_array = ^libusb_interface_array;

{* A structure representing the standard USB configuration descriptor. This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All multiple-byte fields are represented in host-endian format.*}
type
  Plibusb_config_descriptor  = ^libusb_config_descriptor;
  PPlibusb_config_descriptor = ^Plibusb_config_descriptor;
  libusb_config_descriptor = packed record
    bLength,
    bDescriptorType:      cuint8;
    wTotalLength:         cuint16;
    bNumInterfaces,
    bConfigurationValue,
    iConfiguration,
    bmAttributes,
    bMaxPower:            cuint8;
    usb_interface:        Plibusb_interface_array;
    extra:                pcchar;
    extra_length:         cint;
  end;

{ * Setup packet for control transfers. *}
type
  Plibusb_control_setup = ^libusb_control_setup;
  libusb_control_setup = packed record
    bmRequestType,
    bRequest:         cuint8;
    wValue,
    wIndex,
    wLength:          cuint16;
  end;

const
    LIBUSB_CONTROL_SETUP_SIZE =  (SizeOf( libusb_control_setup));

{* libusb *}
type
  Plibusb_context = ^libusb_context;
  PPlibusb_context = Plibusb_context;
  libusb_context = packed record
  {undefined structure}
//         dummy:  uint64;
  end;

const
    LIBUSB_PATH_MAX=512;
    USB_MAXCONFIG=8;

type

  Plibusb_device = ^libusb_device;
  PPlibusb_device = ^Plibusb_device;//1D array of plibusb_device pointers
  PPPlibusb_device = ^PPlibusb_device;//2D array

  libusb_device = packed record
  {undefined structure}
//    dummy:  uint64;
  end;

  {https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html}
  Ttime_t= clong;//size differs for 32 bit OS?
  Ptimeval= ^TTimeval;
  Ttimeval= record
     tv_sec: TTime_t;
     tv_usec:clong;
  end;

  Plibusb_device_handle = ^libusb_device_handle;
  PPlibusb_device_handle = ^Plibusb_device_handle;
  libusb_device_handle = THandle;

{* libusb_transfer.flags values *}
const LIBUSB_TRANSFER_SHORT_NOT_OK  = 1 shl 0;  // $01
const LIBUSB_TRANSFER_FREE_BUFFER   = 1 shl 1;  // $02
const LIBUSB_TRANSFER_FREE_TRANSFER = 1 shl 2;  // $04

{*Isochronous packet descriptor. *}
type
  Plibusb_iso_packet_descriptor = ^libusb_iso_packet_descriptor;
  libusb_iso_packet_descriptor = packed record
  length,
  actual_length: cuint;
  status:        libusb_transfer_status;
    end;

type
  Pstructlibusb_transfer = ^structlibusb_transfer;
  libusb_transfer_cb_fn= procedure(transfer : Pstructlibusb_transfer);

  Plibusb_transfer_cb_fn = ^libusb_transfer_cb_fn;
  structlibusb_transfer = packed record
    dev_handle:       plibusb_device_handle;
    flags:            cuint8;
    endpoint:         cuint8;
    ctype:            cuint8; // type is reserved word
    timeout:          cuint;
    //stream_id:      cuint32;
    status:           libusb_transfer_status;
    length:           csize_t;
    actual_length:    csize_t;
    callback:         Plibusb_transfer_cb_fn;
     user_data:        pointer;
    buffer:           pcchar;
    num_iso_packets:  cint;
    iso_packet_desc:  libusb_iso_packet_descriptor;
  end;

//added Aug 27, 2019
//http://libusb.sourceforge.net/api-1.0/group__hotplug.html#ga556d598ca379618a41bbec3597f55dcf
  libusb_hotplug_event =cuint8;

{$define   LIBUSB_HOTPLUG_MATCH_ANY:=-1}

const LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED = $01; //A device has been plugged in and is ready to use.
const LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT    = $02; //A device has left and is no longer available.

type
  libusb_hotplug_flag=cuint8;

const LIBUSB_HOTPLUG_NO_FLAGS  = $00; //Default value when not using any flags.
const LIBUSB_HOTPLUG_ENUMERATE = $01; //Arm the callback and fire it for all matching currently attached devices.

type
    libusb_hotplug_callback_fn= function(ctx:Plibusb_context; device: Plibusb_device; event:libusb_hotplug_event; user_data:pointer):cint;

    libusb_hotplug_callback_handle=cint;
    Plibusb_hotplug_callback_handle=^libusb_hotplug_callback_handle;

//******************************************************************************
//------------------------------------------------------------------------------
// Library functions and procedures
//------------------------------------------------------------------------------
//******************************************************************************
function libusb_init(ctx:PPlibusb_context):cint;CALLING_CONV;external LIB_NAME;
procedure libusb_exit(ctx:Plibusb_context);CALLING_CONV;external LIB_NAME;

procedure libusb_set_debug(ctx:Plibusb_context; level:cint);CALLING_CONV;external LIB_NAME;

function libusb_get_device_list(ctx:Plibusb_context; list: PPPlibusb_device): csize_t;CALLING_CONV;external LIB_NAME;
procedure libusb_free_device_list(list:PPlibusb_device; unref_devices:cint);CALLING_CONV;external LIB_NAME;
{<stupid bug - related to the semantics of unref_devices parameter; thought it was the length the list; NOT true}
function libusb_get_bus_number(dev: Plibusb_device):cuint8;CALLING_CONV;external LIB_NAME;  //Returns the number of the bus contained by the device dev.
function libusb_get_device_address(dev: Plibusb_device):cuint8;CALLING_CONV;external LIB_NAME;  // Returns the device_address contained by the device dev.

function libusb_get_configuration(dev: Plibusb_device_handle; config: PLongint): cint;CALLING_CONV;external LIB_NAME;

function libusb_get_device_descriptor(dev: Plibusb_device; desc:Plibusb_device_descriptor): cint;CALLING_CONV;external LIB_NAME;
function libusb_get_active_config_descriptor(dev: Plibusb_device; config:PPlibusb_config_descriptor): cint;CALLING_CONV;external LIB_NAME;

function libusb_get_config_descriptor(dev: Plibusb_device;config_index:cuint8; config: PPlibusb_config_descriptor):cint;CALLING_CONV;external LIB_NAME;

function libusb_get_max_packet_size(dev: Plibusb_device; endpoint: Byte): cint;CALLING_CONV;external LIB_NAME;
function libusb_get_max_iso_packet_size(dev: Plibusb_device; endpoint: Byte): cint;CALLING_CONV;external LIB_NAME;

function libusb_open(dev: Plibusb_device; handle: PPlibusb_device_handle): cint;CALLING_CONV;external LIB_NAME;
procedure libusb_close(dev_handle:Plibusb_device_handle);CALLING_CONV;external LIB_NAME;
function libusb_get_device(dev_handle: Plibusb_device_handle): libusb_device;CALLING_CONV;external LIB_NAME;

function libusb_set_configuration(dev: Plibusb_device_handle; configuration: cint): cint;CALLING_CONV;external LIB_NAME;

function libusb_claim_interface(dev: Plibusb_device_handle; iface: cint): cint;CALLING_CONV;external LIB_NAME;
function libusb_release_interface(dev: Plibusb_device_handle; iface: cint): cint;CALLING_CONV;external LIB_NAME;

function libusb_open_device_with_vid_pid(ctx: Plibusb_context; vendor_id: cuint16; product_id: cuint16): Plibusb_device_handle;CALLING_CONV;external LIB_NAME;

function libusb_set_interface_alt_setting(dev: Plibusb_device_handle;interface_number: cint;alternate_setting: cint): cint;CALLING_CONV;external LIB_NAME;
function libusb_clear_halt(dev: Plibusb_device_handle; endpoint: BYTE): cint;CALLING_CONV;external LIB_NAME;
function libusb_reset_device(dev: Plibusb_device_handle): cint;CALLING_CONV;external LIB_NAME;


//function libusb_auto_detach_kernel_driver	(dev:Plibusb_device_handle; enable:cint): cint;CALLING_CONV;external LIB_NAME;
function libusb_kernel_driver_active(dev: Plibusb_device_handle; theinterface: cint): cint;CALLING_CONV;external LIB_NAME;
function libusb_detach_kernel_driver(dev: Plibusb_device_handle; theinterface: cint): cint;CALLING_CONV;external LIB_NAME;
function libusb_attach_kernel_driver(dev: Plibusb_device_handle; theinterface: cint): cint;CALLING_CONV;external LIB_NAME;

//------------------------------------------------------------------------------
// Async I/O
//------------------------------------------------------------------------------
{this needs testing - also see datastructure definitions}
function libusb_submit_transfer(transfer: Pstructlibusb_transfer):cint;CALLING_CONV;external LIB_NAME;
function libusb_cancel_transfer(transfer: Pstructlibusb_transfer):cint;CALLING_CONV;external LIB_NAME;
procedure libusb_free_transfer(transfer: Pstructlibusb_transfer);CALLING_CONV;external LIB_NAME;
function libusb_alloc_transfer(iso_packets:cint):Pstructlibusb_transfer;CALLING_CONV;external LIB_NAME;
//------------------------------------------------------------------------------
// Sync I/O
//------------------------------------------------------------------------------
function libusb_control_transfer(dev_handle: Plibusb_device_handle; request_type: cuint8; request: cuint8; value: cuint16; index: cuint16; data: pcchar; length:cuint16; timeout: cuint):cint;CALLING_CONV;external LIB_NAME;
function libusb_bulk_transfer(dev_handle: Plibusb_device_handle; endpoint:BYTE; data: pcchar; length:cint;actual_length:Plongint; timeout:cuint):cint;CALLING_CONV;external LIB_NAME; //tested
function libusb_interrupt_transfer(dev_handle: Plibusb_device_handle; endpoint:byte; data:pcchar; length:cint; actual_length:Plongint; timeout:cuint):cint;CALLING_CONV;external LIB_NAME;

function libusb_get_string_descriptor_ascii(dev:plibusb_device_handle; index:cuint8; data:pcchar; length:cint):cint;CALLING_CONV;external LIB_NAME;

{/* polling and timeouts */}
function libusb_try_lock_events(ctx:Plibusb_context):cint;CALLING_CONV;external LIB_NAME;
procedure libusb_lock_events(ctx:Plibusb_context);CALLING_CONV;external LIB_NAME;
procedure libusb_unlock_events(ctx:Plibusb_context);CALLING_CONV;external LIB_NAME;
function libusb_event_handling_ok(ctx:Plibusb_context):cint;CALLING_CONV;external LIB_NAME;
function libusb_event_handler_active(ctx:Plibusb_context):cint;CALLING_CONV;external LIB_NAME;
procedure libusb_lock_event_waiters(ctx:Plibusb_context); CALLING_CONV;external LIB_NAME;
procedure libusb_unlock_event_waiters(ctx:Plibusb_context); CALLING_CONV;external LIB_NAME;


{added Aug 23, 2019}
function libusb_handle_events_completed(ctx:Plibusb_context; completed: pcint):cint;CALLING_CONV;external LIB_NAME;
function libusb_handle_events_timeout_completed(ctx:Plibusb_context; tv:Ptimeval; completed:pcint):cint;CALLING_CONV;external LIB_NAME;


//hotplug callback registratino added Aug 27, 2019

function libusb_hotplug_register_callback(ctx:Plibusb_context; events:libusb_hotplug_event; flags:libusb_hotplug_flag; vendor_id, product_id, dev_class:cint; cb_fn:libusb_hotplug_callback_fn; user_data: pointer;
                                          handle:Plibusb_hotplug_callback_handle):cint;external LIB_NAME;
procedure libusb_hotplug_deregister_callback(ctx:Plibusb_context; handle:Plibusb_hotplug_callback_handle);CALLING_CONV;external LIB_NAME;


implementation


end.



