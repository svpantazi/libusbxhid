unit libusbhid;

{<Implements a subset of HID calls using libusb. No other dependencies except libusb.

update log
Aug 29, 2019 - libusbhid_get_index_of_device_from_list return instance count parameter (J. Richters)
Aug 28, 2019 - modified libusbhid_interrupt_read and libusbhid_interrupt_write parameters and return semantics - return is now the libusb result code
Aug 23, 2019 - added libusbhid_detect_device to allow detection of device (J. Richters)
Aug 23, 2019 - added libusb_handle_events_timeout_completed call to allow termination of blocking (0 timeout) calls
Aug 19, 2019 - conditional debug message defines and return codes for debug messages to help with debugging
Aug 18, 2019 - added default timeout params to calls}

{ Copyright (C) 2013-2019  S. V. Pantazi (svpantazi@gmail.com)

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$mode fpc}
{$modeswitch OUT}
{$modeswitch DEFAULTPARAMETERS}
{$modeswitch RESULT}

{$packrecords C}

interface

{$MACRO ON}

{$define DEBUG_MSG} {enable/disable this define for debug messages on/off}

uses
{$ifdef DEBUG_MSG}
  sysutils, {for Format()}
{$endif}

  libusbx;

const

{request type constants}
LIBUSB_CONTROL_REQUEST_TYPE_IN  = LIBUSB_ENDPOINT_IN  or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE;
LIBUSB_CONTROL_REQUEST_TYPE_OUT = LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE;

{report constants}
HID_GET_REPORT = $01;
HID_SET_REPORT = $09;

{report type constants}
HID_REPORT_TYPE_INPUT   = $01;
HID_REPORT_TYPE_OUTPUT  = $02;
HID_REPORT_TYPE_FEATURE = $03;

type

{Plibusbhid_context=^libusbhid_context; no pointer needed since using var and out parameters in open and close device}
  libusbhid_context=record
    usb_context:          Plibusb_context;
    usb_interface_result: longint;
    usb_driver_detached:  boolean;
    usb_lib_init_result:  longint;
    usb_device_handle:    Plibusb_device_handle;
  end;

Var 

   LIBUSB_DEBUG_LEVEL: Integer = LIBUSB_LOG_LEVEL_INFO;

function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; const vid,pid:word; const instance_number:byte; out instance_count:byte):longint;
{<Loads all attached devices in a device list; libusb_device is an opaque record, cannot use its content, but each device gets one and can use it further to get a bus number and address of a device,
but most importantly a device descriptor that can be checked for vid and pid of the desired device}

function libusbhid_detect_device(const vid,pid:word; const instance_number:byte):byte;
{<Initializes libusb library, uses libusbhid_get_index_of_device_from_list to check if the n-th instance (n is the instance number starting at 1) of a device is attached;
Returns the count of instances of a device.}

function  libusbhid_open_device(const vid,pid:word; const instance_number:byte; out hid_device_context:libusbhid_context; const clear_halt_81:boolean=false):boolean;
{<Attempts to open the n-th instance (n is the instance number starting at 1) of a device with a given vid and pid.
The instance number is necessary when multiple identical devices exist on the same system.}

function  libusbhid_get_report(var hid_device_context:libusbhid_context; const report_type,report_number:byte; const report_length:word; out report_data{:array of byte}; const timeout:dword=0):longint;
function  libusbhid_set_report(var hid_device_context:libusbhid_context; const report_type,report_number:byte; const report_length:word; var report_data{:array of byte}; const timeout:dword=0):longint;

function  libusbhid_interrupt_read(var hid_device_context:libusbhid_context; const in_endpoint:byte; out data_from_device{array of byte}; const max_data_length:byte; out transferred_data_length:longint; const timeout:dword=0):longint;
{<Waits for up to max_data_length bytes of data to be read from device. If timeout=0 then this is a blocking read and ideally belongs in a thread.
Returns LIBUSB_SUCCESS or a negative error code. transferred_data_length is the length of actual transfered data.}

function  libusbhid_interrupt_write(var hid_device_context:libusbhid_context; const out_endpoint:byte; var data_into_device{:array of byte}; const max_data_length:byte; out transferred_data_length:longint; const timeout:dword=0):longint;
{<Writes up to max_data_length bytes of data into the device.
Returns LIBUSB_SUCCESS or a negative error code. transferred_data_length is the length of actual transfered data}

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);


{$ifdef DEBUG_MSG}
  var
    DEBUG_MSG_OPTION:boolean=true;
{$endif}

implementation


{$ifdef DEBUG_MSG}

{$define TIME_FORMAT:='h:nn:ss:zzz'}

  procedure DBG_MSG(msg:string);
  begin
    if DEBUG_MSG_OPTION then WriteLn('DEBUG ',msg);
  end;
{$endif}

function libusbhid_get_report(var hid_device_context:libusbhid_context; const report_type,report_number:byte; const report_length:word; out report_data{array of byte}; const timeout:dword=0):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,
              LIBUSB_CONTROL_REQUEST_TYPE_IN, HID_GET_REPORT, (report_type << 8) or report_number,
              0 {interface_num}, @report_data, report_length, timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then DBG_MSG(Format('control transfer from usb device failed! return code: %d',[Result]))
  else DBG_MSG(Format('%s libusbhid_get_report. received: %d bytes from device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_set_report(var hid_device_context:libusbhid_context; const report_type,report_number:byte; const report_length:word; var report_data{array of byte}; const timeout:dword=0):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,
                LIBUSB_CONTROL_REQUEST_TYPE_OUT, HID_SET_REPORT, (report_type << 8) or report_number,
                0{interface_num}, @report_data, report_length, timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then DBG_MSG(Format('control transfer to usb device failed! return code: %d',[Result]))
  else DBG_MSG(Format('%s libusbhid_set_report. written: %d bytes to device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_interrupt_write(var hid_device_context:libusbhid_context; const out_endpoint:byte; var data_into_device{array of byte}; const max_data_length:byte; out transferred_data_length:longint; const timeout:dword=0):longint;
begin
  Result:=libusb_interrupt_transfer(hid_device_context.usb_device_handle, out_endpoint, @data_into_device, max_data_length, @transferred_data_length,timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then
  begin
    if Result<>LIBUSB_ERROR_TIMEOUT then WriteLn('interrupt write to usb device failed! return code: ',Result)
    else DBG_MSG(Format('%s libusbhid_interrupt_write. TIMEOUT bytes written: %d',[FormatDateTime(TIME_FORMAT,Now()),transferred_data_length]))
  end
  else DBG_MSG(Format('%s libusbhid_interrupt_write. sent: %d bytes to device ',[FormatDateTime(TIME_FORMAT,Now()),transferred_data_length]));
{$endif}
end;

function libusbhid_interrupt_read(var hid_device_context:libusbhid_context; const in_endpoint:byte; out data_from_device{array of byte}; const max_data_length:byte; out transferred_data_length:longint; const timeout:dword=0):longint;
begin
  Result:=libusb_interrupt_transfer(hid_device_context.usb_device_handle, in_endpoint, @data_from_device, max_data_length, @transferred_data_length, timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then
  begin
    if Result<>LIBUSB_ERROR_TIMEOUT then WriteLn('libusbhid_interrupt_read. failed! return code: ',Result)
    else DBG_MSG(Format('%s libusbhid_interrupt_read. TIMEOUT bytes read: %d',[FormatDateTime(TIME_FORMAT,Now()),transferred_data_length]))
  end
  else DBG_MSG(Format('%s libusbhid_interrupt_read. received: %d bytes from device ',[FormatDateTime(TIME_FORMAT,Now()),transferred_data_length]));
{$endif}
end;

function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; const vid,pid:word; const instance_number:byte; out instance_count:byte):longint;
var
  {$ifdef DEBUG_MSG}  busNumber,devAddress:byte;  {$endif}
  usb_device:           Plibusb_device;
  descriptor_result,i:  longint;
  usb_descriptor:       libusb_device_descriptor;
begin
  instance_count:=0;
  Result:=-1;
  i:=0;
  while (device_list[i]<> nil) do
  begin
    usb_device:= device_list[i];
    descriptor_result:=libusb_get_device_descriptor(usb_device, @usb_descriptor);
    {debug}//DBG_MSG('Size of usb descriptor record: ',sizeof(usb_descriptor));
    if (descriptor_result < LIBUSB_SUCCESS) then
    begin
{$ifdef DEBUG_MSG}DBG_MSG('Failed to get device descriptor');{$endif}
      Break;
    end
    else
    begin
      {$ifdef DEBUG_MSG}
      busNumber:=libusb_get_bus_number(usb_device);
      devAddress:=libusb_get_device_address(usb_device);
      DBG_MSG(Format('%0.4x:%0.4x, bus: %d, address: %d',[usb_descriptor.idVendor, usb_descriptor.idProduct,busNumber,devAddress]));
      {$endif}

      if (usb_descriptor.idVendor=vid) and (usb_descriptor.idProduct=pid) then
      begin
        {$ifdef DEBUG_MSG}      DBG_MSG(Format('Found device with vid:pid $%x : $%x at idx:%d!',[vid,pid,i]));{$endif}
        Inc(instance_count);
        if (instance_number-instance_count)=0 then
        begin
          {$ifdef DEBUG_MSG}DBG_MSG(Format('Device instance found: %d  at idx:%d!',[instance_number,i]));{$endif}
          Result:=i;
        end;
      end;
    end;
    Inc(i);
  end;
  {$ifdef DEBUG_MSG}DBG_MSG(Format('Number of devices found with vid:pid $%x : $%x = %d',[vid,pid,instance_count]));{$endif}
end;

function libusbhid_detect_device(const vid,pid:word; const instance_number:byte):byte;
var
  devIdx: longint;
  usb_device_list:PPlibusb_device;
  usb_device_count,
  lib_init_result:      longint;
  usb_context:          Plibusb_context;
begin
  Result:=0;//be pessimistic - assume there are no devices attached...
  lib_init_result:=libusb_init(@usb_context);
  if lib_init_result < LIBUSB_SUCCESS then
  begin
{$ifdef DEBUG_MSG}DBG_MSG('Cannot open libusb 1.0 library. You really need this..');{$endif}
  end
  else
  begin
    libusb_set_debug(usb_context,{debug level=}LIBUSB_DEBUG_LEVEL);

    usb_device_count:=libusb_get_device_list(usb_context,@usb_device_list);

{$ifdef DEBUG_MSG}DBG_MSG(Format('Found %d devices attached',[usb_device_count]));{$endif}

{go through hid_device_context list and open the appropriate instance}
    devIdx:=libusbhid_get_index_of_device_from_list(usb_device_list, vid,pid,instance_number,Result);

{$ifdef DEBUG_MSG}
    if devIdx>=0 then DBG_MSG(Format('Index of device $%x:$%x instance %d is %d',[vid,pid,instance_number, devIdx]))
    else DBG_MSG(Format('Device $%x:$%x instance %d cannot be found',[vid,pid,instance_number]));
{$endif}

{cannot use this convenience call to open multiple instance of a hid_device_context with same vid/pid
  usb_device_handle:=libusb_open_device_with_vid_pid(usb_context,vid,pid);}

{whatever the outcome of opening the device, the device list must be freed - or memory leaks will ensue}
{$ifdef DEBUG_MSG} DBG_MSG(Format('Freeing device list with %d devices',[usb_device_count]));{$endif}

    libusb_free_device_list(usb_device_list,{NOT usb_device_count}1{unreference? 0=no, 1=yes}); //free the list, unref the devices in it?

{$ifdef DEBUG_MSG} DBG_MSG('USB device list freed. good boy!');{$endif}

{we're done, must exit libusb as well}
    libusb_exit(usb_context);
  end;
end;


function libusbhid_open_device(const vid,pid:word; const instance_number:byte; out hid_device_context:libusbhid_context; const clear_halt_81:boolean=false):boolean;
var
  device_instance_count:byte;
  devIdx: longint;
  usb_device_list:PPlibusb_device;
  active_config,
  usb_device_count: longint;
  res:longint;
begin
  Result:=false;//so pessimistic...

  with hid_device_context do
  begin
    usb_driver_detached:=false;

    usb_lib_init_result:=libusb_init(@usb_context);

    if usb_lib_init_result < LIBUSB_SUCCESS then
    begin
{$ifdef DEBUG_MSG}DBG_MSG('Cannot open libusb 1.0 library. You really need this..');{$endif}
    end
    else
    begin
      libusb_set_debug(usb_context,LIBUSB_DEBUG_LEVEL);

      usb_device_count:=libusb_get_device_list(usb_context,@usb_device_list);

{$ifdef DEBUG_MSG}DBG_MSG(Format('Found %d devices attached',[usb_device_count]));{$endif}

{go through hid_device_context list and open the appropriate instance}
      devIdx:=libusbhid_get_index_of_device_from_list(usb_device_list, vid,pid,instance_number,device_instance_count);

{$ifdef DEBUG_MSG}DBG_MSG(Format('Index of device %d:%d=%d',[vid,pid,devIdx]));{$endif}
  {cannot use this convenience call to open multiple instance of a hid_device_context with same vid/pid
      usb_device_handle:=libusb_open_device_with_vid_pid(usb_context,vid,pid);}
      if devIdx>=0 then
      begin
        {SP Sep 2015 - there as a bug here - usb_device_list[devIdx] instead of @usb_device_list[devIdx]^}

        res:=libusb_open(@usb_device_list[devIdx]^,@usb_device_handle);

{$ifdef DEBUG_MSG}
        if res=LIBUSB_SUCCESS then DBG_MSG('Device opened. Next I must claim the interface.')
        else DBG_MSG(Format('Cannot open device. Reasons? A clue; result code: %d',[res]));
{$endif}

      end
      else
      begin
        usb_device_handle:=nil;

{$ifdef DEBUG_MSG} DBG_MSG(Format('Cannot open device with idx: %d',[devIdx]));{$endif}

      end;

{whatever the outcome of opening the device, the device list must be freed - or memory leaks will ensue}
{$ifdef DEBUG_MSG} DBG_MSG(Format('Freeing device list with %d devices',[usb_device_count]));{$endif}

      libusb_free_device_list(usb_device_list,{NOT usb_device_count}1{unreference? 0=no, 1=yes}); //free the list, unref the devices in it?

{$ifdef DEBUG_MSG} DBG_MSG('USB device list freed. good boy!');{$endif}


      if usb_device_handle<>nil then
      begin
        if clear_halt_81 then
        begin
{kernel driver attaching problem; device may open but still be busy - this attempts to go around that -
I have never been able to fully test so - beware}
{$ifdef DEBUG_MSG}DBG_MSG('device attempting go clear halt on ep $81');{whatever.. seems to fail everytime anyway}       {$endif}

          res:=libusb_clear_halt(usb_device_handle, $81);

  {$ifdef DEBUG_MSG}
          if res=LIBUSB_SUCCESS then DBG_MSG('clear halt successful')
          else DBG_MSG(Format('clear halt failed (it''s ok, endpoint was NOT busy); error result: %d',[res]));//I've never seen this succeeding :(; eh whatever
  {$endif}
        end;
(*        if libusb_auto_detach_kernel_driver(usb_device_handle,1{enable autodetach})=0 then DBG_MSG('Setting autodetach kernel driver')
        else*)
        begin
//          DBG_MSG('Autodetach did not work. Checking if kernel driver is active...?');
{device busy? try to detach kernel driver so I can claim the interface}
          if (libusb_kernel_driver_active(usb_device_handle,0{interface number})=1) then
          begin

{$ifdef DEBUG_MSG}DBG_MSG('device busy - driver active');        {$endif}

            res:=libusb_detach_kernel_driver(usb_device_handle,0{interface number});
            if res=LIBUSB_SUCCESS then
            begin
              usb_driver_detached:=true;

{$ifdef DEBUG_MSG}DBG_MSG ('driver detached');        {$endif}
            end;
          end
{$ifdef DEBUG_MSG}
          else DBG_MSG('driver inactive - can claim interface');
{$endif}
        end;

{$ifdef DEBUG_MSG}  DBG_MSG('getting configuration....');        {$endif}

        if libusb_get_configuration(usb_device_handle,@active_config)=LIBUSB_SUCCESS then
        begin

{$ifdef DEBUG_MSG}  DBG_MSG(Format('active config: %d',[active_config]));        {$endif}

          if active_config<>1 then
          begin

            res:=libusb_set_configuration(usb_device_handle,{configuration=}1);

{$ifdef DEBUG_MSG}
            if res=LIBUSB_SUCCESS then DBG_MSG('Configuration set')
            else DBG_MSG(Format('Could not set configuration. error result:%d ',[res]));
{$endif}
          end
        end

{$ifdef DEBUG_MSG} else DBG_MSG('Error getting configuration.');   {$else} ;     {$endif}

{some devices may want firmware uploads, configurations, hardware initialization and setup etc. for simple IO devices, nevermind this}
    //    libusb_set_configuration(usb_device_handle, 0);

{$ifdef DEBUG_MSG} DBG_MSG('Claiming interface.....fingers crossed...'); {$endif}

        usb_interface_result:=libusb_claim_interface(usb_device_handle, 0);
        if usb_interface_result=LIBUSB_SUCCESS then
        begin
          Result:=true;//success

{$ifdef DEBUG_MSG} DBG_MSG ('Interface claimed ... yay!'); {$endif}
        end

{$ifdef DEBUG_MSG}
        else
        begin
          DBG_MSG ('Cannot claim interface - drat!');
          case usb_interface_result of
            LIBUSB_ERROR_NOT_FOUND: DBG_MSG('not found');
            LIBUSB_ERROR_BUSY:      DBG_MSG('busy');
            LIBUSB_ERROR_NO_DEVICE: DBG_MSG('no device');
            else DBG_MSG('for some reasons');
          end;
        end
{$endif}

      end;
    end;
  end;
end;

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);
var
  tv:Ttimeval;
  res:longint;
begin
  with hid_device_context do
  begin
    if usb_lib_init_result=LIBUSB_SUCCESS then
    begin
      if usb_device_handle<>nil then
      begin
        if (usb_interface_result=LIBUSB_SUCCESS) then
        begin

          tv.tv_sec:=0;
          tv.tv_usec:=0;
          res:=libusb_handle_events_timeout_completed(hid_device_context.usb_context,@tv,{completed=}nil);

{$ifdef DEBUG_MSG}
          if(res<>LIBUSB_SUCCESS) then DBG_MSG(Format('Cannot cancel event timeout. error result: %d',[res]))
          else DBG_MSG('libusb_handle_events_timeout_completed worked');
{$endif}

          usb_interface_result:=libusb_release_interface(usb_device_handle, 0);

{$ifdef DEBUG_MSG}
          if(usb_interface_result<>LIBUSB_SUCCESS) then DBG_MSG(Format('Cannot release interface. error result: %d',[usb_interface_result]))
          else DBG_MSG('Interface released. Phew..');
{$endif}

          if usb_driver_detached then
          begin
            if libusb_attach_kernel_driver(usb_device_handle,0)=0 then

{$ifdef DEBUG_MSG}DBG_MSG('Driver re-attached. Good boy.');{$endif}

          end;
        end;

        libusb_close(usb_device_handle);
      end;

      libusb_exit(usb_context);
    end;
  end;
end;


end.


