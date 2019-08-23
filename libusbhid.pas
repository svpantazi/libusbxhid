unit libusbhid;

{<Implements a subset of HID calls using libusb. No other dependencies except libusb.


update log
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

{$define DEBUG_MSG} {enable this define for debug messages}

uses
{$ifdef DEBUG_MSG}
  sysutils, {for Format()}
{$endif}

  libusbx;

const

{request type constants}
LIBUSB_CONTROL_REQUEST_TYPE_IN = LIBUSB_ENDPOINT_IN or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE;
LIBUSB_CONTROL_REQUEST_TYPE_OUT = LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE;

{report constants}
HID_GET_REPORT = $01;
HID_SET_REPORT = $09;

{report type constants}
HID_REPORT_TYPE_INPUT = $01;
HID_REPORT_TYPE_OUTPUT = $02;
HID_REPORT_TYPE_FEATURE = $03;

type
  Tuint8=byte;
  Tsint16=SmallInt;

{Plibusbhid_context=^libusbhid_context; not needed since using var and out parameters in open and close device}

  libusbhid_context=record
    usb_context:          Plibusb_context;
    usb_interface_result: longint;
    usb_driver_detached:  boolean;
    usb_lib_init_result:       longint;
    usb_device_handle:    Plibusb_device_handle;
  end;

function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; vid,pid:word; instanceId:Tuint8):Tsint16;
{<Loads all attached devices in a device list; libusb_device is an opaque record, cannot use its content, but each device gets one and can use it further to get a bus number and address of a device,
but most importantly a device descriptor that can be checked for vid and pid of the desired device}

function  libusbhid_open_device(vid,pid:word; instanceId:Tuint8; out hid_device_context:libusbhid_context):boolean;
{<Opens a device instance given by the instance id (starts at 1) of a device with a given vid and pid. The instance id is necessary if multiple identical devices exist on the same system.}

function  libusbhid_get_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; out report_data{:array of byte}; const timeout:dword=0):longint;
function  libusbhid_set_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; var report_data{:array of byte}; const timeout:dword=0):longint;

function  libusbhid_interrupt_read(var hid_device_context:libusbhid_context; in_endpoint:byte; out data_from_device{array of byte}; const data_length:byte; const timeout:dword=0):longint;
{<Waits for data to be read from device. If timeout=0 then this is a blocking read and ideally belongs in a thread.}

function  libusbhid_interrupt_write(var hid_device_context:libusbhid_context; out_endpoint:byte; var data_into_device{:array of byte}; const data_length:byte; const timeout:dword=0):longint;
{<Writes data into the device.}

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);


implementation


{$ifdef DEBUG_MSG}

{$define TIME_FORMAT:='h:nn:ss:zzz'}

	procedure DBG_MSG(msg:string);
  begin
    WriteLn('DEBUG ',msg);
  end;
{$endif}



function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; vid,pid:word; instanceId:Tuint8):Tsint16;
var
  {$ifdef DEBUG_MSG}  busNumber,devAddress:Tuint8;      {$endif}
	usb_device:Plibusb_device;
  instanceCounter:Tuint8;
	descriptor_result, i:Tsint16;
  usb_descriptor: libusb_device_descriptor;
begin
  instanceCounter:=1;
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
        {$ifdef DEBUG_MSG}      DBG_MSG(Format('Found device with vid:pid %d:%d at idx:%d!',[vid,pid,i]));{$endif}
        if (instanceId-instanceCounter)=0 then
        begin
          {$ifdef DEBUG_MSG}DBG_MSG(Format('Device instance found: %d  at idx:%d!',[instanceId,i]));{$endif}
          Result:=i;
        end;
        Inc(instanceCounter);
      end;
    end;
    Inc(i);
  end;
end;

function libusbhid_get_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; out report_data{array of byte}; const timeout:dword=0):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,
					    LIBUSB_CONTROL_REQUEST_TYPE_IN, HID_GET_REPORT, (reportType << 8) or reportNum,
						  0 {interface_num}, @report_data, reportLen, timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then DBG_MSG(Format('control transfer from usb device failed! return code: %d',[Result]))
  else DBG_MSG(Format('%s libusbhid_get_report. received: %d bytes from device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_set_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; var report_data{array of byte}; const timeout:dword=0):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,
					  		LIBUSB_CONTROL_REQUEST_TYPE_OUT, HID_SET_REPORT, (reportType << 8) or reportNum,
    						0{interface_num}, @report_data, reportLen, timeout);

{$ifdef DEBUG_MSG}
  if Result < LIBUSB_SUCCESS then DBG_MSG(Format('control transfer to usb device failed! return code: %d',[Result]))
  else DBG_MSG(Format('%s libusbhid_set_report. written: %d bytes to device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_interrupt_write(var hid_device_context:libusbhid_context; out_endpoint:byte; var data_into_device{array of byte}; const data_length:byte; const timeout:dword=0):longint;
var
  return_code:longint;
begin
  return_code:=libusb_interrupt_transfer(hid_device_context.usb_device_handle,out_endpoint,@data_into_device, data_length, @Result,timeout);

  if return_code < LIBUSB_SUCCESS then
  begin
    if return_code<>LIBUSB_ERROR_TIMEOUT then WriteLn('interrupt write to usb device failed! return code: ',return_code)
{$ifdef DEBUG_MSG}
		else DBG_MSG(Format('%s libusbhid_interrupt_write. TIMEOUT bytes written: %d',[FormatDateTime(TIME_FORMAT,Now()),Result]))
{$endif}
  end
{$ifdef DEBUG_MSG}
  else DBG_MSG(Format('%s libusbhid_interrupt_write. sent: %d bytes to device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_interrupt_read(var hid_device_context:libusbhid_context; in_endpoint:byte; out data_from_device{array of byte}; const data_length:byte; const timeout:dword=0):longint;
var
  return_code:longint;
begin
  return_code:=libusb_interrupt_transfer(hid_device_context.usb_device_handle,in_endpoint,@data_from_device, data_length, @Result, timeout);

  if return_code < LIBUSB_SUCCESS then
  begin
    if return_code<>LIBUSB_ERROR_TIMEOUT then WriteLn('libusbhid_interrupt_read. failed! return code: ',return_code)
{$ifdef DEBUG_MSG}
		else DBG_MSG(Format('%s libusbhid_interrupt_read. TIMEOUT bytes read: %d',[FormatDateTime(TIME_FORMAT,Now()),Result]))
{$endif}
  end
{$ifdef DEBUG_MSG}
  else DBG_MSG(Format('%s libusbhid_interrupt_read. received: %d bytes from device ',[FormatDateTime(TIME_FORMAT,Now()),Result]));
{$endif}
end;

function libusbhid_open_device(vid,pid:word; instanceId:Tuint8; out hid_device_context:libusbhid_context):boolean;
var
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
{$ifdef DEBUG_MSG}DBG_MSG('Cannot open libusb 1.0 library. I really need this..');{$endif}
    end
    else
    begin
      libusb_set_debug(hid_device_context.usb_context,3);

      usb_device_count:=libusb_get_device_list(usb_context,@usb_device_list);

{$ifdef DEBUG_MSG}DBG_MSG(Format('Found %d devices attached',[usb_device_count]));{$endif}

{go through hid_device_context list and open the appropriate instance}
      devIdx:=libusbhid_get_index_of_device_from_list(usb_device_list, vid,pid,instanceId);

{$ifdef DEBUG_MSG}DBG_MSG(Format('Index of device %d:%d=%d',[vid,pid,devIdx]));{$endif}
  {cannot use this convenience call to open multiple instance of a hid_device_context with same vid/pid
  		usb_device_handle:=libusb_open_device_with_vid_pid(usb_context,vid,pid);}
      if devIdx>=0 then
      begin
        {SP sep 2015 - this was the bug - usb_device_list[devIdx] instead of @usb_device_list[devIdx]^}
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
{kernel driver attaching problem; device may open but still be busy - this attempts to go around that -
I have never been able to fully test so - beware}
{$ifdef DEBUG_MSG}DBG_MSG('device attempting go clear halt on ep $81');{whatever.. seems to fail everytime anyway}       {$endif}

        res:=libusb_clear_halt(usb_device_handle, $81);

{$ifdef DEBUG_MSG}
        if res=LIBUSB_SUCCESS then DBG_MSG('clear halt successful')
        else DBG_MSG(Format('clear halt failed (it''s ok, endpoint was NOT busy); error result: %d',[res]));//I've never seen this succeeding :(; eh whatever
{$endif}

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
			      LIBUSB_ERROR_NOT_FOUND:	DBG_MSG('not found');
			      LIBUSB_ERROR_BUSY:		DBG_MSG('busy');
			      LIBUSB_ERROR_NO_DEVICE:	DBG_MSG('no device');
			      else DBG_MSG('for some reasons');
          end;
        end
{$endif}

      end;
    end;
  end;
end;

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);
begin
  with hid_device_context do
  begin
    if usb_lib_init_result=LIBUSB_SUCCESS then
    begin
      if usb_device_handle<>nil then
      begin
        if (usb_interface_result=LIBUSB_SUCCESS) then
        begin

          //libusb_free_transfer(transfer);{???}

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


