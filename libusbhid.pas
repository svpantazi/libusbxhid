unit libusbhid;

{<Implements a subset of HID calls using libusb. No other dependencies except libusb.}

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

{$mode objfpc}{$H+}

{$packrecords C}

interface

{$MACRO ON}

uses libusbx, sysutils;

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

//  Plibusbhid_context=^libusbhid_context;
  libusbhid_context=record
    usb_context:          Plibusb_context;
    usb_interface_result: longint;
    usb_driver_detached:  boolean;
    usb_lib_result:       longint;
    usb_device_handle:    Plibusb_device_handle;
  end;

Var
   Show_LibUSB_Messages:Boolean = True;

Procedure Writeln_Option(Writeln_Data:String);

function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; vid,pid:word; instanceId:Tuint8):Tsint16;
{<Loads all attached devices in a device list; libusb_device is an opaque record, cannot use its content, but each device gets one and can use it further to get a bus number and address of a device,
but most importantly a device descriptor that can be checked for vid and pid of the desired device}

function  libusbhid_open_device(vid,pid:word; instanceId:Tuint8; out hid_device_context:libusbhid_context):boolean;
{<Opens a device instance given by the instance id (starts at 1) of a device with a given vid and pid. The instance id is necessary if multiple identical devices exist on the same system.}

function  libusbhid_get_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; out report_data:array of byte):longint;
function  libusbhid_set_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; var report_data:array of byte):longint;

function  libusbhid_interrupt_read(var hid_device_context:libusbhid_context; in_endpoint:byte; out data_from_device{array of byte}; const data_length:byte; const timeout:dword):longint;
{<Waits for data to be read from device. If timeout=0 then this is a blocking read and ideally belongs in a thread.}

function  libusbhid_interrupt_write(var hid_device_context:libusbhid_context; out_endpoint:byte; var data_into_device:array of byte; const data_length:byte; const timeout:dword):longint;
{<Writes data into the device.}

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);



implementation

Procedure Writeln_Option(Writeln_Data:String);
  Begin
    If Show_LibUSB_Messages Then
       Writeln(Writeln_Data);
  End;


function  libusbhid_get_index_of_device_from_list(device_list:PPlibusb_device; vid,pid:word; instanceId:Tuint8):Tsint16;
var
  busNumber,devAddress:Tuint8;
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
    {debug}//Writeln_Option('Size of usb descriptor record: ',sizeof(usb_descriptor));
      if (descriptor_result < 0) then
      begin
      Writeln_Option('failed to get device descriptor');
         exit;
      end;
    busNumber:=libusb_get_bus_number(usb_device);
    devAddress:=libusb_get_device_address(usb_device);
    Writeln_Option(Format('%0.4x:%0.4x, bus: %d, address: %d',[usb_descriptor.idVendor, usb_descriptor.idProduct,busNumber,devAddress]));
    if (usb_descriptor.idVendor=vid) and (usb_descriptor.idProduct=pid) then
    begin
      Writeln_Option(Format('Found device with vid:pid %d:%d at idx:%d!',[vid,pid,i]));
      if (instanceId-instanceCounter)=0 then
      begin
        Writeln_Option(Format('Device instance found: %d  at idx:%d!',[instanceId,i]));
        Result:=i;
      end;
      Inc(instanceCounter);
    end;
    Inc(i);
	end;
end;

function libusbhid_get_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; out report_data:array of byte):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,
    LIBUSB_CONTROL_REQUEST_TYPE_IN,
    HID_GET_REPORT,
    (reportType << 8) or reportNum,
    0 {interface_num}, @report_data, reportLen{sizeof(data)},0{no timeout});
  if Result < 0 then Writeln_Option('control transfer from usb device failed!')
  else Writeln_Option('received:'+IntToStr(Result)+' bytes from device ');
end;

function libusbhid_set_report(var hid_device_context:libusbhid_context; reportType:byte; reportNum:byte; reportLen:word; var report_data:array of byte):longint;
begin
  Result:=libusb_control_transfer(hid_device_context.usb_device_handle,LIBUSB_CONTROL_REQUEST_TYPE_OUT{????},HID_SET_REPORT,(reportType << 8) or reportNum, 0{interface_num}, @report_data, reportLen{sizeof(data)},0{no timeout});
  if Result < 0 then Writeln_Option('control transfer to usb device failed!')
  else Writeln_Option('written:'+IntToStr(Result)+' bytes to device ');
end;

function libusbhid_interrupt_write(var hid_device_context:libusbhid_context; out_endpoint:byte; var data_into_device:array of byte; const data_length:byte; const timeout:dword):longint;
var
  return_code:longint;
begin
  return_code:=libusb_interrupt_transfer(hid_device_context.usb_device_handle,out_endpoint,@data_into_device, data_length, @Result,timeout);
  if return_code < 0 then
  begin
    if return_code<>LIBUSB_ERROR_TIMEOUT then Writeln_Option('interrupt write to usb device failed!')
  end
  else Writeln_Option('sent:'+IntToStr(Result)+' bytes to device ');
end;

function libusbhid_interrupt_read(var hid_device_context:libusbhid_context; in_endpoint:byte; out data_from_device{:array of byte}; const data_length:byte; const timeout:dword):longint;
var
  return_code:longint;
begin
  return_code:=libusb_interrupt_transfer(hid_device_context.usb_device_handle,in_endpoint,@data_from_device, data_length, @Result, timeout);
  if return_code < 0 then
  begin
    if return_code<>LIBUSB_ERROR_TIMEOUT then Writeln_Option('interrupt read from usb device failed!')
  end
  else Writeln_Option('received:'+IntToStr(Result)+' bytes from device ');
end;

function libusbhid_open_device(vid,pid:word; instanceId:Tuint8; out hid_device_context:libusbhid_context):boolean;
var
  devIdx: longint;
  usb_device_list:PPlibusb_device;
  active_config,
  usb_device_count: longint;
begin
  Result:=false;//so pessimistic...

  with hid_device_context do
  begin
    usb_driver_detached:=false;

    usb_lib_result:=libusb_init(@usb_context);

    if usb_lib_result<0 then Writeln_Option('Cannot open libusb 1.0 library. I really need this..')
    else
    begin
      libusb_set_debug(hid_device_context.usb_context,3);
      usb_device_count:=libusb_get_device_list(usb_context,@usb_device_list);
      Writeln_Option('Found '+InttoStr(usb_device_count)+' devices attached');
      {PRINTING the hid_device_context list seems to create an enourmous amount of problems - probably even writing program memory through the calls to libusb_get_device_descriptor}
      //go through hid_device_context list and open the appropriate instance
      devIdx:=libusbhid_get_index_of_device_from_list(usb_device_list, vid,pid,instanceId);
      Writeln_Option('Index of device '+InttoStr(vid)+':'+InttoStr(pid)+'='+InttoStr(devIdx));
  {cannot use this convenience call to open multiple instance of a hid_device_context with same vid/pid
  		usb_device_handle:=libusb_open_device_with_vid_pid(usb_context,vid,pid);}
      if devIdx>=0 then
      begin
        {SP sep 2015 - this was the bug - usb_device_list[devIdx] instead of @usb_device_list[devIdx]^}
        if libusb_open(@usb_device_list[devIdx]^,@usb_device_handle)=0 then Writeln_Option('Device opened. Next I must claim the interface.')
        else Writeln_Option('Cannot open device. Reasons? many...');
      end
      else
      begin
        Writeln_Option('Cannot open device with idx:'+InttoStr(devIdx));
        usb_device_handle:=nil;
      end;

{whatever the outcome of opening the device, the device list must be freed - or memory leaks will ensue}
      Writeln_Option('Freeing device list with '+InttoStr(usb_device_count)+' devices');
      libusb_free_device_list(usb_device_list,{NOT usb_device_count}1{unreference? 0=no, 1=yes}); //free the list, unref the devices in it?
      Writeln_Option('USB device list freed. good boy!');

      if usb_device_handle=nil then Writeln_Option('Cannot open usb device '+Format('%0.4x:%0.4x',[vid,pid]))
      else
      begin
{kernel driver attaching problem; device may open but still be busy - this attempts to go around that -
I have never been able to fully test so - beware}
        Writeln_Option('device attempting go clear halt on ep $81');//whatever.. seems to fail everytime

        if libusb_clear_halt(usb_device_handle, $81)=0 then Writeln_Option('clear halt successful')
        else Writeln_Option('clear halt failed');//I've never seen this succeeding :(; eh whatever

{        if libusb_auto_detach_kernel_driver(usb_device_handle,1{enable autodetach})=0 then Writeln_Option('Setting autodetach kernel driver')
        else}
        begin
//          Writeln_Option('Autodetach did not work. Checking if kernel driver is active...?');
{device busy? try to detach kernel driver so I can claim the interface}
          if (libusb_kernel_driver_active(usb_device_handle,0{interface number})=1) then
          begin
             Writeln_Option('device busy - driver active');
             if (libusb_detach_kernel_driver(usb_device_handle,0{interface number})=0) then
             begin
                Writeln_Option ('driver detached');
                usb_driver_detached:=true;
             end;
          end
          else Writeln_Option('driver inactive - can claim interface');
        end;
        Writeln_Option('getting configuration....');
        if libusb_get_configuration(usb_device_handle,@active_config)=0 then
        begin
          Writeln_Option('active config:'+InttoStr(active_config));
          if active_config<>1 then
          begin
            if libusb_set_configuration (usb_device_handle, 1)=0 then Writeln_Option('Configuration set')
            else Writeln_Option('Could not set configuration.');
          end
        end
        else Writeln_Option('Error getting configuration.');

{some devices may want firmware uploads, configurations, hardware initialization and setup etc. for simple IO devices, nevermind this}
    //    libusb_set_configuration(usb_device_handle, 0);

        Writeln_Option('Claiming interface.....fingers crossed...');
        usb_interface_result:=libusb_claim_interface(usb_device_handle, 0);
        if usb_interface_result=0 then
        begin
          Writeln_Option ('Interface claimed ... yay!');
          Result:=true;//success
        end
        else
        begin
          Writeln_Option ('Cannot claim interface - drat!');
          case usb_interface_result of
             LIBUSB_ERROR_NOT_FOUND:  Writeln_Option('not found');
             LIBUSB_ERROR_BUSY:       Writeln_Option('busy');
             LIBUSB_ERROR_NO_DEVICE:  Writeln_Option('no device');
             else Writeln_Option('other');
          end;
        end
      end;
    end;
  end;
end;

procedure libusbhid_close_device(var hid_device_context:libusbhid_context);
begin
  with hid_device_context do
  begin
    if usb_lib_result>=0 then
    begin
      if usb_device_handle<>nil then
      begin
        if (usb_interface_result>=0) then
        begin

          //libusb_free_transfer(transfer);{???}

          usb_interface_result:=libusb_release_interface(usb_device_handle, 0);
          if(usb_interface_result<>0) then Writeln_Option('Cannot release interface. be nice to know why?'){middle of transfer, pants down, etc.?}
          else Writeln_Option('Interface released. Phew..');
          if usb_driver_detached then
          begin
            if libusb_attach_kernel_driver(usb_device_handle,0)=0 then Writeln_Option('Driver re-attached. Good boy.');
          end;
        end;

        libusb_close(usb_device_handle);
      end;

      libusb_exit(usb_context);
    end;
  end;
end;


end.

