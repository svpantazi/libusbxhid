{based on information at  http://libusb.sourceforge.net/api-1.0/hotplug.html

tested on Linux CentOS 7}

program libusbx_hotplug_test;
{S. V. Pantazi (svpantazi@gmail.com), 08/27/2019}

{$mode objfpc}{$H+}

uses
  heaptrc,{to hunt for memory leaks}

  {$IFDEF UNIX}
  {$DEFINE UseCThreads}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  sysutils,
  libusbx,
  libusbhid,
  hid_testing_utils;


var
count: integer= 0;

function hotplug_callback_test(ctx:Plibusb_context; device: Plibusb_device; event:libusb_hotplug_event; user_data:pointer):longint;
var
	libusb_handle:Plibusb_device_handle=nil;
	desc:libusb_device_descriptor;
	res:integer;

begin
  libusb_get_device_descriptor(device, @desc);
  case event of
  	LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED:
    begin
      WriteLn('hotplug callback - device arrived');
	    res:= libusb_open(device, @libusb_handle);
    	if (LIBUSB_SUCCESS <> res) then WriteLn('Could not open USB device')
  	end;
		LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT:
    begin
      WriteLn('hotplug callback - device left');
    	if libusb_handle<>nil then
      begin
	      libusb_close(libusb_handle);
	      libusb_handle:=nil;
    	end
    end;
  	else WriteLn(Format('Unhandled hotplug event %d',[ event]));
  end;
  Inc(count);
  Result:=0;
end;


procedure libusbhid_device_hotplug_test(vid,pid:word);
var
  res:longint;
  hotplug_handle:libusb_hotplug_callback_handle;
begin
  res:=libusb_init(nil);
  if res < LIBUSB_SUCCESS then
  begin
{$ifdef DEBUG_MSG}DBG_MSG('Cannot open libusb 1.0 library. You really need this..');{$endif}
  end
  else
  begin
	  res:=libusb_hotplug_register_callback(nil, LIBUSB_HOTPLUG_EVENT_DEVICE_ARRIVED or LIBUSB_HOTPLUG_EVENT_DEVICE_LEFT, {LIBUSB_HOTPLUG_NO_FLAGS}LIBUSB_HOTPLUG_ENUMERATE, vid,pid,-1{=LIBUSB_HOTPLUG_MATCH_ANY}, @hotplug_callback_test, nil,@hotplug_handle);
    if res=LIBUSB_SUCCESS then WriteLn('Callback registered');

    while (count < 3)  do
    begin
    	libusb_handle_events_completed(nil, nil);
      WriteLn('hotplug waiting....',count);
 			Sleep(1000);
    end;

    libusb_hotplug_deregister_callback(nil, @hotplug_handle);
    WriteLn('Callback de-registered');

    {we're done, must exit libusb as well}
 		libusb_exit(nil);
	end;
end;


begin

	libusbhid_device_hotplug_test($056a, $00de);

end.

