program libusbhid_test;
{
S. V. Pantazi (svpantazi@gmail.com), 2013

updated
	08/13/2019
  08/16/2019
}

{$mode objfpc}{$H+}

uses
  heaptrc,{to hunt for memory leaks}

  {$IFDEF UNIX}
  {$DEFINE UseCThreads}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  crt,
  sysutils,

  libusbhid,

  hid_testing_utils;

begin
  if libusbhid_open_device(
        // $046D, $C216 {xbox gamepad }
        // $0000, $0001 {barcode scanner}
        // $24C0, $0003 {weather station}
        // $1784, $0001 {}
        // $056a, $00de {Wacom Bamboo}
        // $04e7, $0050  {elo touch screen}
        // $051d, $0002 {APC UPS}
        $10CE, $EB93  {WHB04B-4 CNC Handwheel}
         ,{instance=}1,device_context) then
  begin

    reportIdx:=0; //devices often use one endpoint (commonly $81) to output data reports

{read report until keypressed, then closes the device}
    repeat
      {interrupt reading   - for joystick or wiimote, or touchscreens, etc.
      NOTE: program execution is blocked until data is read from device!}
      hidReportData[reportIdx].dataLen:=libusbhid_interrupt_read(device_context,$81{endpoint},{out}hidReportData[reportIdx].hid_data,128{report length, varies by device}, {timeout=}50);
      If hidReportData[reportIdx].dataLen > 0 Then
         PrintAndCompareReport(reportIdx);

    until KeyPressed;

		libusbhid_close_device(device_context);
	end
  else WriteLn('unable to open device')
end.

