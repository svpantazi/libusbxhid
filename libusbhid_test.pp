program libusbhid_test;
{
S. V. Pantazi (svpantazi@gmail.com), 2013

updated 08/13/2019  }

{$mode objfpc}{$H+}

uses
  heaptrc,{to hunt for memory leaks}

  {$IFDEF UNIX}
  {$DEFINE UseCThreads}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  sysutils,
  libusbhid;

const
  REPORT_COUNT=1;
  REPORT_LENGTH=128;

type
  THIDDeviceReportData=array[0..REPORT_COUNT-1] of record
    dataLen: Integer;
    hid_data,
    hid_data_copy:array[0..REPORT_LENGTH-1] of byte;
  end;

var
  reportIdx,
  i:integer;
  device_context:libusbhid_context;
  hidReportData:THIDDeviceReportData;
  dataString:string;

  procedure PrintAndUpdateReport(reportIdx:integer);
  var
    i:  Integer;
  begin
    dataString:='';

    for i:=0 to hidReportData[reportIdx].dataLen-1 do
    begin
      if hidReportData[reportIdx].hid_data[i]=hidReportData[reportIdx].hid_data_copy[i] then
      begin
        Write('. ,');
        dataString+='__';
      end
      else
      begin
        Write(Format('%0.2s,',[hexStr(hidReportData[reportIdx].hid_data[i],2)]));
        dataString+=hexStr(hidReportData[reportIdx].hid_data[i],2);
        hidReportData[reportIdx].hid_data_copy[i]:=hidReportData[reportIdx].hid_data[i];
      end;
    end;
    WriteLn();
  end;


begin
  if libusbhid_open_device(
        // $046D, $C216 {xbox gamepad }
        // $0000, $0001 {barcode scanner}
	      // $24C0, $0003 {}
	      // $1784, $0001 {}
        // $056a, $00de {Wacom Bamboo}
	      $04e7, $0050  {elo touch screen}
          																		,{instance=}1,device_context) then
  begin

    reportIdx:=0; //simple devices often use one endpoint (commonly $81) to output data reports

{read report 5 times, then close the device}
    for i:=0 to 4 do
    begin
      {interrupt reading   - for joystick or wiimote, or touchscreens, etc.
      NOTE: program execution is blocked until data is read from device!}
      hidReportData[reportIdx].dataLen:=libusbhid_interrupt_read(device_context,$81{endpoint},{out}hidReportData[reportIdx].hid_data,128{report length, varies by device});

      PrintAndUpdateReport(reportIdx);
    end;
		libusbhid_close_device(device_context);
	end
  else WriteLn('unable to open device')
end.

