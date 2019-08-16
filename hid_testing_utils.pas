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

unit hid_testing_utils;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  libusbhid;

const
  REPORT_COUNT=16;
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

  procedure PrintAndCompareReport(reportIdx:integer);
  function ReportChanged(reportNum:byte):boolean;

implementation

procedure PrintAndCompareReport(reportIdx:integer);
var
  i:  Integer;
begin
//  dataString:='';

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

function ReportChanged(reportNum:byte):boolean;
var
  i:  Integer;
begin
  result:=false;
  for i:=0 to hidReportData[reportIdx].dataLen-1 do
  begin
    if hidReportData[reportNum].hid_data[i]<>hidReportData[reportNum].hid_data_copy[i] then
    begin
      result:=true;
      exit;
    end;
  end;
end;


end.

