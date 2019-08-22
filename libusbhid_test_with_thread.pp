program libusbhid_test_with_thread;
{
S. V. Pantazi (svpantazi@gmail.com), 2013


updated
08/21/2019
08/13/2019  }

{$mode objfpc}{$H+}

uses
  heaptrc,{to hunt for memory leaks}

  {$IFDEF UNIX}
  {$DEFINE UseCThreads}
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  keyboard,
  sysutils,
  classes,
  libusbhid,

  hid_testing_utils;

const
   REPORT_IDX=0;
   WACOM_INTERRUPT_REPORT_LENGTH=10;

var
  criticalSection:TRTLCriticalSection;

  shared_position_data:	record
    XPosition,
    YPosition:	Word;
  end;

type

	TInterruptReadThread=class(TThread)
  private
  protected
    procedure Execute; override;
  public
  end;

procedure TInterruptReadThread.Execute;
begin
  repeat
    {interrupt reading   - for joystick or wiimote, or touchscreens, etc.
    NOTE: thread execution is blocked if timeout=0, until data is read from device! that makes it difficult to terminate the thread!}
    //simple devices often use one endpoint (commonly $81) to output data reports
    hidReportData[REPORT_IDX].dataLen:=libusbhid_interrupt_read(device_context,$81{endpoint},{out}hidReportData[REPORT_IDX].hid_data,WACOM_INTERRUPT_REPORT_LENGTH{128,report length, varies by device},1000);
    if hidReportData[REPORT_IDX].dataLen>0 then
    begin
{use critical section to prevent other threads read access to the shared data while writing/updating it}
      EnterCriticalsection(criticalSection);
    	Move(hidReportData[REPORT_IDX].hid_data[5],shared_position_data.XPosition,2);
    	Move(hidReportData[REPORT_IDX].hid_data[7],shared_position_data.YPosition,2);
      LeaveCriticalsection(criticalSection);
    end;
    //PrintAndCompareReport(REPORT_IDX);

  until Terminated;
end;


procedure SetWACOMTabletMode();
const
  WACOM_FEATURE_REPORT_NUMBER=2; //to set features
  WACOM_TABLET_MODE_FINGER_ENABLED=1; //Works, sets the pad mode (multi touch+buttons)
  WACOM_TABLET_MODE_PENABLED=3;				//works, sets the pen mode (position+pressure, NO buttons)
//	WACOM_TABLET_MODE_3=3;				//NOT WORKING

  WACOM_FEATURE_REPORT_LENGTH=2;

begin
  hidReportData[WACOM_FEATURE_REPORT_NUMBER].hid_data[0]:= WACOM_FEATURE_REPORT_NUMBER;{report number/ID};
  hidReportData[WACOM_FEATURE_REPORT_NUMBER].hid_data[1]:=
  //{setting tablet mode} - Pen Mode or Pad/Finger mode
  //  			WACOM_TABLET_MODE_FINGER_ENABLED
      WACOM_TABLET_MODE_PENABLED
  //      	WACOM_TABLET_MODE_3 - not working! not available?
																			  ;
  hidReportData[WACOM_FEATURE_REPORT_NUMBER].dataLen:=libusbhid_set_report(device_context,	HID_REPORT_TYPE_FEATURE{=$03},	WACOM_FEATURE_REPORT_NUMBER{=2}, 	WACOM_FEATURE_REPORT_LENGTH{=2},hidReportData[WACOM_FEATURE_REPORT_NUMBER].hid_data);
  Sleep(100);//some delay between writing and reading feature report
  hidReportData[WACOM_FEATURE_REPORT_NUMBER].dataLen:=libusbhid_get_report(device_context,  	HID_REPORT_TYPE_FEATURE, 	WACOM_FEATURE_REPORT_NUMBER, 	WACOM_FEATURE_REPORT_LENGTH,hidReportData[WACOM_FEATURE_REPORT_NUMBER].hid_data);
end;

var
  i:integer;
  readThread:TInterruptReadThread;

begin
  keyboard.InitKeyboard();

  if libusbhid_open_device(
        // $046D, $C216 {xbox gamepad }
        // $0000, $0001 {barcode scanner}
	      // $24C0, $0003 {weather station}
	      // $1784, $0001 {}
         $056a, $00de {Wacom Bamboo}
	      // $04e7, $0050  {elo touch screen}
        // $051d,$0002 {APC UPS}

				,{instance=}1,device_context) then
  begin

    readThread:=TInterruptReadThread.Create(false);
{read report until keypressed then close the device}
    repeat
      ThumbTwiddle(); //delay

{Setting and reading the feature report over and over every second does not make sense;
this is just to prove that interrupt reading in a thread and writing to device can be done concurrently}
      SetWACOMTabletMode();

{use critical section to prevent other thread write access to the shared data while reading it}
      EnterCriticalsection(criticalSection);
      WriteLn('WACOM position  X=',shared_position_data.XPosition,', Y=',shared_position_data.YPosition);
      LeaveCriticalsection(criticalSection);

    until KeyPressed {or intSignal};

    readThread.Terminate;
    readThread.Free();
		libusbhid_close_device(device_context);
	end
  else WriteLn('unable to open device');

  keyboard.DoneKeyboard;
end.

