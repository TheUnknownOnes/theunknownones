unit uFormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uWiimote, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    btn_Connnect: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn_ConnnectClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    Conn : TwmDeviceConnection;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn_ConnnectClick(Sender: TObject);
var
  leds : TwmOutputReportLEDs;
  reporting : TwmOutputReportReadMemory;
  inrep : TwmReport;
begin
  if Conn.Connected then
    Conn.Disconnect
  else
  begin
    Conn.Connect(ListBox1.Items[ListBox1.ItemIndex]);

    leds := TwmOutputReportLEDs.Create;
    leds.LEDsOn := [];
    conn.WriteReport(leds);
    leds.Free;

    reporting := TwmOutputReportReadMemory.Create;
    reporting.Address := $40FF;
    reporting.Size := 8;
    reporting.Rumble := False;
    conn.WriteReport(reporting);
    reporting.Free;

    while true do
    begin
      if conn.ReadReport(inrep) then
      begin
        Memo1.Lines.Add(inrep.ClassName);

        if inrep is TwmInputReportButtonsAccel then
        begin
          if wmbHome in TwmInputReportButtonsAccel(inrep).ButtonsDown then
            break;
        end;

        inrep.Free;
      end;

      Application.ProcessMessages;
    end;

    inrep.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;                                               

  ReportMemoryLeaksOnShutdown := true;

  Conn := TwmDeviceConnection.Create;

  conn.ListDevices(ListBox1.Items);

  ListBox1.ItemIndex := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  conn.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if Conn.Connected then
    btn_Connnect.Caption := 'Disconnect'
  else
    btn_Connnect.Caption := 'Connect';
end;

procedure TForm1.Timer2Timer(Sender: TObject);
var
  rep : TwmReport;
begin
  if conn.Connected and conn.ReadReport(rep) then
  begin
    Memo1.Lines.Add(rep.ClassName);
    rep.Free;
  end;
end;

end.
