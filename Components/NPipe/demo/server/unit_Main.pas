//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit unit_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NPipe_Server,NPipe_Types, ComCtrls, Buttons;

type
  TForm4 = class(TForm)
    NPipeServer1: TNPipeServer;
    cb_LocalAccess: TCheckBox;
    cb_Active: TCheckBox;
    ed_PipeName: TLabeledEdit;
    lbl_Timeout: TLabel;
    tb_Timeout: TTrackBar;
    gb_PB: TGroupBox;
    PB: TProgressBar;
    gb_IncomingData: TGroupBox;
    rb_SaveToFile: TRadioButton;
    ed_FileName: TEdit;
    rb_ShowMemo: TRadioButton;
    mem_Data: TMemo;
    gb_Reply: TGroupBox;
    rb_ReplySize: TRadioButton;
    rb_ReplyData: TRadioButton;
    rb_NoReply: TRadioButton;
    procedure NPipeServer1EndWork(Sender: TObject);
    procedure NPipeServer1StartWork(Sender: TObject; bytesTotal: Int64;
      WorkMode: TNPWorkMode);
    procedure NPipeServer1Progress(Sender: TObject; bytesCurrent: Int64);
    procedure ed_PipeNameChange(Sender: TObject);
    procedure cb_ActiveClick(Sender: TObject);
    procedure cb_LocalAccessClick(Sender: TObject);
    procedure tb_TimeoutChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NPipeServer1IncomingData(Sender: TObject; Data: TMemoryStream;
      var Reply: TMemoryStream);
    procedure NPipeServer1Error(Sender: TObject; AException: Exception);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.NPipeServer1Error(Sender: TObject; AException: Exception);
begin
  MessageDlg(AException.Message, mtError, [mbOK], 0);
end;


procedure TForm4.NPipeServer1IncomingData(Sender: TObject; Data: TMemoryStream;
  var Reply: TMemoryStream);
var
  sl : TStrings;
begin
  if (rb_SaveToFile.Checked) then
  begin
    try
      Data.SaveToFile(ed_FileName.Text);
    except
      on e:Exception do
        MessageDlg(e.Message, mtError, [mbOK], 0);
    end;
  end
  else
    mem_Data.Lines.LoadFromStream(Data);

  if (rb_ReplySize.Checked) then
  begin
    sl:=TStringList.Create;
    sl.Add('Incoming data at '+TimeToStr(Now)+': '+IntToStr(Data.Size)+' bytes');
    sl.SaveToStream(Reply);
    sl.Free;
  end
  else if (rb_ReplyData.Checked) then
    Reply.LoadFromStream(Data);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ed_PipeName.Text:=NPipeServer1.PipeName;
  ed_FileName.Text:='c:\temp\pipe_content.txt';
  cb_LocalAccess.Checked:=NPipeServer1.LocalAccessOnly;
  if (NPipeServer1.Timeout<=tb_Timeout.Max) then
    tb_Timeout.Position:=NPipeServer1.Timeout;
  tb_TimeoutChange(Sender);
end;

procedure TForm4.tb_TimeoutChange(Sender: TObject);
begin
  NPipeServer1.Timeout:=tb_Timeout.Position;
  lbl_Timeout.Caption:='Timeout: '+IntToStr(tb_Timeout.Position);
end;

procedure TForm4.cb_LocalAccessClick(Sender: TObject);
begin
  NPipeServer1.LocalAccessOnly:=cb_LocalAccess.Checked;
end;

procedure TForm4.cb_ActiveClick(Sender: TObject);
begin
  NPipeServer1.Active:=cb_Active.Checked;
end;

procedure TForm4.ed_PipeNameChange(Sender: TObject);
begin
  NPipeServer1.PipeName:=ed_PipeName.Text;
end;

procedure TForm4.NPipeServer1Progress(Sender: TObject; bytesCurrent: Int64);
begin
  PB.Position:=bytesCurrent;
end;

procedure TForm4.NPipeServer1StartWork(Sender: TObject; bytesTotal: Int64;
  WorkMode: TNPWorkMode);
begin
  pb.Max:=bytesTotal;
end;

procedure TForm4.NPipeServer1EndWork(Sender: TObject);
begin
  pb.Position:=pb.Min;
end;

end.
